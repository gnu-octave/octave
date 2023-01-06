////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <istream>
#include <ostream>
#include <string>

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "oct-time.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "load-save.h"
#include "ls-oct-binary.h"
#include "ls-utils.h"
#include "ov-cell.h"
#include "ov.h"
#include "pager.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

static bool
load_inline_fcn (std::istream& is, bool swap, octave::mach_info::float_format,
                 octave_value& retval)
{
  int32_t nargs;
  if (! is.read (reinterpret_cast<char *> (&nargs), 4))
    return false;
  if (swap)
    swap_bytes<4> (&nargs);

  if (nargs < 1)
    return false;
  else
    {
      int32_t tmp;
      octave_value_list args (nargs+1);
      for (int i = 0; i < nargs; i++)
        {
          if (! is.read (reinterpret_cast<char *> (&tmp), 4))
            return false;
          if (swap)
            swap_bytes<4> (&tmp);

          OCTAVE_LOCAL_BUFFER (char, ctmp, tmp+1);
          is.read (ctmp, tmp);
          args(i+1) = std::string (ctmp);

          if (! is)
            return false;
        }

      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
        return false;
      if (swap)
        swap_bytes<4> (&tmp);

      OCTAVE_LOCAL_BUFFER (char, ctmp1, tmp+1);
      is.read (ctmp1, tmp);
      // NAME is obsolete and unused.
      // std::string name (ctmp1);

      if (! is)
        return false;

      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
        return false;
      if (swap)
        swap_bytes<4> (&tmp);

      OCTAVE_LOCAL_BUFFER (char, ctmp2, tmp+1);
      is.read (ctmp2, tmp);

      if (is)
        {
          args(0) = std::string (ctmp2);

          octave::interpreter& interp = octave::__get_interpreter__ ();

          octave_value_list tmp_inl = interp.feval ("inline", args, 1);

          if (tmp_inl.length () > 0)
            {
              retval = tmp_inl(0);
              return true;
            }
        }
    }

  return false;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.  If the value
// is tagged as global in the file, return TRUE in GLOBAL.  If SWAP
// is TRUE, swap bytes after reading.
//
// The data is expected to be in the following format:
//
// Header (one per file):
// =====================
//
//   object               type            bytes
//   ------               ----            -----
//   magic number         string             10
//
//   float format         integer             1
//
//
// Data (one set for each item):
// ============================
//
//   object               type            bytes
//   ------               ----            -----
//   name_length          integer             4
//
//   name                 string    name_length
//
//   doc_length           integer             4
//
//   doc                  string     doc_length
//
//   global flag          integer             1
//
//   data type            char                1
//
// In general "data type" is 255, and in that case the next arguments
// in the data set are
//
//   object               type            bytes
//   ------               ----            -----
//   type_length          integer             4
//
//   type                 string    type_length
//
// The string "type" is then used with octave::type_info::lookup_type
// to create an octave_value of the correct type.  The specific load/save
// function is then called.
//
// For backward compatibility "data type" can also be a value between 1
// and 7, where this defines a hardcoded octave_value of the type
//
//   data type                  octave_value
//   ---------                  ------------
//   1                          scalar
//   2                          matrix
//   3                          complex scalar
//   4                          complex matrix
//   5                          string   (old style storage)
//   6                          range
//   7                          string
//
// Except for "data type" equal 5 that requires special treatment, these
// old style "data type" value also cause the specific load/save functions
// to be called.  FILENAME is used for error messages.

std::string
read_binary_data (std::istream& is, bool swap,
                  octave::mach_info::float_format fmt,
                  const std::string& filename, bool& global,
                  octave_value& tc, std::string& doc)
{
  std::string retval;

  unsigned char tmp = 0;

  int32_t name_len = 0;
  int32_t doc_len = 0;

  doc.clear ();

  // We expect to fail here, at the beginning of a record, so not
  // being able to read another name should not result in an error.

  is.read (reinterpret_cast<char *> (&name_len), 4);
  if (! is)
    return retval;
  if (swap)
    swap_bytes<4> (&name_len);

  {
    OCTAVE_LOCAL_BUFFER (char, name, name_len+1);
    name[name_len] = '\0';
    if (! is.read (reinterpret_cast<char *> (name), name_len))
      error ("load: trouble reading binary file '%s'", filename.c_str ());
    retval = name;
  }

  is.read (reinterpret_cast<char *> (&doc_len), 4);
  if (! is)
    error ("load: trouble reading binary file '%s'", filename.c_str ());
  if (swap)
    swap_bytes<4> (&doc_len);

  {
    OCTAVE_LOCAL_BUFFER (char, tdoc, doc_len+1);
    tdoc[doc_len] = '\0';
    if (! is.read (reinterpret_cast<char *> (tdoc), doc_len))
      error ("load: trouble reading binary file '%s'", filename.c_str ());
    doc = tdoc;
  }

  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    error ("load: trouble reading binary file '%s'", filename.c_str ());
  global = (tmp ? 1 : 0);

  tmp = 0;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    error ("load: trouble reading binary file '%s'", filename.c_str ());

  octave::type_info& type_info = octave::__get_type_info__ ();

  // All cases except 255 kept for backwards compatibility
  switch (tmp)
    {
    case 1:
      tc = type_info.lookup_type ("scalar");
      break;

    case 2:
      tc = type_info.lookup_type ("matrix");
      break;

    case 3:
      tc = type_info.lookup_type ("complex scalar");
      break;

    case 4:
      tc = type_info.lookup_type ("complex matrix");
      break;

    case 5:
      {
        // FIXME:
        // This is cruft, since its for a save type that is old.
        // Maybe this is taking backward compatibility too far!
        int32_t len;
        if (! is.read (reinterpret_cast<char *> (&len), 4))
          error ("load: trouble reading binary file '%s'", filename.c_str ());
        if (swap)
          swap_bytes<4> (&len);
        OCTAVE_LOCAL_BUFFER (char, s, len+1);
        if (! is.read (reinterpret_cast<char *> (s), len))
          error ("load: trouble reading binary file '%s'", filename.c_str ());
        s[len] = '\0';
        tc = s;

        // Early return, since don't want rest of this function
        return retval;
      }
      break;

    case 6:
      tc = type_info.lookup_type ("range");
      break;

    case 7:
      tc = type_info.lookup_type ("string");
      break;

    case 255:
      {
        // Read the saved variable type
        int32_t len;
        if (! is.read (reinterpret_cast<char *> (&len), 4))
          error ("load: trouble reading binary file '%s'", filename.c_str ());
        if (swap)
          swap_bytes<4> (&len);
        OCTAVE_LOCAL_BUFFER (char, s, len+1);
        if (! is.read (s, len))
          error ("load: trouble reading binary file '%s'", filename.c_str ());
        s[len] = '\0';
        std::string typ = s;

        if (typ == "inline function")
          {
            // Special case for loading old octave_inline_fcn objects.
            if (! load_inline_fcn (is, swap, fmt, tc))
              error ("load: trouble reading binary file '%s'", filename.c_str ());
            return retval;
          }

        tc = type_info.lookup_type (typ);
      }
      break;
    default:
      error ("load: trouble reading binary file '%s'", filename.c_str ());
      break;
    }

  if (! tc.load_binary (is, swap, fmt))
    error ("load: trouble reading binary file '%s'", filename.c_str ());

  return retval;
}

// Save the data from TC along with the corresponding NAME, help
// string DOC, and global flag MARK_AS_GLOBAL on stream OS in the
// binary format described above for read_binary_data.

bool
save_binary_data (std::ostream& os, const octave_value& tc,
                  const std::string& name, const std::string& doc,
                  bool mark_global, bool save_as_floats)
{
  int32_t name_len = name.length ();

  os.write (reinterpret_cast<char *> (&name_len), 4);
  os << name;

  int32_t doc_len = doc.length ();

  os.write (reinterpret_cast<char *> (&doc_len), 4);
  os << doc;

  unsigned char tmp;

  tmp = mark_global;
  os.write (reinterpret_cast<char *> (&tmp), 1);

  // 255 flags the new binary format
  tmp = 255;
  os.write (reinterpret_cast<char *> (&tmp), 1);

  // Write the string corresponding to the octave_value type
  std::string typ = tc.type_name ();
  int32_t len = typ.length ();
  os.write (reinterpret_cast<char *> (&len), 4);
  const char *btmp = typ.data ();
  os.write (btmp, len);

  // The octave_value of tc is const.  Make a copy...
  octave_value val = tc;

  // Call specific save function
  bool success = val.save_binary (os, save_as_floats);

  return (os && success);
}
