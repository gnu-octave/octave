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

#include <cstring>
#include <cctype>

#include <fstream>
#include <iomanip>
#include <istream>
#include <ostream>
#include <sstream>
#include <string>

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "load-save.h"
#include "ls-ascii-helper.h"
#include "ls-oct-text.h"
#include "ovl.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"

// The number of decimal digits to use when writing ASCII data.
// 17 is the minimum necessary for lossless save/restore of IEEE-754 doubles.
static int Vsave_precision = 17;

// Functions for reading octave format text data.

// Extract a KEYWORD and its value from stream IS, returning the
// associated value in a new string.
//
// Input should look something like:
//
//  [%#][ \t]*keyword[ \t]*:[ \t]*string-value[ \t]*\n

std::string
extract_keyword (std::istream& is, const char *keyword, const bool next_only)
{
  std::string retval;

  int ch = is.peek ();
  if (next_only && ch != '%' && ch != '#')
    return retval;

  char c;
  while (is.get (c))
    {
      if (c == '%' || c == '#')
        {
          std::ostringstream buf;

          while (is.get (c) && (c == ' ' || c == '\t' || c == '%' || c == '#'))
            ; // Skip whitespace and comment characters.

          if (isalpha (c))
            buf << c;

          while (is.get (c) && isalpha (c))
            buf << c;

          std::string tmp = buf.str ();
          bool match = (tmp.substr (0, strlen (keyword)) == keyword);

          if (match)
            {
              std::ostringstream value;
              while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
                ; // Skip whitespace and the colon.

              is.putback (c);
              retval = octave::read_until_newline (is, false);
              break;
            }
          else if (next_only)
            break;
          else
            octave::skip_until_newline (is, false);
        }
    }

  int len = retval.length ();

  if (len > 0)
    {
      while (len)
        {
          c = retval[len-1];

          if (c == ' ' || c == '\t')
            len--;
          else
            {
              retval.resize (len);
              break;
            }
        }
    }

  return retval;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.  If the value
// is tagged as global in the file, return TRUE in GLOBAL.
//
// Each type supplies its own function to load the data, and so this
// function is extensible.
//
// FILENAME is used for error messages.
//
// The data is expected to be in the following format:
//
// The input file must have a header followed by some data.
//
// All lines in the header must begin with a '#' character.
//
// The header must contain a list of keyword and value pairs with the
// keyword and value separated by a colon.
//
// Keywords must appear in the following order:
//
// # name: <name>
// # type: <type>
// # <info>
//
// Where, for the built in types are:
//
//  <name> : a valid identifier
//
//  <type> : <typename>
//         | global <typename>
//
//  <typename> : scalar
//             | complex scalar
//             | matrix
//             | complex matrix
//             | bool
//             | bool matrix
//             | string
//             | range
//
//  <info> : <matrix info>
//         | <string info>
//
//  <matrix info> : # rows: <integer>
//                : # columns: <integer>
//
//  <string info> : # elements: <integer>
//                : # length: <integer> (once before each string)
//
// For backward compatibility the type "string array" is treated as a
// "string" type.  Also "string" can have a single element with no elements
// line such that
//
//  <string info> : # length: <integer>
//
// Formatted ASCII data follows the header.
//
// Example:
//
//  # name: foo
//  # type: matrix
//  # rows: 2
//  # columns: 2
//    2  4
//    1  3
//
// Example:
//
//  # name: foo
//  # type: string
//  # elements: 5
//  # length: 4
//  this
//  # length: 2
//  is
//  # length: 1
//  a
//  # length: 6
//  string
//  # length: 5
//  array
//
// FIXME: This format is fairly rigid, and doesn't allow for arbitrary comments.
// Someone should fix that.  It does allow arbitrary types however.

// Ugh.  The signature of the compare method is not standard in older
// versions of the GNU libstdc++.  Do this instead:

#define SUBSTRING_COMPARE_EQ(s, pos, n, t) (s.substr (pos, n) == (t))

static octave_value
load_inline_fcn (std::istream& is, const std::string& filename)
{
  int nargs;
  if (extract_keyword (is, "nargs", nargs, true))
    {
      std::string name;
      octave_value_list args (nargs+1);
      for (int i = 0; i < nargs; i++)
        {
          std::string tmp;
          is >> tmp;
          args(i+1) = tmp;
        }
      is >> name;
      if (name == "0")
        name = "";

      octave::skip_preceeding_newline (is);

      std::string buf;

      if (is)
        {

          // Get a line of text whitespace characters included,
          // leaving newline in the stream.
          buf = octave::read_until_newline (is, true);
        }

      if (is)
        {
          args(0) = std::string (buf);

          octave::interpreter& interp = octave::__get_interpreter__ ();

          octave_value_list tmp = interp.feval ("inline", args, 1);

          if (tmp.length () > 0)
            return tmp(0);
        }
    }

  error ("load: trouble reading ascii file '%s'", filename.c_str ());
}

std::string
read_text_data (std::istream& is, const std::string& filename, bool& global,
                octave_value& tc, octave_idx_type count,
                const bool do_name_validation)
{
  // Read name for this entry or break on EOF.

  std::string name = extract_keyword (is, "name");

  if (name.empty ())
    {
      if (count == 0)
        error ("load: empty name keyword or no data found in file '%s'",
               filename.c_str ());

      return "";
    }

  if (name != CELL_ELT_TAG
      && do_name_validation && ! octave::valid_identifier (name))
    error ("load: invalid identifier '%s' found in file '%s'",
           name.c_str (), filename.c_str ());

  // Look for type keyword.

  std::string tag = extract_keyword (is, "type");

  if (tag.empty ())
    error ("load: failed to extract keyword specifying value type");

  std::string typ;
  std::size_t pos = tag.rfind (' ');

  if (pos != std::string::npos)
    {
      global = SUBSTRING_COMPARE_EQ (tag, 0, 6, "global");

      typ = (global ? tag.substr (7) : tag);
    }
  else
    typ = tag;

  // Special case for backward compatibility.  A small bit of cruft
  if (SUBSTRING_COMPARE_EQ (typ, 0, 12, "string array"))
    tc = charMatrix ();
  else if (SUBSTRING_COMPARE_EQ (typ, 0, 15, "inline function"))
    {
      // Special case for loading old octave_inline_fcn objects.
      tc = load_inline_fcn (is, filename);
      return name;
    }
  else
    {
      octave::type_info& type_info = octave::__get_type_info__ ();

      tc = type_info.lookup_type (typ);
    }

  if (! tc.load_ascii (is))
    error ("load: trouble reading ascii file '%s'", filename.c_str ());

  return name;
}

// Save the data from TC along with the corresponding NAME, and global
// flag MARK_AS_GLOBAL on stream OS in the plain text format described
// above for load_text_data.  If NAME is empty, the name: line is not
// generated.  PRECISION specifies the number of decimal digits to print.
//
// Assumes ranges and strings cannot contain Inf or NaN values.
//
// Returns 1 for success and 0 for failure.

// FIXME: should probably write the help string here too.

bool
save_text_data (std::ostream& os, const octave_value& val_arg,
                const std::string& name, bool mark_global,
                int precision)
{
  if (! name.empty ())
    os << "# name: " << name << "\n";

  octave_value val = val_arg;

  if (mark_global)
    os << "# type: global " << val.type_name () << "\n";
  else
    os << "# type: " << val.type_name () << "\n";

  if (! precision)
    precision = Vsave_precision;

  long old_precision = os.precision ();
  os.precision (precision);

  bool success = val.save_ascii (os);

  // Insert an extra pair of newline characters after the data so that
  // multiple data elements may be handled separately by gnuplot (see
  // the description of the index qualifier for the plot command in the
  // gnuplot documentation).
  os << "\n\n";

  os.precision (old_precision);

  return (os && success);
}

bool
save_text_data_for_plotting (std::ostream& os, const octave_value& t,
                             const std::string& name)
{
  return save_text_data (os, t, name, false, 6);
}

// Maybe this should be a static function in tree-plot.cc?

// If TC is matrix, save it on stream OS in a format useful for
// making a 3-D plot with gnuplot.  If PARAMETRIC is TRUE,
// assume a parametric 3-D plot will be generated.

bool
save_three_d (std::ostream& os, const octave_value& tc, bool parametric)
{
  octave_idx_type nr = tc.rows ();
  octave_idx_type nc = tc.columns ();

  if (! tc.is_real_matrix ())
    error ("for now, I can only save real matrices in 3-D format");

  os << "# 3-D data...\n"
     << "# type: matrix\n"
     << "# total rows: " << nr << "\n"
     << "# total columns: " << nc << "\n";

  long old_precision = os.precision ();
  os.precision (6);

  if (parametric)
    {
      octave_idx_type extras = nc % 3;
      if (extras)
        warning ("ignoring last %" OCTAVE_IDX_TYPE_FORMAT " columns", extras);

      Matrix tmp = tc.matrix_value ();
      nr = tmp.rows ();

      for (octave_idx_type i = 0; i < nc-extras; i += 3)
        {
          os << tmp.extract (0, i, nr-1, i+2);
          if (i+3 < nc-extras)
            os << "\n";
        }
    }
  else
    {
      Matrix tmp = tc.matrix_value ();
      nr = tmp.rows ();

      for (octave_idx_type i = 0; i < nc; i++)
        {
          os << tmp.extract (0, i, nr-1, i);
          if (i+1 < nc)
            os << "\n";
        }
    }

  os.precision (old_precision);

  return (static_cast<bool> (os));
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (save_precision, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} save_precision ()
@deftypefnx {} {@var{old_val} =} save_precision (@var{new_val})
@deftypefnx {} {@var{old_val} =} save_precision (@var{new_val}, "local")
Query or set the internal variable that specifies the number of digits to
keep when saving data in text format.

The default value is 17 which is the minimum necessary for the lossless saving
and restoring of IEEE-754 double values; For IEEE-754 single values the minimum
value is 9.  If file size is a concern, it is probably better to choose a
binary format for saving data rather than to reduce the precision of the saved
values.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{save_default_options}
@end deftypefn */)
{
  return set_internal_variable (Vsave_precision, args, nargout,
                                "save_precision", -1,
                                std::numeric_limits<int>::max ());
}

OCTAVE_END_NAMESPACE(octave)
