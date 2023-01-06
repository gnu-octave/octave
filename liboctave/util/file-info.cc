////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2001-2023 The Octave Project Developers
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

#include <deque>
#include <fstream>

#include "file-info.h"
#include "file-stat.h"
#include "lo-error.h"
#include "lo-sysdep.h"

OCTAVE_BEGIN_NAMESPACE(octave)

std::string file_info::get_line (std::size_t line) const
{
  std::string retval;

  if (line == 0)
    return retval;

  if (line < m_offsets.size ())
    {
      std::size_t bol = m_offsets[line-1];
      std::size_t eol = m_offsets[line];

      while (eol > 0 && eol > bol
             && (m_file_buf[eol-1] == '\n' || m_file_buf[eol-1] == '\r'))
        eol--;

      retval = m_file_buf.substr (bol, eol - bol);
    }

  return retval;
}

std::deque<std::string>
file_info::get_lines (std::size_t line, std::size_t num_lines) const
{
  std::deque<std::string> retval;

  for (std::size_t i = line; i < line+num_lines; i++)
    retval.push_back (get_line (i));

  return retval;
}

// Read entire file called fname and return the contents as a string

std::string file_info::snarf_file (const std::string& fname)
{
  std::string retval;

  sys::file_stat fs (fname);

  if (! fs)
    (*current_liboctave_error_handler) ("no such file, '%s'", fname.c_str ());

  std::size_t sz = fs.size ();

  std::ifstream file = sys::ifstream (fname.c_str (),
                                      std::ios::in | std::ios::binary);

  if (file)
    {
      std::string buf (sz+1, 0);

      file.read (&buf[0], sz+1);

      if (! file.eof ())
        (*current_liboctave_error_handler)
          ("error reading file %s", fname.c_str ());

      // Expected to read the entire file.
      retval = buf;
    }

  return retval;
}

std::vector<std::size_t> file_info::get_line_offsets (const std::string& buf)
{
  std::deque<std::size_t> tmp_offsets;

  tmp_offsets.push_back (0);

  std::size_t len = buf.length ();

  for (std::size_t i = 0; i < len; i++)
    {
      char c = buf[i];

      if (c == '\r' && ++i < len)
        {
          c = buf[i];

          if (c == '\n')
            tmp_offsets.push_back (i+1);
          else
            tmp_offsets.push_back (i);
        }
      else if (c == '\n')
        tmp_offsets.push_back (i+1);
    }

  tmp_offsets.push_back (len-1);

  std::size_t n = tmp_offsets.size ();

  std::vector<std::size_t> retval (n);
  std::size_t i = 0;
  for (auto& elt : tmp_offsets)
    retval[i++] = elt;

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
