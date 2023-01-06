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

#if ! defined (octave_file_info_h)
#define octave_file_info_h 1

#include "octave-config.h"

#include <deque>
#include <map>
#include <string>
#include <vector>

#include "oct-time.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class file_info
{
public:

  file_info (void)
    : m_file_buf (), m_offsets (), m_timestamp (static_cast<OCTAVE_TIME_T> (0))
  { }

  file_info (const std::string& text, const sys::time& timestamp)
    : m_file_buf (text), m_offsets (get_line_offsets (m_file_buf)),
      m_timestamp (timestamp)
  { }

  file_info (const std::string& fname)
    : m_file_buf (snarf_file (fname)),
      m_offsets (get_line_offsets (m_file_buf)),
      m_timestamp ()
  { }

  file_info (const file_info&) = default;

  file_info& operator = (const file_info&) = default;

  ~file_info (void) = default;

  OCTAVE_API std::string get_line (std::size_t line) const;

  OCTAVE_API std::deque<std::string>
  get_lines (std::size_t line, std::size_t num_lines) const;

  std::size_t num_lines (void) const { return m_offsets.size (); }

  std::string text (void) const { return m_file_buf; }

  std::vector<std::size_t> line_offsets (void) const { return m_offsets; }

  sys::time timestamp (void) const { return m_timestamp; }

  std::size_t size (void) const { return m_file_buf.length (); }

private:

  // File contents as a string.
  std::string m_file_buf;

  // Offsets to line beginnings.
  std::vector<std::size_t> m_offsets;

  sys::time m_timestamp;

  // Read entire file called fname and return the contents as a string
  static OCTAVE_API std::string snarf_file (const std::string& fname);

  static OCTAVE_API std::vector<std::size_t>
  get_line_offsets (const std::string& buf);
};

OCTAVE_END_NAMESPACE(octave)

#endif
