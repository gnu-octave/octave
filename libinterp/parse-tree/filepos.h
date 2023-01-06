////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
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

#if ! defined (octave_filepos_h)
#define octave_filepos_h 1

#include "octave-config.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class filepos
{
public:

  filepos (void) : m_line (0), m_column (0) { }

  filepos (int l, int c) : m_line (l), m_column (c) { }

  filepos (const filepos&) = default;

  filepos& operator = (const filepos&) = default;

  ~filepos (void) = default;

  operator bool () { return m_line > 0 && m_column > 0; }

  void line (int l) { m_line = l; }
  void column (int c) { m_column = c; }

  int line (void) const { return m_line; }
  int column (void) const { return m_column; }

  void increment_line (int val = 1) { m_line += val; }
  void increment_column (int val = 1) { m_column += val; }

  void decrement_line (int val = 1) { m_line -= val; }
  void decrement_column (int val = 1) { m_column -= val; }

  void next_line (void)
  {
    m_line++;
    m_column = 1;
  }

private:

  int m_line;
  int m_column;
};

OCTAVE_END_NAMESPACE(octave)

#endif
