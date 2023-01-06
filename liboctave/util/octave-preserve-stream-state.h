////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_octave_preserve_stream_state_h)
#define octave_octave_preserve_stream_state_h 1

#include "octave-config.h"

#include <ios>

OCTAVE_BEGIN_NAMESPACE(octave)

class
preserve_stream_state
{
public:

  preserve_stream_state (std::ios& s)
    : m_stream (s), m_oflags (s.flags ()), m_oprecision (s.precision ()),
      m_owidth (s.width ()), m_ofill (s.fill ())
  { }

  ~preserve_stream_state (void)
  {
    m_stream.flags (m_oflags);
    m_stream.precision (m_oprecision);
    m_stream.width (m_owidth);
    m_stream.fill (m_ofill);
  }

private:

  std::ios& m_stream;
  std::ios::fmtflags m_oflags;
  std::streamsize m_oprecision;
  int m_owidth;
  char m_ofill;
};

OCTAVE_END_NAMESPACE(octave)

#endif
