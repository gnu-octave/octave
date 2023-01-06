////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#if ! defined (octave_ls_ascii_helper_h)
#define octave_ls_ascii_helper_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

extern OCTINTERP_API void
skip_until_newline (std::istream& is, bool keep_newline = false);

extern OCTINTERP_API void
skip_preceeding_newline (std::istream& is);

extern OCTINTERP_API std::string
read_until_newline (std::istream& is, bool keep_newline = false);

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::skip_until_newline' instead")
inline void
skip_until_newline (std::istream& is, bool keep_newline = false)
{
  return octave::skip_until_newline (is, keep_newline);
}

OCTAVE_DEPRECATED (7, "use 'octave::skip_preceding_newline' instead")
inline void
skip_preceeding_newline (std::istream& is)
{
  return octave::skip_preceeding_newline (is);
}

OCTAVE_DEPRECATED (7, "use 'octave::read_until_newline' instead")
inline std::string
read_until_newline (std::istream& is, bool keep_newline = false)
{
  return octave::read_until_newline (is, keep_newline);
}

#endif

#endif
