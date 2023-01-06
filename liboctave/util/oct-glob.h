////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2010-2023 The Octave Project Developers
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

#if ! defined (octave_oct_glob_h)
#define octave_oct_glob_h 1

#include "octave-config.h"

#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

extern OCTAVE_API bool
fnmatch (const string_vector& pat, const std::string& str,
         int fnmatch_flags);

extern OCTAVE_API string_vector
glob (const string_vector&);

extern OCTAVE_API string_vector
windows_glob (const string_vector&);

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
