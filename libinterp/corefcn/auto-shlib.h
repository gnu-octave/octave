////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022-2023 The Octave Project Developers
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

#if ! defined (octave_auto_shlib_h)
#define octave_auto_shlib_h 1

#include "octave-config.h"

#include "oct-shlib.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// This is a convenience class that calls the
// octave::get_dynamic_library function automatically at construction
// time.  When deriving new classes, you can either use it as a field or
// as a parent (with multiple inheritance).

class
OCTINTERP_API
auto_shlib : public dynamic_library
{
public:

  auto_shlib (void);

  ~auto_shlib (void) = default;

  auto_shlib (const auto_shlib&) = default;

  auto_shlib& operator = (const auto_shlib&) = default;
};

OCTAVE_END_NAMESPACE(octave)

#endif
