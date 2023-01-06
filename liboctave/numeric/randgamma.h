////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2023 The Octave Project Developers
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

/* Original version written by Paul Kienzle distributed as free
   software in the in the public domain.  */

#if ! defined (octave_randgamma_h)
#define octave_randgamma_h 1

#include "octave-config.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename T>
OCTAVE_API void
rand_gamma (T a, octave_idx_type n, T *p);

template <typename T>
T
rand_gamma (T a)
{
  T retval;
  rand_gamma (a, 1, &retval);
  return retval;
}

OCTAVE_END_NAMESPACE(octave)

#endif
