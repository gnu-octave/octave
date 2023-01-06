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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "oct-atomic.h"

/* Some versions of GCC can't compile stdatomic.h with -fopenmp.  */

#if defined (OCTAVE_STDATOMIC_H_OK)
#  include <stdatomic.h>

octave_idx_type
octave_atomic_increment (octave_idx_type *x)
{
  // This cast appears to be needed for some versions of clang.
  atomic_fetch_add ((_Atomic octave_idx_type *) x, 1);

  return *x;
}

octave_idx_type
octave_atomic_decrement (octave_idx_type *x)
{
  // This cast appears to be needed for some versions of clang.
  atomic_fetch_sub ((_Atomic octave_idx_type *) x, 1);

  return *x;
}

#elif defined (__GNUC__)

octave_idx_type
octave_atomic_increment (octave_idx_type *x)
{
  return __sync_add_and_fetch (x,  1);
}

octave_idx_type
octave_atomic_decrement (octave_idx_type *x)
{
  return __sync_sub_and_fetch (x, 1);
}

#elif defined (_MSC_VER)
#  include <intrin.h>

octave_idx_type
octave_atomic_increment (octave_idx_type *x)
{
#if defined (OCTAVE_ENABLE_64)
  return _InterlockedIncrement64 (x);
#else
  return _InterlockedIncrement (x);
#endif
}

octave_idx_type
octave_atomic_decrement (octave_idx_type *x)
{
#if defined (OCTAVE_ENABLE_64)
  return _InterlockedDecrement64 (x);
#else
  return _InterlockedDecrement (x);
#endif
}

#else
#  error "Octave requires atomic integer increment and decrement functions"
#endif
