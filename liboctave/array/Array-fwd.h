////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021-2023 The Octave Project Developers
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

#if ! defined (octave_Array_fwd_h)
#define octave_Array_fwd_h 1

#include "octave-config.h"

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)
#  include <memory_resource>

template <typename T, typename Alloc = std::pmr::polymorphic_allocator<T>>
class OCTAVE_TEMPLATE_API Array;

#else
#  include <memory>

template <typename T, typename Alloc = std::allocator<T>>
class OCTAVE_TEMPLATE_API Array;

#endif

#endif
