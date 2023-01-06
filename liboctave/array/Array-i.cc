////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#include "oct-inttypes-fwd.h"

// Instantiate Arrays of integer values.

#include "Array.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave::idx_vector>;

#include "Array-base.cc"

#define INLINE_ASCENDING_SORT 1
#define INLINE_DESCENDING_SORT 1
#include "oct-sort.cc"

template class octave_sort<signed char>;
//template class octave_sort<short>;
template class octave_sort<int>;
template class octave_sort<long>;
#if defined (OCTAVE_HAVE_LONG_LONG_INT)
template class octave_sort<long long>;
#endif

INSTANTIATE_ARRAY (signed char, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
//INSTANTIATE_ARRAY (short, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (int, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (long, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
#if defined (OCTAVE_HAVE_LONG_LONG_INT)
INSTANTIATE_ARRAY (long long, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
#endif

template class octave_sort<unsigned char>;
template class octave_sort<unsigned short>;
template class octave_sort<unsigned int>;
template class octave_sort<unsigned long>;
#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
template class octave_sort<unsigned long long>;
#endif

INSTANTIATE_ARRAY (unsigned char, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (unsigned short, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (unsigned int, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (unsigned long, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
INSTANTIATE_ARRAY (unsigned long long, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
#endif

template class octave_sort<octave_int8>;
template class octave_sort<octave_int16>;
template class octave_sort<octave_int32>;
template class octave_sort<octave_int64>;

INSTANTIATE_ARRAY (octave_int8, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (octave_int16, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (octave_int32, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (octave_int64, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);

template class octave_sort<octave_uint8>;
template class octave_sort<octave_uint16>;
template class octave_sort<octave_uint32>;
template class octave_sort<octave_uint64>;

INSTANTIATE_ARRAY (octave_uint8, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (octave_uint16, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (octave_uint32, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);
INSTANTIATE_ARRAY (octave_uint64, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class DiagArray2<int>;
