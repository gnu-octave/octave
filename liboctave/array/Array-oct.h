////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2025 The Octave Project Developers
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


// Include this file when instantiating the Array<T> template class with new
// types in projects linking to "liboctave" but not to "liboctinterp".

#if ! defined (octave_Array_oct_h)
#define octave_Array_oct_h 1

#include "octave-config.h"

#include "Array-base.h"

// FIXME: Including "Array-base.h" will have implicitly instantiated
// Array<octave_idx_type>.  Could that be an issue?

#if ! defined (OCTAVE_EXTERN_TEMPLATE_ARRAY)
// guard against double extern template declarations
#define OCTAVE_EXTERN_TEMPLATE_ARRAY

#include <string>
#include "idx-vector.h"
#include "oct-cmplx.h"
#include "oct-inttypes-fwd.h"

// "Protect" Array<T> instantiations that are exported by liboctave from
// being implicitly instantiated in compilation units including this file.

// instantiated in Array-C.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<Complex>;
// instantiated in Array-b.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<bool>;
// instantiated in Array-ch.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<char>;
// instantiated in Array-d.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<double>;
// instantiated in Array-f.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<float>;
// instantiated in Array-fC.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<FloatComplex>;
// instantiated in Array-i.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<signed char>;
// extern template class Array<short>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<int>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<long>;
#if defined (OCTAVE_HAVE_LONG_LONG_INT)
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<long long>;
#endif
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<unsigned char>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<unsigned short>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<unsigned int>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<unsigned long>;
#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<unsigned long long>;
#endif
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave_int8>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave_int16>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave_int32>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave_int64>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave_uint8>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave_uint16>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave_uint32>;
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave_uint64>;
// instantiated in Array-idx-vec.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave::idx_vector>;
// instantiated in Array-s.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<short>;
// instantiated in Array-str.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<std::string>;
// instantiated in Array-voidp.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API Array<void *>;

#endif  // OCTAVE_EXTERN_TEMPLATE_ARRAY

#endif
