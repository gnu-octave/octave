/*

Copyright (C) 2004 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-inttypes.h"

#define INSTANTIATE_INT_DOUBLE_BIN_OP(T, OP) \
  template octave_int<T> operator OP (const octave_int<T>&, double)

#define INSTANTIATE_INT_DOUBLE_BIN_OPS(T) \
  INSTANTIATE_INT_DOUBLE_BIN_OP (T, +); \
  INSTANTIATE_INT_DOUBLE_BIN_OP (T, -); \
  INSTANTIATE_INT_DOUBLE_BIN_OP (T, *); \
  INSTANTIATE_INT_DOUBLE_BIN_OP (T, /)

#define INSTANTIATE_DOUBLE_INT_BIN_OP(T, OP) \
  template octave_int<T> operator OP (double, const octave_int<T>&)

#define INSTANTIATE_DOUBLE_INT_BIN_OPS(T) \
  INSTANTIATE_DOUBLE_INT_BIN_OP (T, +); \
  INSTANTIATE_DOUBLE_INT_BIN_OP (T, -); \
  INSTANTIATE_DOUBLE_INT_BIN_OP (T, *); \
  INSTANTIATE_DOUBLE_INT_BIN_OP (T, /)

#define INSTANTIATE_INT_DOUBLE_CMP_OP(T, OP) \
  template bool operator OP (const octave_int<T>&, const double&)

#define INSTANTIATE_INT_DOUBLE_CMP_OPS(T) \
  INSTANTIATE_INT_DOUBLE_CMP_OP (T, <); \
  INSTANTIATE_INT_DOUBLE_CMP_OP (T, <=); \
  INSTANTIATE_INT_DOUBLE_CMP_OP (T, >=); \
  INSTANTIATE_INT_DOUBLE_CMP_OP (T, >); \
  INSTANTIATE_INT_DOUBLE_CMP_OP (T, ==); \
  INSTANTIATE_INT_DOUBLE_CMP_OP (T, !=)

#define INSTANTIATE_DOUBLE_INT_CMP_OP(T, OP) \
  template bool operator OP (const double&, const octave_int<T>&)

#define INSTANTIATE_DOUBLE_INT_CMP_OPS(T) \
  INSTANTIATE_DOUBLE_INT_CMP_OP (T, <); \
  INSTANTIATE_DOUBLE_INT_CMP_OP (T, <=); \
  INSTANTIATE_DOUBLE_INT_CMP_OP (T, >=); \
  INSTANTIATE_DOUBLE_INT_CMP_OP (T, >); \
  INSTANTIATE_DOUBLE_INT_CMP_OP (T, ==); \
  INSTANTIATE_DOUBLE_INT_CMP_OP (T, !=)

#define INSTANTIATE_INT_BITCMP_OP(T, OP) \
  template octave_int<T> \
  operator OP (const octave_int<T>&, const octave_int<T>&)

#define INSTANTIATE_INT_BITCMP_OPS(T) \
  INSTANTIATE_INT_BITCMP_OP (T, &); \
  INSTANTIATE_INT_BITCMP_OP (T, |); \
  INSTANTIATE_INT_BITCMP_OP (T, ^)

#define INSTANTIATE_INTTYPE(T) \
  template class octave_int<T>; \
  template octave_int<T> pow (const octave_int<T>&, const octave_int<T>&); \
  template octave_int<T> pow (double, const octave_int<T>&); \
  template octave_int<T> pow (const octave_int<T>&, double b); \
  template std::ostream& operator << (std::ostream&, const octave_int<T>&); \
  template std::istream& operator >> (std::istream&, octave_int<T>&); \
  template octave_int<T> \
  bitshift (const octave_int<T>&, int, const octave_int<T>&); \
  INSTANTIATE_INT_DOUBLE_BIN_OPS (T); \
  INSTANTIATE_DOUBLE_INT_BIN_OPS (T); \
  INSTANTIATE_INT_DOUBLE_CMP_OPS (T); \
  INSTANTIATE_DOUBLE_INT_CMP_OPS (T); \
  INSTANTIATE_INT_BITCMP_OPS (T)

INSTANTIATE_INTTYPE (octave_int8_t);
INSTANTIATE_INTTYPE (octave_int16_t);
INSTANTIATE_INTTYPE (octave_int32_t);
INSTANTIATE_INTTYPE (octave_int64_t);

INSTANTIATE_INTTYPE (octave_uint8_t);
INSTANTIATE_INTTYPE (octave_uint16_t);
INSTANTIATE_INTTYPE (octave_uint32_t);
INSTANTIATE_INTTYPE (octave_uint64_t);

#define INSTANTIATE_INTTYPE_BIN_OP(T1, T2, OP) \
  template octave_int<octave_int_binop_traits<T1, T2>::TR> \
  operator OP (const octave_int<T1>&, const octave_int<T2>&)

#define INSTANTIATE_INTTYPE_BIN_OPS(T1, T2) \
  INSTANTIATE_INTTYPE_BIN_OP (T1, T2, +); \
  INSTANTIATE_INTTYPE_BIN_OP (T1, T2, -); \
  INSTANTIATE_INTTYPE_BIN_OP (T1, T2, *); \
  INSTANTIATE_INTTYPE_BIN_OP (T1, T2, /)

INSTANTIATE_INTTYPE_BIN_OPS (octave_int8_t, octave_int8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int8_t, octave_int16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int8_t, octave_int32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int8_t, octave_int64_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int8_t, octave_uint8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int8_t, octave_uint16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int8_t, octave_uint32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int8_t, octave_uint64_t);

INSTANTIATE_INTTYPE_BIN_OPS (octave_int16_t, octave_int8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int16_t, octave_int16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int16_t, octave_int32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int16_t, octave_int64_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int16_t, octave_uint8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int16_t, octave_uint16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int16_t, octave_uint32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int16_t, octave_uint64_t);

INSTANTIATE_INTTYPE_BIN_OPS (octave_int32_t, octave_int8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int32_t, octave_int16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int32_t, octave_int32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int32_t, octave_int64_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int32_t, octave_uint8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int32_t, octave_uint16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int32_t, octave_uint32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int32_t, octave_uint64_t);

INSTANTIATE_INTTYPE_BIN_OPS (octave_int64_t, octave_int8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int64_t, octave_int16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int64_t, octave_int32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int64_t, octave_int64_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int64_t, octave_uint8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int64_t, octave_uint16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int64_t, octave_uint32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_int64_t, octave_uint64_t);

INSTANTIATE_INTTYPE_BIN_OPS (octave_uint8_t, octave_int8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint8_t, octave_int16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint8_t, octave_int32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint8_t, octave_int64_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint8_t, octave_uint8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint8_t, octave_uint16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint8_t, octave_uint32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint8_t, octave_uint64_t);

INSTANTIATE_INTTYPE_BIN_OPS (octave_uint16_t, octave_int8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint16_t, octave_int16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint16_t, octave_int32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint16_t, octave_int64_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint16_t, octave_uint8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint16_t, octave_uint16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint16_t, octave_uint32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint16_t, octave_uint64_t);

INSTANTIATE_INTTYPE_BIN_OPS (octave_uint32_t, octave_int8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint32_t, octave_int16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint32_t, octave_int32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint32_t, octave_int64_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint32_t, octave_uint8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint32_t, octave_uint16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint32_t, octave_uint32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint32_t, octave_uint64_t);

INSTANTIATE_INTTYPE_BIN_OPS (octave_uint64_t, octave_int8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint64_t, octave_int16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint64_t, octave_int32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint64_t, octave_int64_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint64_t, octave_uint8_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint64_t, octave_uint16_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint64_t, octave_uint32_t);
INSTANTIATE_INTTYPE_BIN_OPS (octave_uint64_t, octave_uint64_t);

#define INSTANTIATE_INTTYPE_SHIFT_OP(T, OP) \
  template octave_int<T> operator OP (const octave_int<T>&, const int&)

#define INSTANTIATE_INTTYPE_SHIFT_OPS(T) \
  INSTANTIATE_INTTYPE_SHIFT_OP (T, <<); \
  INSTANTIATE_INTTYPE_SHIFT_OP (T, >>)

INSTANTIATE_INTTYPE_SHIFT_OPS (octave_int8_t);
INSTANTIATE_INTTYPE_SHIFT_OPS (octave_int16_t);
INSTANTIATE_INTTYPE_SHIFT_OPS (octave_int32_t);
INSTANTIATE_INTTYPE_SHIFT_OPS (octave_int64_t);
INSTANTIATE_INTTYPE_SHIFT_OPS (octave_uint8_t);
INSTANTIATE_INTTYPE_SHIFT_OPS (octave_uint16_t);
INSTANTIATE_INTTYPE_SHIFT_OPS (octave_uint32_t);
INSTANTIATE_INTTYPE_SHIFT_OPS (octave_uint64_t);

#define INSTANTIATE_OCTAVE_INT_CMP_OP(OP, T1, T2) \
  template bool operator OP (const octave_int<T1>&, const octave_int<T2>&)

#define INSTANTIATE_OCTAVE_INT_CMP_OPS(T1, T2) \
  INSTANTIATE_OCTAVE_INT_CMP_OP (<, T1, T2); \
  INSTANTIATE_OCTAVE_INT_CMP_OP (<=, T1, T2); \
  INSTANTIATE_OCTAVE_INT_CMP_OP (>=, T1, T2); \
  INSTANTIATE_OCTAVE_INT_CMP_OP (>, T1, T2); \
  INSTANTIATE_OCTAVE_INT_CMP_OP (==, T1, T2); \
  INSTANTIATE_OCTAVE_INT_CMP_OP (!=, T1, T2)

INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int8_t, octave_int8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int8_t, octave_int16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int8_t, octave_int32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int8_t, octave_int64_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int8_t, octave_uint8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int8_t, octave_uint16_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int8_t, octave_uint32_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int8_t, octave_uint64_t);

INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int16_t, octave_int8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int16_t, octave_int16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int16_t, octave_int32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int16_t, octave_int64_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int16_t, octave_uint8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int16_t, octave_uint16_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int16_t, octave_uint32_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int16_t, octave_uint64_t);

INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int32_t, octave_int8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int32_t, octave_int16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int32_t, octave_int32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int32_t, octave_int64_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int32_t, octave_uint8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int32_t, octave_uint16_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int32_t, octave_uint32_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int32_t, octave_uint64_t);

INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int64_t, octave_int8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int64_t, octave_int16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int64_t, octave_int32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int64_t, octave_int64_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int64_t, octave_uint8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int64_t, octave_uint16_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int64_t, octave_uint32_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_int64_t, octave_uint64_t);

INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint8_t, octave_int8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint8_t, octave_int16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint8_t, octave_int32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint8_t, octave_int64_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint8_t, octave_uint8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint8_t, octave_uint16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint8_t, octave_uint32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint8_t, octave_uint64_t);

INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint16_t, octave_int8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint16_t, octave_int16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint16_t, octave_int32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint16_t, octave_int64_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint16_t, octave_uint8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint16_t, octave_uint16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint16_t, octave_uint32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint16_t, octave_uint64_t);

// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint32_t, octave_int8_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint32_t, octave_int16_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint32_t, octave_int32_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint32_t, octave_int64_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint32_t, octave_uint8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint32_t, octave_uint16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint32_t, octave_uint32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint32_t, octave_uint64_t);

// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint64_t, octave_int8_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint64_t, octave_int16_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint64_t, octave_int32_t);
// INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint64_t, octave_int64_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint64_t, octave_uint8_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint64_t, octave_uint16_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint64_t, octave_uint32_t);
INSTANTIATE_OCTAVE_INT_CMP_OPS (octave_uint64_t, octave_uint64_t);

// The following apply if the unsigned type is at least as wide as the
// signed type (then we can cast postive signed values to the unsigned
// type and compare).

#define OCTAVE_US_TYPE1_CMP_OP(OP, LTZ_VAL, UT, ST) \
  bool \
  operator OP (const octave_int<UT>& lhs, const octave_int<ST>& rhs) \
  { \
    return rhs.value () < 0 ? LTZ_VAL \
      : lhs.value () OP static_cast<UT> (rhs.value ()); \
  }

#define OCTAVE_US_TYPE1_CMP_OPS(UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP (<, false, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP (<=, false, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP (>=, true, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP (>, true, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP (==, false, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP (!=, true, UT, ST)

#define OCTAVE_SU_TYPE1_CMP_OP(OP, LTZ_VAL, ST, UT) \
  bool \
  operator OP (const octave_int<ST>& lhs, const octave_int<UT>& rhs) \
  { \
    return lhs.value () < 0 ? LTZ_VAL \
      : static_cast<UT> (lhs.value ()) OP rhs.value (); \
  }

#define OCTAVE_SU_TYPE1_CMP_OPS(ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP (<, true, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP (<=, true, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP (>=, false, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP (>, false, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP (==, false, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP (!=, true, ST, UT)

#define OCTAVE_TYPE1_CMP_OPS(UT, ST) \
  OCTAVE_US_TYPE1_CMP_OPS (UT, ST) \
  OCTAVE_SU_TYPE1_CMP_OPS (ST, UT)

OCTAVE_TYPE1_CMP_OPS (octave_uint32_t, octave_int8_t)
OCTAVE_TYPE1_CMP_OPS (octave_uint32_t, octave_int16_t)
OCTAVE_TYPE1_CMP_OPS (octave_uint32_t, octave_int32_t)

OCTAVE_TYPE1_CMP_OPS (octave_uint64_t, octave_int8_t)
OCTAVE_TYPE1_CMP_OPS (octave_uint64_t, octave_int16_t)
OCTAVE_TYPE1_CMP_OPS (octave_uint64_t, octave_int32_t)
OCTAVE_TYPE1_CMP_OPS (octave_uint64_t, octave_int64_t)

// The following apply if the signed type is wider than the unsigned
// type (then we can cast unsigned values to the signed type and
// compare if the signed value is positive).

#define OCTAVE_US_TYPE2_CMP_OP(OP, LTZ_VAL, UT, ST) \
  bool \
  operator OP (const octave_int<UT>& lhs, const octave_int<ST>& rhs) \
  { \
    return rhs.value () < 0 ? LTZ_VAL \
      : static_cast<ST> (lhs.value ()) OP rhs.value (); \
  }

#define OCTAVE_US_TYPE2_CMP_OPS(ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP (<, false, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP (<=, false, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP (>=, true, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP (>, true, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP (==, false, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP (!=, true, ST, UT)

#define OCTAVE_SU_TYPE2_CMP_OP(OP, LTZ_VAL, ST, UT) \
  bool \
  operator OP (const octave_int<ST>& lhs, const octave_int<UT>& rhs) \
  { \
    return lhs.value () < 0 ? LTZ_VAL \
      : lhs.value () OP static_cast<ST> (rhs.value ()); \
  }

#define OCTAVE_SU_TYPE2_CMP_OPS(ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP (<, true, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP (<=, true, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP (>=, false, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP (>, false, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP (==, false, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP (!=, true, ST, UT)

#define OCTAVE_TYPE2_CMP_OPS(UT, ST) \
  OCTAVE_US_TYPE2_CMP_OPS (UT, ST) \
  OCTAVE_SU_TYPE2_CMP_OPS (ST, UT)

OCTAVE_TYPE2_CMP_OPS (octave_uint32_t, octave_int64_t)



/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
