/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005, 2007
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "MArray2.h"
#include "Array-util.h"
#include "lo-error.h"

#include "MArray-defs.h"

// Two dimensional array with math ops.

// Element by element MArray2 by scalar ops.

template <class T>
MArray2<T>&
operator += (MArray2<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a + s;
  else
    do_ms_inplace_op<MArray2<T>, T> (a, s, mx_inline_add2);
  return a;
}

template <class T>
MArray2<T>&
operator -= (MArray2<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a - s;
  else
    do_ms_inplace_op<MArray2<T>, T> (a, s, mx_inline_sub2);
  return a;
}

template <class T>
MArray2<T>&
operator *= (MArray2<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a * s;
  else
    do_ms_inplace_op<MArray2<T>, T> (a, s, mx_inline_mul2);
  return a;
}

template <class T>
MArray2<T>&
operator /= (MArray2<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a / s;
  else
    do_ms_inplace_op<MArray2<T>, T> (a, s, mx_inline_div2);
  return a;
}

// Element by element MArray2 by MArray2 ops.

template <class T>
MArray2<T>&
operator += (MArray2<T>& a, const MArray2<T>& b)
{
  if (a.is_shared ())
    a = a + b;
  else
    do_mm_inplace_op<MArray2<T>, MArray2<T> > (a, b, mx_inline_add2, "+=");
  return a;
}

template <class T>
MArray2<T>&
operator -= (MArray2<T>& a, const MArray2<T>& b)
{
  if (a.is_shared ())
    a = a - b;
  else
    do_mm_inplace_op<MArray2<T>, MArray2<T> > (a, b, mx_inline_sub2, "-=");
  return a;
}


template <class T>
MArray2<T>&
product_eq (MArray2<T>& a, const MArray2<T>& b)
{
  if (a.is_shared ())
    return a = product (a, b);
  else
    do_mm_inplace_op<MArray2<T>, MArray2<T> > (a, b, mx_inline_mul2, ".*=");
  return a;
}

template <class T>
MArray2<T>&
quotient_eq (MArray2<T>& a, const MArray2<T>& b)
{
  if (a.is_shared ())
    return a = quotient (a, b);
  else
    do_mm_inplace_op<MArray2<T>, MArray2<T> > (a, b, mx_inline_div2, "./=");
  return a;
}

// Element by element MArray2 by scalar ops.

#define MARRAY_A2S_OP(OP, FN) \
  template <class T> \
  MArray2<T> \
  operator OP (const MArray2<T>& a, const T& s) \
  { \
    return do_ms_binary_op<MArray2<T>, MArray2<T>, T> (a, s, FN); \
  }

MARRAY_A2S_OP (+, mx_inline_add)
MARRAY_A2S_OP (-, mx_inline_sub)
MARRAY_A2S_OP (*, mx_inline_mul)
MARRAY_A2S_OP (/, mx_inline_div)

// Element by element scalar by MArray2 ops.

#define MARRAY_SA2_OP(OP, FN) \
  template <class T> \
  MArray2<T> \
  operator OP (const T& s, const MArray2<T>& a) \
  { \
    return do_sm_binary_op<MArray2<T>, T, MArray2<T> > (s, a, FN); \
  }

MARRAY_SA2_OP (+, mx_inline_add)
MARRAY_SA2_OP (-, mx_inline_sub)
MARRAY_SA2_OP (*, mx_inline_mul)
MARRAY_SA2_OP (/, mx_inline_div)

// Element by element MArray2 by MArray2 ops.

#define MARRAY_A2A2_OP(FCN, OP, FN) \
  template <class T> \
  MArray2<T> \
  FCN (const MArray2<T>& a, const MArray2<T>& b) \
  { \
    return do_mm_binary_op<MArray2<T>, MArray2<T>, MArray2<T> > (a, b, FN, #FCN); \
  }

MARRAY_A2A2_OP (operator +, +, mx_inline_add)
MARRAY_A2A2_OP (operator -, -, mx_inline_sub)
MARRAY_A2A2_OP (product,    *, mx_inline_mul)
MARRAY_A2A2_OP (quotient,   /, mx_inline_div)

// Unary MArray2 ops.

template <class T>
MArray2<T>
operator + (const MArray2<T>& a)
{
  return a;
}

template <class T>
MArray2<T>
operator - (const MArray2<T>& a)
{
  return do_mx_unary_op<MArray2<T>, MArray2<T> > (a, mx_inline_uminus); 
}
