/*

Copyright (C) 1993, 1995, 1996, 1997, 2000, 2002, 2003, 2004, 2005,
              2007, 2008 John W. Eaton
Copyright (C) 2009 VZLU Prague              

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

#include "MArray.h"
#include "Array-util.h"
#include "lo-error.h"

#include "MArray-defs.h"

// One dimensional array with math ops.

template <class T>
double
MArray<T>::norm (double) const
{
  (*current_liboctave_error_handler)
    ("norm: only implemented for double and complex values");

  return 0;
}

template <class T>
float
MArray<T>::norm (float) const
{
  (*current_liboctave_error_handler)
    ("norm: only implemented for double and complex values");

  return 0;
}

// Element by element MArray by scalar ops.

template <class T>
MArray<T>&
operator += (MArray<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a + s;
  else
    do_ms_inplace_op<MArray<T>, T> (a, s, mx_inline_add2);
  return a;
}

template <class T>
MArray<T>&
operator -= (MArray<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a - s;
  else
    do_ms_inplace_op<MArray<T>, T> (a, s, mx_inline_sub2);
  return a;
}

template <class T>
MArray<T>&
operator *= (MArray<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a * s;
  else
    do_ms_inplace_op<MArray<T>, T> (a, s, mx_inline_mul2);
  return a;
}

template <class T>
MArray<T>&
operator /= (MArray<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a / s;
  else
    do_ms_inplace_op<MArray<T>, T> (a, s, mx_inline_div2);
  return a;
}

// Element by element MArray by MArray ops.

template <class T>
MArray<T>&
operator += (MArray<T>& a, const MArray<T>& b)
{
  if (a.is_shared ())
    a = a + b;
  else
    do_mm_inplace_op<MArray<T>, MArray<T> > (a, b, mx_inline_add2, "+=");
  return a;
}

template <class T>
MArray<T>&
operator -= (MArray<T>& a, const MArray<T>& b)
{
  if (a.is_shared ())
    a = a - b;
  else
    do_mm_inplace_op<MArray<T>, MArray<T> > (a, b, mx_inline_sub2, "-=");
  return a;
}

template <class T>
MArray<T>&
product_eq (MArray<T>& a, const MArray<T>& b)
{
  if (a.is_shared ())
    return a = product (a, b);
  else
    do_mm_inplace_op<MArray<T>, MArray<T> > (a, b, mx_inline_mul2, ".*=");
  return a;
}

template <class T>
MArray<T>&
quotient_eq (MArray<T>& a, const MArray<T>& b)
{
  if (a.is_shared ())
    return a = quotient (a, b);
  else
    do_mm_inplace_op<MArray<T>, MArray<T> > (a, b, mx_inline_div2, "./=");
  return a;
}

// Element by element MArray by scalar ops.

#define MARRAY_AS_OP(OP, FN) \
  template <class T> \
  MArray<T> \
  operator OP (const MArray<T>& a, const T& s) \
  { \
    return do_ms_binary_op<MArray<T>, MArray<T>, T> (a, s, FN); \
  }

MARRAY_AS_OP (+, mx_inline_add)
MARRAY_AS_OP (-, mx_inline_sub)
MARRAY_AS_OP (*, mx_inline_mul)
MARRAY_AS_OP (/, mx_inline_div)

// Element by element scalar by MArray ops.

#define MARRAY_SA_OP(OP, FN) \
  template <class T> \
  MArray<T> \
  operator OP (const T& s, const MArray<T>& a) \
  { \
    return do_sm_binary_op<MArray<T>, T, MArray<T> > (s, a, FN); \
  }

MARRAY_SA_OP(+, mx_inline_add)
MARRAY_SA_OP(-, mx_inline_sub)
MARRAY_SA_OP(*, mx_inline_mul)
MARRAY_SA_OP(/, mx_inline_div)

// Element by element MArray by MArray ops.

#define MARRAY_AA_OP(FCN, OP, FN) \
  template <class T> \
  MArray<T> \
  FCN (const MArray<T>& a, const MArray<T>& b) \
  { \
    return do_mm_binary_op<MArray<T>, MArray<T>, MArray<T> > (a, b, FN, #FCN); \
  }

MARRAY_AA_OP (operator +, +, mx_inline_add)
MARRAY_AA_OP (operator -, -, mx_inline_sub)
MARRAY_AA_OP (product,    *, mx_inline_mul)
MARRAY_AA_OP (quotient,   /, mx_inline_div)

// Unary MArray ops.

template <class T>
MArray<T>
operator + (const MArray<T>& a)
{
  return a;
}

template <class T>
MArray<T>
operator - (const MArray<T>& a)
{
  return do_mx_unary_op<MArray<T>, MArray<T> > (a, mx_inline_uminus); 
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
