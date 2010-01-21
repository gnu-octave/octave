/*

Copyright (C) 1996, 1997, 2000, 2002, 2003, 2004, 2005, 2007, 2008
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

#include "MDiagArray2.h"
#include "Array-util.h"
#include "lo-error.h"

#include "MArray-defs.h"

template <class T>
bool 
MDiagArray2<T>::is_multiple_of_identity (T val) const
{
  bool retval = this->rows () == this->cols ();
  if (retval)
    {
      octave_idx_type len = this->length (), i = 0;
      for (;i < len; i++) 
        if (DiagArray2<T>::elem (i, i) != val) break;
      retval = i == len;
    }

  return retval;
}

// Two dimensional diagonal array with math ops.

// Element by element MDiagArray2 by MDiagArray2 ops.

template <class T>
MDiagArray2<T>&
operator += (MDiagArray2<T>& a, const MDiagArray2<T>& b)
{
  if (a.is_shared ())
    a = a + b;
  else
    do_mm_inplace_op<MDiagArray2<T>, MDiagArray2<T> > (a, b, mx_inline_add2, "+=");
  return a;
}

template <class T>
MDiagArray2<T>&
operator -= (MDiagArray2<T>& a, const MDiagArray2<T>& b)
{
  if (a.is_shared ())
    a = a - b;
  else
    do_mm_inplace_op<MDiagArray2<T>, MDiagArray2<T> > (a, b, mx_inline_sub2, "-=");
  return a;
}


// Element by element MDiagArray2 by scalar ops.

#define MARRAY_DAS_OP(OP, FN) \
  template <class T> \
  MDiagArray2<T> \
  operator OP (const MDiagArray2<T>& a, const T& s) \
  { \
    return do_ms_binary_op<MDiagArray2<T>, MDiagArray2<T>, T> (a, s, FN); \
  }

MARRAY_DAS_OP (*, mx_inline_mul)
MARRAY_DAS_OP (/, mx_inline_div)

// Element by element scalar by MDiagArray2 ops.

template <class T>
MDiagArray2<T>
operator * (const T& s, const MDiagArray2<T>& a)
{
  return do_sm_binary_op<MDiagArray2<T>, T, MDiagArray2<T> > (s, a, mx_inline_mul);
}

// Element by element MDiagArray2 by MDiagArray2 ops.

#define MARRAY_DADA_OP(FCN, OP, FN) \
  template <class T> \
  MDiagArray2<T> \
  FCN (const MDiagArray2<T>& a, const MDiagArray2<T>& b) \
  { \
    return do_mm_binary_op<MDiagArray2<T>, MDiagArray2<T>, MDiagArray2<T> > (a, b, FN, #FCN); \
  }

MARRAY_DADA_OP (operator +, +, mx_inline_add)
MARRAY_DADA_OP (operator -, -, mx_inline_sub)
MARRAY_DADA_OP (product,    *, mx_inline_mul)

// Unary MDiagArray2 ops.

template <class T>
MDiagArray2<T>
operator + (const MDiagArray2<T>& a)
{
  return a;
}

template <class T>
MDiagArray2<T>
operator - (const MDiagArray2<T>& a)
{
  return do_mx_unary_op<MDiagArray2<T>, MDiagArray2<T> > (a, mx_inline_uminus); 
}
