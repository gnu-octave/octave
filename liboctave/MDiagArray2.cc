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

// Some functions return a reference to this object after a failure.
template <class T> MDiagArray2<T> MDiagArray2<T>::nil_array;

// Two dimensional diagonal array with math ops.

// Element by element MDiagArray2 by MDiagArray2 ops.

template <class T>
MDiagArray2<T>&
operator += (MDiagArray2<T>& a, const MDiagArray2<T>& b)
{
  octave_idx_type r = a.rows ();
  octave_idx_type c = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (r != b_nr || c != b_nc)
    {
      gripe_nonconformant ("operator +=", r, c, b_nr, b_nc);
      return MDiagArray2<T>::nil_array;
    }
  else
    {
      octave_idx_type l = a.length ();
      DO_VV_OP2 (T, a, +=, b);
    }
  return a;
}

template <class T>
MDiagArray2<T>&
operator -= (MDiagArray2<T>& a, const MDiagArray2<T>& b)
{
  octave_idx_type r = a.rows ();
  octave_idx_type c = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (r != b_nr || c != b_nc)
    {
      gripe_nonconformant ("operator -=", r, c, b_nr, b_nc);
      return MDiagArray2<T>::nil_array;
    }
  else
    {
      octave_idx_type l = a.length ();
      DO_VV_OP2 (T, a, -=, b);
    }
  return a;
}

// Element by element MDiagArray2 by scalar ops.

#define MARRAY_DAS_OP(OP) \
  template <class T> \
  MDiagArray2<T> \
  operator OP (const MDiagArray2<T>& a, const T& s) \
  { \
    MDiagArray2<T> result (a.rows (), a.cols ()); \
    T *r = result.fortran_vec (); \
    octave_idx_type l = a.length (); \
    const T *v = a.data (); \
    DO_VS_OP (r, l, v, OP, s); \
    return result; \
  }

MARRAY_DAS_OP (*)
MARRAY_DAS_OP (/)

// Element by element scalar by MDiagArray2 ops.

template <class T>
MDiagArray2<T>
operator * (const T& s, const MDiagArray2<T>& a)
{
  MDiagArray2<T> result (a.rows (), a.cols ()); \
  T *r = result.fortran_vec (); \
  octave_idx_type l = a.length (); \
  const T *v = a.data (); \
  DO_SV_OP (r, l, s, *, v); \
  return result; \
}

// Element by element MDiagArray2 by MDiagArray2 ops.

#define MARRAY_DADA_OP(FCN, OP) \
  template <class T> \
  MDiagArray2<T> \
  FCN (const MDiagArray2<T>& a, const MDiagArray2<T>& b) \
  { \
    octave_idx_type a_nr = a.rows (); \
    octave_idx_type a_nc = a.cols (); \
    octave_idx_type b_nr = b.rows (); \
    octave_idx_type b_nc = b.cols (); \
    if (a_nr != b_nr || a_nc != b_nc) \
      { \
        gripe_nonconformant (#FCN, a_nr, a_nc, b_nr, b_nc); \
	return MDiagArray2<T> (); \
      } \
    if (a_nc == 0 || a_nr == 0) \
      return MDiagArray2<T> (); \
    octave_idx_type l = a.length (); \
    MDiagArray2<T> result (a_nr, a_nc); \
    T *r = result.fortran_vec (); \
    const T *x = a.data (); \
    const T *y = b.data (); \
    DO_VV_OP (r, l, x, OP, y); \
    return result; \
  }

MARRAY_DADA_OP (operator +, +)
MARRAY_DADA_OP (operator -, -)
MARRAY_DADA_OP (product,    *)

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
  octave_idx_type l = a.length ();
  MDiagArray2<T> result (a.rows (), a.cols ());
  T *r = result.fortran_vec ();
  const T *x = a.data ();
  NEG_V (r, l, x);
  return result;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
