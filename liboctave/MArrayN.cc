/*

Copyright (C) 1996, 1997, 2003, 2004, 2005, 2007, 2009 John W. Eaton

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

#include "MArrayN.h"
#include "Array-util.h"
#include "lo-error.h"

#include "MArray-defs.h"
#include "mx-inlines.cc"

template <class T>
struct _idxadds_helper
{
  T *array;
  T val;
  _idxadds_helper (T *a, T v) : array (a), val (v) { }
  void operator () (octave_idx_type i)
    { array[i] += val; }
};

template <class T>
struct _idxadda_helper
{
  T *array;
  const T *vals;
  _idxadda_helper (T *a, const T *v) : array (a), vals (v) { }
  void operator () (octave_idx_type i)
    { array[i] += *vals++; }
};

template <class T>
void
MArrayN<T>::idx_add (const idx_vector& idx, T val)
{
  octave_idx_type n = this->length ();
  octave_idx_type ext = idx.extent (n);
  if (ext > n)
    {
      this->resize (ext);
      n = ext;
    }

  octave_quit ();

  octave_idx_type len = idx.length (n);
  idx.loop (len, _idxadds_helper<T> (this->fortran_vec (), val));
}

template <class T>
void
MArrayN<T>::idx_add (const idx_vector& idx, const MArrayN<T>& vals)
{
  octave_idx_type n = this->length ();
  octave_idx_type ext = idx.extent (n);
  if (ext > n)
    {
      this->resize (ext);
      n = ext;
    }

  octave_quit ();

  octave_idx_type len = std::min (idx.length (n), vals.length ());
  idx.loop (len, _idxadda_helper<T> (this->fortran_vec (), vals.data ()));
}

template <class T, T op (typename ref_param<T>::type, typename ref_param<T>::type)>
struct _idxbinop_helper
{
  T *array;
  const T *vals;
  _idxbinop_helper (T *a, const T *v) : array (a), vals (v) { }
  void operator () (octave_idx_type i)
    { array[i] = op (array[i], *vals++); }
};

template <class T>
void
MArrayN<T>::idx_min (const idx_vector& idx, const MArrayN<T>& vals)
{
  octave_idx_type n = this->length ();
  octave_idx_type ext = idx.extent (n);
  if (ext > n)
    {
      this->resize (ext);
      n = ext;
    }

  octave_quit ();

  octave_idx_type len = std::min (idx.length (n), vals.length ());
  idx.loop (len, _idxbinop_helper<T, xmin> (this->fortran_vec (), vals.data ()));
}

template <class T>
void
MArrayN<T>::idx_max (const idx_vector& idx, const MArrayN<T>& vals)
{
  octave_idx_type n = this->length ();
  octave_idx_type ext = idx.extent (n);
  if (ext > n)
    {
      this->resize (ext);
      n = ext;
    }

  octave_quit ();

  octave_idx_type len = std::min (idx.length (n), vals.length ());
  idx.loop (len, _idxbinop_helper<T, xmax> (this->fortran_vec (), vals.data ()));
}

// N-dimensional array with math ops.
template <class T>
void
MArrayN<T>::changesign (void)
{
  if (Array<T>::is_shared ())
    *this = - *this;
  else
    do_mx_inplace_op<MArrayN<T> > (*this, mx_inline_uminus2);
}

// Element by element MArrayN by scalar ops.

template <class T>
MArrayN<T>&
operator += (MArrayN<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a + s;
  else
    do_ms_inplace_op<MArrayN<T>, T> (a, s, mx_inline_add2);
  return a;
}

template <class T>
MArrayN<T>&
operator -= (MArrayN<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a - s;
  else
    do_ms_inplace_op<MArrayN<T>, T> (a, s, mx_inline_sub2);
  return a;
}

template <class T>
MArrayN<T>&
operator *= (MArrayN<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a * s;
  else
    do_ms_inplace_op<MArrayN<T>, T> (a, s, mx_inline_mul2);
  return a;
}

template <class T>
MArrayN<T>&
operator /= (MArrayN<T>& a, const T& s)
{
  if (a.is_shared ())
    a = a / s;
  else
    do_ms_inplace_op<MArrayN<T>, T> (a, s, mx_inline_div2);
  return a;
}

// Element by element MArrayN by MArrayN ops.

template <class T>
MArrayN<T>&
operator += (MArrayN<T>& a, const MArrayN<T>& b)
{
  if (a.is_shared ())
    a = a + b;
  else
    do_mm_inplace_op<MArrayN<T>, MArrayN<T> > (a, b, mx_inline_add2, "+=");
  return a;
}

template <class T>
MArrayN<T>&
operator -= (MArrayN<T>& a, const MArrayN<T>& b)
{
  if (a.is_shared ())
    a = a - b;
  else
    do_mm_inplace_op<MArrayN<T>, MArrayN<T> > (a, b, mx_inline_sub2, "-=");
  return a;
}


template <class T>
MArrayN<T>&
product_eq (MArrayN<T>& a, const MArrayN<T>& b)
{
  if (a.is_shared ())
    return a = product (a, b);
  else
    do_mm_inplace_op<MArrayN<T>, MArrayN<T> > (a, b, mx_inline_mul2, ".*=");
  return a;
}

template <class T>
MArrayN<T>&
quotient_eq (MArrayN<T>& a, const MArrayN<T>& b)
{
  if (a.is_shared ())
    return a = quotient (a, b);
  else
    do_mm_inplace_op<MArrayN<T>, MArrayN<T> > (a, b, mx_inline_div2, "./=");
  return a;
}

// Element by element MArrayN by scalar ops.

#define MARRAY_NDS_OP(OP, FN) \
  template <class T> \
  MArrayN<T> \
  operator OP (const MArrayN<T>& a, const T& s) \
  { \
    return do_ms_binary_op<MArrayN<T>, MArrayN<T>, T> (a, s, FN); \
  }

MARRAY_NDS_OP (+, mx_inline_add)
MARRAY_NDS_OP (-, mx_inline_sub)
MARRAY_NDS_OP (*, mx_inline_mul)
MARRAY_NDS_OP (/, mx_inline_div)

// Element by element scalar by MArrayN ops.

#define MARRAY_SND_OP(OP, FN) \
  template <class T> \
  MArrayN<T> \
  operator OP (const T& s, const MArrayN<T>& a) \
  { \
    return do_sm_binary_op<MArrayN<T>, T, MArrayN<T> > (s, a, FN); \
  }

MARRAY_SND_OP (+, mx_inline_add)
MARRAY_SND_OP (-, mx_inline_sub)
MARRAY_SND_OP (*, mx_inline_mul)
MARRAY_SND_OP (/, mx_inline_div)

// Element by element MArrayN by MArrayN ops.

#define MARRAY_NDND_OP(FCN, OP, FN) \
  template <class T> \
  MArrayN<T> \
  FCN (const MArrayN<T>& a, const MArrayN<T>& b) \
  { \
    return do_mm_binary_op<MArrayN<T>, MArrayN<T>, MArrayN<T> > (a, b, FN, #FCN); \
  }

MARRAY_NDND_OP (operator +, +, mx_inline_add)
MARRAY_NDND_OP (operator -, -, mx_inline_sub)
MARRAY_NDND_OP (product,    *, mx_inline_mul)
MARRAY_NDND_OP (quotient,   /, mx_inline_div)

template <class T>
MArrayN<T>
operator + (const MArrayN<T>& a)
{
  return a;
}

template <class T>
MArrayN<T>
operator - (const MArrayN<T>& a)
{
  return do_mx_unary_op<MArrayN<T>, MArrayN<T> > (a, mx_inline_uminus); 
}
