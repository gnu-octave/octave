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

// N-dimensional array with math ops.

// Element by element MArrayN by scalar ops.

template <class T>
MArrayN<T>&
operator += (MArrayN<T>& a, const T& s)
{
  DO_VS_OP2 (T, a, +=, s)
  return a;
}

template <class T>
MArrayN<T>&
operator -= (MArrayN<T>& a, const T& s)
{
  DO_VS_OP2 (T, a, -=, s)
  return a;
}

// Element by element MArrayN by MArrayN ops.

template <class T>
MArrayN<T>&
operator += (MArrayN<T>& a, const MArrayN<T>& b)
{
  octave_idx_type l = a.length ();

  if (l > 0)
    {
      dim_vector a_dims = a.dims ();
      dim_vector b_dims = b.dims ();

      if (a_dims != b_dims)
	gripe_nonconformant ("operator +=", a_dims, b_dims);
      else
	DO_VV_OP2 (T, a, +=, b);
    }

  return a;
}

template <class T>
MArrayN<T>&
operator -= (MArrayN<T>& a, const MArrayN<T>& b)
{
  octave_idx_type l = a.length ();

  if (l > 0)
    {
      dim_vector a_dims = a.dims ();
      dim_vector b_dims = b.dims ();

      if (a_dims != b_dims)
	gripe_nonconformant ("operator -=", a_dims, b_dims);
      else
	DO_VV_OP2 (T, a, -=, b);
    }
  return a;
}

// Element by element MArrayN by scalar ops.

#define MARRAYN_NDS_OP(OP) \
  template <class T> \
  MArrayN<T> \
  operator OP (const MArrayN<T>& a, const T& s) \
    { \
      MArrayN<T> result (a.dims ()); \
      T *r = result.fortran_vec (); \
      octave_idx_type l = a.length (); \
      const T *v = a.data (); \
      DO_VS_OP (r, l, v, OP, s); \
      return result; \
    }

MARRAYN_NDS_OP (+)
MARRAYN_NDS_OP (-)
MARRAYN_NDS_OP (*)
MARRAYN_NDS_OP (/)

// Element by element MArrayN by scalar ops.

#define MARRAYN_SND_OP(OP) \
  template <class T> \
  MArrayN<T> \
  operator OP (const T& s, const MArrayN<T>& a) \
  { \
    MArrayN<T> result (a.dims ()); \
    T *r = result.fortran_vec (); \
    octave_idx_type l = a.length (); \
    const T *v = a.data (); \
    DO_SV_OP (r, l, s, OP, v); \
    return result; \
  }

MARRAYN_SND_OP (+)
MARRAYN_SND_OP (-)
MARRAYN_SND_OP (*)
MARRAYN_SND_OP (/)

#define MARRAY_NDND_OP(FCN, OP) \
template <class T> \
MArrayN<T> \
FCN (const MArrayN<T>& a, const MArrayN<T>& b) \
{ \
dim_vector a_dims = a.dims (); \
dim_vector b_dims = b.dims (); \
int dims_ok = 1; \
int any_dims_zero = 0; \
if (a_dims.length () != b_dims.length ()) \
 dims_ok = 0; \
 else \
   { \
     for (int i = 0; i < a_dims.length (); i++) \
       { \
	 if (a_dims (i) != b_dims (i)) \
	   { dims_ok = 0; break; } \
	 if (a_dims (i) == 0) \
	   any_dims_zero = 1; \
       } \
   } \
 if (!dims_ok) \
   { \
     gripe_nonconformant (#FCN, a_dims, b_dims); \
     return MArrayN<T> (); \
   } \
 if (any_dims_zero) \
   return MArrayN<T> (a_dims); \
 octave_idx_type l = a.length (); \
 MArrayN<T> result (a_dims); \
 T* r = result.fortran_vec (); \
 const T *x = a.data (); \
 const T *y = b.data (); \
 DO_VV_OP (r, l, x, OP, y); \
 return result; \
}

MARRAY_NDND_OP (operator +, +)
MARRAY_NDND_OP (operator -, -)
MARRAY_NDND_OP (product,    *)
MARRAY_NDND_OP (quotient,   /)

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
  octave_idx_type l = a.length ();
  MArrayN<T> result (a.dims ());
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
