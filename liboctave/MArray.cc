/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "MArray.h"
#include "lo-error.h"

#include "MArray-defs.h"

// One dimensional array with math ops.

// Element by element MArray by scalar ops.

template <class T>
MArray<T>&
operator += (MArray<T>& a, const T& s)
{
  DO_VS_OP2 (+=)
  return a;
}

template <class T>
MArray<T>&
operator -= (MArray<T>& a, const T& s)
{
  DO_VS_OP2 (-=)
  return a;
}

// Element by element MArray by MArray ops.

template <class T>
MArray<T>&
operator += (MArray<T>& a, const MArray<T>& b)
{
  int l = a.length ();
  if (l > 0)
    {
      int bl = b.length ();
      if (l != bl)
	gripe_nonconformant ("operator +=", l, bl);
      else
	DO_VV_OP2 (+=);
    }
  return a;
}

template <class T>
MArray<T>&
operator -= (MArray<T>& a, const MArray<T>& b)
{
  int l = a.length ();
  if (l > 0)
    {
      int bl = b.length ();
      if (l != bl)
	gripe_nonconformant ("operator -=", l, bl);
      else
	DO_VV_OP2 (-=);
    }
  return a;
}

// Element by element MArray by scalar ops.

#define MARRAY_AS_OP(OP) \
  template <class T> \
  MArray<T> \
  operator OP (const MArray<T>& a, const T& s) \
  { \
    DO_VS_OP (OP); \
    return MArray<T> (result, l); \
  }

MARRAY_AS_OP (+)
MARRAY_AS_OP (-)
MARRAY_AS_OP (*)
MARRAY_AS_OP (/)

// Element by element scalar by MArray ops.

#define MARRAY_SA_OP(OP) \
  template <class T> \
  MArray<T> \
  operator OP (const T& s, const MArray<T>& a) \
  { \
    DO_SV_OP (OP); \
    return MArray<T> (result, l); \
 }

MARRAY_SA_OP(+)
MARRAY_SA_OP(-)
MARRAY_SA_OP(*)
MARRAY_SA_OP(/)

// Element by element MArray by MArray ops.

#define MARRAY_AA_OP(FCN, OP) \
  template <class T> \
  MArray<T> \
  FCN (const MArray<T>& a, const MArray<T>& b) \
  { \
    int l = a.length (); \
    int bl = b.length (); \
    if (l != bl) \
      { \
	gripe_nonconformant (#FCN, l, bl); \
	return MArray<T> (); \
      } \
    if (l == 0) \
      return MArray<T> (); \
    DO_VV_OP (OP); \
    return MArray<T> (result, l); \
  }

MARRAY_AA_OP (operator +, +)
MARRAY_AA_OP (operator -, -)
MARRAY_AA_OP (product,    *)
MARRAY_AA_OP (quotient,   /)

// Unary MArray ops.

template <class T>
MArray<T>
operator - (const MArray<T>& a)
{
  NEG_V;
  return MArray<T> (result, l);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
