// MArray.cc
/*

Copyright (C) 1996 John W. Eaton

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

#include "MArray2.h"
#include "lo-error.h"

#include "MArray-defs.h"

// Two dimensional array with math ops.

// Element by element MArray2 by scalar ops.

template <class T>
MArray2<T>&
operator += (MArray2<T>& a, const T& s)
{
  DO_VS_OP2 (+=)
  return a;
}

template <class T>
MArray2<T>&
operator -= (MArray2<T>& a, const T& s)
{
  DO_VS_OP2 (-=)
  return a;
}

// Element by element MArray2 by MArray2 ops.

template <class T>
MArray2<T>&
operator += (MArray2<T>& a, const MArray2<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant += array operation attempted");
    }
  else
    {
      if (r > 0 && c > 0)
	{
	  int l = a.length ();
	  DO_VV_OP2 (+=);
	}
    }
  return a;
}

template <class T>
MArray2<T>&
operator -= (MArray2<T>& a, const MArray2<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant -= array operation attempted");
    }
  else
    {
      if (r > 0 && c > 0)
	{
	  int l = a.length ();
	  DO_VV_OP2 (-=);
	}
    }
  return a;
}

// Element by element MArray2 by scalar ops.

#define MARRAY_A2S_OP(OP) \
  template <class T> \
  MArray2<T> \
  operator OP (const MArray2<T>& a, const T& s) \
  { \
    DO_VS_OP (OP); \
    return MArray2<T> (result, a.rows (), a.cols ()); \
  }

MARRAY_A2S_OP (+)
MARRAY_A2S_OP (-)
MARRAY_A2S_OP (*)
MARRAY_A2S_OP (/)

// Element by element scalar by MArray2 ops.

#define MARRAY_SA2_OP(OP) \
  template <class T> \
  MArray2<T> \
  operator OP (const T& s, const MArray2<T>& a) \
  { \
    DO_SV_OP (OP); \
    return MArray2<T> (result, a.rows (), a.cols ()); \
  }

MARRAY_SA2_OP (+)
MARRAY_SA2_OP (-)
MARRAY_SA2_OP (*)
MARRAY_SA2_OP (/)

// Element by element MArray2 by MArray2 ops.

#define MARRAY_A2A2_OP(FCN, OP, OP_STR) \
  template <class T> \
  MArray2<T> \
  FCN (const MArray2<T>& a, const MArray2<T>& b) \
  { \
    int r = a.rows (); \
    int c = a.cols (); \
    if (r != b.rows () || c != b.cols ()) \
      { \
	(*current_liboctave_error_handler) \
	  ("nonconformant array " OP_STR " attempted"); \
	return MArray2<T> (); \
      } \
    if (r == 0 || c == 0) \
      return MArray2<T> (); \
    int l = a.length (); \
    DO_VV_OP (OP); \
    return MArray2<T> (result, r, c); \
  }

MARRAY_A2A2_OP (operator +, +, "addition")
MARRAY_A2A2_OP (operator -, -, "subtraction")
MARRAY_A2A2_OP (product,    *, "product")
MARRAY_A2A2_OP (quotient,   /, "quotient")

// Unary MArray2 ops.

template <class T>
MArray2<T>
operator - (const MArray2<T>& a)
{
  NEG_V;
  return MArray2<T> (result, a.rows (), a.cols ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
