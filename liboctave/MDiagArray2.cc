// MArray.cc                                             -*- C++ -*-
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

#include "MDiagArray2.h"
#include "lo-error.h"

#include "MArray-defs.h"

// Two dimensional diagonal array with math ops.

// Element by element MDiagArray2 by MDiagArray2 ops.

template <class T>
MDiagArray2<T>&
operator += (MDiagArray2<T>& a, const MDiagArray2<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array operator += attempted");
      return MDiagArray2<T> ();
    }
  else
    {
      int l = a.length ();
      DO_VV_OP2 (+=);
    }
  return a;
}

template <class T>
MDiagArray2<T>&
operator -= (MDiagArray2<T>& a, const MDiagArray2<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array operator -= attempted");
      return MDiagArray2<T> ();
    }
  else
    {
      int l = a.length ();
      DO_VV_OP2 (-=);
    }
  return a;
}

// Element by element MDiagArray2 by scalar ops.

#define MARRAY_DAS_OP(OP) \
  template <class T> \
  MDiagArray2<T> \
  operator OP (const MDiagArray2<T>& a, const T& s) \
  { \
    DO_VS_OP (OP); \
    return MDiagArray2<T> (result, a.rows (), a.cols ()); \
  }

MARRAY_DAS_OP (*)
MARRAY_DAS_OP (/)

// Element by element scalar by MDiagArray2 ops.

template <class T>
MDiagArray2<T>
operator * (const T& s, const MDiagArray2<T>& a)
{
  DO_SV_OP (*);
  return MDiagArray2<T> (result, a.rows (), a.cols ());
}

// Element by element MDiagArray2 by MDiagArray2 ops.

#define MARRAY_DADA_OP(FCN, OP, OP_STR) \
  template <class T> \
  MDiagArray2<T> \
  FCN (const MDiagArray2<T>& a, const MDiagArray2<T>& b) \
  { \
    int r = a.rows (); \
    int c = a.cols (); \
    if (r != b.rows () || c != b.cols ()) \
      { \
	(*current_liboctave_error_handler) \
	  ("nonconformant diagonal array " OP_STR " attempted"); \
	return MDiagArray2<T> (); \
      } \
    if (c == 0 || r == 0) \
      return MDiagArray2<T> (); \
    int l = a.length (); \
    DO_VV_OP (OP); \
    return MDiagArray2<T> (result, r, c); \
  }

MARRAY_DADA_OP (operator +, +, "addition")
MARRAY_DADA_OP (operator -, -, "subtraction")
MARRAY_DADA_OP (product,    *, "product")

// Unary MDiagArray2 ops.

template <class T>
MDiagArray2<T>
operator - (const MDiagArray2<T>& a)
{
  NEG_V;
  return MDiagArray2<T> (result, a.rows (), a.cols ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
