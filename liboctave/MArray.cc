// MArray.cc                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

// Nothing like a little CPP abuse to brighten everyone's day.  Would
// have been nice to do this with template functions but as of 2.5.x,
// g++ seems to fail to resolve them properly.

#define DO_VS_OP(OP) \
  int l = a.length (); \
  T *result = 0; \
  if (l > 0) \
    { \
      result = new T [l]; \
      const T *x = a.data (); \
      for (int i = 0; i < l; i++) \
	result[i] = x[i] OP s; \
    }

#define DO_SV_OP(OP) \
  int l = a.length (); \
  T *result = 0; \
  if (l > 0) \
    { \
      result = new T [l]; \
      const T *x = a.data (); \
      for (int i = 0; i < l; i++) \
	result[i] = s OP x[i]; \
    }

#define DO_VV_OP(OP) \
  T *result = 0; \
  if (l > 0) \
    { \
      result = new T [l]; \
      const T *x = a.data (); \
      const T *y = b.data (); \
      for (int i = 0; i < l; i++) \
	result[i] = x[i] OP y[i]; \
    }

#define NEG_V \
  int l = a.length (); \
  T *result = 0; \
  if (l > 0) \
    { \
      result = new T [l]; \
      const T *x = a.data (); \
      for (int i = 0; i < l; i++) \
	result[i] = -x[i]; \
    }

#define DO_VS_OP2(OP) \
  int l = a.length (); \
  if (l > 0) \
    { \
      T *tmp = a.fortran_vec (); \
      for (int i = 0; i < l; i++) \
	tmp[i] OP s; \
    }

#define DO_VV_OP2(OP) \
  do \
    { \
      T *a_tmp = a.fortran_vec (); \
      const T *b_tmp = b.data (); \
      for (int i = 0; i < l; i++) \
	a_tmp[i] += b_tmp[i]; \
    } \
  while (0)

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
      if (l != b.length ())
	(*current_liboctave_error_handler) \
	  ("nonconformant += array operation attempted"); \
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
      if (l != b.length ())
	(*current_liboctave_error_handler) \
	  ("nonconformant -= array operation attempted"); \
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

#define MARRAY_AA_OP(FCN, OP, OP_STR) \
  template <class T> \
  MArray<T> \
  FCN (const MArray<T>& a, const MArray<T>& b) \
  { \
    int l = a.length (); \
    if (l != b.length ()) \
      { \
	(*current_liboctave_error_handler) \
	  ("nonconformant array " OP_STR " attempted"); \
	return MArray<T> (); \
      } \
    if (l == 0) \
      return MArray<T> (); \
    DO_VV_OP (OP); \
    return MArray<T> (result, l); \
  }

MARRAY_AA_OP (operator +, +, "addition")
MARRAY_AA_OP (operator -, -, "subtraction")
MARRAY_AA_OP (product,    *, "multiplication")
MARRAY_AA_OP (quotient,   /, "division")

// Unary MArray ops.

template <class T>
MArray<T>
operator - (const MArray<T>& a)
{
  NEG_V;
  return MArray<T> (result, l);
}

// Two dimensional array with math ops.

#ifndef NO_DIAG_ARRAY
template <class T>
MArray2<T>::MArray2 (const MDiagArray<T>& a)
  : Array2<T> (a.rows (), a.cols (), T (0))
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}
#endif

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

// Two dimensional diagonal array with math ops.

#ifndef NO_DIAG_ARRAY

// Element by element MDiagArray by MDiagArray ops.

template <class T>
MDiagArray<T>&
operator += (MDiagArray<T>& a, const MDiagArray<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array " OP_STR " attempted");
      return MArray2<T> ();
    }
  else
    {
      int l = a.length ();
      T *a_tmp = a.fortran_vec ();
      const T *b_tmp = b.data ();
      for (int i = 0; i < l; i++)
	a_tmp[i] += b_tmp[i];
    }
  return a;
}

template <class T>
MDiagArray<T>&
operator -= (MDiagArray<T>& a, const MDiagArray<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array " OP_STR " attempted");
      return MArray2<T> ();
    }
  else
    {
      int l = a.length ();
      T *a_tmp = a.fortran_vec ();
      const T *b_tmp = b.data ();
      for (int i = 0; i < l; i++)
	a_tmp[i] -= b_tmp[i];
    }
  return a;
}

// Element by element MDiagArray by scalar ops.

#define MARRAY_DAS_OP(OP) \
  template <class T> \
  MDiagArray<T> \
  operator OP (const MDiagArray<T>& a, const T& s) \
  { \
    DO_VS_OP (OP); \
    return MDiagArray<T> (result, a.rows (), a.cols ()); \
  }

MARRAY_DAS_OP (*)
MARRAY_DAS_OP (/)

// Element by element scalar by MDiagArray ops.

template <class T>
MDiagArray<T>
operator * (const T& s, const MDiagArray<T>& a)
{
  DO_SV_OP (*);
  return MDiagArray<T> (result, a.rows (), a.cols ());
}

// Element by element MDiagArray by MDiagArray ops.

#define MARRAY_DADA_OP(FCN, OP, OP_STR) \
  template <class T> \
  MDiagArray<T> \
  FCN (const MDiagArray<T>& a, const MDiagArray<T>& b) \
  { \
    int r = a.rows (); \
    int c = a.cols (); \
    if (r != b.rows () || c != b.cols ()) \
      { \
	(*current_liboctave_error_handler) \
	  ("nonconformant diagonal array " OP_STR " attempted"); \
	return MDiagArray<T> (); \
      } \
    if (c == 0 || r == 0) \
      return MDiagArray<T> (); \
    int l = a.length (); \
    DO_VV_OP (OP); \
    return MDiagArray<T> (result, r, c); \
  }

MARRAY_DADA_OP (operator +, +, "addition")
MARRAY_DADA_OP (operator -, -, "subtraction")
MARRAY_DADA_OP (product,    *, "product")

// Unary MDiagArray ops.

template <class T>
MDiagArray<T>
operator - (const MDiagArray<T>& a)
{
  NEG_V;
  return MDiagArray<T> (result, a.rows (), a.cols ());
}
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
