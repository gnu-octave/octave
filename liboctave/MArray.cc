// MArray.cc                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
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

/*
 * One dimensional array with math ops.
 */

// Element by element MArray by scalar ops.

template <class T>
MArray<T>
operator + (const MArray<T>& a, const T& s)
{
  DO_VS_OP (+);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
operator - (const MArray<T>& a, const T& s)
{
  DO_VS_OP (-);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
operator * (const MArray<T>& a, const T& s)
{
  DO_VS_OP (*);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
operator / (const MArray<T>& a, const T& s)
{
  DO_VS_OP (/);
  return MArray<T> (result, l);
}

// Element by element scalar by MArray ops.

template <class T>
MArray<T>
operator + (const T& s, const MArray<T>& a)
{
  DO_SV_OP (+);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
operator - (const T& s, const MArray<T>& a)
{
  DO_SV_OP (-);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
operator * (const T& s, const MArray<T>& a)
{
  DO_SV_OP (*);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
operator / (const T& s, const MArray<T>& a)
{
  DO_SV_OP (/);
  return MArray<T> (result, l);
}

// Element by element MArray by MArray ops.

template <class T>
MArray<T>
operator + (const MArray<T>& a, const MArray<T>& b)
{
  int l = a.length ();
  if (l != b.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array addition attempted");
      return MArray<T> ();
    }

  if (l == 0)
    return MArray<T> ();

  DO_VV_OP (+);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
operator - (const MArray<T>& a, const MArray<T>& b)
{
  int l = a.length ();
  if (l != b.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array subtraction attempted");
      return MArray<T> ();
    }

  if (l == 0)
    return MArray<T> ();

  DO_VV_OP (-);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
product (const MArray<T>& a, const MArray<T>& b)
{
  int l = a.length ();
  if (l != b.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array product attempted");
      return MArray<T> ();
    }

  if (l == 0)
    return MArray<T> ();

  DO_VV_OP (*);
  return MArray<T> (result, l);
}

template <class T>
MArray<T>
quotient (const MArray<T>& a, const MArray<T>& b)
{
  int l = a.length ();
  if (l != b.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array quotient attempted");
      return MArray<T> ();
    }

  if (l == 0)
    return MArray<T> ();

  DO_VV_OP (/);
  return MArray<T> (result, l);
}

// Unary MArray ops.

template <class T>
MArray<T>
operator - (const MArray<T>& a)
{
  NEG_V;
  return MArray<T> (result, l);
}

/*
 * Two dimensional array with math ops.
 */

template <class T>
MArray2<T>::MArray2 (const MDiagArray<T>& a)
  : Array2<T> (a.rows (), a.cols (), T (0))
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

// Element by element MArray2 by scalar ops.

template <class T>
MArray2<T>
operator + (const MArray2<T>& a, const T& s)
{
  DO_VS_OP (+);
  return MArray2<T> (result, a.rows (), a.cols ());
}

template <class T>
MArray2<T>
operator - (const MArray2<T>& a, const T& s)
{
  DO_VS_OP (-);
  return MArray2<T> (result, a.rows (), a.cols ());
}

template <class T>
MArray2<T>
operator * (const MArray2<T>& a, const T& s)
{
  DO_VS_OP (*);
  return MArray2<T> (result, a.rows (), a.cols ());
}

template <class T>
MArray2<T>
operator / (const MArray2<T>& a, const T& s)
{
  DO_VS_OP (/);
  return MArray2<T> (result, a.rows (), a.cols ());
}

// Element by element scalar by MArray2 ops.

template <class T>
MArray2<T>
operator + (const T& s, const MArray2<T>& a)
{
  DO_SV_OP (+);
  return MArray2<T> (result, a.rows (), a.cols ());
}

template <class T>
MArray2<T>
operator - (const T& s, const MArray2<T>& a)
{
  DO_SV_OP (-);
  return MArray2<T> (result, a.rows (), a.cols ());
}

template <class T>
MArray2<T>
operator * (const T& s, const MArray2<T>& a)
{
  DO_SV_OP (*);
  return MArray2<T> (result, a.rows (), a.cols ());
}

template <class T>
MArray2<T>
operator / (const T& s, const MArray2<T>& a)
{
  DO_SV_OP (/);
  return MArray2<T> (result, a.rows (), a.cols ());
}

// Element by element MArray2 by MArray2 ops.

template <class T>
MArray2<T>
operator + (const MArray2<T>& a, const MArray2<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array addition attempted");
      return MArray2<T> ();
    }

  if (r == 0 || c == 0)
    return MArray2<T> ();

  int l = a.length ();
  DO_VV_OP (+);
  return MArray2<T> (result, r, c);
}

template <class T>
MArray2<T>
operator - (const MArray2<T>& a, const MArray2<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array subtraction attempted");
      return MArray2<T> ();
    }

  if (r == 0 || c == 0)
    return MArray2<T> ();

  int l = a.length ();
  DO_VV_OP (-);
  return MArray2<T> (result, r, c);
}

template <class T>
MArray2<T>
product (const MArray2<T>& a, const MArray2<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array product attempted");
      return MArray2<T> ();
    }

  if (r == 0 || c == 0)
    return MArray2<T> ();

  int l = a.length ();
  DO_VV_OP (*);
  return MArray2<T> (result, r, c);
}

template <class T>
MArray2<T>
quotient (const MArray2<T>& a, const MArray2<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant array quotient attempted");
      return MArray2<T> ();
    }

  if (r == 0 || c == 0)
    return MArray2<T> ();

  int l = a.length ();
  DO_VV_OP (/);
  return MArray2<T> (result, r, c);
}

// Unary MArray2 ops.

template <class T>
MArray2<T>
operator - (const MArray2<T>& a)
{
  NEG_V;
  return MArray2<T> (result, a.rows (), a.cols ());
}

/*
 * Two dimensional diagonal array with math ops.
 */

// Element by element MDiagArray by scalar ops.

template <class T>
MDiagArray<T>
operator * (const MDiagArray<T>& a, const T& s)
{
  DO_VS_OP (*);
  return MDiagArray<T> (result, a.rows (), a.cols ());
}

template <class T>
MDiagArray<T>
operator / (const MDiagArray<T>& a, const T& s)
{
  DO_VS_OP (/);
  return MDiagArray<T> (result, a.rows (), a.cols ());
}

// Element by element scalar by MDiagArray ops.

template <class T>
MDiagArray<T>
operator * (const T& s, const MDiagArray<T>& a)
{
  DO_SV_OP (*);
  return MDiagArray<T> (result, a.rows (), a.cols ());
}

// Element by element MDiagArray by MDiagArray ops.

template <class T>
MDiagArray<T>
operator + (const MDiagArray<T>& a, const MDiagArray<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant diagonal array addition attempted");
      return MDiagArray<T> ();
    }

  if (c == 0 || r == 0)
    return MDiagArray<T> ();

  int l = a.length ();
  DO_VV_OP (+);
  return MDiagArray<T> (result, r, c);
}

template <class T>
MDiagArray<T>
operator - (const MDiagArray<T>& a, const MDiagArray<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant diagonal array subtraction attempted");
      return MDiagArray<T> ();
    }

  if (c == 0 || r == 0)
    return MDiagArray<T> ();

  int l = a.length ();
  DO_VV_OP (-);
  return MDiagArray<T> (result, r, c);
}

template <class T>
MDiagArray<T>
product (const MDiagArray<T>& a, const MDiagArray<T>& b)
{
  int r = a.rows ();
  int c = a.cols ();
  if (r != b.rows () || c != b.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant diagonal array product attempted");
      return MDiagArray<T> ();
    }

  if (c == 0 || r == 0)
    return MDiagArray<T> ();

  int l = a.length ();
  DO_VV_OP (*);
  return MDiagArray<T> (result, r, c);
}

// Unary MDiagArray ops.

template <class T>
MDiagArray<T>
operator - (const MDiagArray<T>& a)
{
  NEG_V;
  return MDiagArray<T> (result, a.rows (), a.cols ());
}

#undef DO_SV_OP
#undef DO_VS_OP
#undef DO_VV_OP
#undef NEG_V

#if 0
#ifdef OCTAVE
typedefMArray<double>      octave_mad_template_type;
typedefMArray2<double>     octave_ma2d_template_type;
typedefMDiagArray<double>  octave_mdad_template_type;

#include <Complex.h>
typedefMArray<Complex>     octave_mac_template_type;
typedefMArray2<Complex>    octave_ma2c_template_type;
typedefMDiagArray<Complex> octave_mdac_template_type;
#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
