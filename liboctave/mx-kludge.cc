// kludge.cc                                             -*- C++ -*-
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

// Nothing like a little CPP abuse to brighten everyone's day.  Would
// have been nice to do this with template functions but as of 2.5.x,
// g++ seems to fail in various ways, either not resolving general
// template functions, or not instantiating non-member template
// functions.
//
// When templates work more reliably in g++, this will be replaced by
// the MArray class.

#define DO_VS_OP(OP) \
  int l = a.length (); \
  TYPE *result = 0; \
  if (l > 0) \
    { \
      result = new TYPE [l]; \
      const TYPE *x = a.data (); \
      for (int i = 0; i < l; i++) \
	result[i] = x[i] OP s; \
    }

#define DO_SV_OP(OP) \
  int l = a.length (); \
  TYPE *result = 0; \
  if (l > 0) \
    { \
      result = new TYPE [l]; \
      const TYPE *x = a.data (); \
      for (int i = 0; i < l; i++) \
	result[i] = s OP x[i]; \
    }

#define DO_VV_OP(OP) \
  TYPE *result = 0; \
  if (l > 0) \
    { \
      result = new TYPE [l]; \
      const TYPE *x = a.data (); \
      const TYPE *y = b.data (); \
      for (int i = 0; i < l; i++) \
	result[i] = x[i] OP y[i]; \
    }

#define NEG_V \
  int l = a.length (); \
  TYPE *result = 0; \
  if (l > 0) \
    { \
      result = new TYPE [l]; \
      const TYPE *x = a.data (); \
      for (int i = 0; i < l; i++) \
	result[i] = -x[i]; \
    }

#ifdef KLUDGE_VECTORS

// Like type operations for vectors.

// Element by element vector by scalar ops.

#define KL_VS_OP(OP) \
  KL_VEC_TYPE \
  operator OP (const KL_VEC_TYPE& a, const TYPE& s) \
  { \
    DO_VS_OP (OP); \
    return KL_VEC_TYPE (result, l); \
  }

KL_VS_OP (+)
KL_VS_OP (-)
KL_VS_OP (*)
KL_VS_OP (/)

// Element by element scalar by vector ops.

#define KL_SV_OP(OP) \
  KL_VEC_TYPE \
  operator OP (const TYPE& s, const KL_VEC_TYPE& a) \
  { \
    DO_SV_OP (OP); \
    return KL_VEC_TYPE (result, l); \
  }

KL_SV_OP (+)
KL_SV_OP (-)
KL_SV_OP (*)
KL_SV_OP (/)

// Element by element vector by vector ops.

#define KL_VV_OP(FCN, OP, OP_STR) \
  KL_VEC_TYPE \
  FCN (const KL_VEC_TYPE& a, const KL_VEC_TYPE& b) \
  { \
    int l = a.length (); \
    if (l != b.length ()) \
      { \
        (*current_liboctave_error_handler) \
	  ("nonconformant array " OP_STR " attempted"); \
        return KL_VEC_TYPE (); \
      } \
    if (l == 0) \
      return KL_VEC_TYPE (); \
    DO_VV_OP (OP); \
    return KL_VEC_TYPE (result, l); \
  }

KL_VV_OP(operator +, +, "addition")
KL_VV_OP(operator -, -, "subtraction")
KL_VV_OP(product, *, "product")
KL_VV_OP(quotient, /, "quotient")

// Unary MArray ops.

KL_VEC_TYPE
operator - (const KL_VEC_TYPE& a)
{
  NEG_V;
  return KL_VEC_TYPE (result, l);
}

#endif

#ifdef KLUDGE_MATRICES

// Like type operations for matrices.

// Element by element matrix by scalar ops.

#define KL_MS_OP(OP) \
  KL_MAT_TYPE \
  operator OP (const KL_MAT_TYPE& a, const TYPE& s) \
  { \
    DO_VS_OP (OP); \
    return KL_MAT_TYPE (result, a.rows (), a.cols ()); \
  }

KL_MS_OP(+)
KL_MS_OP(-)
KL_MS_OP(*)
KL_MS_OP(/)

// Element by element scalar by matrix ops.

#define KL_SM_OP(OP) \
  KL_MAT_TYPE \
  operator OP (const TYPE& s, const KL_MAT_TYPE& a) \
  { \
    DO_SV_OP (OP); \
    return KL_MAT_TYPE (result, a.rows (), a.cols ()); \
  }

KL_SM_OP(+)
KL_SM_OP(-)
KL_SM_OP(*)
KL_SM_OP(/)

// Element by element matrix by matrix ops.

#define KL_MM_OP(FCN, OP, OP_STR) \
  KL_MAT_TYPE \
  FCN (const KL_MAT_TYPE& a, const KL_MAT_TYPE& b) \
  { \
    int r = a.rows (); \
    int c = a.cols (); \
    if (r != b.rows () || c != b.cols ()) \
      { \
        (*current_liboctave_error_handler) \
	  ("nonconformant array " OP_STR " attempted"); \
        return KL_MAT_TYPE (); \
      } \
    if (r == 0 || c == 0) \
      return KL_MAT_TYPE (r, c); \
    int l = a.length (); \
    DO_VV_OP (+); \
    return KL_MAT_TYPE (result, r, c); \
  }

KL_MM_OP (operator +, +, "addition")
KL_MM_OP (operator -, -, "subtraction")
KL_MM_OP (product, *, "product")
KL_MM_OP (quotient, /, "quotient")

// Unary matrix ops.

KL_MAT_TYPE
operator - (const KL_MAT_TYPE& a)
{
  NEG_V;
  return KL_MAT_TYPE (result, a.rows (), a.cols ());
}

#endif

#ifdef KLUDGE_DIAG_MATRICES

// Like type operations for diagonal matrices.

// Element by element MDiagArray by scalar ops.

#define KL_DMS_OP(OP) \
  KL_DMAT_TYPE \
  operator OP (const KL_DMAT_TYPE& a, const TYPE& s) \
  { \
    DO_VS_OP (OP); \
    return KL_DMAT_TYPE (result, a.rows (), a.cols ()); \
  }

KL_DMS_OP (*)
KL_DMS_OP (/)

// Element by element scalar by MDiagArray ops.

#define KL_SDM_OP(OP) \
  KL_DMAT_TYPE \
  operator OP (const TYPE& s, const KL_DMAT_TYPE& a) \
  { \
    DO_SV_OP (OP); \
    return KL_DMAT_TYPE (result, a.rows (), a.cols ()); \
  }

KL_SDM_OP (*)

// Element by element MDiagArray by MDiagArray ops.

#define KL_DMDM_OP(FCN, OP, OP_STR) \
  KL_DMAT_TYPE \
  FCN (const KL_DMAT_TYPE& a, const KL_DMAT_TYPE& b) \
  { \
    int r = a.rows (); \
    int c = a.cols (); \
    if (r != b.rows () || c != b.cols ()) \
      { \
        (*current_liboctave_error_handler) \
	  ("nonconformant diagonal array " OP_STR " attempted"); \
        return KL_DMAT_TYPE (); \
      } \
    if (c == 0 || r == 0) \
      return KL_DMAT_TYPE (); \
    int l = a.length (); \
    DO_VV_OP (OP); \
    return KL_DMAT_TYPE (result, r, c); \
  }

KL_DMDM_OP (operator +, +, "addition")
KL_DMDM_OP (operator -, -, "subtraction")
KL_DMDM_OP (product, *, "product")

// Unary MDiagArray ops.

KL_DMAT_TYPE
operator - (const KL_DMAT_TYPE& a)
{
  NEG_V;
  return KL_DMAT_TYPE (result, a.rows (), a.cols ());
}

#endif

#undef DO_VS_OP
#undef DO_SV_OP
#undef DO_VV_OP
#undef NEG_V
#undef KL_VS_OP
#undef KL_SV_OP
#undef KL_VV_OP
#undef KL_MS_OP
#undef KL_SM_OP
#undef KL_MM_OP
#undef KL_DMS_OP
#undef KL_SDM_OP
#undef KL_DMDM_OP

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
