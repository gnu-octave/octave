// kludge.h                                             -*- C++ -*-
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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Nothing like a little CPP abuse to brighten everyone's day.  Would
// have been nice to do this with template functions but as of 2.5.x,
// g++ seems to fail in various ways, either not resolving general
// template functions, or not instatiating non-member template
// functions.
//
// When templates work more reliably in g++, this will be replaced by
// the MArray class.

#ifdef KLUDGE_VECTORS

/*
 * Like type operations for vectors.
 */

// Element by element vector by scalar ops.

friend KL_VEC_TYPE operator + (const KL_VEC_TYPE& a, const TYPE& s);
friend KL_VEC_TYPE operator - (const KL_VEC_TYPE& a, const TYPE& s);
friend KL_VEC_TYPE operator * (const KL_VEC_TYPE& a, const TYPE& s);
friend KL_VEC_TYPE operator / (const KL_VEC_TYPE& a, const TYPE& s);

// Element by element scalar by vector ops.

friend KL_VEC_TYPE operator + (const TYPE& s, const KL_VEC_TYPE& a);
friend KL_VEC_TYPE operator - (const TYPE& s, const KL_VEC_TYPE& a);
friend KL_VEC_TYPE operator * (const TYPE& s, const KL_VEC_TYPE& a);
friend KL_VEC_TYPE operator / (const TYPE& s, const KL_VEC_TYPE& a);

// Element by element vector by vector ops.

friend KL_VEC_TYPE operator + (const KL_VEC_TYPE& a, const KL_VEC_TYPE& b);
friend KL_VEC_TYPE operator - (const KL_VEC_TYPE& a, const KL_VEC_TYPE& b);

friend KL_VEC_TYPE product (const KL_VEC_TYPE& a, const KL_VEC_TYPE& b);
friend KL_VEC_TYPE quotient (const KL_VEC_TYPE& a, const KL_VEC_TYPE& b);

// Unary MArray ops.

friend KL_VEC_TYPE operator - (const KL_VEC_TYPE& a);

#endif

#ifdef KLUDGE_MATRICES

/*
 * Like type operations for matrices
 */

// Element by element matrix by scalar ops.

friend KL_MAT_TYPE operator + (const KL_MAT_TYPE& a, const TYPE& s);
friend KL_MAT_TYPE operator - (const KL_MAT_TYPE& a, const TYPE& s);
friend KL_MAT_TYPE operator * (const KL_MAT_TYPE& a, const TYPE& s);
friend KL_MAT_TYPE operator / (const KL_MAT_TYPE& a, const TYPE& s);

// Element by element scalar by matrix ops.

friend KL_MAT_TYPE operator + (const TYPE& s, const KL_MAT_TYPE& a);
friend KL_MAT_TYPE operator - (const TYPE& s, const KL_MAT_TYPE& a);
friend KL_MAT_TYPE operator * (const TYPE& s, const KL_MAT_TYPE& a);
friend KL_MAT_TYPE operator / (const TYPE& s, const KL_MAT_TYPE& a);

// Element by element matrix by matrix ops.

friend KL_MAT_TYPE operator + (const KL_MAT_TYPE& a, const KL_MAT_TYPE& b);
friend KL_MAT_TYPE operator - (const KL_MAT_TYPE& a, const KL_MAT_TYPE& b);

friend KL_MAT_TYPE product (const KL_MAT_TYPE& a, const KL_MAT_TYPE& b);
friend KL_MAT_TYPE quotient (const KL_MAT_TYPE& a, const KL_MAT_TYPE& b);

// Unary matrix ops.

friend KL_MAT_TYPE operator - (const KL_MAT_TYPE& a);

#endif

#ifdef KLUDGE_DIAG_MATRICES

/*
 * Like type operations for diagonal matrices.
 */

// Element by element MDiagArray by scalar ops.

friend KL_DMAT_TYPE operator * (const KL_DMAT_TYPE& a, const TYPE& s);
friend KL_DMAT_TYPE operator / (const KL_DMAT_TYPE& a, const TYPE& s);

// Element by element scalar by MDiagArray ops.

friend KL_DMAT_TYPE operator * (const TYPE& s, const KL_DMAT_TYPE& a);

// Element by element MDiagArray by MDiagArray ops.

friend KL_DMAT_TYPE operator + (const KL_DMAT_TYPE& a, const KL_DMAT_TYPE& b);
friend KL_DMAT_TYPE operator - (const KL_DMAT_TYPE& a, const KL_DMAT_TYPE& b);

friend KL_DMAT_TYPE product (const KL_DMAT_TYPE& a, const KL_DMAT_TYPE& b);

// Unary MDiagArray ops.

friend KL_DMAT_TYPE operator - (const KL_DMAT_TYPE& a);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
