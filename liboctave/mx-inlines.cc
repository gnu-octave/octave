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

#if !defined (octave_mx_inlines_h)
#define octave_mx_inlines_h 1

#include <cstddef>

#include "oct-cmplx.h"

#define VS_OP_FCN(F, OP) \
  template <class R, class V, class S> \
  static inline void \
  F ## _vs (R *r, const V *v, size_t n, S s) \
  { \
    for (size_t i = 0; i < n; i++) \
      r[i] = v[i] OP s; \
  }

VS_OP_FCN (add,      +)
VS_OP_FCN (subtract, -)
VS_OP_FCN (multiply, *)
VS_OP_FCN (divide,   /)

#define VS_OP(F, OP, R, V, S) \
  extern template void F ## _vs (R *, const V *, size_t, S); \
  static inline R * \
  F (const V *v, size_t n, S s) \
  { \
    R *r = 0; \
    if (n > 0) \
      { \
	r = new R [n]; \
	F ## _vs (r, v, n, s); \
      } \
    return r; \
  }

#define VS_OPS(R, V, S) \
  VS_OP (add,      +, R, V, S) \
  VS_OP (subtract, -, R, V, S) \
  VS_OP (multiply, *, R, V, S) \
  VS_OP (divide,   /, R, V, S)

VS_OPS (double,  double,  double)
VS_OPS (Complex, double,  Complex)
VS_OPS (Complex, Complex, double)
VS_OPS (Complex, Complex, Complex)

#define SV_OP_FCN(F, OP) \
  template <class R, class S, class V> \
  static inline void \
  F ## _sv (R *r, S s, const V *v, size_t n) \
  { \
    for (size_t i = 0; i < n; i++) \
      r[i] = s OP v[i]; \
  } \

SV_OP_FCN (add,      +)
SV_OP_FCN (subtract, -)
SV_OP_FCN (multiply, *)
SV_OP_FCN (divide,   /)

#define SV_OP(F, OP, R, S, V) \
  extern template void F ## _sv (R *, S, const V *, size_t); \
  static inline R * \
  F (S s, const V *v, size_t n) \
  { \
    R *r = 0; \
    if (n > 0) \
      { \
	r = new R [n]; \
        F ## _sv (r, s, v, n); \
      } \
    return r; \
  }

#define SV_OPS(R, S, V) \
  SV_OP (add,      +, R, S, V) \
  SV_OP (subtract, -, R, S, V) \
  SV_OP (multiply, *, R, S, V) \
  SV_OP (divide,   /, R, S, V)

SV_OPS (double,  double,  double)
SV_OPS (Complex, double,  Complex)
SV_OPS (Complex, Complex, double)
SV_OPS (Complex, Complex, Complex)

#define VV_OP_FCN(F, OP) \
  template <class R, class T1, class T2> \
  static inline void \
  F ## _vv (R *r, const T1 *v1, const T2 *v2, size_t n) \
  { \
    for (size_t i = 0; i < n; i++) \
      r[i] = v1[i] OP v2[i]; \
  } \

VV_OP_FCN (add,      +)
VV_OP_FCN (subtract, -)
VV_OP_FCN (multiply, *)
VV_OP_FCN (divide,   /)

#define VV_OP(F, OP, R, T1, T2) \
  extern template void F ## _vv (R *, const T1 *, const T2 *, size_t); \
  static inline R * \
  F (const T1 *v1, const T2 *v2, size_t n) \
  { \
    R *r = 0; \
    if (n > 0) \
      { \
	r = new R [n]; \
	F ## _vv (r, v1, v2, n); \
      } \
    return r; \
  }

#define VV_OPS(R, T1, T2) \
  VV_OP (add,      +, R, T1, T2) \
  VV_OP (subtract, -, R, T1, T2) \
  VV_OP (multiply, *, R, T1, T2) \
  VV_OP (divide,   /, R, T1, T2)

VV_OPS (double,  double,  double)
VV_OPS (Complex, double,  Complex)
VV_OPS (Complex, Complex, double)
VV_OPS (Complex, Complex, Complex)

#define VS_OP2(F, OP, V, S) \
  static inline V * \
  F (V *v, size_t n, S s) \
  { \
    for (size_t i = 0; i < n; i++) \
      v[i] OP s; \
    return v; \
  }

#define VS_OP2S(V, S) \
  VS_OP2 (add2,      +=, V, S) \
  VS_OP2 (subtract2, -=, V, S) \
  VS_OP2 (multiply2, *=, V, S) \
  VS_OP2 (divide2,   /=, V, S) \
  VS_OP2 (copy,       =, V, S)

VS_OP2S (double,  double)
VS_OP2S (Complex, double)
VS_OP2S (Complex, Complex)

#define VV_OP2(F, OP, T1, T2) \
  static inline T1 * \
  F (T1 *v1, const T2 *v2, size_t n) \
  { \
    for (size_t i = 0; i < n; i++) \
      v1[i] OP v2[i]; \
    return v1; \
  }

#define VV_OP2S(T1, T2) \
  VV_OP2 (add2,      +=, T1, T2) \
  VV_OP2 (subtract2, -=, T1, T2) \
  VV_OP2 (multiply2, *=, T1, T2) \
  VV_OP2 (divide2,   /=, T1, T2) \
  VV_OP2 (copy,       =, T1, T2)

VV_OP2S (double,  double)
VV_OP2S (Complex, double)
VV_OP2S (Complex, Complex)

#define OP_EQ_FCN(T1, T2) \
  static inline bool \
  equal (const T1 *x, const T2 *y, size_t n) \
  { \
    for (size_t i = 0; i < n; i++) \
      if (x[i] != y[i]) \
	return false; \
    return true; \
  }

OP_EQ_FCN (bool,    bool)
OP_EQ_FCN (char,    char)
OP_EQ_FCN (double,  double)
OP_EQ_FCN (Complex, Complex)

#define OP_DUP_FCN(OP, F, R, T) \
  static inline R * \
  F (const T *x, size_t n) \
  { \
    R *r = 0; \
    if (n > 0) \
      { \
	r = new R [n]; \
	for (size_t i = 0; i < n; i++) \
	  r[i] = OP (x[i]); \
      } \
    return r; \
  }

OP_DUP_FCN (, dup, double,  double)
OP_DUP_FCN (, dup, Complex, Complex)

// These should really return a bool *.  Also, they should probably be
// in with a collection of other element-by-element boolean ops.
OP_DUP_FCN (0.0 ==, not, double, double)
OP_DUP_FCN (0.0 ==, not, double, Complex)

OP_DUP_FCN (, make_complex, Complex, double)

OP_DUP_FCN (-, change_sign, double,  double)
OP_DUP_FCN (-, change_sign, Complex, Complex)

OP_DUP_FCN (real, real_dup, double,  Complex)
OP_DUP_FCN (imag, imag_dup, double,  Complex)
OP_DUP_FCN (conj, conj_dup, Complex, Complex)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
