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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_mx_op_defs_h)
#define octave_mx_op_defs_h 1

#include "mx-inlines.cc"

#define BIN_OP_DECL(R, OP, X, Y, API) \
  extern API R OP (const X&, const Y&)

class boolMatrix;
class boolNDArray;

#define CMP_OP_DECL(OP, X, Y, API) \
  extern API boolMatrix OP (const X&, const Y&)

#define NDCMP_OP_DECL(OP, X, Y, API) \
  extern API boolNDArray OP (const X&, const Y&)

#define BOOL_OP_DECL(OP, X, Y, API) \
  extern API boolMatrix OP (const X&, const Y&)

#define NDBOOL_OP_DECL(OP, X, Y, API) \
  extern API boolNDArray OP (const X&, const Y&)

// vector by scalar operations.

#define VS_BIN_OP_DECLS(R, V, S, API) \
  BIN_OP_DECL (R, operator +, V, S, API); \
  BIN_OP_DECL (R, operator -, V, S, API); \
  BIN_OP_DECL (R, operator *, V, S, API); \
  BIN_OP_DECL (R, operator /, V, S, API);

#define VS_BIN_OP(R, F, OP, V, S) \
  R \
  F (const V& v, const S& s) \
  { \
    int len = v.length (); \
 \
    R r (len); \
 \
    for (int i = 0; i < len; i++) \
      r.elem(i) = v.elem(i) OP s; \
 \
    return r; \
  }

#define VS_BIN_OPS(R, V, S) \
  VS_BIN_OP (R, operator +, +, V, S) \
  VS_BIN_OP (R, operator -, -, V, S) \
  VS_BIN_OP (R, operator *, *, V, S) \
  VS_BIN_OP (R, operator /, /, V, S)

#define VS_OP_DECLS(R, V, S, API) \
  VS_BIN_OP_DECLS(R, V, S, API)

// scalar by vector by operations.

#define SV_BIN_OP_DECLS(R, S, V, API) \
  BIN_OP_DECL (R, operator +, S, V, API); \
  BIN_OP_DECL (R, operator -, S, V, API); \
  BIN_OP_DECL (R, operator *, S, V, API); \
  BIN_OP_DECL (R, operator /, S, V, API);

#define SV_BIN_OP(R, F, OP, S, V) \
  R \
  F (const S& s, const V& v) \
  { \
    int len = v.length (); \
 \
    R r (len); \
 \
    for (int i = 0; i < len; i++) \
      r.elem(i) = s OP v.elem(i); \
 \
    return r; \
  }

#define SV_BIN_OPS(R, S, V) \
  SV_BIN_OP (R, operator +, +, S, V) \
  SV_BIN_OP (R, operator -, -, S, V) \
  SV_BIN_OP (R, operator *, *, S, V) \
  SV_BIN_OP (R, operator /, /, S, V)

#define SV_OP_DECLS(R, S, V, API) \
  SV_BIN_OP_DECLS(R, S, V, API)

// vector by vector operations.

#define VV_BIN_OP_DECLS(R, V1, V2, API) \
  BIN_OP_DECL (R, operator +, V1, V2, API); \
  BIN_OP_DECL (R, operator -, V1, V2, API); \
  BIN_OP_DECL (R, product,    V1, V2, API); \
  BIN_OP_DECL (R, quotient,   V1, V2, API);

#define VV_BIN_OP(R, F, OP, V1, V2) \
  R \
  F (const V1& v1, const V2& v2) \
  { \
    R r; \
 \
    int v1_len = v1.length (); \
    int v2_len = v2.length (); \
 \
    if (v1_len != v2_len) \
      gripe_nonconformant (#OP, v1_len, v2_len); \
    else \
      { \
	r.resize (v1_len); \
 \
	for (int i = 0; i < v1_len; i++) \
	  r.elem(i) = v1.elem(i) OP v2.elem(i); \
      } \
 \
    return r; \
  }

#define VV_BIN_OPS(R, V1, V2) \
  VV_BIN_OP (R, operator +, +, V1, V2) \
  VV_BIN_OP (R, operator -, -, V1, V2) \
  VV_BIN_OP (R, product,    *, V1, V2) \
  VV_BIN_OP (R, quotient,   /, V1, V2)

#define VV_OP_DECLS(R, V1, V2, API) \
  VV_BIN_OP_DECLS(R, V1, V2, API)

// matrix by scalar operations.

#define MS_BIN_OP_DECLS(R, M, S, API) \
  BIN_OP_DECL (R, operator +, M, S, API); \
  BIN_OP_DECL (R, operator -, M, S, API); \
  BIN_OP_DECL (R, operator *, M, S, API); \
  BIN_OP_DECL (R, operator /, M, S, API);

#define MS_BIN_OP(R, OP, M, S, F) \
  R \
  OP (const M& m, const S& s) \
  { \
    int nr = m.rows (); \
    int nc = m.cols (); \
 \
    R r (nr, nc); \
 \
    if (nr > 0 && nc > 0) \
      F ## _vs (r.fortran_vec (), m.data (), nr * nc, s); \
 \
    return r; \
  }

#define MS_BIN_OPS(R, M, S) \
  MS_BIN_OP (R, operator +, M, S, mx_inline_add) \
  MS_BIN_OP (R, operator -, M, S, mx_inline_subtract) \
  MS_BIN_OP (R, operator *, M, S, mx_inline_multiply) \
  MS_BIN_OP (R, operator /, M, S, mx_inline_divide)

#define MS_CMP_OP_DECLS(M, S, API) \
  CMP_OP_DECL (mx_el_lt, M, S, API); \
  CMP_OP_DECL (mx_el_le, M, S, API); \
  CMP_OP_DECL (mx_el_ge, M, S, API); \
  CMP_OP_DECL (mx_el_gt, M, S, API); \
  CMP_OP_DECL (mx_el_eq, M, S, API); \
  CMP_OP_DECL (mx_el_ne, M, S, API);

#define MS_CMP_OP(F, OP, M, MC, S, SC) \
  boolMatrix \
  F (const M& m, const S& s) \
  { \
    boolMatrix r; \
 \
    int nr = m.rows (); \
    int nc = m.cols (); \
 \
    r.resize (nr, nc); \
 \
    if (nr > 0 && nc > 0) \
      { \
        for (int j = 0; j < nc; j++) \
          for (int i = 0; i < nr; i++) \
	    r.elem(i, j) = MC (m.elem(i, j)) OP SC (s); \
      } \
 \
    return r; \
  }

#define MS_CMP_OPS(M, CM, S, CS) \
  MS_CMP_OP (mx_el_lt, <,  M, CM, S, CS) \
  MS_CMP_OP (mx_el_le, <=, M, CM, S, CS) \
  MS_CMP_OP (mx_el_ge, >=, M, CM, S, CS) \
  MS_CMP_OP (mx_el_gt, >,  M, CM, S, CS) \
  MS_CMP_OP (mx_el_eq, ==, M,   , S,   ) \
  MS_CMP_OP (mx_el_ne, !=, M,   , S,   )

#define MS_BOOL_OP_DECLS(M, S, API) \
  BOOL_OP_DECL (mx_el_and, M, S, API); \
  BOOL_OP_DECL (mx_el_or,  M, S, API); \

#define MS_BOOL_OP(F, OP, M, S, LHS_ZERO, RHS_ZERO) \
  boolMatrix \
  F (const M& m, const S& s) \
  { \
    boolMatrix r; \
 \
    int nr = m.rows (); \
    int nc = m.cols (); \
 \
    if (nr != 0 && nc != 0) \
      { \
        r.resize (nr, nc); \
 \
        for (int j = 0; j < nc; j++) \
          for (int i = 0; i < nr; i++) \
	    r.elem(i, j) = (m.elem(i, j) != LHS_ZERO) OP (s != RHS_ZERO); \
      } \
 \
    return r; \
  }

#define MS_BOOL_OPS2(M, S, LHS_ZERO, RHS_ZERO) \
  MS_BOOL_OP (mx_el_and, &&, M, S, LHS_ZERO, RHS_ZERO) \
  MS_BOOL_OP (mx_el_or,  ||, M, S, LHS_ZERO, RHS_ZERO)

#define MS_BOOL_OPS(M, S, ZERO) \
  MS_BOOL_OPS2(M, S, ZERO, ZERO)

#define MS_OP_DECLS(R, M, S, API) \
  MS_BIN_OP_DECLS (R, M, S, API) \
  MS_CMP_OP_DECLS (M, S, API) \
  MS_BOOL_OP_DECLS (M, S, API) \

// scalar by matrix operations.

#define SM_BIN_OP_DECLS(R, S, M, API) \
  BIN_OP_DECL (R, operator +, S, M, API); \
  BIN_OP_DECL (R, operator -, S, M, API); \
  BIN_OP_DECL (R, operator *, S, M, API); \
  BIN_OP_DECL (R, operator /, S, M, API);

#define SM_BIN_OP(R, OP, S, M, F) \
  R \
  OP (const S& s, const M& m) \
  { \
    int nr = m.rows (); \
    int nc = m.cols (); \
 \
    R r (nr, nc); \
 \
    if (nr > 0 && nc > 0) \
      F ## _sv (r.fortran_vec (), s, m.data (), nr * nc); \
 \
    return r; \
  }

#define SM_BIN_OPS(R, S, M) \
  SM_BIN_OP (R, operator +, S, M, mx_inline_add) \
  SM_BIN_OP (R, operator -, S, M, mx_inline_subtract) \
  SM_BIN_OP (R, operator *, S, M, mx_inline_multiply) \
  SM_BIN_OP (R, operator /, S, M, mx_inline_divide)

#define SM_CMP_OP_DECLS(S, M, API) \
  CMP_OP_DECL (mx_el_lt, S, M, API); \
  CMP_OP_DECL (mx_el_le, S, M, API); \
  CMP_OP_DECL (mx_el_ge, S, M, API); \
  CMP_OP_DECL (mx_el_gt, S, M, API); \
  CMP_OP_DECL (mx_el_eq, S, M, API); \
  CMP_OP_DECL (mx_el_ne, S, M, API);

#define SM_CMP_OP(F, OP, S, SC, M, MC) \
  boolMatrix \
  F (const S& s, const M& m) \
  { \
    boolMatrix r; \
 \
    int nr = m.rows (); \
    int nc = m.cols (); \
 \
    r.resize (nr, nc); \
 \
    if (nr > 0 && nc > 0) \
      { \
        for (int j = 0; j < nc; j++) \
          for (int i = 0; i < nr; i++) \
	    r.elem(i, j) = SC (s) OP MC (m.elem(i, j)); \
      } \
 \
    return r; \
  }

#define SM_CMP_OPS(S, CS, M, CM) \
  SM_CMP_OP (mx_el_lt, <,  S, CS, M, CM) \
  SM_CMP_OP (mx_el_le, <=, S, CS, M, CM) \
  SM_CMP_OP (mx_el_ge, >=, S, CS, M, CM) \
  SM_CMP_OP (mx_el_gt, >,  S, CS, M, CM) \
  SM_CMP_OP (mx_el_eq, ==, S,   , M,   ) \
  SM_CMP_OP (mx_el_ne, !=, S,   , M,   )

#define SM_BOOL_OP_DECLS(S, M, API) \
  BOOL_OP_DECL (mx_el_and, S, M, API); \
  BOOL_OP_DECL (mx_el_or,  S, M, API); \

#define SM_BOOL_OP(F, OP, S, M, LHS_ZERO, RHS_ZERO) \
  boolMatrix \
  F (const S& s, const M& m) \
  { \
    boolMatrix r; \
 \
    int nr = m.rows (); \
    int nc = m.cols (); \
 \
    if (nr != 0 && nc != 0) \
      { \
        r.resize (nr, nc); \
 \
        for (int j = 0; j < nc; j++) \
          for (int i = 0; i < nr; i++) \
	    r.elem(i, j) = (s != LHS_ZERO) OP (m.elem(i, j) != RHS_ZERO); \
      } \
 \
    return r; \
  }

#define SM_BOOL_OPS2(S, M, LHS_ZERO, RHS_ZERO) \
  SM_BOOL_OP (mx_el_and, &&, S, M, LHS_ZERO, RHS_ZERO) \
  SM_BOOL_OP (mx_el_or,  ||, S, M, LHS_ZERO, RHS_ZERO)

#define SM_BOOL_OPS(S, M, ZERO) \
  SM_BOOL_OPS2(S, M, ZERO, ZERO)

#define SM_OP_DECLS(R, S, M, API) \
  SM_BIN_OP_DECLS (R, S, M, API) \
  SM_CMP_OP_DECLS (S, M, API) \
  SM_BOOL_OP_DECLS (S, M, API) \

// matrix by matrix operations.

#define MM_BIN_OP_DECLS(R, M1, M2, API) \
  BIN_OP_DECL (R, operator +, M1, M2, API); \
  BIN_OP_DECL (R, operator -, M1, M2, API); \
  BIN_OP_DECL (R, product,    M1, M2, API); \
  BIN_OP_DECL (R, quotient,   M1, M2, API);

#define MM_BIN_OP(R, OP, M1, M2, F) \
  R \
  OP (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    int m1_nr = m1.rows (); \
    int m1_nc = m1.cols (); \
 \
    int m2_nr = m2.rows (); \
    int m2_nc = m2.cols (); \
 \
    if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#OP, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
	r.resize (m1_nr, m1_nc); \
 \
	if (m1_nr > 0 && m1_nc > 0) \
	  F ## _vv (r.fortran_vec (), m1.data (), m2.data (), m1_nr * m1_nc); \
      } \
 \
    return r; \
  }

#define MM_BIN_OPS(R, M1, M2) \
  MM_BIN_OP (R, operator +, M1, M2, mx_inline_add) \
  MM_BIN_OP (R, operator -, M1, M2, mx_inline_subtract) \
  MM_BIN_OP (R, product,    M1, M2, mx_inline_multiply) \
  MM_BIN_OP (R, quotient,   M1, M2, mx_inline_divide)

#define MM_CMP_OP_DECLS(M1, M2, API) \
  CMP_OP_DECL (mx_el_lt, M1, M2, API); \
  CMP_OP_DECL (mx_el_le, M1, M2, API); \
  CMP_OP_DECL (mx_el_ge, M1, M2, API); \
  CMP_OP_DECL (mx_el_gt, M1, M2, API); \
  CMP_OP_DECL (mx_el_eq, M1, M2, API); \
  CMP_OP_DECL (mx_el_ne, M1, M2, API);

#define MM_CMP_OP(F, OP, M1, C1, M2, C2) \
  boolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    boolMatrix r; \
 \
    int m1_nr = m1.rows (); \
    int m1_nc = m1.cols (); \
 \
    int m2_nr = m2.rows (); \
    int m2_nc = m2.cols (); \
 \
    if (m1_nr == m2_nr && m1_nc == m2_nc) \
      { \
	r.resize (m1_nr, m1_nc); \
 \
	for (int j = 0; j < m1_nc; j++) \
	  for (int i = 0; i < m1_nr; i++) \
	    r.elem(i, j) = C1 (m1.elem(i, j)) OP C2 (m2.elem(i, j)); \
      } \
    else \
      gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
 \
    return r; \
  }

#define MM_CMP_OPS(M1, C1, M2, C2) \
  MM_CMP_OP (mx_el_lt, <,  M1, C1, M2, C2) \
  MM_CMP_OP (mx_el_le, <=, M1, C1, M2, C2) \
  MM_CMP_OP (mx_el_ge, >=, M1, C1, M2, C2) \
  MM_CMP_OP (mx_el_gt, >,  M1, C1, M2, C2) \
  MM_CMP_OP (mx_el_eq, ==, M1,   , M2,   ) \
  MM_CMP_OP (mx_el_ne, !=, M1,   , M2,   )

#define MM_BOOL_OP_DECLS(M1, M2, API) \
  BOOL_OP_DECL (mx_el_and, M1, M2, API); \
  BOOL_OP_DECL (mx_el_or,  M1, M2, API);

#define MM_BOOL_OP(F, OP, M1, M2, LHS_ZERO, RHS_ZERO) \
  boolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    boolMatrix r; \
 \
    int m1_nr = m1.rows (); \
    int m1_nc = m1.cols (); \
 \
    int m2_nr = m2.rows (); \
    int m2_nc = m2.cols (); \
 \
    if (m1_nr == m2_nr && m1_nc == m2_nc) \
      { \
	if (m1_nr != 0 || m1_nc != 0) \
	  { \
	    r.resize (m1_nr, m1_nc); \
 \
	    for (int j = 0; j < m1_nc; j++) \
	      for (int i = 0; i < m1_nr; i++) \
		r.elem(i, j) = (m1.elem(i, j) != LHS_ZERO) \
                                OP (m2.elem(i, j) != RHS_ZERO); \
	  } \
      } \
    else \
      { \
	if ((m1_nr != 0 || m1_nc != 0) && (m2_nr != 0 || m2_nc != 0)) \
	  gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
      } \
 \
    return r; \
  }

#define MM_BOOL_OPS2(M1, M2, LHS_ZERO, RHS_ZERO) \
  MM_BOOL_OP (mx_el_and, &&, M1, M2, LHS_ZERO, RHS_ZERO) \
  MM_BOOL_OP (mx_el_or,  ||, M1, M2, LHS_ZERO, RHS_ZERO)

#define MM_BOOL_OPS(M1, M2, ZERO) \
  MM_BOOL_OPS2(M1, M2, ZERO, ZERO)

#define MM_OP_DECLS(R, M1, M2, API) \
  MM_BIN_OP_DECLS (R, M1, M2, API) \
  MM_CMP_OP_DECLS (M1, M2, API) \
  MM_BOOL_OP_DECLS (M1, M2, API)

// N-d matrix by scalar operations.

#define NDS_BIN_OP_DECLS(R, ND, S, API) \
  BIN_OP_DECL (R, operator +, ND, S, API); \
  BIN_OP_DECL (R, operator -, ND, S, API); \
  BIN_OP_DECL (R, operator *, ND, S, API); \
  BIN_OP_DECL (R, operator /, ND, S, API);

#define NDS_BIN_OP(R, OP, ND, S, F) \
  R \
  OP (const ND& m, const S& s) \
  { \
    R r (m.dims ()); \
 \
    int len = m.length (); \
 \
    if (len > 0) \
      F ## _vs (r.fortran_vec (), m.data (), len, s); \
 \
    return r; \
  }

#define NDS_BIN_OPS(R, ND, S) \
  NDS_BIN_OP (R, operator +, ND, S, mx_inline_add) \
  NDS_BIN_OP (R, operator -, ND, S, mx_inline_subtract) \
  NDS_BIN_OP (R, operator *, ND, S, mx_inline_multiply) \
  NDS_BIN_OP (R, operator /, ND, S, mx_inline_divide)

#define NDS_CMP_OP_DECLS(ND, S, API) \
  NDCMP_OP_DECL (mx_el_lt, ND, S, API); \
  NDCMP_OP_DECL (mx_el_le, ND, S, API); \
  NDCMP_OP_DECL (mx_el_ge, ND, S, API); \
  NDCMP_OP_DECL (mx_el_gt, ND, S, API); \
  NDCMP_OP_DECL (mx_el_eq, ND, S, API); \
  NDCMP_OP_DECL (mx_el_ne, ND, S, API);

#define NDS_CMP_OP(F, OP, ND, NDC, S, SC) \
  boolNDArray \
  F (const ND& m, const S& s) \
  { \
    boolNDArray r; \
 \
    int len = m.length (); \
 \
    r.resize (m.dims ()); \
 \
    for (int i = 0; i < len; i++) \
      r.elem(i) = NDC (m.elem(i)) OP SC (s); \
 \
    return r; \
  }

#define NDS_CMP_OPS(ND, NDC, S, SC) \
  NDS_CMP_OP (mx_el_lt, <,  ND, NDC, S, SC) \
  NDS_CMP_OP (mx_el_le, <=, ND, NDC, S, SC) \
  NDS_CMP_OP (mx_el_ge, >=, ND, NDC, S, SC) \
  NDS_CMP_OP (mx_el_gt, >,  ND, NDC, S, SC) \
  NDS_CMP_OP (mx_el_eq, ==, ND,    , S,   ) \
  NDS_CMP_OP (mx_el_ne, !=, ND,    , S,   )

#define NDS_CMP_OP1(F, OP, ND, NDC, S, SC, SPEC) \
  boolNDArray \
  F (const ND& m, const S& s) \
  { \
    boolNDArray r; \
 \
    int len = m.length (); \
 \
    r.resize (m.dims ()); \
 \
    for (int i = 0; i < len; i++) \
      r.elem(i) = operator OP <SPEC> (NDC (m.elem(i)), SC (s)); \
 \
    return r; \
  }

#define NDS_CMP_OPS1(ND, NDC, S, SC, SPEC) \
  NDS_CMP_OP1 (mx_el_lt, <,  ND, NDC, S, SC, SPEC) \
  NDS_CMP_OP1 (mx_el_le, <=, ND, NDC, S, SC, SPEC) \
  NDS_CMP_OP1 (mx_el_ge, >=, ND, NDC, S, SC, SPEC) \
  NDS_CMP_OP1 (mx_el_gt, >,  ND, NDC, S, SC, SPEC) \
  NDS_CMP_OP1 (mx_el_eq, ==, ND,    , S,   , SPEC) \
  NDS_CMP_OP1 (mx_el_ne, !=, ND,    , S,   , SPEC)

#define NDS_CMP_OP2(F, OP, ND, NDC, S, SC, SPEC1, SPEC2) \
  boolNDArray \
  F (const ND& m, const S& s) \
  { \
    boolNDArray r; \
 \
    int len = m.length (); \
 \
    r.resize (m.dims ()); \
 \
    for (int i = 0; i < len; i++) \
      r.elem(i) = operator OP <SPEC1,SPEC2> (NDC (m.elem(i)), SC (s)); \
 \
    return r; \
  }

#define NDS_CMP_OPS2(ND, NDC, S, SC, SPEC1, SPEC2) \
  NDS_CMP_OP2 (mx_el_lt, <,  ND, NDC, S, SC, SPEC1, SPEC2) \
  NDS_CMP_OP2 (mx_el_le, <=, ND, NDC, S, SC, SPEC1, SPEC2) \
  NDS_CMP_OP2 (mx_el_ge, >=, ND, NDC, S, SC, SPEC1, SPEC2) \
  NDS_CMP_OP2 (mx_el_gt, >,  ND, NDC, S, SC, SPEC1, SPEC2) \
  NDS_CMP_OP2 (mx_el_eq, ==, ND,    , S,   , SPEC1, SPEC2) \
  NDS_CMP_OP2 (mx_el_ne, !=, ND,    , S,   , SPEC1, SPEC2)

#define NDS_BOOL_OP_DECLS(ND, S, API) \
  NDBOOL_OP_DECL (mx_el_and, ND, S, API); \
  NDBOOL_OP_DECL (mx_el_or,  ND, S, API);

#define NDS_BOOL_OP(F, OP, ND, S, LHS_ZERO, RHS_ZERO) \
  boolNDArray \
  F (const ND& m, const S& s) \
  { \
    boolNDArray r; \
 \
    int len = m.length (); \
 \
    if (len > 0) \
      { \
        r.resize (m.dims ()); \
 \
        for (int i = 0; i < len; i++) \
	  r.elem(i) = (m.elem(i) != LHS_ZERO) OP (s != RHS_ZERO); \
      } \
 \
    return r; \
  }

#define NDS_BOOL_OPS2(ND, S, LHS_ZERO, RHS_ZERO) \
  NDS_BOOL_OP (mx_el_and, &&, ND, S, LHS_ZERO, RHS_ZERO) \
  NDS_BOOL_OP (mx_el_or,  ||, ND, S, LHS_ZERO, RHS_ZERO)

#define NDS_BOOL_OPS(ND, S, ZERO) \
  NDS_BOOL_OPS2(ND, S, ZERO, ZERO)

#define NDS_OP_DECLS(R, ND, S, API) \
  NDS_BIN_OP_DECLS (R, ND, S, API) \
  NDS_CMP_OP_DECLS (ND, S, API) \
  NDS_BOOL_OP_DECLS (ND, S, API)

// scalar by N-d matrix operations.

#define SND_BIN_OP_DECLS(R, S, ND, API) \
  BIN_OP_DECL (R, operator +, S, ND, API); \
  BIN_OP_DECL (R, operator -, S, ND, API); \
  BIN_OP_DECL (R, operator *, S, ND, API); \
  BIN_OP_DECL (R, operator /, S, ND, API);

#define SND_BIN_OP(R, OP, S, ND, F) \
  R \
  OP (const S& s, const ND& m) \
  { \
    R r (m.dims ()); \
 \
    int len = m.length (); \
 \
    if (len > 0) \
      F ## _sv (r.fortran_vec (), s, m.data (), len); \
 \
    return r; \
  }

#define SND_BIN_OPS(R, S, ND) \
  SND_BIN_OP (R, operator +, S, ND, mx_inline_add) \
  SND_BIN_OP (R, operator -, S, ND, mx_inline_subtract) \
  SND_BIN_OP (R, operator *, S, ND, mx_inline_multiply) \
  SND_BIN_OP (R, operator /, S, ND, mx_inline_divide)

#define SND_CMP_OP_DECLS(S, ND, API) \
  NDCMP_OP_DECL (mx_el_lt, S, ND, API); \
  NDCMP_OP_DECL (mx_el_le, S, ND, API); \
  NDCMP_OP_DECL (mx_el_ge, S, ND, API); \
  NDCMP_OP_DECL (mx_el_gt, S, ND, API); \
  NDCMP_OP_DECL (mx_el_eq, S, ND, API); \
  NDCMP_OP_DECL (mx_el_ne, S, ND, API);

#define SND_CMP_OP(F, OP, S, SC, ND, NDC) \
  boolNDArray \
  F (const S& s, const ND& m) \
  { \
    boolNDArray r; \
 \
    int len = m.length (); \
 \
    r.resize (m.dims ()); \
 \
    for (int i = 0; i < len; i++) \
      r.elem(i) = SC (s) OP NDC (m.elem(i)); \
 \
    return r; \
  }

#define SND_CMP_OPS(S, CS, ND, CND) \
  SND_CMP_OP (mx_el_lt, <,  S, CS, ND, CND) \
  SND_CMP_OP (mx_el_le, <=, S, CS, ND, CND) \
  SND_CMP_OP (mx_el_ge, >=, S, CS, ND, CND) \
  SND_CMP_OP (mx_el_gt, >,  S, CS, ND, CND) \
  SND_CMP_OP (mx_el_eq, ==, S,   , ND,    ) \
  SND_CMP_OP (mx_el_ne, !=, S,   , ND,    )

#define SND_CMP_OP1(F, OP, S, SC, ND, NDC, SPEC) \
  boolNDArray \
  F (const S& s, const ND& m) \
  { \
    boolNDArray r; \
 \
    int len = m.length (); \
 \
    r.resize (m.dims ()); \
 \
    for (int i = 0; i < len; i++) \
      r.elem(i) = operator OP <SPEC> (SC (s), NDC (m.elem(i))); \
 \
    return r; \
  }

#define SND_CMP_OPS1(S, CS, ND, CND, SPEC) \
  SND_CMP_OP1 (mx_el_lt, <,  S, CS, ND, CND, SPEC) \
  SND_CMP_OP1 (mx_el_le, <=, S, CS, ND, CND, SPEC) \
  SND_CMP_OP1 (mx_el_ge, >=, S, CS, ND, CND, SPEC) \
  SND_CMP_OP1 (mx_el_gt, >,  S, CS, ND, CND, SPEC) \
  SND_CMP_OP1 (mx_el_eq, ==, S,   , ND,    , SPEC) \
  SND_CMP_OP1 (mx_el_ne, !=, S,   , ND,    , SPEC)

#define SND_CMP_OP2(F, OP, S, SC, ND, NDC, SPEC1, SPEC2) \
  boolNDArray \
  F (const S& s, const ND& m) \
  { \
    boolNDArray r; \
 \
    int len = m.length (); \
 \
    r.resize (m.dims ()); \
 \
    for (int i = 0; i < len; i++) \
      r.elem(i) = operator OP <SPEC1, SPEC2> (SC (s), NDC (m.elem(i))); \
 \
    return r; \
  }

#define SND_CMP_OPS2(S, CS, ND, CND, SPEC1, SPEC2) \
  SND_CMP_OP2 (mx_el_lt, <,  S, CS, ND, CND, SPEC1, SPEC2) \
  SND_CMP_OP2 (mx_el_le, <=, S, CS, ND, CND, SPEC1, SPEC2) \
  SND_CMP_OP2 (mx_el_ge, >=, S, CS, ND, CND, SPEC1, SPEC2) \
  SND_CMP_OP2 (mx_el_gt, >,  S, CS, ND, CND, SPEC1, SPEC2) \
  SND_CMP_OP2 (mx_el_eq, ==, S,   , ND,    , SPEC1, SPEC2) \
  SND_CMP_OP2 (mx_el_ne, !=, S,   , ND,    , SPEC1, SPEC2)

#define SND_BOOL_OP_DECLS(S, ND, API) \
  NDBOOL_OP_DECL (mx_el_and, S, ND, API); \
  NDBOOL_OP_DECL (mx_el_or,  S, ND, API);

#define SND_BOOL_OP(F, OP, S, ND, LHS_ZERO, RHS_ZERO) \
  boolNDArray \
  F (const S& s, const ND& m) \
  { \
    boolNDArray r; \
 \
    int len = m.length (); \
 \
    if (len > 0) \
      { \
        r.resize (m.dims ()); \
 \
        for (int i = 0; i < len; i++) \
	    r.elem(i) = (s != LHS_ZERO) OP (m.elem(i) != RHS_ZERO); \
      } \
 \
    return r; \
  }

#define SND_BOOL_OPS2(S, ND, LHS_ZERO, RHS_ZERO) \
  SND_BOOL_OP (mx_el_and, &&, S, ND, LHS_ZERO, RHS_ZERO) \
  SND_BOOL_OP (mx_el_or,  ||, S, ND, LHS_ZERO, RHS_ZERO)

#define SND_BOOL_OPS(S, ND, ZERO) \
  SND_BOOL_OPS2(S, ND, ZERO, ZERO)

#define SND_OP_DECLS(R, S, ND, API) \
  SND_BIN_OP_DECLS (R, S, ND, API) \
  SND_CMP_OP_DECLS (S, ND, API) \
  SND_BOOL_OP_DECLS (S, ND, API)

// N-d matrix by N-d matrix operations.

#define NDND_BIN_OP_DECLS(R, ND1, ND2, API) \
  BIN_OP_DECL (R, operator +, ND1, ND2, API); \
  BIN_OP_DECL (R, operator -, ND1, ND2, API); \
  BIN_OP_DECL (R, product,    ND1, ND2, API); \
  BIN_OP_DECL (R, quotient,   ND1, ND2, API);

#define NDND_BIN_OP(R, OP, ND1, ND2, F) \
  R \
  OP (const ND1& m1, const ND2& m2) \
  { \
    R r; \
 \
    dim_vector m1_dims = m1.dims (); \
    dim_vector m2_dims = m2.dims (); \
 \
    if (m1_dims != m2_dims) \
      gripe_nonconformant (#OP, m1_dims, m2_dims); \
    else \
      { \
	r.resize (m1_dims); \
 \
	int len = m1.length (); \
 \
	if (len > 0) \
	  F ## _vv (r.fortran_vec (), m1.data (), m2.data (), len); \
      } \
 \
    return r; \
  }

#define NDND_BIN_OPS(R, ND1, ND2) \
  NDND_BIN_OP (R, operator +, ND1, ND2, mx_inline_add) \
  NDND_BIN_OP (R, operator -, ND1, ND2, mx_inline_subtract) \
  NDND_BIN_OP (R, product,    ND1, ND2, mx_inline_multiply) \
  NDND_BIN_OP (R, quotient,   ND1, ND2, mx_inline_divide)

#define NDND_CMP_OP_DECLS(ND1, ND2, API) \
  NDCMP_OP_DECL (mx_el_lt, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_le, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_ge, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_gt, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_eq, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_ne, ND1, ND2, API);

#define NDND_CMP_OP(F, OP, ND1, C1, ND2, C2) \
  boolNDArray \
  F (const ND1& m1, const ND2& m2) \
  { \
    boolNDArray r; \
 \
    dim_vector m1_dims = m1.dims (); \
    dim_vector m2_dims = m2.dims (); \
 \
    if (m1_dims == m2_dims) \
      { \
	r.resize (m1_dims); \
 \
	for (int i = 0; i < m1.length (); i++) \
	  r.elem(i) = C1 (m1.elem(i)) OP C2 (m2.elem(i)); \
      } \
    else \
      gripe_nonconformant (#F, m1_dims, m2_dims); \
 \
    return r; \
  }

#define NDND_CMP_OPS(ND1, C1, ND2, C2) \
  NDND_CMP_OP (mx_el_lt, <,  ND1, C1, ND2, C2) \
  NDND_CMP_OP (mx_el_le, <=, ND1, C1, ND2, C2) \
  NDND_CMP_OP (mx_el_ge, >=, ND1, C1, ND2, C2) \
  NDND_CMP_OP (mx_el_gt, >,  ND1, C1, ND2, C2) \
  NDND_CMP_OP (mx_el_eq, ==, ND1,   , ND2,   ) \
  NDND_CMP_OP (mx_el_ne, !=, ND1,   , ND2,   )

#define NDND_BOOL_OP_DECLS(ND1, ND2, API) \
  NDBOOL_OP_DECL (mx_el_and, ND1, ND2, API); \
  NDBOOL_OP_DECL (mx_el_or,  ND1, ND2, API);

#define NDND_BOOL_OP(F, OP, ND1, ND2, LHS_ZERO, RHS_ZERO) \
  boolNDArray \
  F (const ND1& m1, const ND2& m2) \
  { \
    boolNDArray r; \
 \
    dim_vector m1_dims = m1.dims (); \
    dim_vector m2_dims = m2.dims (); \
 \
    if (m1_dims == m2_dims) \
      { \
	if (! m1_dims.all_zero ()) \
	  { \
	    r.resize (m1_dims); \
 \
	    for (int i = 0; i < m1.length (); i++) \
	      r.elem(i) = (m1.elem(i) != LHS_ZERO) OP (m2.elem(i) != RHS_ZERO); \
	  } \
      } \
    else \
      gripe_nonconformant (#F, m1_dims, m2_dims); \
 \
    return r; \
  }

#define NDND_BOOL_OPS2(ND1, ND2, LHS_ZERO, RHS_ZERO) \
  NDND_BOOL_OP (mx_el_and, &&, ND1, ND2, LHS_ZERO, RHS_ZERO) \
  NDND_BOOL_OP (mx_el_or,  ||, ND1, ND2, LHS_ZERO, RHS_ZERO)

#define NDND_BOOL_OPS(ND1, ND2, ZERO) \
  NDND_BOOL_OPS2(ND1, ND2, ZERO, ZERO)

#define NDND_OP_DECLS(R, ND1, ND2, API) \
  NDND_BIN_OP_DECLS (R, ND1, ND2, API) \
  NDND_CMP_OP_DECLS (ND1, ND2, API) \
  NDND_BOOL_OP_DECLS (ND1, ND2, API)

// scalar by diagonal matrix operations.

#define SDM_BIN_OP_DECLS(R, S, DM, API) \
  BIN_OP_DECL (R, operator +, S, DM, API); \
  BIN_OP_DECL (R, operator -, S, DM, API);

#define SDM_BIN_OP(R, OP, S, DM, OPEQ) \
  R \
  OP (const S& s, const DM& dm) \
  { \
    int nr = dm.rows (); \
    int nc = dm.cols (); \
 \
    R r (nr, nc, s); \
 \
    for (int i = 0; i < dm.length (); i++) \
      r.elem(i, i) OPEQ dm.elem(i, i); \
 \
    return r; \
}

#define SDM_BIN_OPS(R, S, DM) \
  SDM_BIN_OP (R, operator +, S, DM, +=) \
  SDM_BIN_OP (R, operator -, S, DM, -=)

#define SDM_OP_DECLS(R, S, DM, API) \
  SDM_BIN_OP_DECLS(R, S, DM, API)

// diagonal matrix by scalar operations.

#define DMS_BIN_OP_DECLS(R, DM, S, API) \
  BIN_OP_DECL (R, operator +, DM, S, API); \
  BIN_OP_DECL (R, operator -, DM, S, API);

#define DMS_BIN_OP(R, OP, DM, S, SGN) \
  R \
  OP (const DM& dm, const S& s) \
  { \
    int nr = dm.rows (); \
    int nc = dm.cols (); \
 \
    R r (nr, nc, SGN s); \
 \
    for (int i = 0; i < dm.length (); i++) \
      r.elem(i, i) += dm.elem(i, i); \
 \
    return r; \
  }

#define DMS_BIN_OPS(R, DM, S) \
  DMS_BIN_OP (R, operator +, DM, S, ) \
  DMS_BIN_OP (R, operator -, DM, S, -)

#define DMS_OP_DECLS(R, DM, S, API) \
  DMS_BIN_OP_DECLS(R, DM, S, API)

// matrix by diagonal matrix operations.

#define MDM_BIN_OP_DECLS(R, M, DM, API) \
  BIN_OP_DECL (R, operator +, M, DM, API); \
  BIN_OP_DECL (R, operator -, M, DM, API); \
  BIN_OP_DECL (R, operator *, M, DM, API);

#define MDM_BIN_OP(R, OP, M, DM, OPEQ) \
R \
OP (const M& m, const DM& dm) \
{ \
  R r; \
 \
  int m_nr = m.rows (); \
  int m_nc = m.cols (); \
 \
  int dm_nr = dm.rows (); \
  int dm_nc = dm.cols (); \
 \
  if (m_nr != dm_nr || m_nc != dm_nc) \
    gripe_nonconformant (#OP, m_nr, m_nc, dm_nr, dm_nc); \
  else \
    { \
      r.resize (m_nr, m_nc); \
 \
      if (m_nr > 0 && m_nc > 0) \
	{ \
	  r = R (m); \
 \
	  int len = dm.length (); \
 \
	  for (int i = 0; i < len; i++) \
	    r.elem(i, i) OPEQ dm.elem(i, i); \
	} \
    } \
 \
  return r; \
}

#define MDM_MULTIPLY_OP(R, M, DM, R_ZERO) \
R \
operator * (const M& m, const DM& dm) \
{ \
  R r; \
 \
  int m_nr = m.rows (); \
  int m_nc = m.cols (); \
 \
  int dm_nr = dm.rows (); \
  int dm_nc = dm.cols (); \
 \
  if (m_nc != dm_nr) \
    gripe_nonconformant ("operator *", m_nr, m_nc, dm_nr, dm_nc); \
  else \
    { \
      r.resize (m_nr, dm_nc, R_ZERO); \
 \
      if (m_nr > 0 && m_nc > 0 && dm_nc > 0) \
	{ \
	  int len = dm.length (); \
 \
	  for (int j = 0; j < len; j++) \
	    { \
	      if (dm.elem(j, j) == 1.0) \
		{ \
		  for (int i = 0; i < m_nr; i++) \
		    r.elem(i, j) = m.elem(i, j); \
		} \
	      else \
		{ \
		  for (int i = 0; i < m_nr; i++) \
		    r.elem(i, j) = dm.elem(j, j) * m.elem(i, j); \
		} \
	    } \
	} \
    } \
 \
  return r; \
}

#define MDM_BIN_OPS(R, M, DM, R_ZERO) \
  MDM_BIN_OP (R, operator +, M, DM, +=) \
  MDM_BIN_OP (R, operator -, M, DM, -=) \
  MDM_MULTIPLY_OP (R, M, DM, R_ZERO)

#define MDM_OP_DECLS(R, M, DM, API) \
  MDM_BIN_OP_DECLS(R, M, DM, API)

// diagonal matrix by matrix operations.

#define DMM_BIN_OP_DECLS(R, DM, M, API) \
  BIN_OP_DECL (R, operator +, DM, M, API); \
  BIN_OP_DECL (R, operator -, DM, M, API); \
  BIN_OP_DECL (R, operator *, DM, M, API);

#define DMM_BIN_OP(R, OP, DM, M, OPEQ, PREOP) \
R \
OP (const DM& dm, const M& m) \
{ \
  R r; \
 \
  int dm_nr = dm.rows (); \
  int dm_nc = dm.cols (); \
 \
  int m_nr = m.rows (); \
  int m_nc = m.cols (); \
 \
  if (dm_nr != m_nr || dm_nc != m_nc) \
    gripe_nonconformant (#OP, dm_nr, dm_nc, m_nr, m_nc); \
  else \
    { \
      if (m_nr > 0 && m_nc > 0) \
	{ \
	  r = R (PREOP m); \
 \
	  int len = dm.length (); \
 \
	  for (int i = 0; i < len; i++) \
	    r.elem(i, i) OPEQ dm.elem(i, i); \
	} \
      else \
	r.resize (m_nr, m_nc); \
    } \
 \
  return r; \
}

#define DMM_MULTIPLY_OP(R, DM, M, R_ZERO) \
R \
operator * (const DM& dm, const M& m) \
{ \
  R r; \
 \
  int dm_nr = dm.rows (); \
  int dm_nc = dm.cols (); \
 \
  int m_nr = m.rows (); \
  int m_nc = m.cols (); \
 \
  if (dm_nc != m_nr) \
    gripe_nonconformant ("operator *", dm_nr, dm_nc, m_nr, m_nc); \
  else \
    { \
      r.resize (dm_nr, m_nc, R_ZERO); \
 \
      if (dm_nr > 0 && dm_nc > 0 && m_nc > 0) \
	{ \
	  int len = dm.length (); \
 \
	  for (int i = 0; i < len; i++) \
	    { \
	      if (dm.elem(i, i) == 1.0) \
		{ \
		  for (int j = 0; j < m_nc; j++) \
		    r.elem(i, j) = m.elem(i, j); \
		} \
	      else \
		{ \
		  for (int j = 0; j < m_nc; j++) \
		    r.elem(i, j) = dm.elem(i, i) * m.elem(i, j); \
		} \
	    } \
	} \
    } \
 \
  return r; \
}

#define DMM_BIN_OPS(R, DM, M, R_ZERO) \
  DMM_BIN_OP (R, operator +, DM, M, +=, ) \
  DMM_BIN_OP (R, operator -, DM, M, +=, -) \
  DMM_MULTIPLY_OP (R, DM, M, R_ZERO)

#define DMM_OP_DECLS(R, DM, M, API) \
  DMM_BIN_OP_DECLS(R, DM, M, API)

// diagonal matrix by diagonal matrix operations.

#define DMDM_BIN_OP_DECLS(R, DM1, DM2, API) \
  BIN_OP_DECL (R, operator +, DM1, DM2, API); \
  BIN_OP_DECL (R, operator -, DM1, DM2, API); \
  BIN_OP_DECL (R, product, DM1, DM2, API);

#define DMDM_BIN_OP(R, OP, DM1, DM2, F) \
  R \
  OP (const DM1& dm1, const DM2& dm2) \
  { \
    R r; \
 \
    int dm1_nr = dm1.rows (); \
    int dm1_nc = dm1.cols (); \
 \
    int dm2_nr = dm2.rows (); \
    int dm2_nc = dm2.cols (); \
 \
    if (dm1_nr != dm2_nr || dm1_nc != dm2_nc) \
      gripe_nonconformant (#OP, dm1_nr, dm1_nc, dm2_nr, dm2_nc); \
    else \
      { \
	r.resize (dm1_nr, dm1_nc); \
 \
	if (dm1_nr > 0 && dm1_nc > 0) \
	  F ## _vv (r.fortran_vec (), dm1.data (), dm2.data (), \
		    dm1_nr * dm2_nc); \
      } \
 \
    return r; \
  }

#define DMDM_BIN_OPS(R, DM1, DM2) \
  DMDM_BIN_OP (R, operator +, DM1, DM2, mx_inline_add) \
  DMDM_BIN_OP (R, operator -, DM1, DM2, mx_inline_subtract) \
  DMDM_BIN_OP (R, product,    DM1, DM2, mx_inline_multiply)

#define DMDM_OP_DECLS(R, DM1, DM2, API) \
  DMDM_BIN_OP_DECLS (R, DM1, DM2, API)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
