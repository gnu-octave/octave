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

#define OCTAVE_S_INT_UNOPS(TYPE) \
  /* scalar unary ops. */  \
 \
  DEFUNOP_OP (s_not, TYPE ## _scalar, !) \
  DEFUNOP_OP (s_uminus, TYPE ## _scalar, -) \
  DEFUNOP_OP (s_transpose, TYPE ## _scalar, /* no-op */) \
  DEFUNOP_OP (s_hermitian, TYPE ## _scalar, /* no-op */) \
 \
  /* DEFNCUNOP_METHOD (s_incr, TYPE ## _scalar, increment) */ \
  /* DEFNCUNOP_METHOD (s_decr, TYPE ## _scalar, decrement) */

#define OCTAVE_SS_INT_ARITH_OPS(T1, T2) \
  /* scalar by scalar ops. */ \
 \
  DEFBINOP_OP (ss_add, T1 ## _scalar, T2 ## _scalar, +) \
  DEFBINOP_OP (ss_sub, T1 ## _scalar, T2 ## _scalar, -) \
  DEFBINOP_OP (ss_mul, T1 ## _scalar, T2 ## _scalar, *) \
 \
  DEFBINOP (ss_div, T1 ## _scalar, T2 ## _scalar) \
  { \
    CAST_BINOP_ARGS (const octave_ ## T1 ## _scalar&, const octave_ ## T2 ## _scalar&); \
 \
    double d = v2.T2 ## _scalar_value (); \
 \
    if (d == 0.0) \
      gripe_divide_by_zero (); \
 \
    return octave_value (v1.T1 ## _scalar_value () / d); \
  } \
 \
  /* DEFBINOP_FN (ss_pow, T1 ## _scalar, T2 ## _scalar, xpow) */ \
 \
  DEFBINOP (ss_ldiv, T1 ## _scalar, T2 ## _scalar) \
  { \
    CAST_BINOP_ARGS (const octave_ ## T1 ## _scalar&, const octave_ ## T2 ## _scalar&); \
 \
    double d = v1.T1 ## _scalar_value (); \
 \
    if (d == 0.0) \
      gripe_divide_by_zero (); \
 \
    return octave_value (v2.T2 ## _scalar_value () / d); \
  } \
 \
  DEFBINOP_OP (ss_el_mul, T1 ## _scalar, T2 ## _scalar, *) \
 \
  DEFBINOP (ss_el_div, T1 ## _scalar, T2 ## _scalar) \
  { \
    CAST_BINOP_ARGS (const octave_ ## T1 ## _scalar&, const octave_ ## T2 ## _scalar&); \
 \
    double d = v2.T2 ## _scalar_value (); \
 \
    if (d == 0.0) \
      gripe_divide_by_zero (); \
 \
    return octave_value (v1.T1 ## _scalar_value () / d); \
  } \
 \
  DEFBINOP_FN (ss_el_pow, T1 ## _scalar, T2 ## _scalar, xpow) \
 \
  DEFBINOP (ss_el_ldiv, T1 ## _scalar, T2 ## _scalar) \
  { \
    CAST_BINOP_ARGS (const octave_ ## T1 ## _scalar&, const octave_ ## T2 ## _scalar&); \
 \
    double d = v1.T1 ## _scalar_value (); \
 \
    if (d == 0.0) \
      gripe_divide_by_zero (); \
 \
    return octave_value (v2.T2 ## _scalar_value () / d); \
  } \

#define OCTAVE_SS_INT_BOOL_OPS(T1, T2) \
  /* DEFBINOP_OP (ss_el_and, T1 ## _scalar, T2 ## _scalar, &&) */ \
  /* DEFBINOP_OP (ss_el_or, T1 ## _scalar, T2 ## _scalar, ||) */

#define OCTAVE_SS_INT_CMP_OPS(T1, T2) \
  DEFBINOP_OP (ss_lt, T1 ## _scalar, T2 ## _scalar, <) \
  DEFBINOP_OP (ss_le, T1 ## _scalar, T2 ## _scalar, <=) \
  DEFBINOP_OP (ss_eq, T1 ## _scalar, T2 ## _scalar, ==) \
  DEFBINOP_OP (ss_ge, T1 ## _scalar, T2 ## _scalar, >=) \
  DEFBINOP_OP (ss_gt, T1 ## _scalar, T2 ## _scalar, >) \
  DEFBINOP_OP (ss_ne, T1 ## _scalar, T2 ## _scalar, !=)

#define OCTAVE_SS_INT_OPS(TYPE) \
  OCTAVE_S_INT_UNOPS (TYPE) \
  OCTAVE_SS_INT_ARITH_OPS (TYPE, TYPE) \
  OCTAVE_SS_INT_CMP_OPS (TYPE, TYPE) \
  OCTAVE_SS_INT_BOOL_OPS (TYPE, TYPE)

#define OCTAVE_SS_INT_OPS2(T1, T2) \
  OCTAVE_SS_INT_ARITH_OPS (T1, T2) \
  OCTAVE_SS_INT_CMP_OPS (T1, T2) \
  OCTAVE_SS_INT_BOOL_OPS (T1, T2)

#define OCTAVE_SM_INT_ARITH_OPS(TS, TM) \
  /* scalar by matrix ops. */ \
 \
  DEFNDBINOP_OP (sm_add, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, +) \
  DEFNDBINOP_OP (sm_sub, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, -) \
  DEFNDBINOP_OP (sm_mul, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, *) \
 \
  /* DEFBINOP (sm_div, TS ## _scalar, TM ## _matrix) */ \
  /* { */ \
  /* CAST_BINOP_ARGS (const octave_ ## TS ## _scalar&, const octave_ ## TM ## _matrix&); */ \
  /* */ \
  /* Matrix m1 = v1.TM ## _matrix_value (); */ \
  /* Matrix m2 = v2.TM ## _matrix_value (); */ \
  /* */ \
  /* return octave_value (xdiv (m1, m2)); */ \
  /* } */ \
 \
  /* DEFBINOP_FN (sm_pow, TS ## _scalar, TM ## _matrix, xpow) */ \
 \
  /* DEFBINOP (sm_ldiv, TS ## _scalar, TM ## _matrix) */ \
  /* { */ \
  /* CAST_BINOP_ARGS (const octave_ ## TS ## _scalar&, const octave_ ## TM ## _matrix&); */ \
  /* */ \
  /* double d = v1.TS ## _scalar_value (); */ \
  /* */ \
  /* if (d == 0) */ \
  /* gripe_divide_by_zero (); */ \
  /* */ \
  /* return octave_value (v2.TS ## _scalar_value () / d); */ \
  /* } */ \
 \
  DEFNDBINOP_OP (sm_el_mul, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, *) \
  /* DEFNDBINOP_FN (sm_el_div, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, x_el_div) */ \
  /* DEFNDBINOP_FN (sm_el_pow, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, elem_xpow) */ \
 \
  /* DEFBINOP (sm_el_ldiv, TS ## _scalar, TM ## _matrix) */ \
  /* { */ \
  /* CAST_BINOP_ARGS (const octave_ ## TS ## _scalar&, const octave_ ## TM ## _matrix&); */ \
  /* */ \
  /* double d = v1.TS ## _scalar_value (); */ \
  /* */ \
  /* if (d == 0) */ \
  /* gripe_divide_by_zero (); */ \
  /* */ \
  /* return octave_value (v2.TM ## _array_value () / d); */ \
  /* } */ \

#define OCTAVE_SM_INT_CMP_OPS(TS, TM) \
  DEFNDBINOP_FN (sm_lt, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, mx_el_lt) \
  DEFNDBINOP_FN (sm_le, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, mx_el_le) \
  DEFNDBINOP_FN (sm_eq, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, mx_el_eq) \
  DEFNDBINOP_FN (sm_ge, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, mx_el_ge) \
  DEFNDBINOP_FN (sm_gt, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, mx_el_gt) \
  DEFNDBINOP_FN (sm_ne, TS ## _scalar, TM ## _matrix, TS ## _scalar, TM ## _array, mx_el_ne)

#define OCTAVE_SM_INT_BOOL_OPS(TS, TM) \
  /* DEFNDBINOP_FN (sm_el_and, TS ## _scalar, TYPE ## _matrix, TS ## _scalar, TYPE ## _array, mx_el_and) */ \
  /* DEFNDBINOP_FN (sm_el_or,  TS ## _scalar, TYPE ## _matrix, TS ## _scalar, TYPE ## _array, mx_el_or) */

#define OCTAVE_SM_INT_OPS(TYPE) \
  OCTAVE_SM_INT_ARITH_OPS (TYPE, TYPE) \
  OCTAVE_SM_INT_CMP_OPS (TYPE, TYPE) \
  OCTAVE_SM_INT_BOOL_OPS (TYPE, TYPE) \
 \
  /* DEFCONV (TYPE ## _matrix_conv, TYPE ## _scalar, TYPE ## _matrix) */ \
  /* { */ \
  /* CAST_CONV_ARG (const octave_ ## TYPE ## _scalar&); */ \
  /* */ \
  /* return new octave_ ## TYPE ## _matrix (v.TYPE ## _matrix_value ()); */ \
  /* } */

#define OCTAVE_SM_INT_OPS2(TS, TM) \
  OCTAVE_SM_INT_ARITH_OPS (TS, TM) \
  OCTAVE_SM_INT_CMP_OPS (TS, TM) \
  OCTAVE_SM_INT_BOOL_OPS (TS, TM)

#define OCTAVE_MS_INT_ARITH_OPS(TM, TS) \
  /* matrix by scalar ops. */ \
 \
  DEFNDBINOP_OP (ms_add, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, +) \
  DEFNDBINOP_OP (ms_sub, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, -) \
  DEFNDBINOP_OP (ms_mul, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, *) \
 \
  /* DEFBINOP (ms_div, TM ## _matrix, TS ## _scalar) */ \
  /* { */ \
  /* CAST_BINOP_ARGS (const octave_ ## TM ## _matrix&, const octave_ ## TS ## _scalar&); */ \
  /* */ \
  /* double d = v2.TM ## _ ## TS ## _scalar_value (); */ \
  /* */ \
  /* if (d == 0.0) */ \
  /* gripe_divide_by_zero (); */ \
  /* */ \
  /* return octave_value (v1.TM ## _array_value () / d); */ \
  /* } */ \
 \
  /* DEFBINOP_FN (ms_pow, TM ## _matrix, TS ## _scalar, xpow) */ \
 \
  /* DEFBINOP (ms_ldiv, TM ## _matrix, TS ## _scalar) */ \
  /* { */ \
  /* CAST_BINOP_ARGS (const octave_ ## TM ## _matrix&, const octave_ ## TS ## _scalar&); */ \
  /* */ \
  /* Matrix m1 = v1.TM ## _matrix_value (); */ \
  /* Matrix m2 = v2.TM ## _matrix_value (); */ \
  /* */ \
  /* return octave_value (xleftdiv (m1, m2)); */ \
  /* } */ \
 \
  DEFNDBINOP_OP (ms_el_mul, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, *) \
 \
  /* DEFBINOP (ms_el_div, TM ## _matrix, TS ## _scalar) */ \
  /* { */ \
  /* CAST_BINOP_ARGS (const octave_ ## TM ## _matrix&, const octave_ ## TS ## _scalar&); */ \
  /* */ \
  /* double d = v2.TM ## _ ## TS ## _scalar_value (); */ \
  /* */ \
  /* if (d == 0.0) */ \
  /* gripe_divide_by_zero (); */ \
  /* */ \
  /* return octave_value (v1.TM ## _array_value () / d); */ \
  /* } */ \
 \
  /* DEFNDBINOP_FN (ms_el_pow, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, elem_xpow) */ \
 \
  /* DEFBINOP (el_ldiv, TM ## _matrix, TS ## _scalar) */ \
  /* { */ \
  /* CAST_BINOP_ARGS (const octave_ ## TM ## _matrix&, const octave_ ## TS ## _scalar&); */ \
  /* */ \
  /* return x_el_div (v2.TM ## _ ## TS ## _scalar_value (), v1.TM ## _array_value ()); */ \
  /* } */

#define OCTAVE_MS_INT_CMP_OPS(TM, TS) \
  DEFNDBINOP_FN (ms_lt, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, mx_el_lt) \
  DEFNDBINOP_FN (ms_le, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, mx_el_le) \
  DEFNDBINOP_FN (ms_eq, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, mx_el_eq) \
  DEFNDBINOP_FN (ms_ge, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, mx_el_ge) \
  DEFNDBINOP_FN (ms_gt, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, mx_el_gt) \
  DEFNDBINOP_FN (ms_ne, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, mx_el_ne) \

#define OCTAVE_MS_INT_BOOL_OPS(TM, TS) \
  /* DEFNDBINOP_FN (ms_el_and, TM ## _matrix, TS ## _scalar, TM ## _array, TS ## _scalar, mx_el_and) */ \
  /* DEFNDBINOP_FN (ms_el_or, TM ## _matrix, TS ## _scalar, TM
     ## _array, TS ## _scalar, mx_el_or) */

#define OCTAVE_MS_INT_ASSIGN_OPS(TM, TS) \
  DEFNDASSIGNOP_FN (ms_assign, TM ## _matrix, TS ## _scalar, TS ## _array, assign)

#define OCTAVE_MS_INT_OPS(TYPE) \
  OCTAVE_MS_INT_ARITH_OPS (TYPE, TYPE) \
  OCTAVE_MS_INT_CMP_OPS (TYPE, TYPE) \
  OCTAVE_MS_INT_BOOL_OPS (TYPE, TYPE) \
  OCTAVE_MS_INT_ASSIGN_OPS (TYPE, TYPE)

#define OCTAVE_M_INT_UNOPS(TYPE) \
  /* matrix unary ops. */ \
 \
  DEFNDUNOP_OP (m_not, TYPE ## _matrix, TYPE ## _array, !) \
  DEFNDUNOP_OP (m_uminus, TYPE ## _matrix, TYPE ## _array, -) \
 \
  DEFUNOP (m_transpose, TYPE ## _matrix) \
  { \
    CAST_UNOP_ARG (const octave_ ## TYPE ## _matrix&); \
 \
    if (v.ndims () > 2) \
      { \
	error ("transpose not defined for N-d objects"); \
	return octave_value (); \
      } \
    else \
      return octave_value (v.TYPE ## _array_value().transpose ()); \
  } \
 \
  /* DEFNCUNOP_METHOD (m_incr, TYPE ## _matrix, increment) */ \
  /* DEFNCUNOP_METHOD (m_decr, TYPE ## _matrix, decrement) */

#define OCTAVE_MM_INT_ARITH_OPS(T1, T2) \
  /* matrix by matrix ops. */ \
 \
  DEFNDBINOP_OP (mm_add, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, +) \
  DEFNDBINOP_OP (mm_sub, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, -) \
 \
  /* DEFBINOP_OP (mm_mul, T1 ## _matrix, T2 ## _matrix, *) */ \
  /* DEFBINOP_FN (mm_div, T1 ## _matrix, T2 ## _matrix, xdiv) */ \
 \
  DEFBINOPX (mm_pow, T1 ## _matrix, T2 ## _matrix) \
  { \
    error ("can't do A ^ B for A and B both matrices"); \
    return octave_value (); \
  } \
 \
  /* DEFBINOP_FN (ldiv, T1 ## _matrix, T2 ## _matrix, xleftdiv) */ \
 \
  DEFNDBINOP_FN (mm_el_mul, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, product) \
 \
  DEFNDBINOP_FN (mm_el_div, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, quotient) \
 \
  /* DEFNDBINOP_FN (mm_el_pow, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, elem_xpow) */ \
 \
  /* DEFBINOP (mm_el_ldiv, T1 ## _matrix, T2 ## _matrix) */ \
  /* { */ \
  /* CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&); */ \
  /* */ \
  /* return octave_value (quotient (v2.array_value (), v1.array_value ())); */ \
  /* } */

#define OCTAVE_MM_INT_CMP_OPS(T1, T2) \
  DEFNDBINOP_FN (mm_lt, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, mx_el_lt) \
  DEFNDBINOP_FN (mm_le, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, mx_el_le) \
  DEFNDBINOP_FN (mm_eq, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, mx_el_eq) \
  DEFNDBINOP_FN (mm_ge, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, mx_el_ge) \
  DEFNDBINOP_FN (mm_gt, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, mx_el_gt) \
  DEFNDBINOP_FN (mm_ne, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, mx_el_ne)

#define OCTAVE_MM_INT_BOOL_OPS(T1, T2) \
  DEFNDBINOP_FN (mm_el_and, T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, mx_el_and) \
  DEFNDBINOP_FN (mm_el_or,  T1 ## _matrix, T2 ## _matrix, T1 ## _array, T2 ## _array, mx_el_or)

#define OCTAVE_MM_INT_ASSIGN_OPS(TYPE) \
  DEFNDASSIGNOP_FN (mm_assign, TYPE ## _matrix, TYPE ## _matrix, TYPE ## _array, assign)

#define OCTAVE_MM_INT_OPS(TYPE) \
  OCTAVE_M_INT_UNOPS (TYPE) \
  OCTAVE_MM_INT_ARITH_OPS (TYPE, TYPE) \
  OCTAVE_MM_INT_CMP_OPS (TYPE, TYPE) \
  OCTAVE_MM_INT_BOOL_OPS (TYPE, TYPE) \
  OCTAVE_MM_INT_ASSIGN_OPS (TYPE)

#define OCTAVE_MM_INT_OPS2(T1, T2) \
  OCTAVE_MM_INT_ARITH_OPS (T1, T2) \
  OCTAVE_MM_INT_CMP_OPS (T1, T2) \
  OCTAVE_MM_INT_BOOL_OPS (T1, T2)

#define OCTAVE_INT_OPS(TYPE) \
  OCTAVE_SS_INT_OPS (TYPE) \
  OCTAVE_SM_INT_OPS (TYPE) \
  OCTAVE_MS_INT_OPS (TYPE) \
  OCTAVE_MM_INT_OPS (TYPE)

#define OCTAVE_INSTALL_S_INT_UNOPS(TYPE) \
  INSTALL_UNOP (op_not, octave_ ## TYPE ## _scalar, s_not); \
  INSTALL_UNOP (op_uminus, octave_ ## TYPE ## _scalar, s_uminus); \
  INSTALL_UNOP (op_transpose, octave_ ## TYPE ## _scalar, s_transpose); \
  INSTALL_UNOP (op_hermitian, octave_ ## TYPE ## _scalar, s_hermitian); \
 \
  /* INSTALL_NCUNOP (op_incr, octave_ ## TYPE ## _scalar, s_incr); */ \
  /* INSTALL_NCUNOP (op_decr, octave_ ## TYPE ## _scalar, s_decr); */

#define OCTAVE_INSTALL_SS_INT_ARITH_OPS(T1, T2) \
  INSTALL_BINOP (op_add, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_add); \
  INSTALL_BINOP (op_sub, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_sub); \
  INSTALL_BINOP (op_mul, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_mul); \
  INSTALL_BINOP (op_div, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_div); \
  /* INSTALL_BINOP (op_pow, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_pow); */ \
  INSTALL_BINOP (op_ldiv, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_ldiv); \
  INSTALL_BINOP (op_el_mul, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_el_mul); \
  INSTALL_BINOP (op_el_div, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_el_div); \
  INSTALL_BINOP (op_el_pow, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_el_pow); \
  INSTALL_BINOP (op_el_ldiv, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_el_ldiv);

#define OCTAVE_INSTALL_SS_INT_CMP_OPS(T1, T2) \
  INSTALL_BINOP (op_lt, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_lt); \
  INSTALL_BINOP (op_le, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_le); \
  INSTALL_BINOP (op_eq, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_eq); \
  INSTALL_BINOP (op_ge, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_ge); \
  INSTALL_BINOP (op_gt, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_gt); \
  INSTALL_BINOP (op_ne, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_ne);

#define OCTAVE_INSTALL_SS_INT_BOOL_OPS(T1, T2) \
  /* INSTALL_BINOP (op_el_and, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_el_and); */ \
  /* INSTALL_BINOP (op_el_or, octave_ ## T1 ## _scalar, octave_ ## T2 ## _scalar, ss_el_or); */

#define OCTAVE_INSTALL_SS_INT_OPS(TYPE) \
  OCTAVE_INSTALL_S_INT_UNOPS (TYPE) \
  OCTAVE_INSTALL_SS_INT_ARITH_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_SS_INT_CMP_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_SS_INT_BOOL_OPS (TYPE, TYPE) \
  INSTALL_ASSIGNCONV (octave_ ## TYPE ## _scalar, octave_ ## TYPE ## _scalar, octave_ ## TYPE ## _matrix)

#define OCTAVE_INSTALL_SS_INT_OPS2(T1, T2) \
  OCTAVE_INSTALL_SS_INT_ARITH_OPS (T1, T2) \
  OCTAVE_INSTALL_SS_INT_CMP_OPS (T1, T2) \
  OCTAVE_INSTALL_SS_INT_BOOL_OPS (T1, T2)

#define OCTAVE_INSTALL_SM_INT_ARITH_OPS(T1, T2) \
  INSTALL_BINOP (op_add, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_add); \
  INSTALL_BINOP (op_sub, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_sub); \
  INSTALL_BINOP (op_mul, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_mul); \
  /* INSTALL_BINOP (op_div, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_div); */ \
  /* INSTALL_BINOP (op_pow, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_pow); */ \
  /* INSTALL_BINOP (op_ldiv, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_ldiv); */ \
  INSTALL_BINOP (op_el_mul, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_el_mul); \
  /* INSTALL_BINOP (op_el_div, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_el_div); */ \
  /* INSTALL_BINOP (op_el_pow, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_el_pow); */ \
  /* INSTALL_BINOP (op_el_ldiv, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_el_ldiv); */

#define OCTAVE_INSTALL_SM_INT_CMP_OPS(T1, T2) \
  INSTALL_BINOP (op_lt, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_lt); \
  INSTALL_BINOP (op_le, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_le); \
  INSTALL_BINOP (op_eq, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_eq); \
  INSTALL_BINOP (op_ge, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_ge); \
  INSTALL_BINOP (op_gt, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_gt); \
  INSTALL_BINOP (op_ne, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_ne);

#define OCTAVE_INSTALL_SM_INT_BOOL_OPS(T1, T2) \
  /* INSTALL_BINOP (op_el_and, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_el_and); */ \
  /* INSTALL_BINOP (op_el_or, octave_ ## T1 ## _scalar, octave_ ## T2 ## _matrix, sm_el_or); */

#define OCTAVE_INSTALL_SM_INT_OPS(TYPE) \
  OCTAVE_INSTALL_SM_INT_ARITH_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_SM_INT_CMP_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_SM_INT_BOOL_OPS (TYPE, TYPE) \
  /* INSTALL_WIDENOP (octave_ ## TYPE ## _scalar, octave_ ## TYPE ## _matrix, TYPE ## _matrix_conv); */ \
  INSTALL_ASSIGNCONV (octave_ ## TYPE ## _scalar, octave_ ## TYPE ## _matrix, octave_ ## TYPE ## _matrix)

#define OCTAVE_INSTALL_SM_INT_OPS2(T1, T2) \
  OCTAVE_INSTALL_SM_INT_ARITH_OPS (T1, T2) \
  OCTAVE_INSTALL_SM_INT_CMP_OPS (T1, T2) \
  OCTAVE_INSTALL_SM_INT_BOOL_OPS (T1, T2)

#define OCTAVE_INSTALL_MS_INT_ARITH_OPS(T1, T2) \
  INSTALL_BINOP (op_add, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_add); \
  INSTALL_BINOP (op_sub, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_sub); \
  INSTALL_BINOP (op_mul, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_mul); \
  /* INSTALL_BINOP (op_div, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_div); */ \
  /* INSTALL_BINOP (op_pow, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_pow); */ \
  /* INSTALL_BINOP (op_ldiv, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_ldiv); */ \
 \
  INSTALL_BINOP (op_el_mul, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_el_mul); \
  /* INSTALL_BINOP (op_el_div, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_el_div); */ \
  /* INSTALL_BINOP (op_el_pow, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_el_pow); */ \
  /* INSTALL_BINOP (op_el_ldiv, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_el_ldiv); */

#define OCTAVE_INSTALL_MS_INT_CMP_OPS(T1, T2) \
  /* INSTALL_BINOP (op_lt, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_lt); */ \
 \
  octave_value_typeinfo::register_binary_op \
    (octave_value::op_lt, octave_ ## T1 ## _matrix::static_type_id (), \
     octave_ ## T2 ## _scalar::static_type_id (), oct_binop_ms_lt); \
 \
  INSTALL_BINOP (op_le, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_le); \
  INSTALL_BINOP (op_eq, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_eq); \
  INSTALL_BINOP (op_ge, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_ge); \
  INSTALL_BINOP (op_gt, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_gt); \
  INSTALL_BINOP (op_ne, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_ne);

#define OCTAVE_INSTALL_MS_INT_BOOL_OPS(T1, T2) \
  /* INSTALL_BINOP (op_el_and, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_el_and); */ \
  /* INSTALL_BINOP (op_el_or, octave_ ## T1 ## _matrix, octave_ ## T2 ## _scalar, ms_el_or); */

#define OCTAVE_INSTALL_MS_INT_OPS(TYPE) \
  OCTAVE_INSTALL_MS_INT_ARITH_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_MS_INT_CMP_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_MS_INT_BOOL_OPS (TYPE, TYPE) \
  INSTALL_ASSIGNOP (op_asn_eq, octave_ ## TYPE ## _matrix, octave_ ## TYPE ## _scalar, ms_assign)

#define OCTAVE_INSTALL_MS_INT_OPS2(T1, T2) \
  OCTAVE_INSTALL_MS_INT_ARITH_OPS (T1, T2) \
  OCTAVE_INSTALL_MS_INT_CMP_OPS (T1, T2) \
  OCTAVE_INSTALL_MS_INT_BOOL_OPS (T1, T2)

#define OCTAVE_INSTALL_M_INT_UNOPS(TYPE) \
  INSTALL_UNOP (op_not, octave_ ## TYPE ## _matrix, m_not); \
  INSTALL_UNOP (op_uminus, octave_ ## TYPE ## _matrix, m_uminus); \
  INSTALL_UNOP (op_transpose, octave_ ## TYPE ## _matrix, m_transpose); \
  INSTALL_UNOP (op_hermitian, octave_ ## TYPE ## _matrix, m_transpose); \
 \
  /* INSTALL_NCUNOP (op_incr, octave_ ## TYPE ## _matrix, m_incr); */ \
  /* INSTALL_NCUNOP (op_decr, octave_ ## TYPE ## _matrix, m_decr); */

#define OCTAVE_INSTALL_MM_INT_ARITH_OPS(T1, T2) \
  INSTALL_BINOP (op_add, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_add); \
  INSTALL_BINOP (op_sub, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_sub); \
  /* INSTALL_BINOP (op_mul, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_mul); */ \
  /* INSTALL_BINOP (op_div, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_div); */ \
  INSTALL_BINOP (op_pow, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_pow); \
  /* INSTALL_BINOP (op_ldiv, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_ldiv); */ \
  INSTALL_BINOP (op_el_mul, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_el_mul); \
  INSTALL_BINOP (op_el_div, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_el_div); \
  /* INSTALL_BINOP (op_el_pow, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_el_pow); */ \
  /* INSTALL_BINOP (op_el_ldiv, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_el_ldiv); */

#define OCTAVE_INSTALL_MM_INT_CMP_OPS(T1, T2) \
  INSTALL_BINOP (op_lt, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_lt); \
  INSTALL_BINOP (op_le, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_le); \
  INSTALL_BINOP (op_eq, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_eq); \
  INSTALL_BINOP (op_ge, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_ge); \
  INSTALL_BINOP (op_gt, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_gt); \
  INSTALL_BINOP (op_ne, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_ne);

#define OCTAVE_INSTALL_MM_INT_BOOL_OPS(T1, T2) \
  INSTALL_BINOP (op_el_and, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_el_and); \
  INSTALL_BINOP (op_el_or, octave_ ## T1 ## _matrix, octave_ ## T2 ## _matrix, mm_el_or);

#define OCTAVE_INSTALL_MM_INT_ASSIGN_OPS(TYPE) \
  INSTALL_ASSIGNOP (op_asn_eq, octave_ ## TYPE ## _matrix, octave_ ## TYPE ## _matrix, mm_assign)

#define OCTAVE_INSTALL_MM_INT_OPS(TYPE) \
  OCTAVE_INSTALL_M_INT_UNOPS (TYPE) \
  OCTAVE_INSTALL_MM_INT_ARITH_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_MM_INT_CMP_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_MM_INT_BOOL_OPS (TYPE, TYPE) \
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (TYPE)

#define OCTAVE_INSTALL_MM_INT_OPS2(T1, T2) \
  OCTAVE_INSTALL_MM_INT_ARITH_OPS (T1, T2) \
  OCTAVE_INSTALL_MM_INT_CMP_OPS (T1, T2) \
  OCTAVE_INSTALL_MM_INT_BOOL_OPS (T1, T2)

#define OCTAVE_INSTALL_INT_OPS(TYPE) \
  OCTAVE_INSTALL_SS_INT_OPS (TYPE) \
  OCTAVE_INSTALL_SM_INT_OPS (TYPE) \
  OCTAVE_INSTALL_MS_INT_OPS (TYPE) \
  OCTAVE_INSTALL_MM_INT_OPS (TYPE)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

