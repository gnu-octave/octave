////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// unary complex matrix ops.

DEFNDUNOP_OP (not, float_complex_matrix, float_complex_array, !)
DEFNDUNOP_OP (uplus, float_complex_matrix, float_complex_array, /* no-op */)
DEFNDUNOP_OP (uminus, float_complex_matrix, float_complex_array, -)

DEFUNOP (transpose, float_complex_matrix)
{
  const octave_float_complex_matrix& v
    = dynamic_cast<const octave_float_complex_matrix&> (a);

  if (v.ndims () > 2)
    error ("transpose not defined for N-D objects");

  return octave_value (v.float_complex_matrix_value ().transpose ());
}

DEFUNOP (hermitian, float_complex_matrix)
{
  const octave_float_complex_matrix& v
    = dynamic_cast<const octave_float_complex_matrix&> (a);

  if (v.ndims () > 2)
    error ("complex-conjugate transpose not defined for N-D objects");

  return octave_value (v.float_complex_matrix_value ().hermitian ());
}

DEFNCUNOP_METHOD (incr, float_complex_matrix, increment)
DEFNCUNOP_METHOD (decr, float_complex_matrix, decrement)
DEFNCUNOP_METHOD (changesign, float_complex_matrix, changesign)

// complex matrix by complex matrix ops.

DEFNDBINOP_OP (add, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, +)
DEFNDBINOP_OP (sub, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, -)

DEFBINOP_OP (mul, float_complex_matrix, float_complex_matrix, *)

DEFBINOP (div, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);
  MatrixType typ = v2.matrix_type ();

  FloatComplexMatrix ret = xdiv (v1.float_complex_matrix_value (),
                                 v2.float_complex_matrix_value (), typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOPX (pow, float_complex_matrix, float_complex_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
}

DEFBINOP (ldiv, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (v1.float_complex_matrix_value (),
                                     v2.float_complex_matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP (trans_mul, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);
  return octave_value(xgemm (v1.float_complex_matrix_value (),
                             v2.float_complex_matrix_value (),
                             blas_trans, blas_no_trans));
}

DEFBINOP (mul_trans, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);
  return octave_value(xgemm (v1.float_complex_matrix_value (),
                             v2.float_complex_matrix_value (),
                             blas_no_trans, blas_trans));
}

DEFBINOP (herm_mul, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);
  return octave_value(xgemm (v1.float_complex_matrix_value (),
                             v2.float_complex_matrix_value (),
                             blas_conj_trans, blas_no_trans));
}

DEFBINOP (mul_herm, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);
  return octave_value(xgemm (v1.float_complex_matrix_value (),
                             v2.float_complex_matrix_value (),
                             blas_no_trans, blas_conj_trans));
}

DEFBINOP (trans_ldiv, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (v1.float_complex_matrix_value (),
                                     v2.float_complex_matrix_value (),
                                     typ, blas_trans);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP (herm_ldiv, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (v1.float_complex_matrix_value (),
                                     v2.float_complex_matrix_value (),
                                     typ, blas_conj_trans);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_ne)

DEFNDBINOP_FN (el_mul, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, product)
DEFNDBINOP_FN (el_div, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, quotient)
DEFNDBINOP_FN (el_pow, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, elem_xpow)

DEFBINOP (el_ldiv, float_complex_matrix, float_complex_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_complex_matrix& v2
    = dynamic_cast<const octave_float_complex_matrix&> (a2);

  return octave_value (quotient (v2.float_complex_array_value (),
                                 v1.float_complex_array_value ()));
}

DEFNDBINOP_FN (el_and, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, mx_el_and)
DEFNDBINOP_FN (el_or,  float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, mx_el_or)

DEFNDCATOP_FN (fcm_fcm, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, concat)

DEFNDCATOP_FN (cm_fcm, complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, concat)

DEFNDCATOP_FN (fcm_cm, float_complex_matrix, complex_matrix,
               float_complex_array, float_complex_array, concat)

DEFNDASSIGNOP_FN (assign, float_complex_matrix, float_complex_matrix,
                  float_complex_array, assign)
DEFNDASSIGNOP_FN (sgl_clx_assign, float_complex_matrix, complex_matrix,
                  float_complex_array, assign)
DEFNDASSIGNOP_FN (sgl_assign, float_complex_matrix, matrix,
                  float_complex_array, assign)
DEFNDASSIGNOP_FN (dbl_assign, complex_matrix, float_complex_matrix,
                  complex_array, assign)

DEFNULLASSIGNOP_FN (null_assign, float_complex_matrix, delete_elements)

DEFNDASSIGNOP_OP (assign_add, float_complex_matrix,
                  float_complex_matrix, float_complex_array, +=)
DEFNDASSIGNOP_OP (assign_sub, float_complex_matrix,
                  float_complex_matrix, float_complex_array, -=)
DEFNDASSIGNOP_FNOP (assign_el_mul, float_complex_matrix, float_complex_matrix,
                    float_complex_array, product_eq)
DEFNDASSIGNOP_FNOP (assign_el_div, float_complex_matrix, float_complex_matrix,
                    float_complex_array, quotient_eq)

void
install_fcm_fcm_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_not, octave_float_complex_matrix, not);
  INSTALL_UNOP_TI (ti, op_uplus, octave_float_complex_matrix, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_float_complex_matrix, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_float_complex_matrix, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_float_complex_matrix, hermitian);

  INSTALL_NCUNOP_TI (ti, op_incr, octave_float_complex_matrix, incr);
  INSTALL_NCUNOP_TI (ti, op_decr, octave_float_complex_matrix, decr);
  INSTALL_NCUNOP_TI (ti, op_uminus, octave_float_complex_matrix, changesign);

  INSTALL_BINOP_TI (ti, op_add, octave_float_complex_matrix,
                    octave_float_complex_matrix, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_float_complex_matrix,
                    octave_float_complex_matrix, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_float_complex_matrix,
                    octave_float_complex_matrix, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_float_complex_matrix,
                    octave_float_complex_matrix, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_float_complex_matrix,
                    octave_float_complex_matrix, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_float_complex_matrix,
                    octave_float_complex_matrix, ldiv);
  INSTALL_BINOP_TI (ti, op_trans_mul, octave_float_complex_matrix,
                    octave_float_complex_matrix, trans_mul);
  INSTALL_BINOP_TI (ti, op_mul_trans, octave_float_complex_matrix,
                    octave_float_complex_matrix, mul_trans);
  INSTALL_BINOP_TI (ti, op_herm_mul, octave_float_complex_matrix,
                    octave_float_complex_matrix, herm_mul);
  INSTALL_BINOP_TI (ti, op_mul_herm, octave_float_complex_matrix,
                    octave_float_complex_matrix, mul_herm);
  INSTALL_BINOP_TI (ti, op_trans_ldiv, octave_float_complex_matrix,
                    octave_float_complex_matrix, trans_ldiv);
  INSTALL_BINOP_TI (ti, op_herm_ldiv, octave_float_complex_matrix,
                    octave_float_complex_matrix, herm_ldiv);

  INSTALL_BINOP_TI (ti, op_lt, octave_float_complex_matrix,
                    octave_float_complex_matrix, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_float_complex_matrix,
                    octave_float_complex_matrix, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_float_complex_matrix,
                    octave_float_complex_matrix, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_float_complex_matrix,
                    octave_float_complex_matrix, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_float_complex_matrix,
                    octave_float_complex_matrix, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_float_complex_matrix,
                    octave_float_complex_matrix, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_float_complex_matrix,
                    octave_float_complex_matrix, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_float_complex_matrix,
                    octave_float_complex_matrix, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_float_complex_matrix,
                    octave_float_complex_matrix, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_float_complex_matrix,
                    octave_float_complex_matrix, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_float_complex_matrix,
                    octave_float_complex_matrix, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_float_complex_matrix,
                    octave_float_complex_matrix, el_or);

  INSTALL_CATOP_TI (ti, octave_float_complex_matrix,
                    octave_float_complex_matrix, fcm_fcm);
  INSTALL_CATOP_TI (ti, octave_complex_matrix,
                    octave_float_complex_matrix, cm_fcm);
  INSTALL_CATOP_TI (ti, octave_float_complex_matrix,
                    octave_complex_matrix, fcm_cm);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix,
                       octave_float_complex_matrix, assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix,
                       octave_complex_matrix, sgl_clx_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix,
                       octave_matrix, sgl_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_complex_matrix,
                       octave_float_complex_matrix, dbl_assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix,
                       octave_null_matrix, null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix,
                       octave_null_str, null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix,
                       octave_null_sq_str, null_assign);

  INSTALL_ASSIGNOP_TI (ti, op_add_eq, octave_float_complex_matrix,
                       octave_float_complex_matrix, assign_add);
  INSTALL_ASSIGNOP_TI (ti, op_sub_eq, octave_float_complex_matrix,
                       octave_float_complex_matrix, assign_sub);
  INSTALL_ASSIGNOP_TI (ti, op_el_mul_eq, octave_float_complex_matrix,
                       octave_float_complex_matrix, assign_el_mul);
  INSTALL_ASSIGNOP_TI (ti, op_el_div_eq, octave_float_complex_matrix,
                       octave_float_complex_matrix, assign_el_div);
}

OCTAVE_END_NAMESPACE(octave)
