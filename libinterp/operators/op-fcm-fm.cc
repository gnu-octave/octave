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

#include "mx-fcm-fm.h"
#include "mx-fm-fcm.h"
#include "mx-fcnda-fnda.h"
#include "mx-fnda-fcnda.h"

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// complex matrix by matrix ops.

DEFNDBINOP_OP (add, float_complex_matrix, float_matrix, float_complex_array,
               float_array, +)
DEFNDBINOP_OP (sub, float_complex_matrix, float_matrix, float_complex_array,
               float_array, -)

DEFBINOP_OP (mul, float_complex_matrix, float_matrix, *)

DEFBINOP (mul_trans, float_complex_matrix, float_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_matrix& v2
    = dynamic_cast<const octave_float_matrix&> (a2);

  FloatComplexMatrix m1 = v1.float_complex_matrix_value ();
  FloatMatrix m2 = v2.float_matrix_value ();

  return FloatComplexMatrix (xgemm (real (m1), m2, blas_no_trans, blas_trans),
                             xgemm (imag (m1), m2, blas_no_trans, blas_trans));
}

DEFBINOP (div, float_complex_matrix, float_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_matrix& v2
    = dynamic_cast<const octave_float_matrix&> (a2);
  MatrixType typ = v2.matrix_type ();

  FloatComplexMatrix ret = xdiv (v1.float_complex_matrix_value (),
                                 v2.float_matrix_value (), typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOPX (pow, float_complex_matrix, float_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
}

DEFBINOP (ldiv, float_complex_matrix, float_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_matrix& v2
    = dynamic_cast<const octave_float_matrix&> (a2);
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (v1.float_complex_matrix_value (),
                                     v2.float_matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, float_complex_matrix, float_matrix,
                    float_complex_array, float_array, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, float_complex_matrix, float_matrix,
                    float_complex_array, float_array, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, float_complex_matrix, float_matrix,
                    float_complex_array, float_array, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, float_complex_matrix, float_matrix,
                    float_complex_array, float_array, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, float_complex_matrix, float_matrix,
                    float_complex_array, float_array, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, float_complex_matrix, float_matrix,
                    float_complex_array, float_array, mx_el_ne)

DEFNDBINOP_FN (el_mul, float_complex_matrix, float_matrix,
               float_complex_array, float_array, product)
DEFNDBINOP_FN (el_div, float_complex_matrix, float_matrix,
               float_complex_array, float_array, quotient)
DEFNDBINOP_FN (el_pow, float_complex_matrix, float_matrix,
               float_complex_array, float_array, elem_xpow)

DEFBINOP (el_ldiv, float_complex_matrix, float_matrix)
{
  const octave_float_complex_matrix& v1
    = dynamic_cast<const octave_float_complex_matrix&> (a1);
  const octave_float_matrix& v2
    = dynamic_cast<const octave_float_matrix&> (a2);

  return quotient (v2.float_array_value (), v1.float_complex_array_value ());
}

DEFNDBINOP_FN (el_and, float_complex_matrix, float_matrix,
               float_complex_array, float_array, mx_el_and)
DEFNDBINOP_FN (el_or,  float_complex_matrix, float_matrix,
               float_complex_array, float_array, mx_el_or)

DEFNDCATOP_FN (fcm_fm, float_complex_matrix, float_matrix,
               float_complex_array, float_array, concat)

DEFNDCATOP_FN (cm_fm, complex_matrix, float_matrix,
               float_complex_array, float_array, concat)

DEFNDCATOP_FN (fcm_m, float_complex_matrix, matrix,
               float_complex_array, float_array, concat)

DEFNDASSIGNOP_FN (assign, float_complex_matrix, float_matrix,
                  float_complex_array, assign)
DEFNDASSIGNOP_FN (dbl_assign, complex_matrix, float_matrix,
                  complex_array, assign)

void
install_fcm_fm_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_float_complex_matrix, octave_float_matrix,
                    add);
  INSTALL_BINOP_TI (ti, op_sub, octave_float_complex_matrix, octave_float_matrix,
                    sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_float_complex_matrix, octave_float_matrix,
                    mul);
  INSTALL_BINOP_TI (ti, op_div, octave_float_complex_matrix, octave_float_matrix,
                    div);
  INSTALL_BINOP_TI (ti, op_pow, octave_float_complex_matrix, octave_float_matrix,
                    pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_float_complex_matrix,
                    octave_float_matrix, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_float_complex_matrix, octave_float_matrix,
                    lt);
  INSTALL_BINOP_TI (ti, op_le, octave_float_complex_matrix, octave_float_matrix,
                    le);
  INSTALL_BINOP_TI (ti, op_eq, octave_float_complex_matrix, octave_float_matrix,
                    eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_float_complex_matrix, octave_float_matrix,
                    ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_float_complex_matrix, octave_float_matrix,
                    gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_float_complex_matrix, octave_float_matrix,
                    ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_float_complex_matrix,
                    octave_float_matrix, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_float_complex_matrix,
                    octave_float_matrix, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_float_complex_matrix,
                    octave_float_matrix, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_float_complex_matrix,
                    octave_float_matrix, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_float_complex_matrix,
                    octave_float_matrix, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_float_complex_matrix,
                    octave_float_matrix, el_or);
  INSTALL_BINOP_TI (ti, op_mul_trans, octave_float_complex_matrix,
                    octave_float_matrix, mul_trans);
  INSTALL_BINOP_TI (ti, op_mul_herm, octave_float_complex_matrix,
                    octave_float_matrix, mul_trans);

  INSTALL_CATOP_TI (ti, octave_float_complex_matrix, octave_float_matrix, fcm_fm);
  INSTALL_CATOP_TI (ti, octave_complex_matrix, octave_float_matrix, cm_fm);
  INSTALL_CATOP_TI (ti, octave_float_complex_matrix, octave_matrix, fcm_m);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix,
                       octave_float_matrix, assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_complex_matrix,
                       octave_float_matrix, dbl_assign);
}

OCTAVE_END_NAMESPACE(octave)
