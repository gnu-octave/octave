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

#include "mx-cm-m.h"
#include "mx-m-cm.h"
#include "mx-cnda-nda.h"
#include "mx-nda-cnda.h"

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// complex matrix by matrix ops.

DEFNDBINOP_OP (add, complex_matrix, matrix, complex_array, array, +)
DEFNDBINOP_OP (sub, complex_matrix, matrix, complex_array, array, -)

DEFBINOP_OP (mul, complex_matrix, matrix, *)

DEFBINOP (mul_trans, complex_matrix, matrix)
{
  const octave_complex_matrix& v1
    = dynamic_cast<const octave_complex_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);

  ComplexMatrix m1 = v1.complex_matrix_value ();
  Matrix m2 = v2.matrix_value ();

  return ComplexMatrix (xgemm (real (m1), m2, blas_no_trans, blas_trans),
                        xgemm (imag (m1), m2, blas_no_trans, blas_trans));
}

DEFBINOP (div, complex_matrix, matrix)
{
  const octave_complex_matrix& v1
    = dynamic_cast<const octave_complex_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);
  MatrixType typ = v2.matrix_type ();

  ComplexMatrix ret = xdiv (v1.complex_matrix_value (),
                            v2.matrix_value (), typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOPX (pow, complex_matrix, matrix)
{
  error ("can't do A ^ B for A and B both matrices");
}

DEFBINOP (ldiv, complex_matrix, matrix)
{
  const octave_complex_matrix& v1
    = dynamic_cast<const octave_complex_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (v1.complex_matrix_value (),
                                v2.matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, complex_matrix, matrix, complex_array, array, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, complex_matrix, matrix, complex_array, array, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, complex_matrix, matrix, complex_array, array, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, complex_matrix, matrix, complex_array, array, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, complex_matrix, matrix, complex_array, array, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, complex_matrix, matrix, complex_array, array, mx_el_ne)

DEFNDBINOP_FN (el_mul, complex_matrix, matrix, complex_array, array, product)
DEFNDBINOP_FN (el_div, complex_matrix, matrix, complex_array, array, quotient)
DEFNDBINOP_FN (el_pow, complex_matrix, matrix, complex_array, array, elem_xpow)

DEFBINOP (el_ldiv, complex_matrix, matrix)
{
  const octave_complex_matrix& v1
    = dynamic_cast<const octave_complex_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);

  return quotient (v2.array_value (), v1.complex_array_value ());
}

DEFNDBINOP_FN (el_and, complex_matrix, matrix, complex_array, array, mx_el_and)
DEFNDBINOP_FN (el_or,  complex_matrix, matrix, complex_array, array, mx_el_or)

DEFNDCATOP_FN (cm_m, complex_matrix, matrix, complex_array, array, concat)

DEFNDASSIGNOP_FN (assign, complex_matrix, matrix, complex_array, assign)

void
install_cm_m_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_complex_matrix, octave_matrix, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_complex_matrix, octave_matrix, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_complex_matrix, octave_matrix, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_complex_matrix, octave_matrix, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_complex_matrix, octave_matrix, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_complex_matrix, octave_matrix, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_complex_matrix, octave_matrix, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_complex_matrix, octave_matrix, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_complex_matrix, octave_matrix, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_complex_matrix, octave_matrix, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_complex_matrix, octave_matrix, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_complex_matrix, octave_matrix, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_complex_matrix, octave_matrix, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_complex_matrix, octave_matrix, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_complex_matrix, octave_matrix, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_complex_matrix, octave_matrix,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_complex_matrix, octave_matrix, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_complex_matrix, octave_matrix, el_or);
  INSTALL_BINOP_TI (ti, op_mul_trans, octave_complex_matrix, octave_matrix,
                    mul_trans);
  INSTALL_BINOP_TI (ti, op_mul_herm, octave_complex_matrix, octave_matrix,
                    mul_trans);

  INSTALL_CATOP_TI (ti, octave_complex_matrix, octave_matrix, cm_m);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_complex_matrix, octave_matrix,
                       assign);
}

OCTAVE_END_NAMESPACE(octave)
