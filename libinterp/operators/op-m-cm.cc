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

#include "mx-m-cm.h"
#include "mx-cm-m.h"
#include "mx-nda-cnda.h"
#include "mx-cnda-nda.h"

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// matrix by complex matrix ops.

DEFNDBINOP_OP (add, matrix, complex_matrix, array, complex_array, +)
DEFNDBINOP_OP (sub, matrix, complex_matrix, array, complex_array, -)

DEFBINOP_OP (mul, matrix, complex_matrix, *)

DEFBINOP (trans_mul, matrix, complex_matrix)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex_matrix& v2
    = dynamic_cast<const octave_complex_matrix&> (a2);

  Matrix m1 = v1.matrix_value ();
  ComplexMatrix m2 = v2.complex_matrix_value ();

  return ComplexMatrix (xgemm (m1, real (m2), blas_trans, blas_no_trans),
                        xgemm (m1, imag (m2), blas_trans, blas_no_trans));
}

DEFBINOP (div, matrix, complex_matrix)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex_matrix& v2
    = dynamic_cast<const octave_complex_matrix&> (a2);
  MatrixType typ = v2.matrix_type ();

  ComplexMatrix ret = xdiv (v1.matrix_value (),
                            v2.complex_matrix_value (), typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOPX (pow, matrix, complex_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
}

DEFBINOP (ldiv, matrix, complex_matrix)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex_matrix& v2
    = dynamic_cast<const octave_complex_matrix&> (a2);
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (v1.matrix_value (),
                                v2.complex_matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP (trans_ldiv, matrix, complex_matrix)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex_matrix& v2
    = dynamic_cast<const octave_complex_matrix&> (a2);
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (v1.matrix_value (),
                                v2.complex_matrix_value (), typ, blas_trans);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, matrix, complex_matrix, array, complex_array, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, matrix, complex_matrix, array, complex_array, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, matrix, complex_matrix, array, complex_array, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, matrix, complex_matrix, array, complex_array, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, matrix, complex_matrix, array, complex_array, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, matrix, complex_matrix, array, complex_array, mx_el_ne)

DEFNDBINOP_FN (el_mul, matrix, complex_matrix, array, complex_array, product)
DEFNDBINOP_FN (el_div, matrix, complex_matrix, array, complex_array, quotient)
DEFNDBINOP_FN (el_pow, matrix, complex_matrix, array, complex_array, elem_xpow)

DEFBINOP (el_ldiv, matrix, complex_matrix)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex_matrix& v2
    = dynamic_cast<const octave_complex_matrix&> (a2);

  return quotient (v2.complex_array_value (), v1.array_value ());
}

DEFNDBINOP_FN (el_and, matrix, complex_matrix, array, complex_array, mx_el_and)
DEFNDBINOP_FN (el_or,  matrix, complex_matrix, array, complex_array, mx_el_or)

DEFNDCATOP_FN (m_cm, matrix, complex_matrix, array, complex_array, concat)

DEFCONV (complex_matrix_conv, matrix, complex_matrix)
{
  const octave_matrix& v = dynamic_cast<const octave_matrix&> (a);

  return new octave_complex_matrix (ComplexNDArray (v.array_value ()));
}

void
install_m_cm_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_matrix, octave_complex_matrix, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_matrix, octave_complex_matrix, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_matrix, octave_complex_matrix, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_matrix, octave_complex_matrix, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_matrix, octave_complex_matrix, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_matrix, octave_complex_matrix, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_matrix, octave_complex_matrix, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_matrix, octave_complex_matrix, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_matrix, octave_complex_matrix, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_matrix, octave_complex_matrix, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_matrix, octave_complex_matrix, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_matrix, octave_complex_matrix, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_matrix, octave_complex_matrix, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_matrix, octave_complex_matrix, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_matrix, octave_complex_matrix, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_matrix, octave_complex_matrix,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_matrix, octave_complex_matrix, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_matrix, octave_complex_matrix, el_or);
  INSTALL_BINOP_TI (ti, op_trans_mul, octave_matrix, octave_complex_matrix,
                    trans_mul);
  INSTALL_BINOP_TI (ti, op_herm_mul, octave_matrix, octave_complex_matrix,
                    trans_mul);
  INSTALL_BINOP_TI (ti, op_trans_ldiv, octave_matrix, octave_complex_matrix,
                    trans_ldiv);
  INSTALL_BINOP_TI (ti, op_herm_ldiv, octave_matrix, octave_complex_matrix,
                    trans_ldiv);

  INSTALL_CATOP_TI (ti, octave_matrix, octave_complex_matrix, m_cm);

  INSTALL_ASSIGNCONV_TI (ti, octave_matrix, octave_complex_matrix,
                         octave_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_float_matrix, octave_complex_matrix,
                         octave_float_complex_matrix);

  INSTALL_WIDENOP_TI (ti, octave_matrix, octave_complex_matrix,
                      complex_matrix_conv);
}

OCTAVE_END_NAMESPACE(octave)
