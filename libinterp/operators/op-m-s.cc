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

#include "ovl.h"
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// matrix by scalar ops.

DEFNDBINOP_OP (add, matrix, scalar, array, scalar, +)
DEFNDBINOP_OP (sub, matrix, scalar, array, scalar, -)
DEFNDBINOP_OP (mul, matrix, scalar, array, scalar, *)

DEFBINOP (div, matrix, scalar)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v1.array_value () / v2.double_value ());
}

DEFBINOP_FN (pow, matrix, scalar, xpow)

DEFBINOP (ldiv, matrix, scalar)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  Matrix m1 = v1.matrix_value ();
  Matrix m2 = v2.matrix_value ();
  MatrixType typ = v1.matrix_type ();

  Matrix ret = xleftdiv (m1, m2, typ);

  v1.matrix_type (typ);
  return ret;
}

DEFNDBINOP_FN (lt, matrix, scalar, array, scalar, mx_el_lt)
DEFNDBINOP_FN (le, matrix, scalar, array, scalar, mx_el_le)
DEFNDBINOP_FN (eq, matrix, scalar, array, scalar, mx_el_eq)
DEFNDBINOP_FN (ge, matrix, scalar, array, scalar, mx_el_ge)
DEFNDBINOP_FN (gt, matrix, scalar, array, scalar, mx_el_gt)
DEFNDBINOP_FN (ne, matrix, scalar, array, scalar, mx_el_ne)

DEFNDBINOP_OP (el_mul, matrix, scalar, array, scalar, *)

DEFBINOP (el_div, matrix, scalar)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v1.array_value () / v2.double_value ());
}

DEFNDBINOP_FN (el_pow, matrix, scalar, array, scalar, elem_xpow)

DEFBINOP (el_ldiv, matrix, scalar)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return elem_xdiv (v2.double_value (), v1.array_value ());
}

DEFNDBINOP_FN (el_and, matrix, scalar, array, scalar, mx_el_and)
DEFNDBINOP_FN (el_or, matrix, scalar, array, scalar, mx_el_or)

DEFNDCATOP_FN (m_s, matrix, scalar, array, array, concat)

DEFNDASSIGNOP_FN (assign, matrix, scalar, scalar, assign)
DEFNDASSIGNOP_FN (sgl_assign, float_matrix, scalar, float_scalar, assign)
DEFNDASSIGNOP_FN (clx_sgl_assign, float_complex_matrix, scalar, float_complex,
                  assign)

DEFNDASSIGNOP_OP (assign_add, matrix, scalar, scalar, +=)
DEFNDASSIGNOP_OP (assign_sub, matrix, scalar, scalar, -=)
DEFNDASSIGNOP_OP (assign_mul, matrix, scalar, scalar, *=)
DEFNDASSIGNOP_OP (assign_div, matrix, scalar, scalar, /=)

void
install_m_s_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_matrix, octave_scalar, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_matrix, octave_scalar, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_matrix, octave_scalar, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_matrix, octave_scalar, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_matrix, octave_scalar, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_matrix, octave_scalar, ldiv);

  INSTALL_BINOP_TI (ti, op_lt, octave_matrix, octave_scalar, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_matrix, octave_scalar, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_matrix, octave_scalar, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_matrix, octave_scalar, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_matrix, octave_scalar, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_matrix, octave_scalar, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_matrix, octave_scalar, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_matrix, octave_scalar, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_matrix, octave_scalar, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_matrix, octave_scalar, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_matrix, octave_scalar, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_matrix, octave_scalar, el_or);

  INSTALL_CATOP_TI (ti, octave_matrix, octave_scalar, m_s);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_matrix, octave_scalar, assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_matrix, octave_scalar,
                       sgl_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix, octave_scalar,
                       clx_sgl_assign);

  INSTALL_ASSIGNOP_TI (ti, op_add_eq, octave_matrix, octave_scalar, assign_add);
  INSTALL_ASSIGNOP_TI (ti, op_sub_eq, octave_matrix, octave_scalar, assign_sub);
  INSTALL_ASSIGNOP_TI (ti, op_mul_eq, octave_matrix, octave_scalar, assign_mul);
  INSTALL_ASSIGNOP_TI (ti, op_div_eq, octave_matrix, octave_scalar, assign_div);
}

OCTAVE_END_NAMESPACE(octave)
