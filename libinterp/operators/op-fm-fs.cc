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
#include "ov-flt-re-mat.h"
#include "ov-float.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// matrix by scalar ops.

DEFNDBINOP_OP (add, float_matrix, float_scalar, float_array, float_scalar, +)
DEFNDBINOP_OP (sub, float_matrix, float_scalar, float_array, float_scalar, -)
DEFNDBINOP_OP (mul, float_matrix, float_scalar, float_array, float_scalar, *)

DEFBINOP (div, float_matrix, float)
{
  const octave_float_matrix& v1 = dynamic_cast<const octave_float_matrix&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v1.float_array_value () / v2.float_value ());
}

DEFBINOP_FN (pow, float_matrix, float_scalar, xpow)

DEFBINOP (ldiv, float_matrix, float)
{
  const octave_float_matrix& v1 = dynamic_cast<const octave_float_matrix&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  FloatMatrix m1 = v1.float_matrix_value ();
  FloatMatrix m2 = v2.float_matrix_value ();
  MatrixType typ = v1.matrix_type ();

  FloatMatrix ret = xleftdiv (m1, m2, typ);

  v1.matrix_type (typ);
  return ret;
}

DEFNDBINOP_FN (lt, float_matrix, float_scalar, float_array,
               float_scalar, mx_el_lt)
DEFNDBINOP_FN (le, float_matrix, float_scalar, float_array,
               float_scalar, mx_el_le)
DEFNDBINOP_FN (eq, float_matrix, float_scalar, float_array,
               float_scalar, mx_el_eq)
DEFNDBINOP_FN (ge, float_matrix, float_scalar, float_array,
               float_scalar, mx_el_ge)
DEFNDBINOP_FN (gt, float_matrix, float_scalar, float_array,
               float_scalar, mx_el_gt)
DEFNDBINOP_FN (ne, float_matrix, float_scalar, float_array,
               float_scalar, mx_el_ne)

DEFNDBINOP_OP (el_mul, float_matrix, float_scalar, float_array, float_scalar, *)

DEFBINOP (el_div, float_matrix, float)
{
  const octave_float_matrix& v1 = dynamic_cast<const octave_float_matrix&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v1.float_array_value () / v2.float_value ());
}

DEFNDBINOP_FN (el_pow, float_matrix, float_scalar, float_array,
               float_scalar, elem_xpow)

DEFBINOP (el_ldiv, float_matrix, float)
{
  const octave_float_matrix& v1 = dynamic_cast<const octave_float_matrix&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return elem_xdiv (v2.float_value (), v1.float_array_value ());
}

DEFNDBINOP_FN (el_and, float_matrix, float_scalar, float_array,
               float_scalar, mx_el_and)
DEFNDBINOP_FN (el_or, float_matrix, float_scalar, float_array,
               float_scalar, mx_el_or)

DEFNDCATOP_FN (fm_fs, float_matrix, float_scalar, float_array,
               float_array, concat)

DEFNDCATOP_FN (m_fs, matrix, float_scalar, float_array, float_array, concat)

DEFNDCATOP_FN (fm_s, float_matrix, scalar, float_array, float_array, concat)

DEFNDASSIGNOP_FN (assign, float_matrix, float_scalar, float_scalar, assign)
DEFNDASSIGNOP_FN (dbl_assign, matrix, float_scalar, scalar, assign)

DEFNDASSIGNOP_OP (assign_add, float_matrix, float_scalar, float_scalar, +=)
DEFNDASSIGNOP_OP (assign_sub, float_matrix, float_scalar, float_scalar, -=)
DEFNDASSIGNOP_OP (assign_mul, float_matrix, float_scalar, float_scalar, *=)
DEFNDASSIGNOP_OP (assign_div, float_matrix, float_scalar, float_scalar, /=)

void
install_fm_fs_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_float_matrix, octave_float_scalar, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_float_matrix, octave_float_scalar, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_float_matrix, octave_float_scalar, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_float_matrix, octave_float_scalar, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_float_matrix, octave_float_scalar, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_float_matrix, octave_float_scalar, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_float_matrix, octave_float_scalar, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_float_matrix, octave_float_scalar, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_float_matrix, octave_float_scalar, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_float_matrix, octave_float_scalar, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_float_matrix, octave_float_scalar, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_float_matrix, octave_float_scalar, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_float_matrix, octave_float_scalar,
                    el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_float_matrix, octave_float_scalar,
                    el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_float_matrix, octave_float_scalar,
                    el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_float_matrix, octave_float_scalar,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_float_matrix, octave_float_scalar,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_float_matrix, octave_float_scalar,
                    el_or);

  INSTALL_CATOP_TI (ti, octave_float_matrix, octave_float_scalar, fm_fs);
  INSTALL_CATOP_TI (ti, octave_matrix, octave_float_scalar, m_fs);
  INSTALL_CATOP_TI (ti, octave_float_matrix, octave_scalar, fm_s);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_matrix, octave_float_scalar,
                       assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_matrix, octave_float_scalar,
                       dbl_assign);

  INSTALL_ASSIGNOP_TI (ti, op_add_eq, octave_float_matrix, octave_float_scalar,
                       assign_add);
  INSTALL_ASSIGNOP_TI (ti, op_sub_eq, octave_float_matrix, octave_float_scalar,
                       assign_sub);
  INSTALL_ASSIGNOP_TI (ti, op_mul_eq, octave_float_matrix, octave_float_scalar,
                       assign_mul);
  INSTALL_ASSIGNOP_TI (ti, op_div_eq, octave_float_matrix, octave_float_scalar,
                       assign_div);
}

OCTAVE_END_NAMESPACE(octave)
