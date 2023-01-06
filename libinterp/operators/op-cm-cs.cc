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
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-complex.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// complex matrix by complex scalar ops.

DEFNDBINOP_OP (add, complex_matrix, complex, complex_array, complex, +)
DEFNDBINOP_OP (sub, complex_matrix, complex, complex_array, complex, -)
DEFNDBINOP_OP (mul, complex_matrix, complex, complex_array, complex, *)

DEFBINOP (div, complex_matrix, complex)
{
  const octave_complex_matrix& v1
    = dynamic_cast<const octave_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.complex_array_value () / v2.complex_value ());
}

DEFBINOP_FN (pow, complex_matrix, complex, xpow)

DEFBINOP (ldiv, complex_matrix, complex)
{
  const octave_complex_matrix& v1
    = dynamic_cast<const octave_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  ComplexMatrix m1 = v1.complex_matrix_value ();
  ComplexMatrix m2 = v2.complex_matrix_value ();
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (m1, m2, typ);
  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, complex_matrix, complex, complex_array, complex,
                    mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, complex_matrix, complex, complex_array, complex,
                    mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, complex_matrix, complex, complex_array, complex,
                    mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, complex_matrix, complex, complex_array, complex,
                    mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, complex_matrix, complex, complex_array, complex,
                    mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, complex_matrix, complex, complex_array, complex,
                    mx_el_ne)

DEFNDBINOP_OP (el_mul, complex_matrix, complex, complex_array, complex, *)

DEFBINOP (el_div, complex_matrix, complex)
{
  const octave_complex_matrix& v1
    = dynamic_cast<const octave_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.complex_array_value () / v2.complex_value ());
}

DEFNDBINOP_FN (el_pow, complex_matrix, complex, complex_array, complex,
               elem_xpow)

DEFBINOP (el_ldiv, complex_matrix, complex)
{
  const octave_complex_matrix& v1
    = dynamic_cast<const octave_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return elem_xdiv (v2.complex_value (), v1.complex_array_value ());
}

DEFNDBINOP_FN (el_and, complex_matrix, complex, complex_array, complex,
               mx_el_and)
DEFNDBINOP_FN (el_or,  complex_matrix, complex, complex_array, complex,
               mx_el_or)

DEFNDCATOP_FN (cm_cs, complex_matrix, complex, complex_array, complex_array,
               concat)

DEFNDASSIGNOP_FN (assign, complex_matrix, complex, complex, assign)
DEFNDASSIGNOP_FN (sgl_assign, float_complex_matrix, complex, float_complex,
                  assign)

DEFNDASSIGNOP_OP (assign_add, complex_matrix, complex_scalar, complex, +=)
DEFNDASSIGNOP_OP (assign_sub, complex_matrix, complex_scalar, complex, -=)
DEFNDASSIGNOP_OP (assign_mul, complex_matrix, complex_scalar, complex, *=)
DEFNDASSIGNOP_OP (assign_div, complex_matrix, complex_scalar, complex, /=)

void
install_cm_cs_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_complex_matrix, octave_complex, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_complex_matrix, octave_complex, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_complex_matrix, octave_complex, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_complex_matrix, octave_complex, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_complex_matrix, octave_complex, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_complex_matrix, octave_complex, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_complex_matrix, octave_complex, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_complex_matrix, octave_complex, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_complex_matrix, octave_complex, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_complex_matrix, octave_complex, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_complex_matrix, octave_complex, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_complex_matrix, octave_complex, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_complex_matrix, octave_complex, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_complex_matrix, octave_complex, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_complex_matrix, octave_complex, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_complex_matrix, octave_complex,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_complex_matrix, octave_complex, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_complex_matrix, octave_complex, el_or);

  INSTALL_CATOP_TI (ti, octave_complex_matrix, octave_complex, cm_cs);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_complex_matrix, octave_complex,
                       assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_float_complex_matrix, octave_complex,
                       sgl_assign);

  INSTALL_ASSIGNOP_TI (ti, op_add_eq, octave_complex_matrix,
                       octave_complex_scalar,
                       assign_add);
  INSTALL_ASSIGNOP_TI (ti, op_sub_eq, octave_complex_matrix,
                       octave_complex_scalar,
                       assign_sub);
  INSTALL_ASSIGNOP_TI (ti, op_mul_eq, octave_complex_matrix,
                       octave_complex_scalar,
                       assign_mul);
  INSTALL_ASSIGNOP_TI (ti, op_div_eq, octave_complex_matrix,
                       octave_complex_scalar,
                       assign_div);
}

OCTAVE_END_NAMESPACE(octave)
