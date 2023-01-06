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

#include "mx-m-cs.h"
#include "mx-cs-m.h"
#include "mx-nda-cs.h"
#include "mx-cs-nda.h"

#include "ovl.h"
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-complex.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// matrix by complex scalar ops.

DEFNDBINOP_OP (add, matrix, complex, array, complex, +)
DEFNDBINOP_OP (sub, matrix, complex, array, complex, -)
DEFNDBINOP_OP (mul, matrix, complex, array, complex, *)

DEFBINOP (div, matrix, complex)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.array_value () / v2.complex_value ());
}

DEFBINOP_FN (pow, matrix, complex, xpow)

DEFBINOP (ldiv, matrix, complex)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  Matrix m1 = v1.matrix_value ();
  ComplexMatrix m2 = v2.complex_matrix_value ();
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (m1, m2, typ);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, matrix, complex, array, complex, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, matrix, complex, array, complex, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, matrix, complex, array, complex, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, matrix, complex, array, complex, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, matrix, complex, array, complex, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, matrix, complex, array, complex, mx_el_ne)

DEFNDBINOP_OP (el_mul, matrix, complex, array, complex, *)

DEFBINOP (el_div, matrix, complex)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.array_value () / v2.complex_value ());
}

DEFNDBINOP_FN (el_pow, matrix, complex, array, complex, elem_xpow)

DEFBINOP (el_ldiv, matrix, complex)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return elem_xdiv (v2.complex_value (), v1.array_value ());
}

DEFNDBINOP_FN (el_and, matrix, complex, array, complex, mx_el_and)
DEFNDBINOP_FN (el_or, matrix, complex, array, complex, mx_el_or)

DEFNDCATOP_FN (m_cs, matrix, complex, array, complex_array, concat)

void
install_m_cs_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_matrix, octave_complex, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_matrix, octave_complex, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_matrix, octave_complex, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_matrix, octave_complex, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_matrix, octave_complex, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_matrix, octave_complex, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_matrix, octave_complex, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_matrix, octave_complex, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_matrix, octave_complex, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_matrix, octave_complex, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_matrix, octave_complex, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_matrix, octave_complex, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_matrix, octave_complex, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_matrix, octave_complex, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_matrix, octave_complex, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_matrix, octave_complex, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_matrix, octave_complex, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_matrix, octave_complex, el_or);

  INSTALL_CATOP_TI (ti, octave_matrix, octave_complex, m_cs);

  INSTALL_ASSIGNCONV_TI (ti, octave_matrix, octave_complex,
                         octave_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_float_matrix, octave_complex,
                         octave_float_complex_matrix);
}

OCTAVE_END_NAMESPACE(octave)
