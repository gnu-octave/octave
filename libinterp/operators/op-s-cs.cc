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
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// scalar by complex scalar ops.

DEFBINOP_OP (add, scalar, complex, +)
DEFBINOP_OP (sub, scalar, complex, -)
DEFBINOP_OP (mul, scalar, complex, *)

DEFBINOP (div, scalar, complex)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.double_value () / v2.complex_value ());
}

DEFBINOP_FN (pow, scalar, complex, xpow)

DEFBINOP (ldiv, scalar, complex)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v2.complex_value () / v1.double_value ());
}

DEFCMPLXCMPOP_OP (lt, scalar, complex, <)
DEFCMPLXCMPOP_OP (le, scalar, complex, <=)
DEFCMPLXCMPOP_OP (eq, scalar, complex, ==)
DEFCMPLXCMPOP_OP (ge, scalar, complex, >=)
DEFCMPLXCMPOP_OP (gt, scalar, complex, >)
DEFCMPLXCMPOP_OP (ne, scalar, complex, !=)

DEFBINOP_OP (el_mul, scalar, complex, *)

DEFBINOP (el_div, scalar, complex)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.double_value () / v2.complex_value ());
}

DEFBINOP_FN (el_pow, scalar, complex, xpow)

DEFBINOP (el_ldiv, scalar, complex)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v2.complex_value () / v1.double_value ());
}

DEFBINOP (el_and, scalar, complex)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.double_value () && (v2.complex_value () != 0.0));
}

DEFBINOP (el_or, scalar, complex)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.double_value () || (v2.complex_value () != 0.0));
}

DEFNDCATOP_FN (s_cs, scalar, complex, array, complex_array, concat)

void
install_s_cs_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_scalar, octave_complex, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_scalar, octave_complex, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_scalar, octave_complex, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_scalar, octave_complex, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_scalar, octave_complex, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_scalar, octave_complex, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_scalar, octave_complex, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_scalar, octave_complex, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_scalar, octave_complex, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_scalar, octave_complex, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_scalar, octave_complex, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_scalar, octave_complex, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_scalar, octave_complex, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_scalar, octave_complex, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_scalar, octave_complex, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_scalar, octave_complex, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_scalar, octave_complex, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_scalar, octave_complex, el_or);

  INSTALL_CATOP_TI (ti, octave_scalar, octave_complex, s_cs);

  INSTALL_ASSIGNCONV_TI (ti, octave_scalar, octave_complex,
                         octave_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_float_scalar, octave_complex,
                         octave_float_complex_matrix);
}

OCTAVE_END_NAMESPACE(octave)
