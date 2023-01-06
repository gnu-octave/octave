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
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// complex scalar by scalar ops.

DEFBINOP_OP (add, complex, scalar, +)
DEFBINOP_OP (sub, complex, scalar, -)
DEFBINOP_OP (mul, complex, scalar, *)

DEFBINOP (div, complex, scalar)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v1.complex_value () / v2.double_value ());
}

DEFBINOP_FN (pow, complex, scalar, xpow)

DEFBINOP (ldiv, complex, scalar)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v2.double_value () / v1.complex_value ());
}

DEFCMPLXCMPOP_OP (lt, complex, scalar, <)
DEFCMPLXCMPOP_OP (le, complex, scalar, <=)
DEFCMPLXCMPOP_OP (eq, complex, scalar, ==)
DEFCMPLXCMPOP_OP (ge, complex, scalar, >=)
DEFCMPLXCMPOP_OP (gt, complex, scalar, >)
DEFCMPLXCMPOP_OP (ne, complex, scalar, !=)

DEFBINOP_OP (el_mul, complex, scalar, *)

DEFBINOP (el_div, complex, scalar)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v1.complex_value () / v2.double_value ());
}

DEFBINOP_FN (el_pow, complex, scalar, xpow)

DEFBINOP (el_ldiv, complex, scalar)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v2.double_value () / v1.complex_value ());
}

DEFBINOP (el_and, complex, scalar)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return v1.complex_value () != 0.0 && v2.double_value ();
}

DEFBINOP (el_or, complex, scalar)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return v1.complex_value () != 0.0 || v2.double_value ();
}

DEFNDCATOP_FN (cs_s, complex, scalar, complex_array, array, concat)

void
install_cs_s_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_complex, octave_scalar, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_complex, octave_scalar, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_complex, octave_scalar, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_complex, octave_scalar, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_complex, octave_scalar, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_complex, octave_scalar, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_complex, octave_scalar, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_complex, octave_scalar, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_complex, octave_scalar, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_complex, octave_scalar, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_complex, octave_scalar, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_complex, octave_scalar, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_complex, octave_scalar, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_complex, octave_scalar, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_complex, octave_scalar, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_complex, octave_scalar, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_complex, octave_scalar, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_complex, octave_scalar, el_or);

  INSTALL_CATOP_TI (ti, octave_complex, octave_scalar, cs_s);

  INSTALL_ASSIGNCONV_TI (ti, octave_complex, octave_scalar,
                         octave_complex_matrix);
}

OCTAVE_END_NAMESPACE(octave)
