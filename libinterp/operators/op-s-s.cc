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

#include "Array-util.h"

#include "ovl.h"
#include "ov.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// scalar unary ops.

DEFUNOP (not, scalar)
{
  const octave_scalar& v = dynamic_cast<const octave_scalar&> (a);
  double x = v.scalar_value ();
  if (octave::math::isnan (x))
    octave::err_nan_to_logical_conversion ();

  return octave_value (x == 0.0);
}

DEFUNOP_OP (uplus, scalar, /* no-op */)
DEFUNOP_OP (uminus, scalar, -)
DEFUNOP_OP (transpose, scalar, /* no-op */)
DEFUNOP_OP (hermitian, scalar, /* no-op */)

DEFNCUNOP_METHOD (incr, scalar, increment)
DEFNCUNOP_METHOD (decr, scalar, decrement)

// scalar by scalar ops.

DEFBINOP_OP (add, scalar, scalar, +)
DEFBINOP_OP (sub, scalar, scalar, -)
DEFBINOP_OP (mul, scalar, scalar, *)

DEFBINOP (div, scalar, scalar)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v1.double_value () / v2.double_value ());
}

DEFBINOP_FN (pow, scalar, scalar, xpow)

DEFBINOP (ldiv, scalar, scalar)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v2.double_value () / v1.double_value ());
}

DEFBINOP_OP (lt, scalar, scalar, <)
DEFBINOP_OP (le, scalar, scalar, <=)
DEFBINOP_OP (eq, scalar, scalar, ==)
DEFBINOP_OP (ge, scalar, scalar, >=)
DEFBINOP_OP (gt, scalar, scalar, >)
DEFBINOP_OP (ne, scalar, scalar, !=)

DEFBINOP_OP (el_mul, scalar, scalar, *)

DEFBINOP (el_div, scalar, scalar)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v1.double_value () / v2.double_value ());
}

DEFBINOP_FN (el_pow, scalar, scalar, xpow)

DEFBINOP (el_ldiv, scalar, scalar)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v2.double_value () / v1.double_value ());
}

DEFSCALARBOOLOP_OP (el_and, scalar, scalar, &&)
DEFSCALARBOOLOP_OP (el_or, scalar, scalar, ||)

DEFNDCATOP_FN (s_s, scalar, scalar, array, array, concat)

void
install_s_s_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_not, octave_scalar, not);
  INSTALL_UNOP_TI (ti, op_uplus, octave_scalar, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_scalar, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_scalar, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_scalar, hermitian);

  INSTALL_NCUNOP_TI (ti, op_incr, octave_scalar, incr);
  INSTALL_NCUNOP_TI (ti, op_decr, octave_scalar, decr);

  INSTALL_BINOP_TI (ti, op_add, octave_scalar, octave_scalar, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_scalar, octave_scalar, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_scalar, octave_scalar, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_scalar, octave_scalar, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_scalar, octave_scalar, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_scalar, octave_scalar, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_scalar, octave_scalar, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_scalar, octave_scalar, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_scalar, octave_scalar, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_scalar, octave_scalar, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_scalar, octave_scalar, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_scalar, octave_scalar, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_scalar, octave_scalar, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_scalar, octave_scalar, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_scalar, octave_scalar, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_scalar, octave_scalar, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_scalar, octave_scalar, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_scalar, octave_scalar, el_or);

  INSTALL_CATOP_TI (ti, octave_scalar, octave_scalar, s_s);

  INSTALL_ASSIGNCONV_TI (ti, octave_scalar, octave_scalar, octave_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_float_scalar, octave_scalar,
                         octave_float_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_scalar, octave_null_matrix, octave_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_scalar, octave_null_str, octave_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_scalar, octave_null_sq_str, octave_matrix);
}

OCTAVE_END_NAMESPACE(octave)
