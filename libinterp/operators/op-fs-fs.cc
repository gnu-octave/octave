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
#include "ov-flt-re-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// scalar unary ops.

DEFUNOP (not, float_scalar)
{
  const octave_float_scalar& v = dynamic_cast<const octave_float_scalar&> (a);
  float x = v.float_value ();
  if (octave::math::isnan (x))
    octave::err_nan_to_logical_conversion ();

  return octave_value (x == 0.0f);
}

DEFUNOP_OP (uplus, float_scalar, /* no-op */)
DEFUNOP_OP (uminus, float_scalar, -)
DEFUNOP_OP (transpose, float_scalar, /* no-op */)
DEFUNOP_OP (hermitian, float_scalar, /* no-op */)

DEFNCUNOP_METHOD (incr, float_scalar, increment)
DEFNCUNOP_METHOD (decr, float_scalar, decrement)

// float by float ops.

DEFBINOP_OP (add, float_scalar, float_scalar, +)
DEFBINOP_OP (sub, float_scalar, float_scalar, -)
DEFBINOP_OP (mul, float_scalar, float_scalar, *)

DEFBINOP (div, float_scalar, float_scalar)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v1.float_value () / v2.float_value ());
}

DEFBINOP_FN (pow, float_scalar, float_scalar, xpow)

DEFBINOP (ldiv, float_scalar, float_scalar)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v2.float_value () / v1.float_value ());
}

DEFBINOP_OP (lt, float_scalar, float_scalar, <)
DEFBINOP_OP (le, float_scalar, float_scalar, <=)
DEFBINOP_OP (eq, float_scalar, float_scalar, ==)
DEFBINOP_OP (ge, float_scalar, float_scalar, >=)
DEFBINOP_OP (gt, float_scalar, float_scalar, >)
DEFBINOP_OP (ne, float_scalar, float_scalar, !=)

DEFBINOP_OP (el_mul, float_scalar, float_scalar, *)

DEFBINOP (el_div, float_scalar, float_scalar)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v1.float_value () / v2.float_value ());
}

DEFBINOP_FN (el_pow, float_scalar, float_scalar, xpow)

DEFBINOP (el_ldiv, float_scalar, float_scalar)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v2.float_value () / v1.float_value ());
}

DEFSCALARBOOLOP_OP (el_and, float_scalar, float_scalar, &&)
DEFSCALARBOOLOP_OP (el_or, float_scalar, float_scalar, ||)

DEFNDCATOP_FN (fs_fs, float_scalar, float_scalar, float_array, float_array,
               concat)
DEFNDCATOP_FN (s_fs, scalar, float_scalar, float_array, float_array, concat)
DEFNDCATOP_FN (fs_s, float_scalar, scalar, float_array, float_array, concat)

void
install_fs_fs_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_not, octave_float_scalar, not);
  INSTALL_UNOP_TI (ti, op_uplus, octave_float_scalar, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_float_scalar, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_float_scalar, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_float_scalar, hermitian);

  INSTALL_NCUNOP_TI (ti, op_incr, octave_float_scalar, incr);
  INSTALL_NCUNOP_TI (ti, op_decr, octave_float_scalar, decr);

  INSTALL_BINOP_TI (ti, op_add, octave_float_scalar, octave_float_scalar, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_float_scalar, octave_float_scalar, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_float_scalar, octave_float_scalar, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_float_scalar, octave_float_scalar, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_float_scalar, octave_float_scalar, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_float_scalar, octave_float_scalar, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_float_scalar, octave_float_scalar, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_float_scalar, octave_float_scalar, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_float_scalar, octave_float_scalar, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_float_scalar, octave_float_scalar, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_float_scalar, octave_float_scalar, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_float_scalar, octave_float_scalar, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_float_scalar, octave_float_scalar,
                    el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_float_scalar, octave_float_scalar,
                    el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_float_scalar, octave_float_scalar,
                    el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_float_scalar, octave_float_scalar,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_float_scalar, octave_float_scalar,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_float_scalar, octave_float_scalar,
                    el_or);

  INSTALL_CATOP_TI (ti, octave_float_scalar, octave_float_scalar, fs_fs);
  INSTALL_CATOP_TI (ti, octave_scalar, octave_float_scalar, s_fs);
  INSTALL_CATOP_TI (ti, octave_float_scalar, octave_scalar, fs_s);

  INSTALL_ASSIGNCONV_TI (ti, octave_float_scalar, octave_float_scalar,
                         octave_float_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_scalar, octave_float_scalar, octave_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_float_scalar, octave_null_matrix,
                         octave_float_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_float_scalar, octave_null_str,
                         octave_float_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_float_scalar, octave_null_sq_str,
                         octave_float_matrix);
}

OCTAVE_END_NAMESPACE(octave)
