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
#include "ov-flt-complex.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// unary complex scalar ops.

DEFUNOP (not, float_complex)
{
  const octave_float_complex& v = dynamic_cast<const octave_float_complex&> (a);
  FloatComplex x = v.float_complex_value ();
  if (octave::math::isnan (x))
    octave::err_nan_to_logical_conversion ();

  return octave_value (x == 0.0f);
}

DEFUNOP_OP (uplus, float_complex, /* no-op */)
DEFUNOP_OP (uminus, float_complex, -)
DEFUNOP_OP (transpose, float_complex, /* no-op */)

DEFUNOP (hermitian, float_complex)
{
  const octave_float_complex& v = dynamic_cast<const octave_float_complex&> (a);

  return octave_value (conj (v.float_complex_value ()));
}

DEFNCUNOP_METHOD (incr, float_complex, increment)
DEFNCUNOP_METHOD (decr, float_complex, decrement)

// complex scalar by complex scalar ops.

DEFBINOP_OP (add, float_complex, float_complex, +)
DEFBINOP_OP (sub, float_complex, float_complex, -)
DEFBINOP_OP (mul, float_complex, float_complex, *)

DEFBINOP (div, float_complex, float_complex)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v1.float_complex_value () / v2.float_complex_value ());
}

DEFBINOP_FN (pow, float_complex, float_complex, xpow)

DEFBINOP (ldiv, float_complex, float_complex)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v2.float_complex_value () / v1.float_complex_value ());
}

DEFCMPLXCMPOP_OP (lt, float_complex, float_complex, <)
DEFCMPLXCMPOP_OP (le, float_complex, float_complex, <=)
DEFCMPLXCMPOP_OP (eq, float_complex, float_complex, ==)
DEFCMPLXCMPOP_OP (ge, float_complex, float_complex, >=)
DEFCMPLXCMPOP_OP (gt, float_complex, float_complex, >)
DEFCMPLXCMPOP_OP (ne, float_complex, float_complex, !=)

DEFBINOP_OP (el_mul, float_complex, float_complex, *)

DEFBINOP (el_div, float_complex, float_complex)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v1.float_complex_value () / v2.float_complex_value ());
}

DEFBINOP_FN (el_pow, float_complex, float_complex, xpow)

DEFBINOP (el_ldiv, float_complex, float_complex)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v2.float_complex_value () / v1.float_complex_value ());
}

DEFBINOP (el_and, float_complex, float_complex)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return (v1.float_complex_value () != 0.0f
          && v2.float_complex_value () != 0.0f);
}

DEFBINOP (el_or, float_complex, float_complex)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return (v1.float_complex_value () != 0.0f
          || v2.float_complex_value () != 0.0f);
}

DEFNDCATOP_FN (fcs_fcs, float_complex, float_complex, float_complex_array,
               float_complex_array, concat)

DEFNDCATOP_FN (cs_fcs, complex, float_complex, float_complex_array,
               float_complex_array, concat)

DEFNDCATOP_FN (fcs_cs, float_complex, complex, float_complex_array,
               float_complex_array, concat)

void
install_fcs_fcs_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_not, octave_float_complex, not);
  INSTALL_UNOP_TI (ti, op_uplus, octave_float_complex, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_float_complex, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_float_complex, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_float_complex, hermitian);

  INSTALL_NCUNOP_TI (ti, op_incr, octave_float_complex, incr);
  INSTALL_NCUNOP_TI (ti, op_decr, octave_float_complex, decr);

  INSTALL_BINOP_TI (ti, op_add, octave_float_complex, octave_float_complex, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_float_complex, octave_float_complex, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_float_complex, octave_float_complex, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_float_complex, octave_float_complex, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_float_complex, octave_float_complex, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_float_complex, octave_float_complex,
                    ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_float_complex, octave_float_complex, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_float_complex, octave_float_complex, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_float_complex, octave_float_complex, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_float_complex, octave_float_complex, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_float_complex, octave_float_complex, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_float_complex, octave_float_complex, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_float_complex, octave_float_complex,
                    el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_float_complex, octave_float_complex,
                    el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_float_complex, octave_float_complex,
                    el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_float_complex, octave_float_complex,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_float_complex, octave_float_complex,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_float_complex, octave_float_complex,
                    el_or);

  INSTALL_CATOP_TI (ti, octave_float_complex, octave_float_complex, fcs_fcs);
  INSTALL_CATOP_TI (ti, octave_complex, octave_float_complex, cs_fcs);
  INSTALL_CATOP_TI (ti, octave_float_complex, octave_complex, fcs_cs);

  INSTALL_ASSIGNCONV_TI (ti, octave_float_complex, octave_float_complex,
                         octave_float_complex_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_complex, octave_float_complex,
                         octave_complex_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_float_complex, octave_null_matrix,
                         octave_float_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_float_complex, octave_null_str,
                         octave_float_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_float_complex, octave_null_sq_str,
                         octave_float_complex_matrix);
}

OCTAVE_END_NAMESPACE(octave)
