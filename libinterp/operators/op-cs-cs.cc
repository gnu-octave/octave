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
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// unary complex scalar ops.

DEFUNOP (not, complex)
{
  const octave_complex& v = dynamic_cast<const octave_complex&> (a);
  Complex x = v.complex_value ();
  if (octave::math::isnan (x))
    octave::err_nan_to_logical_conversion ();

  return octave_value (x == 0.0);
}

DEFUNOP_OP (uplus, complex, /* no-op */)
DEFUNOP_OP (uminus, complex, -)
DEFUNOP_OP (transpose, complex, /* no-op */)

DEFUNOP (hermitian, complex)
{
  const octave_complex& v = dynamic_cast<const octave_complex&> (a);

  return octave_value (conj (v.complex_value ()));
}

DEFNCUNOP_METHOD (incr, complex, increment)
DEFNCUNOP_METHOD (decr, complex, decrement)

// complex scalar by complex scalar ops.

DEFBINOP_OP (add, complex, complex, +)
DEFBINOP_OP (sub, complex, complex, -)
DEFBINOP_OP (mul, complex, complex, *)

DEFBINOP (div, complex, complex)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.complex_value () / v2.complex_value ());
}

DEFBINOP_FN (pow, complex, complex, xpow)

DEFBINOP (ldiv, complex, complex)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v2.complex_value () / v1.complex_value ());
}

DEFCMPLXCMPOP_OP (lt, complex, complex, <)
DEFCMPLXCMPOP_OP (le, complex, complex, <=)
DEFCMPLXCMPOP_OP (eq, complex, complex, ==)
DEFCMPLXCMPOP_OP (ge, complex, complex, >=)
DEFCMPLXCMPOP_OP (gt, complex, complex, >)
DEFCMPLXCMPOP_OP (ne, complex, complex, !=)

DEFBINOP_OP (el_mul, complex, complex, *)

DEFBINOP (el_div, complex, complex)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.complex_value () / v2.complex_value ());
}

DEFBINOP_FN (el_pow, complex, complex, xpow)

DEFBINOP (el_ldiv, complex, complex)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v2.complex_value () / v1.complex_value ());
}

DEFBINOP (el_and, complex, complex)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return v1.complex_value () != 0.0 && v2.complex_value () != 0.0;
}

DEFBINOP (el_or, complex, complex)
{
  const octave_complex& v1 = dynamic_cast<const octave_complex&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return v1.complex_value () != 0.0 || v2.complex_value () != 0.0;
}

DEFNDCATOP_FN (cs_cs, complex, complex, complex_array, complex_array, concat)

void
install_cs_cs_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_not, octave_complex, not);
  INSTALL_UNOP_TI (ti, op_uplus, octave_complex, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_complex, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_complex, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_complex, hermitian);

  INSTALL_NCUNOP_TI (ti, op_incr, octave_complex, incr);
  INSTALL_NCUNOP_TI (ti, op_decr, octave_complex, decr);

  INSTALL_BINOP_TI (ti, op_add, octave_complex, octave_complex, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_complex, octave_complex, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_complex, octave_complex, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_complex, octave_complex, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_complex, octave_complex, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_complex, octave_complex, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_complex, octave_complex, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_complex, octave_complex, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_complex, octave_complex, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_complex, octave_complex, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_complex, octave_complex, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_complex, octave_complex, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_complex, octave_complex, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_complex, octave_complex, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_complex, octave_complex, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_complex, octave_complex, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_complex, octave_complex, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_complex, octave_complex, el_or);

  INSTALL_CATOP_TI (ti, octave_complex, octave_complex, cs_cs);

  INSTALL_ASSIGNCONV_TI (ti, octave_complex, octave_complex,
                         octave_complex_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_complex, octave_null_matrix,
                         octave_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_complex, octave_null_str,
                         octave_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_complex, octave_null_sq_str,
                         octave_complex_matrix);
}

OCTAVE_END_NAMESPACE(octave)
