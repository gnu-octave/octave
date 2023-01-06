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
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-float.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// complex scalar by scalar ops.

DEFBINOP_OP (add, float_complex, float_scalar, +)
DEFBINOP_OP (sub, float_complex, float_scalar, -)
DEFBINOP_OP (mul, float_complex, float_scalar, *)

DEFBINOP (div, float_complex, float)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v1.float_complex_value () / v2.float_value ());
}

DEFBINOP_FN (pow, float_complex, float_scalar, xpow)

DEFBINOP (ldiv, float_complex, float)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v2.float_value () / v1.float_complex_value ());
}

DEFCMPLXCMPOP_OP (lt, float_complex, float_scalar, <)
DEFCMPLXCMPOP_OP (le, float_complex, float_scalar, <=)
DEFCMPLXCMPOP_OP (eq, float_complex, float_scalar, ==)
DEFCMPLXCMPOP_OP (ge, float_complex, float_scalar, >=)
DEFCMPLXCMPOP_OP (gt, float_complex, float_scalar, >)
DEFCMPLXCMPOP_OP (ne, float_complex, float_scalar, !=)

DEFBINOP_OP (el_mul, float_complex, float_scalar, *)

DEFBINOP (el_div, float_complex, float)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v1.float_complex_value () / v2.float_value ());
}

DEFBINOP_FN (el_pow, float_complex, float_scalar, xpow)

DEFBINOP (el_ldiv, float_complex, float)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return octave_value (v2.float_value () / v1.float_complex_value ());
}

DEFBINOP (el_and, float_complex, float)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return (v1.float_complex_value () != 0.0f && v2.float_value ());
}

DEFBINOP (el_or, float_complex, float)
{
  const octave_float_complex& v1 = dynamic_cast<const octave_float_complex&> (a1);
  const octave_float_scalar& v2 = dynamic_cast<const octave_float_scalar&> (a2);

  return (v1.float_complex_value () != 0.0f || v2.float_value ());
}

DEFNDCATOP_FN (fcs_fs, float_complex, float_scalar, float_complex_array,
               float_array, concat)

DEFNDCATOP_FN (cs_fs, complex, float_scalar, float_complex_array,
               float_array, concat)

DEFNDCATOP_FN (fcs_s, float_complex, scalar, float_complex_array,
               float_array, concat)

void
install_fcs_fs_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_float_complex, octave_float_scalar, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_float_complex, octave_float_scalar, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_float_complex, octave_float_scalar, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_float_complex, octave_float_scalar, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_float_complex, octave_float_scalar, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_float_complex, octave_float_scalar, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_float_complex, octave_float_scalar, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_float_complex, octave_float_scalar, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_float_complex, octave_float_scalar, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_float_complex, octave_float_scalar, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_float_complex, octave_float_scalar, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_float_complex, octave_float_scalar, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_float_complex, octave_float_scalar,
                    el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_float_complex, octave_float_scalar,
                    el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_float_complex, octave_float_scalar,
                    el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_float_complex, octave_float_scalar,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_float_complex, octave_float_scalar,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_float_complex, octave_float_scalar,
                    el_or);

  INSTALL_CATOP_TI (ti, octave_float_complex, octave_float_scalar, fcs_fs);
  INSTALL_CATOP_TI (ti, octave_complex, octave_float_scalar, cs_fs);
  INSTALL_CATOP_TI (ti, octave_float_complex, octave_scalar, fcs_s);

  INSTALL_ASSIGNCONV_TI (ti, octave_float_complex, octave_float_scalar,
                         octave_float_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_complex, octave_float_scalar,
                         octave_complex_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_float_complex, octave_scalar,
                         octave_float_complex_matrix);
}

OCTAVE_END_NAMESPACE(octave)
