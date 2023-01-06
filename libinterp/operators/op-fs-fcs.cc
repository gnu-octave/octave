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
#include "ov-flt-complex.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// scalar by complex scalar ops.

DEFBINOP_OP (add, float_scalar, float_complex, +)
DEFBINOP_OP (sub, float_scalar, float_complex, -)
DEFBINOP_OP (mul, float_scalar, float_complex, *)

DEFBINOP (div, float_scalar, float_complex)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v1.float_value () / v2.float_complex_value ());
}

DEFBINOP_FN (pow, float_scalar, float_complex, xpow)

DEFBINOP (ldiv, float_scalar, float_complex)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v2.float_complex_value () / v1.float_value ());
}

DEFCMPLXCMPOP_OP (lt, float_scalar, float_complex, <)
DEFCMPLXCMPOP_OP (le, float_scalar, float_complex, <=)
DEFCMPLXCMPOP_OP (eq, float_scalar, float_complex, ==)
DEFCMPLXCMPOP_OP (ge, float_scalar, float_complex, >=)
DEFCMPLXCMPOP_OP (gt, float_scalar, float_complex, >)
DEFCMPLXCMPOP_OP (ne, float_scalar, float_complex, !=)

DEFBINOP_OP (el_mul, float_scalar, float_complex, *)

DEFBINOP (el_div, float_scalar, float_complex)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v1.float_value () / v2.float_complex_value ());
}

DEFBINOP_FN (el_pow, float_scalar, float_complex, xpow)

DEFBINOP (el_ldiv, float_scalar, float_complex)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v2.float_complex_value () / v1.float_value ());
}

DEFBINOP (el_and, float_scalar, float_complex)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v1.float_scalar_value ()
                       && (v2.float_complex_value () != 0.0f));
}

DEFBINOP (el_or, float_scalar, float_complex)
{
  const octave_float_scalar& v1 = dynamic_cast<const octave_float_scalar&> (a1);
  const octave_float_complex& v2 = dynamic_cast<const octave_float_complex&> (a2);

  return octave_value (v1.float_scalar_value ()
                       || (v2.float_complex_value () != 0.0f));
}

DEFNDCATOP_FN (fs_fcs, float_scalar, float_complex, float_array,
               float_complex_array, concat)

DEFNDCATOP_FN (s_fcs, scalar, float_complex, float_array,
               float_complex_array, concat)

DEFNDCATOP_FN (fs_cs, float_scalar, complex, float_array,
               float_complex_array, concat)

void
install_fs_fcs_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_float_scalar, octave_float_complex, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_float_scalar, octave_float_complex, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_float_scalar, octave_float_complex, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_float_scalar, octave_float_complex, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_float_scalar, octave_float_complex, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_float_scalar, octave_float_complex, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_float_scalar, octave_float_complex, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_float_scalar, octave_float_complex, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_float_scalar, octave_float_complex, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_float_scalar, octave_float_complex, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_float_scalar, octave_float_complex, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_float_scalar, octave_float_complex, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_float_scalar, octave_float_complex,
                    el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_float_scalar, octave_float_complex,
                    el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_float_scalar, octave_float_complex,
                    el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_float_scalar, octave_float_complex,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_float_scalar, octave_float_complex,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_float_scalar, octave_float_complex,
                    el_or);

  INSTALL_CATOP_TI (ti, octave_float_scalar, octave_float_complex, fs_fcs);
  INSTALL_CATOP_TI (ti, octave_scalar, octave_float_complex, s_fcs);
  INSTALL_CATOP_TI (ti, octave_float_scalar, octave_complex, fs_cs);

  INSTALL_ASSIGNCONV_TI (ti, octave_float_scalar, octave_float_complex,
                         octave_float_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_scalar, octave_float_complex,
                         octave_complex_matrix);
}

OCTAVE_END_NAMESPACE(octave)
