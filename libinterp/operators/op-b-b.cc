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

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-float.h"
#include "ov-flt-re-mat.h"
#include "ov-flt-complex.h"
#include "ov-flt-cx-mat.h"
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-null-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// bool unary ops.

// scalar unary ops.

DEFUNOP_OP (not, bool, !)

static octave_value
oct_unop_uplus (const octave_base_value& a)
{
  const octave_bool& v = dynamic_cast<const octave_bool&> (a);
  return octave_value (v.double_value ());
}

static octave_value
oct_unop_uminus (const octave_base_value& a)
{
  const octave_bool& v = dynamic_cast<const octave_bool&> (a);
  return octave_value (- v.double_value ());
}

DEFUNOP_OP (transpose, bool, /* no-op */)
DEFUNOP_OP (hermitian, bool, /* no-op */)

// bool by bool ops.

DEFBINOP_OP (eq, bool, bool, ==)
DEFBINOP_OP (ne, bool, bool, !=)
DEFBINOP_OP (el_and, bool, bool, &&)
DEFBINOP_OP (el_or, bool, bool, ||)

DEFNDCATOP_FN (b_b, bool, bool, bool_array, bool_array, concat)
DEFNDCATOP_FN (b_s, bool, scalar, array, array, concat)
DEFNDCATOP_FN (s_b, scalar, bool, array, array, concat)
DEFNDCATOP_FN (b_f, bool, float_scalar, float_array, float_array, concat)
DEFNDCATOP_FN (f_b, float_scalar, bool, float_array, float_array, concat)

#define OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(TRHS)                    \
  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_ ## TRHS ## _scalar, octave_bool_matrix) \
  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_ ## TRHS ## _matrix, octave_bool_matrix)

void
install_b_b_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_not, octave_bool, not);
  INSTALL_UNOP_TI (ti, op_uplus, octave_bool, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_bool, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_bool, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_bool, hermitian);

  INSTALL_BINOP_TI (ti, op_eq, octave_bool, octave_bool, eq);
  INSTALL_BINOP_TI (ti, op_ne, octave_bool, octave_bool, ne);
  INSTALL_BINOP_TI (ti, op_el_and, octave_bool, octave_bool, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_bool, octave_bool, el_or);

  INSTALL_CATOP_TI (ti, octave_bool, octave_bool, b_b);
  INSTALL_CATOP_TI (ti, octave_bool, octave_scalar, b_s);
  INSTALL_CATOP_TI (ti, octave_scalar, octave_bool, s_b);
  INSTALL_CATOP_TI (ti, octave_bool, octave_float_scalar, b_f);
  INSTALL_CATOP_TI (ti, octave_float_scalar, octave_bool, f_b);

  OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(int8);
  OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(int16);
  OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(int32);
  OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(int64);

  OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(uint8);
  OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(uint16);
  OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(uint32);
  OCTAVE_INSTALL_BOOL_INT_ASSIGNCONV(uint64);

  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_scalar, octave_bool_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_matrix, octave_bool_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_complex, octave_bool_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_complex_matrix, octave_bool_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_float_scalar, octave_bool_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_float_matrix, octave_bool_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_float_complex_scalar, octave_bool_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_float_complex_matrix, octave_bool_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_bool, octave_bool_matrix);

  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_null_matrix, octave_bool_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_null_str, octave_bool_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_null_sq_str, octave_bool_matrix);
}

OCTAVE_END_NAMESPACE(octave)
