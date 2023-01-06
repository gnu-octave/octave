////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
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
#include "ov-float.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// bool matrix by bool ops.

DEFNDBINOP_FN (el_and, bool, bool_matrix, bool, bool_array, mx_el_and)
DEFNDBINOP_FN (el_or, bool, bool_matrix, bool, bool_array, mx_el_or)

DEFNDBINOP_FN (el_and_not, bool, bool_matrix, bool, bool_array, mx_el_and_not)
DEFNDBINOP_FN (el_or_not, bool, bool_matrix, bool, bool_array, mx_el_or_not)

DEFNDCATOP_FN (b_bm, bool, bool_matrix, bool_array, bool_array, concat)
DEFNDCATOP_FN (b_m, bool, matrix, array, array, concat)
DEFNDCATOP_FN (s_bm, scalar, bool_matrix, array, array, concat)

DEFNDCATOP_FN (b_fm, bool, float_matrix, float_array, float_array, concat)
DEFNDCATOP_FN (f_bm, float_scalar, bool_matrix, float_array, float_array,
               concat)

DEFCONV (bool_matrix_conv, bool, bool_matrix)
{
  const octave_bool& v = dynamic_cast<const octave_bool&> (a);

  return new octave_bool_matrix (v.bool_matrix_value ());
}

void
install_b_bm_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_el_and, octave_bool, octave_bool_matrix, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_bool, octave_bool_matrix, el_or);
  INSTALL_BINOP_TI (ti, op_el_and_not, octave_bool, octave_bool_matrix, el_and_not);
  INSTALL_BINOP_TI (ti, op_el_or_not, octave_bool, octave_bool_matrix, el_or_not);

  INSTALL_CATOP_TI (ti, octave_bool, octave_bool_matrix, b_bm);
  INSTALL_CATOP_TI (ti, octave_bool, octave_matrix, b_m);
  INSTALL_CATOP_TI (ti, octave_scalar, octave_bool_matrix, s_bm);
  INSTALL_CATOP_TI (ti, octave_bool, octave_float_matrix, b_fm);
  INSTALL_CATOP_TI (ti, octave_float_scalar, octave_bool_matrix, f_bm);

  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_bool_matrix, octave_bool_matrix);

  INSTALL_WIDENOP_TI (ti, octave_bool, octave_bool_matrix, bool_matrix_conv);
}

OCTAVE_END_NAMESPACE(octave)
