////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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
#include "ov-flt-re-mat.h"
#include "ov-flt-re-diag.h"
#include "ov-re-diag.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// matrix unary ops.

DEFUNOP_OP (uplus, float_diag_matrix, /* no-op */)
DEFUNOP_OP (uminus, float_diag_matrix, -)

DEFUNOP (transpose, float_diag_matrix)
{
  const octave_float_diag_matrix& v
    = dynamic_cast<const octave_float_diag_matrix&> (a);
  return octave_value (v.float_diag_matrix_value ().transpose ());
}

// matrix by matrix ops.

DEFBINOP_OP (add, float_diag_matrix, float_diag_matrix, +)
DEFBINOP_OP (sub, float_diag_matrix, float_diag_matrix, -)
DEFBINOP_OP (mul, float_diag_matrix, float_diag_matrix, *)

DEFBINOP (div, float_diag_matrix, float_diag_matrix)
{
  const octave_float_diag_matrix& v1
    = dynamic_cast<const octave_float_diag_matrix&> (a1);
  const octave_float_diag_matrix& v2
    = dynamic_cast<const octave_float_diag_matrix&> (a2);

  return xdiv (v1.float_diag_matrix_value (),
               v2.float_diag_matrix_value ());
}

DEFBINOP (ldiv, float_diag_matrix, float_diag_matrix)
{
  const octave_float_diag_matrix& v1
    = dynamic_cast<const octave_float_diag_matrix&> (a1);
  const octave_float_diag_matrix& v2
    = dynamic_cast<const octave_float_diag_matrix&> (a2);

  return xleftdiv (v1.float_diag_matrix_value (),
                   v2.float_diag_matrix_value ());
}

CONVDECL (float_diag_matrix_to_float_matrix)
{
  const octave_float_diag_matrix& v
    = dynamic_cast<const octave_float_diag_matrix&> (a);

  return new octave_float_matrix (v.float_matrix_value ());
}

void
install_fdm_fdm_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_uplus, octave_float_diag_matrix, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_float_diag_matrix, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_float_diag_matrix, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_float_diag_matrix, transpose);

  INSTALL_BINOP_TI (ti, op_add, octave_float_diag_matrix,
                    octave_float_diag_matrix,
                    add);
  INSTALL_BINOP_TI (ti, op_sub, octave_float_diag_matrix,
                    octave_float_diag_matrix,
                    sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_float_diag_matrix,
                    octave_float_diag_matrix,
                    mul);
  INSTALL_BINOP_TI (ti, op_div, octave_float_diag_matrix,
                    octave_float_diag_matrix,
                    div);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_float_diag_matrix,
                    octave_float_diag_matrix,
                    ldiv);

  INSTALL_ASSIGNCONV_TI (ti, octave_float_diag_matrix, octave_float_matrix,
                         octave_float_matrix);
  INSTALL_WIDENOP_TI (ti, octave_float_diag_matrix, octave_float_matrix,
                      float_diag_matrix_to_float_matrix);
}

OCTAVE_END_NAMESPACE(octave)
