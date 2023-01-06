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
#include "ov-re-mat.h"
#include "ov-re-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// matrix unary ops.

DEFUNOP_OP (uplus, diag_matrix, /* no-op */)
DEFUNOP_OP (uminus, diag_matrix, -)

DEFUNOP (transpose, diag_matrix)
{
  const octave_diag_matrix& v = dynamic_cast<const octave_diag_matrix&> (a);
  return octave_value (v.diag_matrix_value ().transpose ());
}

// matrix by matrix ops.

DEFBINOP_OP (add, diag_matrix, diag_matrix, +)
DEFBINOP_OP (sub, diag_matrix, diag_matrix, -)
DEFBINOP_OP (mul, diag_matrix, diag_matrix, *)

DEFBINOP (div, diag_matrix, diag_matrix)
{
  const octave_diag_matrix& v1 = dynamic_cast<const octave_diag_matrix&> (a1);
  const octave_diag_matrix& v2 = dynamic_cast<const octave_diag_matrix&> (a2);

  return xdiv (v1.diag_matrix_value (),
               v2.diag_matrix_value ());
}

DEFBINOP (ldiv, diag_matrix, diag_matrix)
{
  const octave_diag_matrix& v1 = dynamic_cast<const octave_diag_matrix&> (a1);
  const octave_diag_matrix& v2 = dynamic_cast<const octave_diag_matrix&> (a2);

  return xleftdiv (v1.diag_matrix_value (),
                   v2.diag_matrix_value ());
}

CONVDECL (diag_matrix_to_matrix)
{
  const octave_diag_matrix& v = dynamic_cast<const octave_diag_matrix&> (a);

  return new octave_matrix (v.matrix_value ());
}

void
install_dm_dm_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_uplus, octave_diag_matrix, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_diag_matrix, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_diag_matrix, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_diag_matrix, transpose);

  INSTALL_BINOP_TI (ti, op_add, octave_diag_matrix, octave_diag_matrix, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_diag_matrix, octave_diag_matrix, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_diag_matrix, octave_diag_matrix, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_diag_matrix, octave_diag_matrix, div);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_diag_matrix, octave_diag_matrix, ldiv);

  INSTALL_ASSIGNCONV_TI (ti, octave_diag_matrix, octave_matrix, octave_matrix);
  INSTALL_WIDENOP_TI (ti, octave_diag_matrix, octave_matrix,
                      diag_matrix_to_matrix);
}

OCTAVE_END_NAMESPACE(octave)
