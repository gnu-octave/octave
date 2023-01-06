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
#include "ov-perm.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUNOP (transpose, perm_matrix)
{
  const octave_perm_matrix& v = dynamic_cast<const octave_perm_matrix&> (a);
  return octave_value (v.perm_matrix_value ().transpose ());
}

DEFBINOP_OP (mul, perm_matrix, perm_matrix, *)

DEFBINOP (div, perm_matrix, perm_matrix)
{
  const octave_perm_matrix& v1 = dynamic_cast<const octave_perm_matrix&> (a1);
  const octave_perm_matrix& v2 = dynamic_cast<const octave_perm_matrix&> (a2);

  return (v1.perm_matrix_value () * v2.perm_matrix_value ().inverse ());
}

DEFBINOP (ldiv, perm_matrix, perm_matrix)
{
  const octave_perm_matrix& v1 = dynamic_cast<const octave_perm_matrix&> (a1);
  const octave_perm_matrix& v2 = dynamic_cast<const octave_perm_matrix&> (a2);

  return (v1.perm_matrix_value ().inverse () * v2.perm_matrix_value ());
}

DEFBINOP (pow, perm_matrix, scalar)
{
  const octave_perm_matrix& v1 = dynamic_cast<const octave_perm_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return xpow (v1.perm_matrix_value (), v2.scalar_value ());
}

CONVDECL (perm_matrix_to_matrix)
{
  const octave_perm_matrix& v = dynamic_cast<const octave_perm_matrix&> (a);

  return new octave_matrix (v.matrix_value ());
}

void
install_pm_pm_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_transpose, octave_perm_matrix, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_perm_matrix, transpose);

  INSTALL_BINOP_TI (ti, op_mul, octave_perm_matrix, octave_perm_matrix, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_perm_matrix, octave_perm_matrix, div);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_perm_matrix, octave_perm_matrix, ldiv);
  INSTALL_BINOP_TI (ti, op_pow, octave_perm_matrix, octave_scalar, pow);

  INSTALL_ASSIGNCONV_TI (ti, octave_perm_matrix, octave_matrix, octave_matrix);
  INSTALL_WIDENOP_TI (ti, octave_perm_matrix, octave_matrix,
                      perm_matrix_to_matrix);
}

OCTAVE_END_NAMESPACE(octave)
