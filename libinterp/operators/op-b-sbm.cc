////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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
#include "ov-typeinfo.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-scalar.h"
#include "ops.h"

#include "ov-re-sparse.h"
#include "ov-bool-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// bool by sparse bool matrix ops.

DEFBINOP_FN (ne, bool, sparse_bool_matrix, mx_el_ne)
DEFBINOP_FN (eq, bool, sparse_bool_matrix, mx_el_eq)

DEFBINOP_FN (el_and, bool, sparse_bool_matrix, mx_el_and)
DEFBINOP_FN (el_or, bool, sparse_bool_matrix, mx_el_or)

DEFCATOP (b_sbm, bool, sparse_bool_matrix)
{
  const octave_bool& v1 = dynamic_cast<const octave_bool&> (a1);
  const octave_sparse_bool_matrix& v2
    = dynamic_cast<const octave_sparse_bool_matrix&> (a2);
  SparseBoolMatrix tmp (1, 1, v1.bool_value ());
  return octave_value (tmp. concat (v2.sparse_bool_matrix_value (),
                                    ra_idx));
}

DEFCATOP (b_sm, bool, sparse_matrix)
{
  const octave_bool& v1 = dynamic_cast<const octave_bool&> (a1);
  const octave_sparse_matrix& v2 = dynamic_cast<const octave_sparse_matrix&> (a2);
  SparseMatrix tmp (1, 1, v1.scalar_value ());
  return octave_value (tmp. concat (v2.sparse_matrix_value (), ra_idx));
}

DEFCATOP (s_sbm, scalar, sparse_bool_matrix)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_sparse_bool_matrix& v2
    = dynamic_cast<const octave_sparse_bool_matrix&> (a2);
  SparseMatrix tmp (1, 1, v1.scalar_value ());
  return octave_value(tmp. concat (v2.sparse_matrix_value (), ra_idx));
}

DEFCONV (sparse_bool_matrix_conv, bool, sparse_bool_matrix)
{
  const octave_bool& v = dynamic_cast<const octave_bool&> (a);

  return new octave_sparse_bool_matrix
         (SparseBoolMatrix (1, 1, v.bool_value ()));
}

void
install_b_sbm_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_eq, octave_bool, octave_sparse_bool_matrix, eq);
  INSTALL_BINOP_TI (ti, op_ne, octave_bool, octave_sparse_bool_matrix, ne);

  INSTALL_BINOP_TI (ti, op_el_and, octave_bool, octave_sparse_bool_matrix, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_bool, octave_sparse_bool_matrix, el_or);

  INSTALL_CATOP_TI (ti, octave_bool, octave_sparse_bool_matrix, b_sbm);
  INSTALL_CATOP_TI (ti, octave_bool, octave_sparse_matrix, b_sm);
  INSTALL_CATOP_TI (ti, octave_scalar, octave_sparse_bool_matrix, s_sbm);

  INSTALL_ASSIGNCONV_TI (ti, octave_bool, octave_sparse_bool_matrix,
                         octave_bool_matrix);

  INSTALL_WIDENOP_TI (ti, octave_bool, octave_sparse_bool_matrix,
                      sparse_bool_matrix_conv);
}

OCTAVE_END_NAMESPACE(octave)
