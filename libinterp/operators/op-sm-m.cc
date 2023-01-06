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

#include "ovl.h"
#include "ov.h"
#include "ov-typeinfo.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ops.h"
#include "xdiv.h"

#include "sparse-xpow.h"
#include "sparse-xdiv.h"
#include "smx-sm-m.h"
#include "smx-m-sm.h"
#include "ov-re-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// sparse matrix by matrix ops.

DEFBINOP_OP (add, sparse_matrix, matrix, +)
DEFBINOP_OP (sub, sparse_matrix, matrix, -)

DEFBINOP_OP (mul, sparse_matrix, matrix, *)

DEFBINOP (div, sparse_matrix, matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);
  MatrixType typ = v2.matrix_type ();

  Matrix ret = xdiv (v1.matrix_value (), v2.matrix_value (), typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOPX (pow, sparse_matrix, matrix)
{
  error ("can't do A ^ B for A and B both matrices");
}

DEFBINOP (ldiv, sparse_matrix, matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);

  if (v1.rows () == 1 && v1.columns () == 1)
    return octave_value (v2.array_value () / v1.scalar_value ());
  else
    {
      MatrixType typ = v1.matrix_type ();

      Matrix ret = xleftdiv (v1.sparse_matrix_value (),
                             v2.matrix_value (), typ);

      v1.matrix_type (typ);
      return ret;
    }
}

DEFBINOP_FN (trans_mul, sparse_matrix, matrix, trans_mul);

DEFBINOP_FN (lt, sparse_matrix, matrix, mx_el_lt)
DEFBINOP_FN (le, sparse_matrix, matrix, mx_el_le)
DEFBINOP_FN (eq, sparse_matrix, matrix, mx_el_eq)
DEFBINOP_FN (ge, sparse_matrix, matrix, mx_el_ge)
DEFBINOP_FN (gt, sparse_matrix, matrix, mx_el_gt)
DEFBINOP_FN (ne, sparse_matrix, matrix, mx_el_ne)

DEFBINOP_FN (el_mul, sparse_matrix, matrix, product)
DEFBINOP_FN (el_div, sparse_matrix, matrix, quotient)

DEFBINOP (el_pow, sparse_matrix, matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);

  return octave_value (elem_xpow (v1.sparse_matrix_value (),
                                  SparseMatrix (v2.matrix_value ())));
}

DEFBINOP (el_ldiv, sparse_matrix, matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);

  return octave_value
         (quotient (v2.matrix_value (), v1.sparse_matrix_value ()));
}

DEFBINOP_FN (el_and, sparse_matrix, matrix, mx_el_and)
DEFBINOP_FN (el_or,  sparse_matrix, matrix, mx_el_or)

DEFCATOP (sm_m, sparse_matrix, matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);
  SparseMatrix tmp (v2.matrix_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFNDASSIGNOP_FN (assign, sparse_matrix, matrix, matrix, assign);
DEFNDASSIGNOP_FN (sgl_assign, sparse_matrix, float_matrix, matrix, assign);

void
install_sm_m_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_sparse_matrix, octave_matrix, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_sparse_matrix, octave_matrix, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_sparse_matrix, octave_matrix, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_sparse_matrix, octave_matrix, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_sparse_matrix, octave_matrix, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_sparse_matrix, octave_matrix, ldiv);
  INSTALL_BINOP_TI (ti, op_trans_mul, octave_sparse_matrix, octave_matrix,
                    trans_mul);
  INSTALL_BINOP_TI (ti, op_herm_mul, octave_sparse_matrix, octave_matrix,
                    trans_mul);
  INSTALL_BINOP_TI (ti, op_lt, octave_sparse_matrix, octave_matrix, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_sparse_matrix, octave_matrix, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_sparse_matrix, octave_matrix, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_sparse_matrix, octave_matrix, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_sparse_matrix, octave_matrix, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_sparse_matrix, octave_matrix, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_sparse_matrix, octave_matrix, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_sparse_matrix, octave_matrix, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_sparse_matrix, octave_matrix, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_sparse_matrix, octave_matrix, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_sparse_matrix, octave_matrix, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_sparse_matrix, octave_matrix,  el_or);

  INSTALL_CATOP_TI (ti, octave_sparse_matrix, octave_matrix, sm_m);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_matrix, octave_matrix,
                       assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_matrix, octave_float_matrix,
                       sgl_assign);
}

OCTAVE_END_NAMESPACE(octave)
