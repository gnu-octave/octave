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
#include "ov-scalar.h"
#include "ov-float.h"
#include "ops.h"
#include "xpow.h"

#include "sparse-xpow.h"
#include "sparse-xdiv.h"
#include "ov-re-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// sparse matrix by scalar ops.

DEFBINOP_OP (add, sparse_matrix, scalar, +)
DEFBINOP_OP (sub, sparse_matrix, scalar, -)
DEFBINOP_OP (mul, sparse_matrix, scalar, *)

DEFBINOP (div, sparse_matrix, scalar)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v1.sparse_matrix_value () / v2.double_value ());
}

DEFBINOP (pow, sparse_matrix, scalar)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  double tmp = v2.scalar_value ();
  if (static_cast<int> (tmp) == tmp)
    return xpow (v1.sparse_matrix_value (), tmp);
  else
    return xpow (v1.matrix_value (), tmp);
}

DEFBINOP (ldiv, sparse_matrix, scalar)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  if (v1.rows () == 1 && v1.columns () == 1)
    return octave_value (SparseMatrix(1, 1, v2.scalar_value () / v1.scalar_value ()));
  else
    {
      MatrixType typ = v1.matrix_type ();
      SparseMatrix m1 = v1.sparse_matrix_value ();
      Matrix m2 = Matrix (1, 1, v2.scalar_value ());
      Matrix ret = xleftdiv (m1, m2, typ);
      v1.matrix_type (typ);
      return ret;
    }
}

DEFBINOP_FN (lt, sparse_matrix, scalar, mx_el_lt)
DEFBINOP_FN (le, sparse_matrix, scalar, mx_el_le)
DEFBINOP_FN (eq, sparse_matrix, scalar, mx_el_eq)
DEFBINOP_FN (ge, sparse_matrix, scalar, mx_el_ge)
DEFBINOP_FN (gt, sparse_matrix, scalar, mx_el_gt)
DEFBINOP_FN (ne, sparse_matrix, scalar, mx_el_ne)

DEFBINOP_OP (el_mul, sparse_matrix, scalar, *)

DEFBINOP (el_div, sparse_matrix, scalar)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value (v1.sparse_matrix_value () / v2.double_value ());
}

DEFBINOP_FN (el_pow, sparse_matrix, scalar, elem_xpow)

DEFBINOP (el_ldiv, sparse_matrix, scalar)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return octave_value
         (elem_xdiv (v2.complex_value (), v1.sparse_matrix_value ()));
}

DEFBINOP_FN (el_and, sparse_matrix, scalar, mx_el_and)
DEFBINOP_FN (el_or, sparse_matrix, scalar, mx_el_or)

DEFCATOP (sm_s, sparse_matrix, scalar)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);
  SparseMatrix tmp (1, 1, v2.scalar_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFNDASSIGNOP_FN (assign, sparse_matrix, scalar, scalar, assign);
DEFNDASSIGNOP_FN (sgl_assign, sparse_matrix, float_scalar, scalar, assign);

void
install_sm_s_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_sparse_matrix, octave_scalar, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_sparse_matrix, octave_scalar, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_sparse_matrix, octave_scalar, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_sparse_matrix, octave_scalar, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_sparse_matrix, octave_scalar, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_sparse_matrix, octave_scalar, ldiv);

  INSTALL_BINOP_TI (ti, op_lt, octave_sparse_matrix, octave_scalar, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_sparse_matrix, octave_scalar, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_sparse_matrix, octave_scalar, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_sparse_matrix, octave_scalar, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_sparse_matrix, octave_scalar, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_sparse_matrix, octave_scalar, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_sparse_matrix, octave_scalar, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_sparse_matrix, octave_scalar, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_sparse_matrix, octave_scalar, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_sparse_matrix, octave_scalar, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_sparse_matrix, octave_scalar, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_sparse_matrix, octave_scalar, el_or);

  INSTALL_CATOP_TI (ti, octave_sparse_matrix, octave_scalar, sm_s);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_matrix, octave_scalar,
                       assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_matrix, octave_float_scalar,
                       sgl_assign);
}

OCTAVE_END_NAMESPACE(octave)
