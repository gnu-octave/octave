////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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
#include "ops.h"

#include "ov-re-diag.h"
#include "ov-re-sparse.h"

#include "sparse-xdiv.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// diagonal matrix by sparse matrix ops

DEFBINOP (mul_dm_sm, diag_matrix, sparse_matrix)
{
  const octave_diag_matrix& v1 = dynamic_cast<const octave_diag_matrix&> (a1);
  const octave_sparse_matrix& v2 = dynamic_cast<const octave_sparse_matrix&> (a2);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v2.scalar_value ();

      return octave_value (v1.diag_matrix_value () * d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      SparseMatrix ret = v1.diag_matrix_value () * v2.sparse_matrix_value ();
      octave_value out = octave_value (ret);
      typ.mark_as_unsymmetric ();
      out.matrix_type (typ);
      return out;
    }
}

DEFBINOP (ldiv_dm_sm, diag_matrix, sparse_matrix)
{
  const octave_diag_matrix& v1 = dynamic_cast<const octave_diag_matrix&> (a1);
  const octave_sparse_matrix& v2 = dynamic_cast<const octave_sparse_matrix&> (a2);

  MatrixType typ = v2.matrix_type ();
  return xleftdiv (v1.diag_matrix_value (), v2.sparse_matrix_value (), typ);
}

DEFBINOP (add_dm_sm, diag_matrix, sparse_matrix)
{
  const octave_diag_matrix& v1 = dynamic_cast<const octave_diag_matrix&> (a1);
  const octave_sparse_matrix& v2 = dynamic_cast<const octave_sparse_matrix&> (a2);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v2.scalar_value ();

      return octave_value (v1.matrix_value () + d);
    }
  else
    return v1.diag_matrix_value () + v2.sparse_matrix_value ();
}

DEFBINOP (sub_dm_sm, diag_matrix, sparse_matrix)
{
  const octave_diag_matrix& v1 = dynamic_cast<const octave_diag_matrix&> (a1);
  const octave_sparse_matrix& v2 = dynamic_cast<const octave_sparse_matrix&> (a2);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v2.scalar_value ();

      return octave_value (v1.matrix_value () - d);
    }
  else
    return v1.diag_matrix_value () - v2.sparse_matrix_value ();
}

// sparse matrix by diagonal matrix ops

DEFBINOP (mul_sm_dm, sparse_matrix, diag_matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_diag_matrix& v2 = dynamic_cast<const octave_diag_matrix&> (a2);

  if (v1.rows () == 1 && v1.columns () == 1)
    // If v1 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v1.scalar_value ();

      return octave_value (d * v2.diag_matrix_value ());
    }
  else
    {
      MatrixType typ = v1.matrix_type ();
      SparseMatrix ret = v1.sparse_matrix_value () * v2.diag_matrix_value ();
      octave_value out = octave_value (ret);
      typ.mark_as_unsymmetric ();
      out.matrix_type (typ);
      return out;
    }
}

DEFBINOP (div_sm_dm, sparse_matrix, diag_matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_diag_matrix& v2 = dynamic_cast<const octave_diag_matrix&> (a2);

  if (v2.rows () == 1 && v2.columns () == 1)
    return octave_value (v1.sparse_matrix_value () / v2.scalar_value ());
  else
    {
      MatrixType typ = v2.matrix_type ();
      return xdiv (v1.sparse_matrix_value (), v2.diag_matrix_value (), typ);
    }
}

DEFBINOP (add_sm_dm, sparse_matrix, diag_matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_diag_matrix& v2 = dynamic_cast<const octave_diag_matrix&> (a2);

  if (v1.rows () == 1 && v1.columns () == 1)
    // If v1 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v1.scalar_value ();

      return octave_value (d + v2.matrix_value ());
    }
  else
    return v1.sparse_matrix_value () + v2.diag_matrix_value ();
}

DEFBINOP (sub_sm_dm, sparse_matrix, diag_matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_diag_matrix& v2 = dynamic_cast<const octave_diag_matrix&> (a2);

  if (v1.rows () == 1 && v1.columns () == 1)
    // If v1 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v1.scalar_value ();

      return octave_value (d - v2.matrix_value ());
    }
  else
    return v1.sparse_matrix_value () - v2.diag_matrix_value ();
}

void
install_dm_sm_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_mul, octave_diag_matrix, octave_sparse_matrix,
                    mul_dm_sm);

  INSTALL_BINOP_TI (ti, op_add, octave_diag_matrix, octave_sparse_matrix,
                    add_dm_sm);
  INSTALL_BINOP_TI (ti, op_sub, octave_diag_matrix, octave_sparse_matrix,
                    sub_dm_sm);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_diag_matrix, octave_sparse_matrix,
                    ldiv_dm_sm);

  INSTALL_BINOP_TI (ti, op_mul, octave_sparse_matrix, octave_diag_matrix,
                    mul_sm_dm);

  INSTALL_BINOP_TI (ti, op_add, octave_sparse_matrix, octave_diag_matrix,
                    add_sm_dm);
  INSTALL_BINOP_TI (ti, op_sub, octave_sparse_matrix, octave_diag_matrix,
                    sub_sm_dm);
  INSTALL_BINOP_TI (ti, op_div, octave_sparse_matrix, octave_diag_matrix,
                    div_sm_dm);
}

OCTAVE_END_NAMESPACE(octave)
