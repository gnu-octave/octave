/*

Copyright (C) 2009 Jason Riedy

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-typeinfo.h"
#include "ops.h"

#include "ov-re-diag.h"
#include "ov-re-sparse.h"

// diagonal matrix by sparse matrix ops

DEFBINOP (mul_dm_sm, diag_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_diag_matrix&, const octave_sparse_matrix&);

  if (v2.rows() == 1 && v2.columns() == 1)
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

// sparse matrix by diagonal matrix ops

DEFBINOP (mul_sm_dm, sparse_matrix, diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&, const octave_diag_matrix&);

  if (v1.rows() == 1 && v1.columns() == 1)
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

void
install_dm_sm_ops (void)
{
  INSTALL_BINOP (op_mul, octave_diag_matrix, octave_sparse_matrix,
		 mul_dm_sm);

  INSTALL_BINOP (op_mul, octave_sparse_matrix, octave_diag_matrix,
		 mul_sm_dm);
}
