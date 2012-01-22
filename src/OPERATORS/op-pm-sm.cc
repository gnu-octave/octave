/*

Copyright (C) 2009-2012 Jason Riedy

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

#include "ov-perm.h"
#include "ov-re-sparse.h"
#include "ov-bool-sparse.h"

// Unary permutation ops, some cast to sparse

//Avoid casting to a full matrix
DEFUNOP_OP (uplus, perm_matrix, /* no-op */)

// Not calling standard CAST_UNOP_ARG for these next two because a
// dynamic_cast would fail.
DEFUNOP (not, perm_matrix)
{
  // Obviously negation of a permutation matrix destroys sparsity
  return octave_value ( ! a.bool_array_value ());
}

DEFUNOP (uminus, perm_matrix)
{
  return octave_value ( - a.sparse_matrix_value ());
}

// Most other logical operations cast to SparseBoolMatrix
DEFBINOP (eq_pm, perm_matrix, perm_matrix)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_perm_matrix&);
  return v1.sparse_bool_matrix_value () == v2.sparse_bool_matrix_value ();
}
DEFBINOP (ne_pm, perm_matrix, perm_matrix)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_perm_matrix&);
  return v1.sparse_bool_matrix_value () != v2.sparse_bool_matrix_value ();
}
DEFBINOP (el_and_pm, perm_matrix, perm_matrix)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_perm_matrix&);
  return mx_el_and(v1.sparse_bool_matrix_value (),
                   v2.sparse_bool_matrix_value ());
}
DEFBINOP (el_or_pm,  perm_matrix, perm_matrix)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_perm_matrix&);
  return mx_el_or(v1.sparse_bool_matrix_value (),
                  v2.sparse_bool_matrix_value ());
}

// permutation matrix by sparse matrix ops

DEFBINOP (mul_pm_sm, perm_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_sparse_matrix&);

  if (v2.rows() == 1 && v2.columns() == 1)
    {
      double d = v2.scalar_value ();

      return octave_value (v1.sparse_matrix_value () * d);
    }
  else if (v1.rows() == 1 && v1.columns() == 1)
    return octave_value (v2.sparse_matrix_value ());
  else
    return v1.perm_matrix_value  () * v2.sparse_matrix_value ();
}

DEFBINOP (ldiv_pm_sm, perm_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_sparse_matrix&);

  return v1.perm_matrix_value ().inverse () * v2.sparse_matrix_value ();
}

// sparse matrix by diagonal matrix ops

DEFBINOP (mul_sm_pm, sparse_matrix, perm_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&, const octave_perm_matrix&);

  if (v1.rows() == 1 && v1.columns() == 1)
    {
      double d = v1.scalar_value ();

      return octave_value (d * v2.sparse_matrix_value ());
    }
  else if (v2.rows() == 1 && v2.columns() == 1)
    return octave_value (v1.sparse_matrix_value ());
  else
    return v1.sparse_matrix_value  () * v2.perm_matrix_value ();
}

DEFBINOP (div_sm_pm, sparse_matrix, perm_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&, const octave_perm_matrix&);

  return v1.sparse_matrix_value () * v2.perm_matrix_value ().inverse ();
}

void
install_pm_sm_ops (void)
{
  INSTALL_UNOP (op_not, octave_perm_matrix, not);
  INSTALL_UNOP (op_uplus, octave_perm_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_perm_matrix, uminus);


  INSTALL_BINOP (op_mul, octave_perm_matrix, octave_sparse_matrix,
                 mul_pm_sm);
  INSTALL_BINOP (op_ldiv, octave_perm_matrix, octave_sparse_matrix,
                 ldiv_pm_sm);
  INSTALL_BINOP (op_mul, octave_sparse_matrix, octave_perm_matrix,
                 mul_sm_pm);
  INSTALL_BINOP (op_div, octave_sparse_matrix, octave_perm_matrix,
                 div_sm_pm);

  INSTALL_BINOP (op_eq, octave_perm_matrix, octave_perm_matrix, eq_pm);
  INSTALL_BINOP (op_ne, octave_perm_matrix, octave_perm_matrix, ne_pm);
  INSTALL_BINOP (op_el_and, octave_perm_matrix, octave_perm_matrix, el_and_pm);
  INSTALL_BINOP (op_el_or,  octave_perm_matrix, octave_perm_matrix, el_or_pm);
}
