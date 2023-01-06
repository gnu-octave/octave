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
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "ops.h"
#include "xpow.h"

#include "sparse-xpow.h"
#include "sparse-xdiv.h"
#include "smx-s-scm.h"
#include "smx-scm-s.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// scalar by sparse complex matrix ops.

DEFBINOP_OP (add, scalar, sparse_complex_matrix, +)
DEFBINOP_OP (sub, scalar, sparse_complex_matrix, -)
DEFBINOP_OP (mul, scalar, sparse_complex_matrix, *)

DEFBINOP (div, scalar, sparse_complex_matrix)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);

  if (v2.rows () == 1 && v2.columns () == 1)
    return octave_value (SparseComplexMatrix (1, 1, v1.scalar_value ()
                         / v2.complex_value ()));
  else
    {
      MatrixType typ = v2.matrix_type ();
      Matrix m1 = Matrix (1, 1, v1.scalar_value ());
      SparseComplexMatrix m2 = v2.sparse_complex_matrix_value ();
      ComplexMatrix ret = xdiv (m1, m2, typ);
      v2.matrix_type (typ);
      return ret;
    }
}

DEFBINOP (pow, scalar, sparse_complex_matrix)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);
  return xpow (v1.scalar_value (), v2.complex_matrix_value ());
}

DEFBINOP (ldiv, scalar, sparse_complex_matrix)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);

  return octave_value (v2.sparse_complex_matrix_value () / v1.double_value ());
}

DEFBINOP_FN (lt, scalar, sparse_complex_matrix, mx_el_lt)
DEFBINOP_FN (le, scalar, sparse_complex_matrix, mx_el_le)
DEFBINOP_FN (eq, scalar, sparse_complex_matrix, mx_el_eq)
DEFBINOP_FN (ge, scalar, sparse_complex_matrix, mx_el_ge)
DEFBINOP_FN (gt, scalar, sparse_complex_matrix, mx_el_gt)
DEFBINOP_FN (ne, scalar, sparse_complex_matrix, mx_el_ne)

DEFBINOP_OP (el_mul, scalar, sparse_complex_matrix, *)
DEFBINOP_FN (el_div, scalar, sparse_complex_matrix, elem_xdiv)
DEFBINOP_FN (el_pow, scalar, sparse_complex_matrix, elem_xpow)

DEFBINOP (el_ldiv, scalar, sparse_complex_matrix)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);

  return octave_value (v2.sparse_complex_matrix_value () / v1.double_value ());
}

DEFBINOP_FN (el_and, scalar, sparse_complex_matrix, mx_el_and)
DEFBINOP_FN (el_or, scalar, sparse_complex_matrix, mx_el_or)

DEFCATOP (s_scm, scalar, sparse_compelx_matrix)
{
  const octave_scalar& v1 = dynamic_cast<const octave_scalar&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);
  SparseMatrix tmp (1, 1, v1.scalar_value ());
  return octave_value
         (tmp.concat (v2.sparse_complex_matrix_value (), ra_idx));
}

DEFCONV (sparse_complex_matrix_conv, scalar, sparse_complex_matrix)
{
  const octave_scalar& v = dynamic_cast<const octave_scalar&> (a);

  return new octave_sparse_complex_matrix
         (SparseComplexMatrix (v.complex_matrix_value ()));
}

void
install_s_scm_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_scalar, octave_sparse_complex_matrix, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_scalar, octave_sparse_complex_matrix, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_scalar, octave_sparse_complex_matrix, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_scalar, octave_sparse_complex_matrix, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_scalar, octave_sparse_complex_matrix, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_scalar, octave_sparse_complex_matrix,
                    ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_scalar, octave_sparse_complex_matrix, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_scalar, octave_sparse_complex_matrix, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_scalar, octave_sparse_complex_matrix, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_scalar, octave_sparse_complex_matrix, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_scalar, octave_sparse_complex_matrix, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_scalar, octave_sparse_complex_matrix, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_scalar, octave_sparse_complex_matrix,
                    el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_scalar, octave_sparse_complex_matrix,
                    el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_scalar, octave_sparse_complex_matrix,
                    el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_scalar, octave_sparse_complex_matrix,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_scalar, octave_sparse_complex_matrix,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_scalar, octave_sparse_complex_matrix,
                    el_or);

  INSTALL_CATOP_TI (ti, octave_scalar, octave_sparse_complex_matrix, s_scm);

  INSTALL_ASSIGNCONV_TI (ti, octave_scalar, octave_sparse_complex_matrix,
                         octave_complex_matrix);

  INSTALL_WIDENOP_TI (ti, octave_scalar, octave_sparse_complex_matrix,
                      sparse_complex_matrix_conv);
}

OCTAVE_END_NAMESPACE(octave)
