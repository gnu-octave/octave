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
#include "ov-null-mat.h"
#include "ops.h"

#include "sparse-xdiv.h"
#include "sparse-xpow.h"
#include "ov-cx-mat.h"
#include "ov-cx-sparse.h"
#include "ov-re-sparse.h"

#include "xdiv.h"
#include "xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// unary sparse complex matrix ops.

DEFUNOP_OP (not, sparse_complex_matrix, !)
DEFUNOP_OP (uplus, sparse_complex_matrix, /* no-op */)
DEFUNOP_OP (uminus, sparse_complex_matrix, -)

DEFUNOP (transpose, sparse_complex_matrix)
{
  const octave_sparse_complex_matrix& v
    = dynamic_cast<const octave_sparse_complex_matrix&> (a);
  return octave_value
         (v.sparse_complex_matrix_value ().transpose (),
          v.matrix_type ().transpose ());
}

DEFUNOP (hermitian, sparse_complex_matrix)
{
  const octave_sparse_complex_matrix& v
    = dynamic_cast<const octave_sparse_complex_matrix&> (a);
  return octave_value
         (v.sparse_complex_matrix_value ().hermitian (),
          v.matrix_type ().transpose ());
}

// complex matrix by complex matrix ops.

DEFBINOP_OP (add, sparse_complex_matrix, sparse_complex_matrix, +)
DEFBINOP_OP (sub, sparse_complex_matrix, sparse_complex_matrix, -)

DEFBINOP_OP (mul, sparse_complex_matrix, sparse_complex_matrix, *)

DEFBINOP (div, sparse_complex_matrix, sparse_complex_matrix)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);

  if (v2.rows () == 1 && v2.columns () == 1)
    return octave_value (v1.sparse_complex_matrix_value () / v2.complex_value ());
  else
    {
      MatrixType typ = v2.matrix_type ();
      SparseComplexMatrix ret = xdiv (v1.sparse_complex_matrix_value (),
                                      v2.sparse_complex_matrix_value (), typ);

      v2.matrix_type (typ);
      return ret;
    }
}

DEFBINOPX (pow, sparse_complex_matrix, sparse_complex_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
}

DEFBINOP (ldiv, sparse_complex_matrix, sparse_complex_matrix)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);

  if (v1.rows () == 1 && v1.columns () == 1)
    return octave_value (v2.sparse_complex_matrix_value () / v1.complex_value ());
  else
    {
      MatrixType typ = v1.matrix_type ();

      SparseComplexMatrix ret
        = xleftdiv (v1.sparse_complex_matrix_value (),
                    v2.sparse_complex_matrix_value (), typ);

      v1.matrix_type (typ);
      return ret;
    }
}

DEFBINOP_FN (lt, sparse_complex_matrix, sparse_complex_matrix, mx_el_lt)
DEFBINOP_FN (le, sparse_complex_matrix, sparse_complex_matrix, mx_el_le)
DEFBINOP_FN (eq, sparse_complex_matrix, sparse_complex_matrix, mx_el_eq)
DEFBINOP_FN (ge, sparse_complex_matrix, sparse_complex_matrix, mx_el_ge)
DEFBINOP_FN (gt, sparse_complex_matrix, sparse_complex_matrix, mx_el_gt)
DEFBINOP_FN (ne, sparse_complex_matrix, sparse_complex_matrix, mx_el_ne)

DEFBINOP_FN (el_mul, sparse_complex_matrix, sparse_complex_matrix, product)
DEFBINOP_FN (el_div, sparse_complex_matrix, sparse_complex_matrix, quotient)
DEFBINOP_FN (el_pow, sparse_complex_matrix, sparse_complex_matrix, elem_xpow)

DEFBINOP (el_ldiv, sparse_complex_matrix, sparse_complex_matrix)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);

  return octave_value (quotient (v2.sparse_complex_matrix_value (),
                                 v1.sparse_complex_matrix_value ()));
}

DEFBINOP_FN (el_and, sparse_complex_matrix, sparse_complex_matrix, mx_el_and)
DEFBINOP_FN (el_or,  sparse_complex_matrix, sparse_complex_matrix, mx_el_or)

DEFCATOP_FN (scm_scm, sparse_complex_matrix, sparse_complex_matrix, concat)

DEFASSIGNOP_FN (assign, sparse_complex_matrix, sparse_complex_matrix, assign)

DEFNULLASSIGNOP_FN (null_assign, sparse_complex_matrix, delete_elements)

void
install_scm_scm_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_not, octave_sparse_complex_matrix, not);
  INSTALL_UNOP_TI (ti, op_uplus, octave_sparse_complex_matrix, uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_sparse_complex_matrix, uminus);
  INSTALL_UNOP_TI (ti, op_transpose, octave_sparse_complex_matrix, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_sparse_complex_matrix, hermitian);

  INSTALL_BINOP_TI (ti, op_add, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, add);
  INSTALL_BINOP_TI (ti, op_sub, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, mul);
  INSTALL_BINOP_TI (ti, op_div, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, div);
  INSTALL_BINOP_TI (ti, op_pow, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, el_or);

  INSTALL_CATOP_TI (ti, octave_sparse_complex_matrix,
                    octave_sparse_complex_matrix, scm_scm);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_complex_matrix,
                       octave_sparse_complex_matrix, assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_complex_matrix,
                       octave_null_matrix, null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_complex_matrix,
                       octave_null_str, null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_complex_matrix,
                       octave_null_sq_str, null_assign);
}

OCTAVE_END_NAMESPACE(octave)
