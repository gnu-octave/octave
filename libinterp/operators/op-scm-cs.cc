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
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ops.h"
#include "xpow.h"

#include "sparse-xpow.h"
#include "sparse-xdiv.h"
#include "ov-cx-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// sparse complex matrix by complex scalar ops.

DEFBINOP_OP (add, sparse_complex_matrix, complex, +)
DEFBINOP_OP (sub, sparse_complex_matrix, complex, -)
DEFBINOP_OP (mul, sparse_complex_matrix, complex, *)

DEFBINOP (div, sparse_complex_matrix, complex)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.sparse_complex_matrix_value () / v2.complex_value ());
}

DEFBINOP (pow, sparse_complex_matrix, complex)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);
  return xpow (v1.complex_matrix_value (), v2.complex_value ());
}

DEFBINOP (ldiv, sparse_complex_matrix, complex)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  if (v1.rows () == 1 && v1.columns () == 1)
    return octave_value (SparseComplexMatrix (1, 1, v2.complex_value ()
                         / v1.complex_value ()));
  else
    {
      MatrixType typ = v1.matrix_type ();
      SparseComplexMatrix m1 = v1.sparse_complex_matrix_value ();
      ComplexMatrix m2 = ComplexMatrix (1, 1, v2.complex_value ());
      ComplexMatrix ret = xleftdiv (m1, m2, typ);
      v1.matrix_type (typ);
      return ret;
    }
}

DEFBINOP_FN (lt, sparse_complex_matrix, complex, mx_el_lt)
DEFBINOP_FN (le, sparse_complex_matrix, complex, mx_el_le)
DEFBINOP_FN (eq, sparse_complex_matrix, complex, mx_el_eq)
DEFBINOP_FN (ge, sparse_complex_matrix, complex, mx_el_ge)
DEFBINOP_FN (gt, sparse_complex_matrix, complex, mx_el_gt)
DEFBINOP_FN (ne, sparse_complex_matrix, complex, mx_el_ne)

DEFBINOP_OP (el_mul, sparse_complex_matrix, complex, *)

DEFBINOP (el_div, sparse_complex_matrix, complex)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value (v1.sparse_complex_matrix_value () / v2.complex_value ());
}

DEFBINOP_FN (el_pow, sparse_complex_matrix, complex, elem_xpow)

DEFBINOP (el_ldiv, sparse_complex_matrix, complex)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);

  return octave_value
         (elem_xdiv (v2.complex_value (), v1.sparse_complex_matrix_value ()));
}

DEFBINOP_FN (el_and, sparse_complex_matrix, complex, mx_el_and)
DEFBINOP_FN (el_or,  sparse_complex_matrix, complex, mx_el_or)

DEFCATOP (scm_cs, sparse_complex_matrix, complex)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_complex& v2 = dynamic_cast<const octave_complex&> (a2);
  SparseComplexMatrix tmp (1, 1, v2.complex_value ());
  return octave_value
         (v1.sparse_complex_matrix_value ().concat (tmp, ra_idx));
}

DEFNDASSIGNOP_FN (assign, sparse_complex_matrix, complex, complex, assign);
DEFNDASSIGNOP_FN (sgl_assign, sparse_complex_matrix, float_complex, complex, assign);

void
install_scm_cs_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_add, octave_sparse_complex_matrix, octave_complex,
                    add);
  INSTALL_BINOP_TI (ti, op_sub, octave_sparse_complex_matrix, octave_complex,
                    sub);
  INSTALL_BINOP_TI (ti, op_mul, octave_sparse_complex_matrix, octave_complex,
                    mul);
  INSTALL_BINOP_TI (ti, op_div, octave_sparse_complex_matrix, octave_complex,
                    div);
  INSTALL_BINOP_TI (ti, op_pow, octave_sparse_complex_matrix, octave_complex,
                    pow);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_sparse_complex_matrix, octave_complex,
                    ldiv);
  INSTALL_BINOP_TI (ti, op_lt, octave_sparse_complex_matrix, octave_complex, lt);
  INSTALL_BINOP_TI (ti, op_le, octave_sparse_complex_matrix, octave_complex, le);
  INSTALL_BINOP_TI (ti, op_eq, octave_sparse_complex_matrix, octave_complex, eq);
  INSTALL_BINOP_TI (ti, op_ge, octave_sparse_complex_matrix, octave_complex, ge);
  INSTALL_BINOP_TI (ti, op_gt, octave_sparse_complex_matrix, octave_complex, gt);
  INSTALL_BINOP_TI (ti, op_ne, octave_sparse_complex_matrix, octave_complex, ne);
  INSTALL_BINOP_TI (ti, op_el_mul, octave_sparse_complex_matrix, octave_complex,
                    el_mul);
  INSTALL_BINOP_TI (ti, op_el_div, octave_sparse_complex_matrix, octave_complex,
                    el_div);
  INSTALL_BINOP_TI (ti, op_el_pow, octave_sparse_complex_matrix, octave_complex,
                    el_pow);
  INSTALL_BINOP_TI (ti, op_el_ldiv, octave_sparse_complex_matrix, octave_complex,
                    el_ldiv);
  INSTALL_BINOP_TI (ti, op_el_and, octave_sparse_complex_matrix, octave_complex,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_sparse_complex_matrix, octave_complex,
                    el_or);

  INSTALL_CATOP_TI (ti, octave_sparse_complex_matrix, octave_complex, scm_cs);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_complex_matrix,
                       octave_complex, assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_complex_matrix,
                       octave_float_complex, sgl_assign);
}

OCTAVE_END_NAMESPACE(octave)
