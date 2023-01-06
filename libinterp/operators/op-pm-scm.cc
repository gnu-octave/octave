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

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-typeinfo.h"
#include "ops.h"

#include "ov-perm.h"
#include "ov-cx-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// permutation matrix by sparse matrix ops

DEFBINOP (mul_pm_scm, perm_matrix, sparse_complex_matrix)
{
  const octave_perm_matrix& v1 = dynamic_cast<const octave_perm_matrix&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);

  if (v2.rows () == 1 && v2.columns () == 1)
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.sparse_matrix_value () * d);
    }
  else if (v1.rows () == 1 && v1.columns () == 1)
    return octave_value (v2.sparse_complex_matrix_value ());
  else
    return v1.perm_matrix_value  () * v2.sparse_complex_matrix_value ();
}

DEFBINOP (ldiv_pm_scm, perm_matrix, sparse_complex_matrix)
{
  const octave_perm_matrix& v1 = dynamic_cast<const octave_perm_matrix&> (a1);
  const octave_sparse_complex_matrix& v2
    = dynamic_cast<const octave_sparse_complex_matrix&> (a2);

  return v1.perm_matrix_value ().inverse () * v2.sparse_complex_matrix_value ();
}

// sparse matrix by diagonal matrix ops

DEFBINOP (mul_scm_pm, sparse_complex_matrix, perm_matrix)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_perm_matrix& v2 = dynamic_cast<const octave_perm_matrix&> (a2);

  if (v1.rows () == 1 && v1.columns () == 1)
    {
      std::complex<double> d = v1.scalar_value ();

      return octave_value (d * v2.sparse_matrix_value ());
    }
  else if (v2.rows () == 1 && v2.columns () == 1)
    return octave_value (v1.sparse_complex_matrix_value ());
  else
    return v1.sparse_complex_matrix_value  () * v2.perm_matrix_value ();
}

DEFBINOP (div_scm_pm, sparse_complex_matrix, perm_matrix)
{
  const octave_sparse_complex_matrix& v1
    = dynamic_cast<const octave_sparse_complex_matrix&> (a1);
  const octave_perm_matrix& v2 = dynamic_cast<const octave_perm_matrix&> (a2);

  return v1.sparse_complex_matrix_value () * v2.perm_matrix_value ().inverse ();
}

void
install_pm_scm_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_mul, octave_perm_matrix, octave_sparse_complex_matrix,
                    mul_pm_scm);
  INSTALL_BINOP_TI (ti, op_ldiv, octave_perm_matrix, octave_sparse_complex_matrix,
                    ldiv_pm_scm);
  INSTALL_BINOP_TI (ti, op_mul, octave_sparse_complex_matrix, octave_perm_matrix,
                    mul_scm_pm);
  INSTALL_BINOP_TI (ti, op_div, octave_sparse_complex_matrix, octave_perm_matrix,
                    div_scm_pm);
}

OCTAVE_END_NAMESPACE(octave)
