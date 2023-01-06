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
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-scalar.h"
#include "ops.h"

#include "ov-re-sparse.h"
#include "ov-bool-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// sparse bool matrix by bool ops.

DEFBINOP_FN (ne, sparse_bool_matrix, bool, mx_el_ne)
DEFBINOP_FN (eq, sparse_bool_matrix, bool, mx_el_eq)

DEFBINOP_FN (el_and, sparse_bool_matrix, bool, mx_el_and)
DEFBINOP_FN (el_or, sparse_bool_matrix, bool, mx_el_or)

DEFCATOP (sbm_b, sparse_bool_matrix, bool)
{
  const octave_sparse_bool_matrix& v1 = dynamic_cast<const octave_sparse_bool_matrix&> (a1);
  const octave_bool& v2 = dynamic_cast<const octave_bool&> (a2);

  SparseBoolMatrix tmp (1, 1, v2.bool_value ());
  return octave_value (v1.sparse_bool_matrix_value (). concat (tmp, ra_idx));
}

DEFCATOP (sm_b, sparse_matrix, bool)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_bool& v2 = dynamic_cast<const octave_bool&> (a2);

  SparseMatrix tmp (1, 1, v2.scalar_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFCATOP (sbm_s, sparse_bool_matrix, scalar)
{
  const octave_sparse_bool_matrix& v1 = dynamic_cast<const octave_sparse_bool_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  SparseMatrix tmp (1, 1, v2.scalar_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFASSIGNOP (assign, sparse_bool_matrix, bool)
{
  octave_sparse_bool_matrix& v1 = dynamic_cast<octave_sparse_bool_matrix&> (a1);
  const octave_bool& v2 = dynamic_cast<const octave_bool&> (a2);

  SparseBoolMatrix tmp (1, 1, v2.bool_value ());
  v1.assign (idx, tmp);
  return octave_value ();
}

static octave_value
oct_assignop_conv_and_assign (octave_base_value& a1,
                              const octave_value_list& idx,
                              const octave_base_value& a2)
{
  octave_sparse_bool_matrix& v1 = dynamic_cast<octave_sparse_bool_matrix&> (a1);

  // FIXME: perhaps add a warning for this conversion if the values
  // are not all 0 or 1?

  SparseBoolMatrix v2 (1, 1, a2.bool_value ());

  v1.assign (idx, v2);

  return octave_value ();
}

void
install_sbm_b_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_eq, octave_sparse_bool_matrix, octave_bool, eq);
  INSTALL_BINOP_TI (ti, op_ne, octave_sparse_bool_matrix, octave_bool, ne);

  INSTALL_BINOP_TI (ti, op_el_and, octave_sparse_bool_matrix, octave_bool,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_sparse_bool_matrix, octave_bool, el_or);

  INSTALL_CATOP_TI (ti, octave_sparse_bool_matrix, octave_bool, sbm_b);
  INSTALL_CATOP_TI (ti, octave_sparse_bool_matrix, octave_scalar, sbm_s);
  INSTALL_CATOP_TI (ti, octave_sparse_matrix, octave_bool, sm_b);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix, octave_bool,
                       assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix, octave_scalar,
                       conv_and_assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_int8_scalar,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_int16_scalar,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_int32_scalar,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_int64_scalar,
                       conv_and_assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_uint8_scalar,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_uint16_scalar,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_uint32_scalar,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_uint64_scalar,
                       conv_and_assign);
}

OCTAVE_END_NAMESPACE(octave)
