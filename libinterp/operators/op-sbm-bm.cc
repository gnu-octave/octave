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
#include "ov-bool-mat.h"
#include "boolMatrix.h"
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-range.h"
#include "ov-scalar.h"
#include "ov-str-mat.h"
#include "ops.h"
#include "ov-null-mat.h"
#include "ov-re-sparse.h"
#include "ov-bool-sparse.h"
#include "smx-bm-sbm.h"
#include "smx-sbm-bm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// sparse bool matrix by bool matrix ops.

DEFBINOP_FN (eq, sparse_bool_matrix, bool_matrix, mx_el_eq)
DEFBINOP_FN (ne, sparse_bool_matrix, bool_matrix, mx_el_ne)

DEFBINOP_FN (el_and, sparse_bool_matrix, bool_matrix, mx_el_and)
DEFBINOP_FN (el_or,  sparse_bool_matrix, bool_matrix, mx_el_or)

DEFCATOP (sbm_bm, sparse_bool_matrix, bool_matrix)
{
  const octave_sparse_bool_matrix& v1 = dynamic_cast<const octave_sparse_bool_matrix&> (a1);
  const octave_bool_matrix& v2 = dynamic_cast<const octave_bool_matrix&> (a2);

  SparseBoolMatrix tmp (v2.bool_matrix_value ());
  return octave_value (v1.sparse_bool_matrix_value (). concat (tmp, ra_idx));
}

DEFCATOP (sbm_m, sparse_bool_matrix, matrix)
{
  const octave_sparse_bool_matrix& v1 = dynamic_cast<const octave_sparse_bool_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);

  SparseMatrix tmp (v2.matrix_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFCATOP (sm_bm, sparse_matrix, bool_matrix)
{
  const octave_sparse_matrix& v1 = dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_bool_matrix& v2 = dynamic_cast<const octave_bool_matrix&> (a2);

  SparseMatrix tmp (v2.matrix_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFASSIGNOP (assign, sparse_bool_matrix, bool_matrix)
{
  octave_sparse_bool_matrix& v1 = dynamic_cast<octave_sparse_bool_matrix&> (a1);
  const octave_bool_matrix& v2 = dynamic_cast<const octave_bool_matrix&> (a2);

  v1.assign (idx, SparseBoolMatrix (v2.bool_matrix_value ()));
  return octave_value ();
}

DEFNULLASSIGNOP_FN (null_assign, sparse_bool_matrix, delete_elements)

static octave_value
oct_assignop_conv_and_assign (octave_base_value& a1,
                              const octave_value_list& idx,
                              const octave_base_value& a2)
{
  octave_sparse_bool_matrix& v1 = dynamic_cast<octave_sparse_bool_matrix&> (a1);

  // FIXME: perhaps add a warning for this conversion if the values
  // are not all 0 or 1?

  SparseBoolMatrix v2 (a2.bool_array_value ());

  v1.assign (idx, v2);

  return octave_value ();
}

void
install_sbm_bm_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_eq, octave_sparse_bool_matrix, octave_bool_matrix, eq);
  INSTALL_BINOP_TI (ti, op_ne, octave_sparse_bool_matrix, octave_bool_matrix, ne);

  INSTALL_BINOP_TI (ti, op_el_and, octave_sparse_bool_matrix, octave_bool_matrix,
                    el_and);
  INSTALL_BINOP_TI (ti, op_el_or, octave_sparse_bool_matrix, octave_bool_matrix,
                    el_or);

  INSTALL_CATOP_TI (ti, octave_sparse_bool_matrix, octave_bool_matrix, sbm_bm);
  INSTALL_CATOP_TI (ti, octave_sparse_matrix, octave_bool_matrix, sm_bm);
  INSTALL_CATOP_TI (ti, octave_sparse_bool_matrix, octave_matrix, sbm_m);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_bool_matrix, assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix, octave_matrix,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_char_matrix_str, conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_char_matrix_sq_str, conv_and_assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix, octave_range,
                       conv_and_assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_sparse_matrix,
                       conv_and_assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_int8_matrix,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_int16_matrix,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_int32_matrix,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_int64_matrix,
                       conv_and_assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_uint8_matrix,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_uint16_matrix,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_uint32_matrix,
                       conv_and_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_uint64_matrix,
                       conv_and_assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_null_matrix,
                       null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix, octave_null_str,
                       null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_sparse_bool_matrix,
                       octave_null_sq_str,
                       null_assign);
}

OCTAVE_END_NAMESPACE(octave)
