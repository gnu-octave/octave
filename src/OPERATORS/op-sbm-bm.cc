/*

Copyright (C) 2004-2011 David Bateman
Copyright (C) 1998-2004 Andy Adler

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
#include "ov-bool-mat.h"
#include "boolMatrix.h"
#include "ov-scalar.h"
#include "ops.h"

#include "ov-re-sparse.h"
#include "ov-bool-sparse.h"
#include "smx-bm-sbm.h"
#include "smx-sbm-bm.h"

// sparse bool matrix by bool matrix ops.

DEFBINOP_FN (eq, sparse_bool_matrix, bool_matrix, mx_el_eq)
DEFBINOP_FN (ne, sparse_bool_matrix, bool_matrix, mx_el_ne)

DEFBINOP_FN (el_and, sparse_bool_matrix, bool_matrix, mx_el_and)
DEFBINOP_FN (el_or,  sparse_bool_matrix, bool_matrix, mx_el_or)

DEFCATOP (sbm_bm, sparse_bool_matrix, bool_matrix)
{
  CAST_BINOP_ARGS (octave_sparse_bool_matrix&, const octave_bool_matrix&); 
                   
  SparseBoolMatrix tmp (v2.bool_matrix_value ());
  return octave_value (v1.sparse_bool_matrix_value (). concat (tmp, ra_idx));
}

DEFCATOP (sbm_m, sparse_bool_matrix, matrix)
{
  CAST_BINOP_ARGS (octave_sparse_bool_matrix&, const octave_matrix&);
                   
  SparseMatrix tmp (v2.matrix_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFCATOP (sm_bm, sparse_matrix, bool_matrix)
{
  CAST_BINOP_ARGS (octave_sparse_matrix&, const octave_bool_matrix&); 
                   
  SparseMatrix tmp (v2.matrix_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFASSIGNOP (assign, sparse_bool_matrix, bool_matrix)
{
  CAST_BINOP_ARGS (octave_sparse_bool_matrix&, const octave_bool_matrix&);

  v1.assign (idx, SparseBoolMatrix (v2.bool_matrix_value ()));
  return octave_value ();
}

void
install_sbm_bm_ops (void)
{
  INSTALL_BINOP (op_eq, octave_sparse_bool_matrix, octave_bool_matrix, eq);
  INSTALL_BINOP (op_ne, octave_sparse_bool_matrix, octave_bool_matrix, ne);

  INSTALL_BINOP (op_el_and, octave_sparse_bool_matrix, octave_bool_matrix, 
                 el_and);
  INSTALL_BINOP (op_el_or, octave_sparse_bool_matrix, octave_bool_matrix, 
                 el_or);

  INSTALL_CATOP (octave_sparse_bool_matrix, octave_bool_matrix, sbm_bm);
  INSTALL_CATOP (octave_sparse_matrix, octave_bool_matrix, sm_bm);
  INSTALL_CATOP (octave_sparse_bool_matrix, octave_matrix, sbm_m);

  INSTALL_ASSIGNOP (op_asn_eq, octave_sparse_bool_matrix, 
                    octave_bool_matrix, assign);
}
