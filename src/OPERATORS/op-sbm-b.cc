/*

Copyright (C) 2004, 2005, 2007, 2008, 2009 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Andy Adler

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
#include "ov-bool.h"
#include "ov-scalar.h"
#include "ops.h"

#include "ov-re-sparse.h"
#include "ov-bool-sparse.h"

// sparse bool matrix by bool ops.

DEFBINOP_FN (ne, sparse_bool_matrix, bool, mx_el_ne)
DEFBINOP_FN (eq, sparse_bool_matrix, bool, mx_el_eq)

DEFBINOP_FN (el_and, sparse_bool_matrix, bool, mx_el_and)
DEFBINOP_FN (el_or, sparse_bool_matrix, bool, mx_el_or)

DEFCATOP (sbm_b, sparse_bool_matrix, bool)
{
  CAST_BINOP_ARGS (octave_sparse_bool_matrix&, const octave_bool&); 
                   
  SparseBoolMatrix tmp (1, 1, v2.bool_value ());
  return octave_value (v1.sparse_bool_matrix_value (). concat (tmp, ra_idx));
}

DEFCATOP (sm_b, sparse_matrix, bool)
{
  CAST_BINOP_ARGS (octave_sparse_matrix&, const octave_bool&);
                   
  SparseMatrix tmp (1, 1, v2.scalar_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFCATOP (sbm_s, sparse_bool_matrix, scalar)
{
  CAST_BINOP_ARGS (octave_sparse_bool_matrix&, const octave_scalar&); 
                   
  SparseMatrix tmp (1, 1, v2.scalar_value ());
  return octave_value (v1.sparse_matrix_value (). concat (tmp, ra_idx));
}

DEFASSIGNOP (assign, sparse_bool_matrix, bool)
{
  CAST_BINOP_ARGS (octave_sparse_bool_matrix&, const octave_bool&);

  SparseBoolMatrix tmp (1, 1, v2.bool_value ());
  v1.assign (idx, tmp);
  return octave_value ();
}

void
install_sbm_b_ops (void)
{
  INSTALL_BINOP (op_eq, octave_sparse_bool_matrix, octave_bool, eq);
  INSTALL_BINOP (op_ne, octave_sparse_bool_matrix, octave_bool, ne);

  INSTALL_BINOP (op_el_and, octave_sparse_bool_matrix, octave_bool, el_and);
  INSTALL_BINOP (op_el_or, octave_sparse_bool_matrix, octave_bool, el_or);

  INSTALL_CATOP (octave_sparse_bool_matrix, octave_bool, sbm_b);
  INSTALL_CATOP (octave_sparse_bool_matrix, octave_scalar, sbm_s);
  INSTALL_CATOP (octave_sparse_matrix, octave_bool, sm_b);

  INSTALL_ASSIGNOP (op_asn_eq, octave_sparse_bool_matrix, octave_bool, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
