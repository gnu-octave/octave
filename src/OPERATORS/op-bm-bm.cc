/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-bool-mat.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// unary bool matrix ops.

DEFNDUNOP_OP (not, bool_matrix, bool_array, !)
DEFNDUNOP_OP (uplus, bool_matrix, array, +)
DEFNDUNOP_OP (uminus, bool_matrix, array, -)

DEFUNOP (transpose, bool_matrix)
{
  CAST_UNOP_ARG (const octave_bool_matrix&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.bool_matrix_value().transpose ());
}

// bool matrix by bool matrix ops.

DEFNDBINOP_FN (eq, bool_matrix, bool_matrix, bool_array, bool_array, mx_el_eq)
DEFNDBINOP_FN (ne, bool_matrix, bool_matrix, bool_array, bool_array, mx_el_ne)

DEFNDBINOP_FN (el_and, bool_matrix, bool_matrix, bool_array, bool_array,
	       mx_el_and)

DEFNDBINOP_FN (el_or,  bool_matrix, bool_matrix, bool_array, bool_array,
	       mx_el_or)

DEFNDCATOP_FN (bm_bm, bool_matrix, bool_matrix, bool_array, bool_array, concat)
DEFNDCATOP_FN (bm_m, bool_matrix, matrix, array, array, concat)
DEFNDCATOP_FN (m_bm, matrix, bool_matrix, array, array, concat)

DEFNDASSIGNOP_FN (assign, bool_matrix, bool_matrix, bool_array, assign)

void
install_bm_bm_ops (void)
{
  INSTALL_UNOP (op_not, octave_bool_matrix, not);
  INSTALL_UNOP (op_uplus, octave_bool_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_bool_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_bool_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_bool_matrix, transpose);

  INSTALL_BINOP (op_eq, octave_bool_matrix, octave_bool_matrix, eq);
  INSTALL_BINOP (op_ne, octave_bool_matrix, octave_bool_matrix, ne);

  INSTALL_BINOP (op_el_and, octave_bool_matrix, octave_bool_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_bool_matrix, octave_bool_matrix, el_or);

  INSTALL_CATOP (octave_bool_matrix, octave_bool_matrix, bm_bm);
  INSTALL_CATOP (octave_bool_matrix, octave_matrix, bm_m);
  INSTALL_CATOP (octave_matrix, octave_bool_matrix, m_bm);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_bool_matrix, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
