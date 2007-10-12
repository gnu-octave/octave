/*

Copyright (C) 1996, 1997 John W. Eaton

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
#include "ov-range.h"
#include "ov-ch-mat.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"

// range unary ops.

DEFUNOP (not, range)
{
  CAST_UNOP_ARG (const octave_range&);

  return octave_value (! v.matrix_value());
}

DEFUNOP_OP (uplus, range, /* no-op */)
DEFUNOP_OP (uminus, range, -)

DEFUNOP (transpose, range)
{
  CAST_UNOP_ARG (const octave_range&);

  return octave_value (v.matrix_value().transpose ());
}

DEFNDCATOP_FN (r_r, range, range, array, array, concat)
DEFNDCATOP_FN (r_s, range, scalar, array, array, concat)
DEFNDCATOP_FN (r_m, range, matrix, array, array, concat)
DEFNDCATOP_FN (r_cs, range, complex, array, complex_array, concat)
DEFNDCATOP_FN (r_cm, range, complex_matrix, array, complex_array, concat)
DEFNDCATOP_FN (r_b, range, bool, array, array, concat)
DEFNDCATOP_FN (r_bm, range, bool_matrix, array, array, concat)
DEFNDCATOP_FN (r_chm, range, char_matrix, array, char_array, concat)
DEFNDCATOP_FN (s_r, scalar, range, array, array, concat)
DEFNDCATOP_FN (m_r, matrix, range, array, array, concat)
DEFNDCATOP_FN (cs_r, complex, range, complex_array, array, concat)
DEFNDCATOP_FN (cm_r, complex_matrix, range, complex_array, array, concat)
DEFNDCATOP_FN (b_r, bool, range, array, array, concat)
DEFNDCATOP_FN (bm_r, bool_matrix, range, array, array, concat)
DEFNDCATOP_FN (chm_r, char_matrix, range, char_array, array, concat)

void
install_range_ops (void)
{
  INSTALL_UNOP (op_not, octave_range, not);
  INSTALL_UNOP (op_uplus, octave_range, uplus);
  INSTALL_UNOP (op_uminus, octave_range, uminus);
  INSTALL_UNOP (op_transpose, octave_range, transpose);
  INSTALL_UNOP (op_hermitian, octave_range, transpose);

  INSTALL_CATOP (octave_range, octave_range, r_r);
  INSTALL_CATOP (octave_range, octave_scalar, r_s);
  INSTALL_CATOP (octave_range, octave_matrix, r_m);
  INSTALL_CATOP (octave_range, octave_complex, r_cs);
  INSTALL_CATOP (octave_range, octave_complex_matrix, r_cm);
  INSTALL_CATOP (octave_range, octave_bool, r_b);
  INSTALL_CATOP (octave_range, octave_bool_matrix, r_bm);
  INSTALL_CATOP (octave_range, octave_char_matrix, r_chm);
  INSTALL_CATOP (octave_scalar, octave_range, s_r);
  INSTALL_CATOP (octave_matrix, octave_range, m_r);
  INSTALL_CATOP (octave_complex, octave_range, cs_r);
  INSTALL_CATOP (octave_complex_matrix, octave_range, cm_r);
  INSTALL_CATOP (octave_bool, octave_range, b_r);
  INSTALL_CATOP (octave_bool_matrix, octave_range, bm_r);
  INSTALL_CATOP (octave_char_matrix, octave_range, chm_r);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
