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
#include "ov-str-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"

// string unary ops.

DEFUNOP (transpose, matrix)
{
  CAST_UNOP_ARG (const octave_char_matrix_str&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.char_matrix_value().transpose (), true);
}

// string by string ops.

DEFBINOP (eq, char_matrix_str, char_matrix_str)
{
  CAST_BINOP_ARGS (const octave_char_matrix_str&,
		   const octave_char_matrix_str&);

  charMatrix cm1 = v1.char_matrix_value ();
  charMatrix cm2 = v2.char_matrix_value ();

  if (cm1.rows () == 1 && cm1.columns () == 1)
    {
      if (cm2.rows () == 1 && cm2.columns () == 1)
	return octave_value (cm1 (0, 0) == cm2 (0, 0));
      else
	SC_MX_BOOL_OP (char, c, cm1 (0, 0), charMatrix, m, cm2,
		       c == m (i, j), 0.0);
    }
  else
    {
      int cm2_nr = cm2.rows ();
      int cm2_nc = cm2.cols ();

      if (cm2_nr == 1 && cm2_nc == 1)
	MX_SC_BOOL_OP (charMatrix, m, cm1, char, c, cm2 (0, 0),
		       c == m (i, j), 0.0);
      else
	MX_MX_BOOL_OP (charMatrix, m1, cm1, charMatrix, m2, cm2,
		       m1 (i, j) == m2 (i, j), "==", 0.0, 1.0);
    }
}

DEFBINOP (ne, char_matrix_str, char_matrix_str)
{
  CAST_BINOP_ARGS (const octave_char_matrix_str&,
		   const octave_char_matrix_str&);

  charMatrix cm1 = v1.char_matrix_value ();
  charMatrix cm2 = v2.char_matrix_value ();

  if (cm1.rows () == 1 && cm1.columns () == 1)
    {
      if (cm2.rows () == 1 && cm2.columns () == 1)
	return octave_value (cm1 (0, 0) != cm2 (0, 0));
      else
	SC_MX_BOOL_OP (char, c, cm1 (0, 0), charMatrix, m, cm2,
		       c != m (i, j), 1.0);
    }
  else
    {
      if (cm2.rows () == 1 && cm2.columns () == 1)
	MX_SC_BOOL_OP (charMatrix, m, cm1, char, c, cm2 (0, 0),
		       c != m (i, j), 1.0);
      else
	MX_MX_BOOL_OP (charMatrix, m1, cm1, charMatrix, m2, cm2,
		       m1 (i, j) != m2 (i, j), "!=", 1.0, 0.0);
    }
}

DEFASSIGNOP (assign, char_matrix_str, char_matrix_str)
{
  CAST_BINOP_ARGS (octave_char_matrix_str&, const octave_char_matrix_str&);

  v1.assign (idx, v2.char_matrix_value ());
  return octave_value ();
}

DEFCATOP (str_str, char_matrix_str, char_matrix_str)
{
  CAST_BINOP_ARGS (octave_char_matrix_str&, const octave_char_matrix_str&);
  return octave_value (v1.char_array_value (). concat (v2.char_array_value (), 
			       ra_idx), true);
}

void
install_str_str_ops (void)
{
  INSTALL_UNOP (op_transpose, octave_char_matrix_str, transpose);
  INSTALL_UNOP (op_hermitian, octave_char_matrix_str, transpose);

  INSTALL_BINOP (op_eq, octave_char_matrix_str, octave_char_matrix_str, eq);
  INSTALL_BINOP (op_ne, octave_char_matrix_str, octave_char_matrix_str, ne);

  INSTALL_CATOP (octave_char_matrix_str, octave_char_matrix_str, str_str);

  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_str, octave_char_matrix_str, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
