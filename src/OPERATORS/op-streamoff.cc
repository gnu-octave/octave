/*

Copyright (C) 2003 John W. Eaton

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
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-streamoff.h"
#include "ov-typeinfo.h"
#include "ops.h"

// streamoff unary ops.

DEFUNOP (transpose, streamoff)
{
  CAST_UNOP_ARG (const octave_streamoff&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (streamoff_array (v.streamoff_array_value().transpose ()));
}

DEFNCUNOP_METHOD (incr, streamoff, increment)
DEFNCUNOP_METHOD (decr, streamoff, decrement)

// streamoff by streamoff ops.

DEFNDBINOP_OP (add, streamoff, streamoff, streamoff_array, streamoff_array, +)
DEFNDBINOP_OP (sub, streamoff, streamoff, streamoff_array, streamoff_array, -)

DEFNDBINOP_OP (add_so_m, streamoff, matrix, streamoff_array, streamoff_array, +)
DEFNDBINOP_OP (sub_so_m, streamoff, matrix, streamoff_array, streamoff_array, -)

DEFNDBINOP_OP (add_m_so, matrix, streamoff, streamoff_array, streamoff_array, +)
DEFNDBINOP_OP (sub_m_so, matrix, streamoff, streamoff_array, streamoff_array, +)
DEFNDBINOP_OP (add_so_s, streamoff, scalar, streamoff_array, streamoff, +)
DEFNDBINOP_OP (sub_so_s, streamoff, scalar, streamoff_array, streamoff, -)

DEFNDBINOP_OP (add_s_so, scalar, streamoff, streamoff, streamoff_array, +)
DEFNDBINOP_OP (sub_s_so, scalar, streamoff, streamoff, streamoff_array, +)

DEFBINOP (eq, streamoff, streamoff)
{
  CAST_BINOP_ARGS (const octave_streamoff&,
		   const octave_streamoff&);

  streamoff_array cm1 = v1.streamoff_array_value ();
  streamoff_array cm2 = v2.streamoff_array_value ();

  if (cm1.rows () == 1 && cm1.columns () == 1)
    {
      if (cm2.rows () == 1 && cm2.columns () == 1)
	return octave_value (cm1 (0, 0) == cm2 (0, 0));
      else
	SC_MX_BOOL_OP (std::streamoff, c, cm1 (0, 0), streamoff_array, m, cm2,
		       c == m (i, j), 0.0);
    }
  else
    {
      int cm2_nr = cm2.rows ();
      int cm2_nc = cm2.cols ();

      if (cm2_nr == 1 && cm2_nc == 1)
	MX_SC_BOOL_OP (streamoff_array, m, cm1, std::streamoff, c, cm2 (0, 0),
		       c == m (i, j), 0.0);
      else
	MX_MX_BOOL_OP (streamoff_array, m1, cm1, streamoff_array, m2, cm2,
		       m1 (i, j) == m2 (i, j), "==", 0.0, 1.0);
    }
}

DEFBINOP (ne, streamoff, streamoff)
{
  CAST_BINOP_ARGS (const octave_streamoff&,
		   const octave_streamoff&);

  streamoff_array cm1 = v1.streamoff_array_value ();
  streamoff_array cm2 = v2.streamoff_array_value ();

  if (cm1.rows () == 1 && cm1.columns () == 1)
    {
      if (cm2.rows () == 1 && cm2.columns () == 1)
	return octave_value (cm1 (0, 0) != cm2 (0, 0));
      else
	SC_MX_BOOL_OP (std::streamoff, c, cm1 (0, 0), streamoff_array, m, cm2,
		       c != m (i, j), 1.0);
    }
  else
    {
      if (cm2.rows () == 1 && cm2.columns () == 1)
	MX_SC_BOOL_OP (streamoff_array, m, cm1, std::streamoff, c, cm2 (0, 0),
		       c != m (i, j), 1.0);
      else
	MX_MX_BOOL_OP (streamoff_array, m1, cm1, streamoff_array, m2, cm2,
		       m1 (i, j) != m2 (i, j), "!=", 1.0, 0.0);
    }
}

DEFASSIGNOP (assign, streamoff, streamoff)
{
  CAST_BINOP_ARGS (octave_streamoff&, const octave_streamoff&);

  v1.assign (idx, v2.streamoff_array_value ());
  return octave_value ();
}

void
install_streamoff_ops (void)
{
  INSTALL_UNOP (op_transpose, octave_streamoff, transpose);
  INSTALL_UNOP (op_hermitian, octave_streamoff, transpose);

  INSTALL_NCUNOP (op_incr, octave_streamoff, incr);
  INSTALL_NCUNOP (op_decr, octave_streamoff, decr);

  INSTALL_BINOP (op_eq, octave_streamoff, octave_streamoff, eq);
  INSTALL_BINOP (op_ne, octave_streamoff, octave_streamoff, ne);

  INSTALL_BINOP (op_add, octave_streamoff, octave_streamoff, add);
  INSTALL_BINOP (op_sub, octave_streamoff, octave_streamoff, sub);

  INSTALL_BINOP (op_add, octave_streamoff, octave_matrix, add_so_m);
  INSTALL_BINOP (op_sub, octave_streamoff, octave_matrix, sub_so_m);

  INSTALL_BINOP (op_add, octave_matrix, octave_streamoff, add_m_so);
  INSTALL_BINOP (op_sub, octave_matrix, octave_streamoff, sub_m_so);

  INSTALL_BINOP (op_add, octave_streamoff, octave_scalar, add_so_s);
  INSTALL_BINOP (op_sub, octave_streamoff, octave_scalar, sub_so_s);

  INSTALL_BINOP (op_add, octave_scalar, octave_streamoff, add_s_so);
  INSTALL_BINOP (op_sub, octave_scalar, octave_streamoff, sub_s_so);

  INSTALL_ASSIGNOP (op_asn_eq, octave_streamoff, octave_streamoff, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
