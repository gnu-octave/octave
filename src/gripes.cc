// gripes.cc                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#include "error.h"
#include "gripes.h"
#include "tree-const.h"

void
gripe_string_invalid (void)
{
  error ("string constant used in invalid context");
}

void
gripe_range_invalid (void)
{
  error ("range constant used in invalid context");
}

void
gripe_nonconformant (void)
{
  error ("nonconformant matrices");
}

void
gripe_nonconformant (int r1, int c1, int r2, int c2)
{
  error ("nonconformant matrices (op1 is %dx%d, op2 is %dx%d)",
	 r1, c1, r2, c2);
}

void
gripe_empty_arg (const char *name, int is_error)
{
  if (is_error)
    error ("%s: empty matrix is invalid as an argument", name);
  else
    warning ("%s: argument is empty matrix", name);
}

void
gripe_square_matrix_required (const char *name)
{
  error ("%s: argument must be a square matrix", name);
}

void
gripe_user_supplied_eval (const char *name)
{
  error ("%s: evaluation of user-supplied function failed", name);
}

void
gripe_user_returned_invalid (const char *name)
{
  error ("%s: user-supplied function returned invalid value", name);
}

void
gripe_invalid_conversion (const char *from, const char *to)
{
  error ("invalid conversion from %s to %s", from, to);
}

void
gripe_2_or_3_dim_plot (void)
{
  error ("plot: can only plot in 2 or 3 dimensions");
}

void
gripe_unrecognized_float_fmt (void)
{
  error ("unrecognized floating point format requested");
}

void
gripe_unrecognized_data_fmt (const char *warn_for)
{
  error ("%s: unrecognized data format requested", warn_for);
}

void
gripe_data_conversion (const char *from, const char *to)
{
  error ("unable to convert from %s to %s format", from, to);
}

void
gripe_wrong_type_arg (const char *name, const tree_constant& tc)
{
  error ("%s: wrong type argument `%s'", name, tc.type_as_string ());
}

void
gripe_wrong_type_arg_for_unary_op (const tree_constant& op)
{
  error ("invalid operand `%s' for unary operator", op.type_as_string ());
}

void
gripe_wrong_type_arg_for_binary_op (const tree_constant& op)
{
  error ("invalid operand `%s' for binary operator", op.type_as_string ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
