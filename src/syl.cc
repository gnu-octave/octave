// f-syl.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

// Written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD_BUILTIN ("syl", Fsyl, Ssyl, FSsyl, 11,
  "X = syl (A, B, C): solve the Sylvester equation A X + X B + C = 0")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 3 || nargout > 1)
    {
      print_usage ("syl");
      return retval;
    }

  tree_constant arg_a = args(0);
  tree_constant arg_b = args(1);
  tree_constant arg_c = args(2);

  int a_nr = arg_a.rows ();
  int a_nc = arg_a.columns ();

  int b_nr = arg_b.rows ();
  int b_nc = arg_b.columns ();

  int c_nr = arg_c.rows ();
  int c_nc = arg_c.columns ();

  int arg_a_is_empty = empty_arg ("syl", a_nr, a_nc);
  int arg_b_is_empty = empty_arg ("syl", b_nr, b_nc);
  int arg_c_is_empty = empty_arg ("syl", c_nr, c_nc);

  if (arg_a_is_empty > 0 && arg_b_is_empty > 0 && arg_c_is_empty > 0)
    return Matrix ();
  else if (arg_a_is_empty || arg_b_is_empty || arg_c_is_empty)
    return retval;

  // Arguments are not empty, so check for correct dimensions.

  if (a_nr != a_nc || b_nr != b_nc)
    {
      gripe_square_matrix_required ("syl: first two parameters:");
      return retval;
    }
  else if (a_nr != c_nr || b_nr != c_nc)
    {
      gripe_nonconformant ();
      return retval;
    }
  
  // Dimensions look o.k., let's solve the problem.

    if (arg_a.is_complex_type ()
	|| arg_b.is_complex_type ()
	|| arg_c.is_complex_type ())
      {
	// Do everything in complex arithmetic;

	ComplexMatrix ca = arg_a.complex_matrix_value ();

	if (error_state)
	  return retval;

	ComplexMatrix cb = arg_b.complex_matrix_value ();

	if (error_state)
	  return retval;

	ComplexMatrix cc = arg_c.complex_matrix_value ();

	if (error_state)
	  return retval;

	retval = Sylvester (ca, cb, cc);
      }
    else
      {
	// Do everything in real arithmetic.

	Matrix ca = arg_a.matrix_value ();

	if (error_state)
	  return retval;

	Matrix cb = arg_b.matrix_value ();

	if (error_state)
	  return retval;

	Matrix cc = arg_c.matrix_value ();

	if (error_state)
	  return retval;

	retval = Sylvester (ca, cb, cc);
      }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
