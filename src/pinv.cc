// f-pinv.cc                                           -*- C++ -*-
/*

Copyright (C) 1994, 1995 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "dMatrix.h"
#include "CMatrix.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD_BUILTIN ("pinv", Fpinv, Spinv, 3, 1,
  "pinv ( [, tol])\n\
Returns the pseudoinverse of X; singular values less than tol are ignored.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ("pinv");
      return retval;
    }

  tree_constant arg = args(0);

  double tol = 0.0;
  if (nargin == 2)
    tol = args(1).double_value ();

  if (error_state)
    return retval;

  if (tol < 0.0)
    {
      error ("pinv: tol must be greater than zero");
      return retval;
    }

  int arg_is_empty = empty_arg ("pinv", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return Matrix ();

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	retval = m.pseudo_inverse (tol);
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	retval = m.pseudo_inverse (tol);
    }
  else
    {
      gripe_wrong_type_arg ("pinv", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
