// f-svd.cc                                           -*- C++ -*-
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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "dbleSVD.h"
#include "CmplxSVD.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD_BUILTIN ("svd", Fsvd, Ssvd, 2, 3,
  "S = svd (X) or [U, S, V] = svd (X [, 0])\n\
\n\
Compute the singular value decomposition of X.  Given a second input\n\
argument, an `economy' sized factorization is computed that omits\n\
unnecessary rows and columns of U and V")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout == 2 || nargout > 3)
    {
      print_usage ("svd");
      return retval;
    }

  tree_constant arg = args(0);

  int arg_is_empty = empty_arg ("svd", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return Octave_object (3, Matrix ());

  SVD::type type = (nargin == 2) ? SVD::economy : SVD::std;

  if (arg.is_real_type ())
    {
      Matrix tmp = arg.matrix_value ();

      if (! error_state)
	{
	  SVD result (tmp, type);

	  DiagMatrix sigma = result.singular_values ();

	  if (nargout == 0 || nargout == 1)
	    {
	      retval(0) = tree_constant (sigma.diag (), 1);
	    }
	  else
	    {
	      retval(2) = result.right_singular_matrix ();
	      retval(1) = sigma;
	      retval(0) = result.left_singular_matrix ();
	    }
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix ctmp = arg.complex_matrix_value ();

      if (! error_state)
	{
	  ComplexSVD result (ctmp, type);

	  DiagMatrix sigma = result.singular_values ();

	  if (nargout == 0 || nargout == 1)
	    {
	      retval(0) = tree_constant (sigma.diag (), 1);
	    }
	  else
	    {
	      retval(2) = result.right_singular_matrix ();
	      retval(1) = sigma;
	      retval(0) = result.left_singular_matrix ();
	    }
	}
    }
  else
    {
      gripe_wrong_type_arg ("svd", arg);
      return retval;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
