// f-svd.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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
#include "defun-dld.h"

DEFUN_DLD ("svd", Fsvd, Ssvd, 2, 3,
  "S = svd (X) or [U, S, V] = svd (X [, 0])\n\
\n\
Compute the singular value decomposition of X.  Given a second input\n\
argument, an `economy' sized factorization is computed that omits\n\
unnecessary rows and columns of U and V")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 2 || nargin > 3 || nargout == 2 || nargout > 3)
    {
      print_usage ("svd");
      return retval;
    }

  tree_constant arg = args(1).make_numeric ();

  if (arg.rows () == 0 || arg.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("svd", 0);

	  Matrix m;
	  retval.resize (3);
	  retval(0) = m;
	  retval(1) = m;
	  retval(2) = m;
	}
      else
	gripe_empty_arg ("svd", 1);

      return retval;
    }

  Matrix tmp;
  ComplexMatrix ctmp;
  switch (arg.const_type ())
    {
    case tree_constant_rep::scalar_constant:
      tmp.resize (1, 1);
      tmp.elem (0, 0) = arg.double_value ();
      break;
    case tree_constant_rep::matrix_constant:
      tmp = arg.matrix_value ();
      break;
    case tree_constant_rep::complex_scalar_constant:
      ctmp.resize (1, 1);
      ctmp.elem (0, 0) = arg.complex_value ();
      break;
    case tree_constant_rep::complex_matrix_constant:
      ctmp = arg.complex_matrix_value ();
      break;
    default:
      panic_impossible ();
      break;
    }

  SVD::type type = (nargin == 3) ? SVD::economy : SVD::std;

  switch (arg.const_type ())
    {
    case tree_constant_rep::scalar_constant:
    case tree_constant_rep::matrix_constant:
      {
	SVD result (tmp, type);

	DiagMatrix sigma = result.singular_values ();

	if (nargout == 0 || nargout == 1)
	  {
	    retval.resize (1);
	    retval(0) = tree_constant (sigma.diag (), 1);
	  }
	else
	  {
	    retval.resize (3);
	    retval(0) = result.left_singular_matrix ();
	    retval(1) = sigma;
	    retval(2) = result.right_singular_matrix ();
	  }
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexSVD result (ctmp, type);

	DiagMatrix sigma = result.singular_values ();

	if (nargout == 0 || nargout == 1)
	  {
	    retval.resize (1);
	    retval(0) = tree_constant (sigma.diag (), 1);
	  }
	else
	  {
	    retval.resize (3);
	    retval(0) = result.left_singular_matrix ();
	    retval(1) = sigma;
	    retval(2) = result.right_singular_matrix ();
	  }
      }
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
