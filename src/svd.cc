/*

Copyright (C) 1996 John W. Eaton

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

#include "CmplxSVD.h"
#include "dbleSVD.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "mappers.h"
#include "oct-obj.h"
#include "pr-output.h"
#include "utils.h"

DEFUN_DLD_BUILTIN (svd, args, nargout,
  "S = svd (X) or [U, S, V] = svd (X [, 0])\n\
\n\
Compute the singular value decomposition of X.  Given a second input\n\
argument, an `economy' sized factorization is computed that omits\n\
unnecessary rows and columns of U and V.\n\
\n\
X may not contain any Inf or NaN values.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout == 2 || nargout > 3)
    {
      print_usage ("svd");
      return retval;
    }

  octave_value arg = args(0);

  int arg_is_empty = empty_arg ("svd", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (3, Matrix ());

  SVD::type type = ((nargout == 0 || nargout == 1)
		    ? SVD::sigma_only
		    : (nargin == 2) ? SVD::economy : SVD::std);

  if (arg.is_real_type ())
    {
      Matrix tmp = arg.matrix_value ();

      if (! error_state)
	{
	  if (any_element_is_inf_or_nan (tmp))
	    {
	      error ("svd: cannot take SVD of matrix containing Inf or\
 NaN values"); 
	      return retval;
	    }

	  SVD result (tmp, type);

	  DiagMatrix sigma = result.singular_values ();

	  if (nargout == 0 || nargout == 1)
	    {
	      retval(0) = octave_value (sigma.diag (), 1);
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
	  if (any_element_is_inf_or_nan (ctmp))
	    {
	      error ("svd: cannot take SVD of matrix containing Inf or\
 NaN values"); 
	      return retval;
	    }

	  ComplexSVD result (ctmp, type);

	  DiagMatrix sigma = result.singular_values ();

	  if (nargout == 0 || nargout == 1)
	    {
	      retval(0) = octave_value (sigma.diag (), 1);
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
;;; End: ***
*/
