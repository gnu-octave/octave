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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "CmplxSVD.h"
#include "dbleSVD.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "pr-output.h"
#include "utils.h"

DEFUN_DLD (svd, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{s} =} svd (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{u}, @var{s}, @var{v}] =} svd (@var{a})\n\
@cindex singular value decomposition\n\
Compute the singular value decomposition of @var{a}\n\
@iftex\n\
@tex\n\
$$\n\
 A = U\\Sigma V^H\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
a = u * sigma * v'\n\
@end example\n\
@end ifinfo\n\
\n\
The function @code{svd} normally returns the vector of singular values.\n\
If asked for three return values, it computes\n\
@iftex\n\
@tex\n\
$U$, $S$, and $V$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
U, S, and V.\n\
@end ifinfo\n\
For example,\n\
\n\
@example\n\
svd (hilb (3))\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
ans =\n\
\n\
  1.4083189\n\
  0.1223271\n\
  0.0026873\n\
@end example\n\
\n\
@noindent\n\
and\n\
\n\
@example\n\
[u, s, v] = svd (hilb (3))\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
u =\n\
\n\
  -0.82704   0.54745   0.12766\n\
  -0.45986  -0.52829  -0.71375\n\
  -0.32330  -0.64901   0.68867\n\
\n\
s =\n\
\n\
  1.40832  0.00000  0.00000\n\
  0.00000  0.12233  0.00000\n\
  0.00000  0.00000  0.00269\n\
\n\
v =\n\
\n\
  -0.82704   0.54745   0.12766\n\
  -0.45986  -0.52829  -0.71375\n\
  -0.32330  -0.64901   0.68867\n\
@end example\n\
\n\
If given a second argument, @code{svd} returns an economy-sized\n\
decomposition, eliminating the unnecessary rows or columns of @var{u} or\n\
@var{v}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout == 2 || nargout > 3)
    {
      print_usage ("svd");
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (nr == 0 || nc == 0)
    {
      if (nargout == 3)
	{
	  retval(3) = identity_matrix (nr, nr);
	  retval(2) = Matrix (nr, nc);
	  retval(1) = identity_matrix (nc, nc);
	}
      else
	retval(0) = Matrix (0, 1);
    }
  else
    {
      SVD::type type = ((nargout == 0 || nargout == 1)
			? SVD::sigma_only
			: (nargin == 2) ? SVD::economy : SVD::std);

      if (arg.is_real_type ())
	{
	  Matrix tmp = arg.matrix_value ();

	  if (! error_state)
	    {
	      if (tmp.any_element_is_inf_or_nan ())
		{
		  error ("svd: cannot take SVD of matrix containing Inf or NaN values"); 
		  return retval;
		}

	      SVD result (tmp, type);

	      DiagMatrix sigma = result.singular_values ();

	      if (nargout == 0 || nargout == 1)
		{
		  retval(0) = sigma.diag ();
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
	      if (ctmp.any_element_is_inf_or_nan ())
		{
		  error ("svd: cannot take SVD of matrix containing Inf or NaN values"); 
		  return retval;
		}

	      ComplexSVD result (ctmp, type);

	      DiagMatrix sigma = result.singular_values ();

	      if (nargout == 0 || nargout == 1)
		{
		  retval(0) = sigma.diag ();
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
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
