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

#include "CmplxCHOL.h"
#include "dbleCHOL.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (chol, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} chol (@var{a})\n\
@cindex Cholesky factorization\n\
Compute the Cholesky factor, @var{r}, of the symmetric positive definite\n\
matrix @var{a}, where\n\
@iftex\n\
@tex\n\
$ R^T R = A $.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
r' * r = a.\n\
@end example\n\
@end ifinfo\n\
@seealso{cholinv, chol2inv}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);
    
  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("chol", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return octave_value (Matrix ());

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  octave_idx_type info;
	  CHOL fact (m, info);
	  if (nargout == 2 || info == 0)
	    {
	      retval(1) = static_cast<double> (info);
	      retval(0) = fact.chol_matrix ();
	    }
	  else
	    error ("chol: matrix not positive definite");
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
	  octave_idx_type info;
	  ComplexCHOL fact (m, info);
	  if (nargout == 2 || info == 0)
	    {
	      retval(1) = static_cast<double> (info);
	      retval(0) = fact.chol_matrix ();
	    }
	  else
	    error ("chol: matrix not positive definite");
	}
    }
  else
    {
      gripe_wrong_type_arg ("chol", arg);
    }

  return retval;
}

DEFUN_DLD (cholinv, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} cholinv (@var{a})\n\
Use the Cholesky factorization to compute the inverse of the\n\
symmetric positive definite matrix @var{a}.\n\
@seealso{chol, chol2inv}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);
    
      octave_idx_type nr = arg.rows ();
      octave_idx_type nc = arg.columns ();

      if (nr == 0 || nc == 0)
	retval = Matrix ();
      else
	{
	  if (arg.is_real_type ())
	    {
	      Matrix m = arg.matrix_value ();

	      if (! error_state)
		{
		  octave_idx_type info;
		  CHOL chol (m, info);
		  if (info == 0)
		    retval = chol.inverse ();
		  else
		    error ("cholinv: matrix not positive definite");
		}
	    }
	  else if (arg.is_complex_type ())
	    {
	      ComplexMatrix m = arg.complex_matrix_value ();

	      if (! error_state)
		{
		  octave_idx_type info;
		  ComplexCHOL chol (m, info);
		  if (info == 0)
		    retval = chol.inverse ();
		  else
		    error ("cholinv: matrix not positive definite");
		}
	    }
	  else
	    gripe_wrong_type_arg ("chol", arg);
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (chol2inv, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} chol2inv (@var{u})\n\
Invert a symmetric, positive definite square matrix from its Cholesky\n\
decomposition, @var{u}.  Note that @var{u} should be an upper-triangular\n\
matrix with positive diagonal elements.  @code{chol2inv (@var{u})}\n\
provides @code{inv (@var{u}'*@var{u})} but it is much faster than\n\
using @code{inv}.\n\
@seealso{chol, cholinv}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);
    
      octave_idx_type nr = arg.rows ();
      octave_idx_type nc = arg.columns ();

      if (nr == 0 || nc == 0)
	retval = Matrix ();
      else
	{
	  if (arg.is_real_type ())
	    {
	      Matrix r = arg.matrix_value ();

	      if (! error_state)
		retval = chol2inv (r);
	    }
	  else if (arg.is_complex_type ())
	    {
	      ComplexMatrix r = arg.complex_matrix_value ();

	      if (! error_state)
		retval = chol2inv (r);
	    }
	  else
	    gripe_wrong_type_arg ("chol2inv", arg);
	}
    }
  else
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

