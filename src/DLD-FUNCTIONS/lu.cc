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

#include "CmplxLU.h"
#include "dbleLU.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (lu, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{l}, @var{u}, @var{p}] =} lu (@var{a})\n\
@cindex LU decomposition\n\
Compute the LU decomposition of @var{a}, using subroutines from\n\
@sc{Lapack}.  The result is returned in a permuted form, according to\n\
the optional return value @var{p}.  For example, given the matrix\n\
@code{a = [1, 2; 3, 4]},\n\
\n\
@example\n\
[l, u, p] = lu (a)\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
l =\n\
\n\
  1.00000  0.00000\n\
  0.33333  1.00000\n\
\n\
u =\n\
\n\
  3.00000  4.00000\n\
  0.00000  0.66667\n\
\n\
p =\n\
\n\
  0  1\n\
  1  0\n\
@end example\n\
\n\
The matrix is not required to be square..\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 3)
    {
      print_usage ("lu");
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("lu", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (3, Matrix ());

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  LU fact (m);

	  switch (nargout)
	    {
	    case 0:
	    case 1:
	    case 2:
	      {
		Matrix P = fact.P ();
		Matrix L = P.transpose () * fact.L ();
		retval(1) = fact.U ();
		retval(0) = L;
	      }
	      break;

	    case 3:
	    default:
	      retval(2) = fact.P ();
	      retval(1) = fact.U ();
	      retval(0) = fact.L ();
	      break;
	    }
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
	  ComplexLU fact (m);

	  switch (nargout)
	    {
	    case 0:
	    case 1:
	    case 2:
	      {
		Matrix P = fact.P ();
		ComplexMatrix L = P.transpose () * fact.L ();
		retval(1) = fact.U ();
		retval(0) = L;
	      }
	      break;

	    case 3:
	    default:
	      retval(2) = fact.P ();
	      retval(1) = fact.U ();
	      retval(0) = fact.L ();
	      break;
	    }
	}
    }
  else
    {
      gripe_wrong_type_arg ("lu", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
