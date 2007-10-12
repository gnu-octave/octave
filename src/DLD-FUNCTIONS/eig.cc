/*

Copyright (C) 1996, 1997, 1999, 2000, 2003, 2004, 2005, 2006, 2007
              John W. Eaton

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

#include "EIG.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (eig, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{lambda} =} eig (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{v}, @var{lambda}] =} eig (@var{a})\n\
The eigenvalues (and eigenvectors) of a matrix are computed in a several\n\
step process which begins with a Hessenberg decomposition, followed by a\n\
Schur decomposition, from which the eigenvalues are apparent.  The\n\
eigenvectors, when desired, are computed by further manipulations of the\n\
Schur decomposition.\n\
\n\
The eigenvalues returned by @code{eig} are not ordered.\n\
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

  int arg_is_empty = empty_arg ("eig", nr, nc);
  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (2, Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("eig");
      return retval;
    }

  Matrix tmp;
  ComplexMatrix ctmp;
  EIG result;

  if (arg.is_real_type ())
    {
      tmp = arg.matrix_value ();

      if (error_state)
	return retval;
      else
	result = EIG (tmp, nargout > 1);
    }
  else if (arg.is_complex_type ())
    {
      ctmp = arg.complex_matrix_value ();

      if (error_state)
	return retval;
      else
	result = EIG (ctmp, nargout > 1);
    }
  else
    {
      gripe_wrong_type_arg ("eig", tmp);
      return retval;
    }

  if (nargout == 0 || nargout == 1)
    {
      retval(0) = result.eigenvalues ();
    }
  else
    {
      // Blame it on Matlab.

      ComplexDiagMatrix d (result.eigenvalues ());

      retval(1) = d;
      retval(0) = result.eigenvectors ();
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
