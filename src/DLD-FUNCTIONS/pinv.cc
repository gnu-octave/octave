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

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (pinv, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} pinv (@var{x}, @var{tol})\n\
Return the pseudoinverse of @var{x}.  Singular values less than\n\
@var{tol} are ignored. \n\
\n\
If the second argument is omitted, it is assumed that\n\
\n\
@example\n\
tol = max (size (@var{x})) * sigma_max (@var{x}) * eps,\n\
@end example\n\
\n\
@noindent\n\
where @code{sigma_max (@var{x})} is the maximal singular value of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

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
    return octave_value (Matrix ());

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
;;; End: ***
*/
