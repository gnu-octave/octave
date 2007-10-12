/*

Copyright (C) 1996, 1997, 1999, 2000, 2002, 2005, 2006, 2007
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

// Author: A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (expm, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} expm (@var{a})\n\
Return the exponential of a matrix, defined as the infinite Taylor\n\
series\n\
@iftex\n\
@tex\n\
$$\n\
 \\exp (A) = I + A + {A^2 \\over 2!} + {A^3 \\over 3!} + \\cdots\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
expm(a) = I + a + a^2/2! + a^3/3! + ...\n\
@end example\n\
\n\
@end ifinfo\n\
The Taylor series is @emph{not} the way to compute the matrix\n\
exponential; see Moler and Van Loan, @cite{Nineteen Dubious Ways to\n\
Compute the Exponential of a Matrix}, SIAM Review, 1978.  This routine\n\
uses Ward's diagonal\n\
@iftex\n\
@tex\n\
Pad\\'e\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
Pade'\n\
@end ifinfo\n\
approximation method with three step preconditioning (SIAM Journal on\n\
Numerical Analysis, 1977).  Diagonal\n\
@iftex\n\
@tex\n\
Pad\\'e\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
Pade'\n\
@end ifinfo\n\
 approximations are rational polynomials of matrices\n\
@iftex\n\
@tex\n\
$D_q(a)^{-1}N_q(a)$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
     -1\n\
D (a)   N (a)\n\
@end example\n\
\n\
@end ifinfo\n\
 whose Taylor series matches the first\n\
@iftex\n\
@tex\n\
$2 q + 1 $\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@code{2q+1}\n\
@end ifinfo\n\
terms of the Taylor series above; direct evaluation of the Taylor series\n\
(with the same preconditioning steps) may be desirable in lieu of the\n\
@iftex\n\
@tex\n\
Pad\\'e\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
Pade'\n\
@end ifinfo\n\
approximation when\n\
@iftex\n\
@tex\n\
$D_q(a)$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@code{Dq(a)}\n\
@end ifinfo\n\
is ill-conditioned.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("expm", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return octave_value (Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("expm");
      return retval;
    }

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (error_state)
	return retval;
      else
	retval = m.expm ();
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (error_state)
	return retval;
      else
	retval = m.expm ();
    }
  else
    {
      gripe_wrong_type_arg ("expm", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
