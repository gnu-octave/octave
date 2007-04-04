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

#include "CmplxQR.h"
#include "CmplxQRP.h"
#include "dbleQR.h"
#include "dbleQRP.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

// [Q, R] = qr (X):      form Q unitary and R upper triangular such
//                        that Q * R = X
//
// [Q, R] = qr (X, 0):    form the economy decomposition such that if X is
//                        m by n then only the first n columns of Q are
//                        computed.
//
// [Q, R, P] = qr (X):    form QRP factorization of X where
//                        P is a permutation matrix such that
//                        A * P = Q * R
//
// [Q, R, P] = qr (X, 0): form the economy decomposition with
//                        permutation vector P such that Q * R = X (:, P)
//
// qr (X) alone returns the output of the LAPACK routine dgeqrf, such
// that R = triu (qr (X))

DEFUN_DLD (qr, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{q}, @var{r}, @var{p}] =} qr (@var{a})\n\
@cindex QR factorization\n\
Compute the QR factorization of @var{a}, using standard @sc{Lapack}\n\
subroutines.  For example, given the matrix @code{a = [1, 2; 3, 4]},\n\
\n\
@example\n\
[q, r] = qr (a)\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
q =\n\
\n\
  -0.31623  -0.94868\n\
  -0.94868   0.31623\n\
\n\
r =\n\
\n\
  -3.16228  -4.42719\n\
   0.00000  -0.63246\n\
@end example\n\
\n\
The @code{qr} factorization has applications in the solution of least\n\
squares problems\n\
@iftex\n\
@tex\n\
$$\n\
\\min_x \\left\\Vert A x - b \\right\\Vert_2\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
@code{min norm(A x - b)}\n\
@end example\n\
\n\
@end ifinfo\n\
for overdetermined systems of equations (i.e.,\n\
@iftex\n\
@tex\n\
$A$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@code{a}\n\
@end ifinfo\n\
 is a tall, thin matrix).  The QR factorization is\n\
@iftex\n\
@tex\n\
$QR = A$ where $Q$ is an orthogonal matrix and $R$ is upper triangular.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@code{q * r = a} where @code{q} is an orthogonal matrix and @code{r} is\n\
upper triangular.\n\
@end ifinfo\n\
\n\
The permuted QR factorization @code{[@var{q}, @var{r}, @var{p}] =\n\
qr (@var{a})} forms the QR factorization such that the diagonal\n\
entries of @code{r} are decreasing in magnitude order.  For example,\n\
given the matrix @code{a = [1, 2; 3, 4]},\n\
\n\
@example\n\
[q, r, p] = qr(a)\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
q = \n\
\n\
  -0.44721  -0.89443\n\
  -0.89443   0.44721\n\
\n\
r =\n\
\n\
  -4.47214  -3.13050\n\
   0.00000   0.44721\n\
\n\
p =\n\
\n\
   0  1\n\
   1  0\n\
@end example\n\
\n\
The permuted @code{qr} factorization @code{[q, r, p] = qr (a)}\n\
factorization allows the construction of an orthogonal basis of\n\
@code{span (a)}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 3)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  int arg_is_empty = empty_arg ("qr", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (3, Matrix ());

  QR::type type = (nargout == 0 || nargout == 1) ? QR::raw
    : (nargin == 2 ? QR::economy : QR::std);

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  switch (nargout)
	    {
	    case 0:
	    case 1:
	      {
		QR fact (m, type);
		retval(0) = fact.R ();
	      }
	      break;

	    case 2:
	      {
		QR fact (m, type);
		retval(1) = fact.R ();
		retval(0) = fact.Q ();
	      }
	      break;

	    default:
	      {
		QRP fact (m, type);
		retval(2) = fact.P ();
		retval(1) = fact.R ();
		retval(0) = fact.Q ();
	      }
	      break;
	    }
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
	  switch (nargout)
	    {
	    case 0:
	    case 1:
	      {
		ComplexQR fact (m, type);
		retval(0) = fact.R ();
	      }
	      break;

	    case 2:
	      {
		ComplexQR fact (m, type);
		retval(1) = fact.R ();
		retval(0) = fact.Q ();
	      }
	      break;

	    default:
	      {
		ComplexQRP fact (m, type);
		retval(2) = fact.P ();
		retval(1) = fact.R ();
		retval(0) = fact.Q ();
	      }
	      break;
	    }
	}
    }
  else
    {
      gripe_wrong_type_arg ("qr", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
