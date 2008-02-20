/*

Copyright (C) 1996, 1997, 1999, 2000, 2005, 2006, 2007 John W. Eaton

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

#include "CmplxQR.h"
#include "CmplxQRP.h"
#include "dbleQR.h"
#include "dbleQRP.h"
#include "SparseQR.h"
#include "SparseCmplxQR.h"


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
@deftypefnx {Loadable Function} {[@var{q}, @var{r}, @var{p}] =} qr (@var{a}, '0')\n\
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
If given a second argument of '0', @code{qr} returns an economy-sized\n\
QR factorization, omitting zero rows of @var{R} and the corresponding\n\
columns of @var{Q}.\n\
\n\
If the matrix @var{a} is full, the permuted QR factorization\n\
@code{[@var{q}, @var{r}, @var{p}] = qr (@var{a})} forms the QR factorization\n\
such that the diagonal entries of @code{r} are decreasing in magnitude\n\
order.  For example,given the matrix @code{a = [1, 2; 3, 4]},\n\
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
\n\
If the matrix @var{a} is sparse, then compute the sparse QR factorization\n\
of @var{a}, using @sc{CSparse}. As the matrix @var{Q} is in general a full\n\
matrix, this function returns the @var{Q}-less factorization @var{r} of\n\
@var{a}, such that @code{@var{r} = chol (@var{a}' * @var{a})}.\n\
\n\
If the final argument is the scalar @code{0} and the number of rows is\n\
larger than the number of columns, then an economy factorization is\n\
returned. That is @var{r} will have only @code{size (@var{a},1)} rows.\n\
\n\
If an additional matrix @var{b} is supplied, then @code{qr} returns\n\
@var{c}, where @code{@var{c} = @var{q}' * @var{b}}. This allows the\n\
least squares approximation of @code{@var{a} \\ @var{b}} to be calculated\n\
as\n\
\n\
@example\n\
[@var{c},@var{r}] = spqr (@var{a},@var{b})\n\
@var{x} = @var{r} \\ @var{c}\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > (args(0).is_sparse_type() ? 3 : 2) || nargout > 3)
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

  if (arg.is_sparse_type ())
    {
      bool economy = false;
      bool is_cmplx = false;
      int have_b = 0;

      if (arg.is_complex_type ())
	is_cmplx = true;
      if (nargin > 1)
	{
	  have_b = 1;
	  if (args(nargin-1).is_scalar_type ())
	    {
	      int val = args(nargin-1).int_value ();
	      if (val == 0)
		{
		  economy = true;
		  have_b = (nargin > 2 ? 2 : 0);
		}
	    }
	  if (have_b > 0 && args(have_b).is_complex_type ())
	    is_cmplx = true;
	}
	
      if (!error_state)
	{
	  if (have_b && nargout < 2)
	    error ("qr: incorrect number of output arguments");
	  else if (is_cmplx)
	    {
	      SparseComplexQR q (arg.sparse_complex_matrix_value ());
	      if (!error_state)
		{
		  if (have_b > 0)
		    {
		      retval(1) = q.R (economy);
		      retval(0) = q.C (args(have_b).complex_matrix_value ());
		      if (arg.rows() < arg.columns())
			warning ("qr: non minimum norm solution for under-determined problem");
		    }
		  else if (nargout > 1)
		    {
		      retval(1) = q.R (economy);
		      retval(0) = q.Q ();
		    }
		  else
		    retval(0) = q.R (economy);
		}
	    }
	  else
	    {
	      SparseQR q (arg.sparse_matrix_value ());
	      if (!error_state)
		{
		  if (have_b > 0)
		    {
		      retval(1) = q.R (economy);
		      retval(0) = q.C (args(have_b).matrix_value ());
		      if (args(0).rows() < args(0).columns())
			warning ("qr: non minimum norm solution for under-determined problem");
		    }
		  else if (nargout > 1)
		    {
		      retval(1) = q.R (economy);
		      retval(0) = q.Q ();
		    }
		  else
		    retval(0) = q.R (economy);
		}
	    }
	}
    }
  else
    {
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
    }

  return retval;
}

/*

The deactivated tests below can't be tested till rectangular back-subs is
implemented for sparse matrices.

%!testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = sprandn(n,n,d)+speye(n,n);
%! r = qr(a);
%! assert(r'*r,a'*a,1e-10)

%!testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = sprandn(n,n,d)+speye(n,n);
%! q = symamd(a);
%! a = a(q,q);
%! r = qr(a);
%! assert(r'*r,a'*a,1e-10)

%!testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = sprandn(n,n,d)+speye(n,n);
%! [c,r] = qr(a,ones(n,1));
%! assert (r\c,full(a)\ones(n,1),10e-10)

%!testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = sprandn(n,n,d)+speye(n,n);
%! b = randn(n,2);
%! [c,r] = qr(a,b);
%! assert (r\c,full(a)\b,10e-10)

%% Test under-determined systems!!
%!#testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = sprandn(n,n+1,d)+speye(n,n+1);
%! b = randn(n,2);
%! [c,r] = qr(a,b);
%! assert (r\c,full(a)\b,10e-10)

%!testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n,d)+speye(n,n);
%! r = qr(a);
%! assert(r'*r,a'*a,1e-10)

%!testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n,d)+speye(n,n);
%! q = symamd(a);
%! a = a(q,q);
%! r = qr(a);
%! assert(r'*r,a'*a,1e-10)

%!testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n,d)+speye(n,n);
%! [c,r] = qr(a,ones(n,1));
%! assert (r\c,full(a)\ones(n,1),10e-10)

%!testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n,d)+speye(n,n);
%! b = randn(n,2);
%! [c,r] = qr(a,b);
%! assert (r\c,full(a)\b,10e-10)

%% Test under-determined systems!!
%!#testif HAVE_CXSPARSE
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n+1,d)+speye(n,n+1);
%! b = randn(n,2);
%! [c,r] = qr(a,b);
%! assert (r\c,full(a)\b,10e-10)

%!error qr(sprandn(10,10,0.2),ones(10,1));

*/


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
