/*

Copyright (C) 1997 John W. Eaton

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

#include "lo-specfun.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

enum bessel_type
{
  BESSEL_J,
  BESSEL_Y,
  BESSEL_I,
  BESSEL_K,
  BESSEL_H1,
  BESSEL_H2
};

#define DO_BESSEL(type, alpha, x, scaled, ierr, result) \
  do \
    { \
      switch (type) \
	{ \
	  case BESSEL_J: \
	    result = besselj (alpha, x, scaled, ierr); \
	    break; \
 \
	  case BESSEL_Y: \
	    result = bessely (alpha, x, scaled, ierr); \
	    break; \
 \
	  case BESSEL_I: \
	    result = besseli (alpha, x, scaled, ierr); \
	    break; \
 \
	  case BESSEL_K: \
	    result = besselk (alpha, x, scaled, ierr); \
	    break; \
 \
	  case BESSEL_H1: \
	    result = besselh1 (alpha, x, scaled, ierr); \
	    break; \
 \
	  case BESSEL_H2: \
	    result = besselh2 (alpha, x, scaled, ierr); \
	    break; \
 \
	  default: \
	    break; \
        } \
    } \
  while (0)

static inline Matrix
int_array2_to_matrix (const Array2<int>& a)
{
  int nr = a.rows ();
  int nc = a.cols ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = (double) (a(i,j));

  return retval;
}

static void
gripe_bessel_arg (const char *fn, const char *arg)
{
  error ("%s: expecting scalar or matrix as %s argument", fn, arg);
}

octave_value_list
do_bessel (enum bessel_type type, const char *fn,
	   const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      bool scaled = (nargin == 3);

      octave_value alpha_arg = args(0);
      octave_value x_arg = args(1);

      if (alpha_arg.is_scalar_type ())
	{
	  double alpha = args(0).double_value ();

	  if (! error_state)
	    {
	      if (x_arg.is_scalar_type ())
		{
		  Complex x = x_arg.complex_value ();

		  if (! error_state)
		    {
		      int ierr;
		      octave_value result;

		      DO_BESSEL (type, alpha, x, scaled, ierr, result);

		      if (nargout > 1)
			retval(1) = (double) ierr;

		      retval(0) = result;
		    }
		  else
		    gripe_bessel_arg (fn, "second");
		}
	      else
		{
		  ComplexMatrix x = x_arg.complex_matrix_value ();

		  if (! error_state)
		    {
		      Array2<int> ierr;
		      octave_value result;

		      DO_BESSEL (type, alpha, x, scaled, ierr, result);

		      if (nargout > 1)
			retval(1) = int_array2_to_matrix (ierr);

		      retval(0) = result;
		    }
		  else
		    gripe_bessel_arg (fn, "second");
		}
	    }
	  else
	    gripe_bessel_arg (fn, "first");
	}
      else
	{
	  Matrix alpha = args(0).matrix_value ();

	  if (! error_state)
	    {
	      if (x_arg.is_scalar_type ())
		{
		  Complex x = x_arg.complex_value ();

		  if (! error_state)
		    {
		      Array2<int> ierr;
		      octave_value result;

		      DO_BESSEL (type, alpha, x, scaled, ierr, result);

		      if (nargout > 1)
			retval(1) = int_array2_to_matrix (ierr);

		      retval(0) = result;
		    }
		  else
		    gripe_bessel_arg (fn, "second");
		}
	      else
		{
		  ComplexMatrix x = x_arg.complex_matrix_value ();

		  if (! error_state)
		    {
		      if (alpha.rows () == 1 && x.columns () == 1)
			{
			  RowVector ralpha = alpha.row (0);
			  ComplexColumnVector cx = x.column (0);

			  Array2<int> ierr;
			  octave_value result;

			  DO_BESSEL (type, ralpha, cx, scaled, ierr, result);

			  if (nargout > 1)
			    retval(1) = int_array2_to_matrix (ierr);

			  retval(0) = result;
			}
		      else
			{
			  Array2<int> ierr;
			  octave_value result;

			  DO_BESSEL (type, alpha, x, scaled, ierr, result);

			  if (nargout > 1)
			    retval(1) = int_array2_to_matrix (ierr);

			  retval(0) = result;
			}
		    }
		  else
		    gripe_bessel_arg (fn, "second");
		}
	    }
	  else
	    gripe_bessel_arg (fn, "first");
	}
    }
  else
    print_usage (fn);

  return retval;
}

DEFUN_DLD (besselj, args, nargout,
  "[J, IERR] = BESSELJ (ALPHA, X [, 1])\n\
\n\
Compute Bessel functions of the first kind.\n\
\n\
If a third argument is supplied, scale the result by exp(-I*Z) for\n\
K = 1 or exp(I*Z) for K = 2.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If X is a\n\
scalar, the result is the same size as ALPHA.  If ALPHA is a row\n\
vector and X is a column vector, the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.  Otherwise, ALPHA and X must\n\
conform and the result will be the same size.\n\
\n\
ALPHA must be real.  X may be complex.\n\
\n\
If requested, IERR contains the following status information and is\n\
the same size as the result.\n\
\n
  0  normal return\n\
  1  input error, return NaN\n\
  2  overflow, return Inf\n\
  3  loss of significance by argument reduction results in less than\n\
     half of machine accuracy\n\
  4  complete loss of significance by argument reduction, return NaN\n\
  5  error -- no computation, algorithm termination condition not met,\n\
     return NaN")
{
  return do_bessel (BESSEL_J, "besselj", args, nargout);
}

DEFUN_DLD (bessely, args, nargout,
  "[Y, IERR] = BESSELY (ALPHA, X [, 1])\n\
\n\
Compute Bessel functions of the second kind.\n\
\n\
If a third argument is supplied, scale the result by exp(-I*Z) for\n\
K = 1 or exp(I*Z) for K = 2.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If X is a\n\
scalar, the result is the same size as ALPHA.  If ALPHA is a row\n\
vector and X is a column vector, the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.  Otherwise, ALPHA and X must\n\
conform and the result will be the same size.\n\
\n\
ALPHA must be real.  X may be complex.\n\
\n\
If requested, IERR contains the following status information and is\n\
the same size as the result.\n\
\n
  0  normal return\n\
  1  input error, return NaN\n\
  2  overflow, return Inf\n\
  3  loss of significance by argument reduction results in less than\n\
     half of machine accuracy\n\
  4  complete loss of significance by argument reduction, return NaN\n\
  5  error -- no computation, algorithm termination condition not met,\n\
     return NaN")
{
  return do_bessel (BESSEL_Y, "bessely", args, nargout);
}

DEFUN_DLD (besseli, args, nargout,
  "[I, IERR] = BESSELI (ALPHA, X [, 1])\n\
\n\
Compute modified Bessel functions of the first kind.\n\
\n\
If a third argument is supplied, scale the result by exp(-I*Z) for\n\
K = 1 or exp(I*Z) for K = 2.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If X is a\n\
scalar, the result is the same size as ALPHA.  If ALPHA is a row\n\
vector and X is a column vector, the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.  Otherwise, ALPHA and X must\n\
conform and the result will be the same size.\n\
\n\
ALPHA must be real.  X may be complex.\n\
\n\
If requested, IERR contains the following status information and is\n\
the same size as the result.\n\
\n
  0  normal return\n\
  1  input error, return NaN\n\
  2  overflow, return Inf\n\
  3  loss of significance by argument reduction results in less than\n\
     half of machine accuracy\n\
  4  complete loss of significance by argument reduction, return NaN\n\
  5  error -- no computation, algorithm termination condition not met,\n\
     return NaN")
{
  return do_bessel (BESSEL_I, "besseli", args, nargout);
}

DEFUN_DLD (besselk, args, nargout,
  "[K, IERR] = BESSELK (ALPHA, X [, 1])\n\
\n\
Compute modified Bessel functions of the second kind.\n\
\n\
If a third argument is supplied, scale the result by exp(-I*Z) for\n\
K = 1 or exp(I*Z) for K = 2.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If X is a\n\
scalar, the result is the same size as ALPHA.  If ALPHA is a row\n\
vector and X is a column vector, the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.  Otherwise, ALPHA and X must\n\
conform and the result will be the same size.\n\
\n\
ALPHA must be real.  X may be complex.\n\
\n\
If requested, IERR contains the following status information and is\n\
the same size as the result.\n\
\n
  0  normal return\n\
  1  input error, return NaN\n\
  2  overflow, return Inf\n\
  3  loss of significance by argument reduction results in less than\n\
     half of machine accuracy\n\
  4  complete loss of significance by argument reduction, return NaN\n\
  5  error -- no computation, algorithm termination condition not met,\n\
     return NaN")
{
  return do_bessel (BESSEL_K, "besselk", args, nargout);
}

DEFUN_DLD (besselh, args, nargout,
  "[H, IERR] = besselh (ALPHA, K, X [, 1])\n\
\n\
Compute Hankel functions of the first (K = 1) or second (K = 2) kind.\n\
\n\
If a fourth argument is supplied, scale the result by exp(-I*Z) for\n\
K = 1 or exp(I*Z) for K = 2.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If X is a\n\
scalar, the result is the same size as ALPHA.  If ALPHA is a row\n\
vector and X is a column vector, the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.  Otherwise, ALPHA and X must\n\
conform and the result will be the same size.\n\
\n\
ALPHA must be real.  X may be complex.\n\
\n\
If requested, IERR contains the following status information and is\n\
the same size as the result.\n\
\n
  0  normal return\n\
  1  input error, return NaN\n\
  2  overflow, return Inf\n\
  3  loss of significance by argument reduction results in less than\n\
     half of machine accuracy\n\
  4  complete loss of significance by argument reduction, return NaN\n\
  5  error -- no computation, algorithm termination condition not met,\n\
     return NaN")
{
  octave_value_list retval;

  int nargin = args.length ();

  int kind = 1;

  if (nargin == 2)
    {
      retval = do_bessel (BESSEL_H1, "besselh", args, nargout);
    }
  else if (nargin == 3 || nargin == 4)
    {
      double d_kind = args(1).double_value ();

      if (! error_state && D_NINT (d_kind) == d_kind)
	{
	  octave_value_list tmp_args;

	  if (nargin == 4)
	    tmp_args(2) = args(3);

	  tmp_args(1) = args(2);
	  tmp_args(0) = args(0);

	  if (kind == 1)
	    retval = do_bessel (BESSEL_H1, "besselh", tmp_args, nargout);
	  else if (kind == 2)
	    retval = do_bessel (BESSEL_H2, "besselh", tmp_args, nargout);
	  else
	    error ("besselh: expecting K = 1 or 2");
	}
      else
	error ("besselh: invalid value of K");
    }
  else
    print_usage ("besselh");

  return retval;
}

DEFUN_DLD (airy, args, nargout,
  "[A, IERR] = airy (K, Z, [, 1])\n\
\n\
Compute Airy functions of the first and second kind, and their\n\
derivatives.\n\
\n\
  K   Function   Scale factor (if a third argument is supplied)\n\
 ---  --------   ----------------------------------------------\n\
  0   Ai (Z)     exp ((2/3) * Z * sqrt (Z))\n\
  1   dAi(Z)/dZ  exp ((2/3) * Z * sqrt (Z))\n\
  2   Bi (Z)     exp (-abs (real ((2/3) * Z *sqrt (Z))))\n\
  3   dBi(Z)/dZ  exp (-abs (real ((2/3) * Z *sqrt (Z))))\n\
\n\
The function call airy (Z) is equivalent to airy (0, Z).\n\
\n\
The result is the same size as Z.
\n\
If requested, IERR contains the following status information and is\n\
the same size as the result.\n\
\n
  0  normal return\n\
  1  input error, return NaN\n\
  2  overflow, return Inf\n\
  3  loss of significance by argument reduction results in less than\n\
     half of machine accuracy\n\
  4  complete loss of significance by argument reduction, return NaN\n\
  5  error -- no computation, algorithm termination condition not met,\n\
     return NaN")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 4)
    {
      bool scale = (nargin == 3);

      int kind = 0;

      ComplexMatrix z;

      if (nargin > 1)
	{
	  double d_kind = args(0).double_value ();

	  if (! error_state)
	    {
	      kind = (int) d_kind;

	      if (kind < 0 || kind > 3)
		error ("airy: expecting K = 0, 1, 2, or 3");
	    }	      
	  else
	    error ("airy: expecting integer value for K");
	}

      if (! error_state)
	{
	  z = args(nargin == 1 ? 0 : 1).complex_matrix_value ();

	  if (! error_state)
	    {
	      Array2<int> ierr;
	      octave_value result;

	      if (kind > 1)
		result = biry (z, kind == 3, scale, ierr);
	      else
		result = airy (z, kind == 1, scale, ierr);

	      if (nargout > 1)
		retval(1) = int_array2_to_matrix (ierr);

	      retval(0) = result;
	    }
	  else
	    error ("airy: expecting complex matrix for Z");
	}
    }
  else
    print_usage ("airy");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

