/*

Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006,
              2007 John W. Eaton

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

#include "lo-specfun.h"
#include "quit.h"

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
int_array2_to_matrix (const Array2<octave_idx_type>& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  Matrix retval (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;

	retval(i,j) = static_cast<double> (a(i,j));
      }

  return retval;
}

static inline NDArray
int_arrayN_to_array (const ArrayN<octave_idx_type>& a)
{
  dim_vector dv = a.dims ();
  int nel = dv.numel ();

  NDArray retval (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      
      retval(i) = static_cast<double> (a(i));
    }

  return retval;
}

static inline FloatMatrix
int_array2_to_float_matrix (const Array2<octave_idx_type>& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  FloatMatrix retval (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;

	retval(i,j) = static_cast<float> (a(i,j));
      }

  return retval;
}

static inline FloatNDArray
int_arrayN_to_float_array (const ArrayN<octave_idx_type>& a)
{
  dim_vector dv = a.dims ();
  int nel = dv.numel ();

  FloatNDArray retval (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      
      retval(i) = static_cast<float> (a(i));
    }

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

      if (alpha_arg.is_single_type () || x_arg.is_single_type ())
	{
	  if (alpha_arg.is_scalar_type ())
	    {
	      float alpha = args(0).float_value ();

	      if (! error_state)
		{
		  if (x_arg.is_scalar_type ())
		    {
		      FloatComplex x = x_arg.float_complex_value ();

		      if (! error_state)
			{
			  octave_idx_type ierr;
			  octave_value result;

			  DO_BESSEL (type, alpha, x, scaled, ierr, result);

			  if (nargout > 1)
			    retval(1) = static_cast<float> (ierr);

			  retval(0) = result;
			}
		      else
			gripe_bessel_arg (fn, "second");
		    }
		  else
		    {
		      FloatComplexNDArray x = x_arg.float_complex_array_value ();

		      if (! error_state)
			{
			  ArrayN<octave_idx_type> ierr;
			  octave_value result;

			  DO_BESSEL (type, alpha, x, scaled, ierr, result);

			  if (nargout > 1)
			    retval(1) = int_arrayN_to_float_array (ierr);

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
	      dim_vector dv0 = args(0).dims ();
	      dim_vector dv1 = args(1).dims ();

	      bool args0_is_row_vector = (dv0 (1) == dv0.numel ());
	      bool args1_is_col_vector = (dv1 (0) == dv1.numel ());

	      if (args0_is_row_vector && args1_is_col_vector)
		{
		  FloatRowVector ralpha = args(0).float_row_vector_value ();

		  if (! error_state)
		    {
		      FloatComplexColumnVector cx = 
			x_arg.float_complex_column_vector_value ();

		      if (! error_state)
			{
			  Array2<octave_idx_type> ierr;
			  octave_value result;

			  DO_BESSEL (type, ralpha, cx, scaled, ierr, result);

			  if (nargout > 1)
			    retval(1) = int_array2_to_float_matrix (ierr);

			  retval(0) = result;
			}
		      else
			gripe_bessel_arg (fn, "second");
		    }
		  else
		    gripe_bessel_arg (fn, "first");
		}
	      else
		{
		  FloatNDArray alpha = args(0).float_array_value ();

		  if (! error_state)
		    {
		      if (x_arg.is_scalar_type ())
			{
			  FloatComplex x = x_arg.float_complex_value ();

			  if (! error_state)
			    {
			      ArrayN<octave_idx_type> ierr;
			      octave_value result;

			      DO_BESSEL (type, alpha, x, scaled, ierr, result);

			      if (nargout > 1)
				retval(1) = int_arrayN_to_float_array (ierr);

			      retval(0) = result;
			    }
			  else
			    gripe_bessel_arg (fn, "second");
			}
		      else
			{
			  FloatComplexNDArray x = x_arg.float_complex_array_value ();

			  if (! error_state)
			    {
			      ArrayN<octave_idx_type> ierr;
			      octave_value result;
			  
			      DO_BESSEL (type, alpha, x, scaled, ierr, result);
			  
			      if (nargout > 1)
				retval(1) = int_arrayN_to_float_array (ierr);

			      retval(0) = result;
			    }
			  else
			    gripe_bessel_arg (fn, "second");
			}
		    }
		  else
		    gripe_bessel_arg (fn, "first");
		}
	    }
	}
      else
	{
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
			  octave_idx_type ierr;
			  octave_value result;

			  DO_BESSEL (type, alpha, x, scaled, ierr, result);

			  if (nargout > 1)
			    retval(1) = static_cast<double> (ierr);

			  retval(0) = result;
			}
		      else
			gripe_bessel_arg (fn, "second");
		    }
		  else
		    {
		      ComplexNDArray x = x_arg.complex_array_value ();

		      if (! error_state)
			{
			  ArrayN<octave_idx_type> ierr;
			  octave_value result;

			  DO_BESSEL (type, alpha, x, scaled, ierr, result);

			  if (nargout > 1)
			    retval(1) = int_arrayN_to_array (ierr);

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
	      dim_vector dv0 = args(0).dims ();
	      dim_vector dv1 = args(1).dims ();

	      bool args0_is_row_vector = (dv0 (1) == dv0.numel ());
	      bool args1_is_col_vector = (dv1 (0) == dv1.numel ());

	      if (args0_is_row_vector && args1_is_col_vector)
		{
		  RowVector ralpha = args(0).row_vector_value ();

		  if (! error_state)
		    {
		      ComplexColumnVector cx = 
			x_arg.complex_column_vector_value ();

		      if (! error_state)
			{
			  Array2<octave_idx_type> ierr;
			  octave_value result;

			  DO_BESSEL (type, ralpha, cx, scaled, ierr, result);

			  if (nargout > 1)
			    retval(1) = int_array2_to_matrix (ierr);

			  retval(0) = result;
			}
		      else
			gripe_bessel_arg (fn, "second");
		    }
		  else
		    gripe_bessel_arg (fn, "first");
		}
	      else
		{
		  NDArray alpha = args(0).array_value ();

		  if (! error_state)
		    {
		      if (x_arg.is_scalar_type ())
			{
			  Complex x = x_arg.complex_value ();

			  if (! error_state)
			    {
			      ArrayN<octave_idx_type> ierr;
			      octave_value result;

			      DO_BESSEL (type, alpha, x, scaled, ierr, result);

			      if (nargout > 1)
				retval(1) = int_arrayN_to_array (ierr);

			      retval(0) = result;
			    }
			  else
			    gripe_bessel_arg (fn, "second");
			}
		      else
			{
			  ComplexNDArray x = x_arg.complex_array_value ();

			  if (! error_state)
			    {
			      ArrayN<octave_idx_type> ierr;
			      octave_value result;
			  
			      DO_BESSEL (type, alpha, x, scaled, ierr, result);
			  
			      if (nargout > 1)
				retval(1) = int_arrayN_to_array (ierr);

			      retval(0) = result;
			    }
			  else
			    gripe_bessel_arg (fn, "second");
			}
		    }
		  else
		    gripe_bessel_arg (fn, "first");
		}
	    }
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (besselj, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{j}, @var{ierr}] =} besselj (@var{alpha}, @var{x}, @var{opt})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{ierr}] =} bessely (@var{alpha}, @var{x}, @var{opt})\n\
@deftypefnx {Loadable Function} {[@var{i}, @var{ierr}] =} besseli (@var{alpha}, @var{x}, @var{opt})\n\
@deftypefnx {Loadable Function} {[@var{k}, @var{ierr}] =} besselk (@var{alpha}, @var{x}, @var{opt})\n\
@deftypefnx {Loadable Function} {[@var{h}, @var{ierr}] =} besselh (@var{alpha}, @var{k}, @var{x}, @var{opt})\n\
Compute Bessel or Hankel functions of various kinds:\n\
\n\
@table @code\n\
@item besselj\n\
Bessel functions of the first kind.\n\
@item bessely\n\
Bessel functions of the second kind.\n\
@item besseli\n\
Modified Bessel functions of the first kind.\n\
@item besselk\n\
Modified Bessel functions of the second kind.\n\
@item besselh\n\
Compute Hankel functions of the first (@var{k} = 1) or second (@var{k}\n\
 = 2) kind.\n\
@end table\n\
\n\
If the argument @var{opt} is supplied, the result is scaled by the\n\
@code{exp (-I*@var{x})} for @var{k} = 1 or @code{exp (I*@var{x})} for\n\
 @var{k} = 2.\n\
\n\
If @var{alpha} is a scalar, the result is the same size as @var{x}.\n\
If @var{x} is a scalar, the result is the same size as @var{alpha}.\n\
If @var{alpha} is a row vector and @var{x} is a column vector, the\n\
result is a matrix with @code{length (@var{x})} rows and\n\
@code{length (@var{alpha})} columns.  Otherwise, @var{alpha} and\n\
@var{x} must conform and the result will be the same size.\n\
\n\
The value of @var{alpha} must be real.  The value of @var{x} may be\n\
complex.\n\
\n\
If requested, @var{ierr} contains the following status information\n\
and is the same size as the result.\n\
\n\
@enumerate 0\n\
@item\n\
Normal return.\n\
@item\n\
Input error, return @code{NaN}.\n\
@item\n\
Overflow, return @code{Inf}.\n\
@item\n\
Loss of significance by argument reduction results in less than\n\
half of machine accuracy.\n\
@item\n\
Complete loss of significance by argument reduction, return @code{NaN}.\n\
@item\n\
Error---no computation, algorithm termination condition not met,\n\
return @code{NaN}.\n\
@end enumerate\n\
@end deftypefn")
{
  return do_bessel (BESSEL_J, "besselj", args, nargout);
}

DEFUN_DLD (bessely, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{y}, @var{ierr}] =} bessely (@var{alpha}, @var{x}, @var{opt})\n\
See besselj.\n\
@end deftypefn")
{
  return do_bessel (BESSEL_Y, "bessely", args, nargout);
}

DEFUN_DLD (besseli, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{i}, @var{ierr}] =} besseli (@var{alpha}, @var{x}, @var{opt})\n\
See besselj.\n\
@end deftypefn")
{
  return do_bessel (BESSEL_I, "besseli", args, nargout);
}

DEFUN_DLD (besselk, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{k}, @var{ierr}] =} besselk (@var{alpha}, @var{x}, @var{opt})\n\
See besselj.\n\
@end deftypefn")
{
  return do_bessel (BESSEL_K, "besselk", args, nargout);
}

DEFUN_DLD (besselh, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{h}, @var{ierr}] =} besselh (@var{alpha}, @var{k}, @var{x}, @var{opt})\n\
See besselj.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      retval = do_bessel (BESSEL_H1, "besselh", args, nargout);
    }
  else if (nargin == 3 || nargin == 4)
    {
      octave_idx_type kind = args(1).int_value ();

      if (! error_state)
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
    print_usage ();

  return retval;
}

DEFUN_DLD (airy, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{a}, @var{ierr}] =} airy (@var{k}, @var{z}, @var{opt})\n\
Compute Airy functions of the first and second kind, and their\n\
derivatives.\n\
\n\
@example\n\
 K   Function   Scale factor (if 'opt' is supplied)\n\
---  --------   ---------------------------------------\n\
 0   Ai (Z)     exp ((2/3) * Z * sqrt (Z))\n\
 1   dAi(Z)/dZ  exp ((2/3) * Z * sqrt (Z))\n\
 2   Bi (Z)     exp (-abs (real ((2/3) * Z *sqrt (Z))))\n\
 3   dBi(Z)/dZ  exp (-abs (real ((2/3) * Z *sqrt (Z))))\n\
@end example\n\
\n\
The function call @code{airy (@var{z})} is equivalent to\n\
@code{airy (0, @var{z})}.\n\
\n\
The result is the same size as @var{z}.\n\
\n\
If requested, @var{ierr} contains the following status information and\n\
is the same size as the result.\n\
\n\
@enumerate 0\n\
@item\n\
Normal return.\n\
@item\n\
Input error, return @code{NaN}.\n\
@item\n\
Overflow, return @code{Inf}.\n\
@item\n\
Loss of significance by argument reduction results in less than half\n\
 of machine accuracy.\n\
@item\n\
Complete loss of significance by argument reduction, return @code{NaN}.\n\
@item\n\
Error---no computation, algorithm termination condition not met,\n\
return @code{NaN}.\n\
@end enumerate\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 4)
    {
      bool scale = (nargin == 3);

      int kind = 0;

      if (nargin > 1)
	{
	  kind = args(0).int_value ();

	  if (! error_state)
	    {
	      if (kind < 0 || kind > 3)
		error ("airy: expecting K = 0, 1, 2, or 3");
	    }	      
	  else
	    error ("airy: expecting integer value for K");
	}

      if (! error_state)
	{
	  int idx = nargin == 1 ? 0 : 1;

	  if (args (idx).is_single_type ())
	    {
	      FloatComplexNDArray z = args(idx).float_complex_array_value ();

	      if (! error_state)
		{
		  ArrayN<octave_idx_type> ierr;
		  octave_value result;

		  if (kind > 1)
		    result = biry (z, kind == 3, scale, ierr);
		  else
		    result = airy (z, kind == 1, scale, ierr);

		  if (nargout > 1)
		    retval(1) = int_arrayN_to_float_array (ierr);

		  retval(0) = result;
		}
	      else
		error ("airy: expecting complex matrix for Z");
	    }
	  else
	    {
	      ComplexNDArray z = args(idx).complex_array_value ();

	      if (! error_state)
		{
		  ArrayN<octave_idx_type> ierr;
		  octave_value result;

		  if (kind > 1)
		    result = biry (z, kind == 3, scale, ierr);
		  else
		    result = airy (z, kind == 1, scale, ierr);

		  if (nargout > 1)
		    retval(1) = int_arrayN_to_array (ierr);

		  retval(0) = result;
		}
	      else
		error ("airy: expecting complex matrix for Z");
	    }
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
