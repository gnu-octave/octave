/*

Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
              2003, 2004, 2005, 2006, 2007 John W. Eaton

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

#include "systime.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include <cfloat>

#include <string>

#include "lo-ieee.h"
#include "lo-math.h"
#include "str-vec.h"
#include "quit.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-float.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-cx-sparse.h"
#include "parse.h"
#include "pt-mat.h"
#include "utils.h"
#include "variables.h"
#include "pager.h"
#include "xnorm.h"

#if ! defined (HAVE_HYPOTF) && defined (HAVE__HYPOTF)
#define hypotf _hypotf
#define HAVE_HYPOTF 1
#endif

#define ANY_ALL(FCN) \
 \
  octave_value retval; \
 \
  int nargin = args.length (); \
 \
  if (nargin == 1 || nargin == 2) \
    { \
      int dim = (nargin == 1 ? -1 : args(1).int_value (true) - 1); \
 \
      if (! error_state) \
        { \
          if (dim >= -1) \
	    retval = args(0).FCN (dim); \
          else \
	    error (#FCN ": invalid dimension argument = %d", dim + 1); \
        } \
      else \
        error (#FCN ": expecting dimension argument to be an integer"); \
    } \
  else \
    print_usage (); \
 \
  return retval

DEFUN (all, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} all (@var{x}, @var{dim})\n\
The function @code{all} behaves like the function @code{any}, except\n\
that it returns true only if all the elements of a vector, or all the\n\
elements along dimension @var{dim} of a matrix, are nonzero.\n\
@end deftypefn")
{
  ANY_ALL (all);
}

/*

%!test
%! x = ones (3);
%! x(1,1) = 0;
%! assert((all (all (rand (3) + 1) == [1, 1, 1]) == 1
%! && all (all (x) == [0, 1, 1]) == 1
%! && all (x, 1) == [0, 1, 1]
%! && all (x, 2) == [0; 1; 1]));

%!test
%! x = ones (3, 'single');
%! x(1,1) = 0;
%! assert((all (all (single (rand (3) + 1)) == [1, 1, 1]) == 1
%! && all (all (x) == [0, 1, 1]) == 1
%! && all (x, 1) == [0, 1, 1]
%! && all (x, 2) == [0; 1; 1]));

%!error <Invalid call to all.*> all ();
%!error <Invalid call to all.*> all (1, 2, 3);

 */

DEFUN (any, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} any (@var{x}, @var{dim})\n\
For a vector argument, return 1 if any element of the vector is\n\
nonzero.\n\
\n\
For a matrix argument, return a row vector of ones and\n\
zeros with each element indicating whether any of the elements of the\n\
corresponding column of the matrix are nonzero.  For example,\n\
\n\
@example\n\
@group\n\
any (eye (2, 4))\n\
     @result{} [ 1, 1, 0, 0 ]\n\
@end group\n\
@end example\n\
\n\
If the optional argument @var{dim} is supplied, work along dimension\n\
@var{dim}.  For example,\n\
\n\
@example\n\
@group\n\
any (eye (2, 4), 2)\n\
     @result{} [ 1; 1 ]\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  ANY_ALL (any);
}

/*

%!test
%! x = zeros (3);
%! x(3,3) = 1;
%! assert((all (any (x) == [0, 0, 1]) == 1
%! && all (any (ones (3)) == [1, 1, 1]) == 1
%! && any (x, 1) == [0, 0, 1]
%! && any (x, 2) == [0; 0; 1]));

%!test
%! x = zeros (3,'single');
%! x(3,3) = 1;
%! assert((all (any (x) == [0, 0, 1]) == 1
%! && all (any (ones (3, 'single')) == [1, 1, 1]) == 1
%! && any (x, 1) == [0, 0, 1]
%! && any (x, 2) == [0; 0; 1]));

%!error <Invalid call to any.*> any ();
%!error <Invalid call to any.*> any (1, 2, 3);

 */

// These mapping functions may also be useful in other places, eh?

typedef double (*d_dd_fcn) (double, double);
typedef float (*f_ff_fcn) (float, float);

static NDArray
map_d_m (d_dd_fcn f, double x, const NDArray& y)
{
  NDArray retval (y.dims ());
  double *r_data = retval.fortran_vec ();

  const double *y_data = y.data ();

  octave_idx_type nel = y.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      r_data[i] = f (x, y_data[i]);
    }

  return retval;
}

static FloatNDArray
map_f_fm (f_ff_fcn f, float x, const FloatNDArray& y)
{
  FloatNDArray retval (y.dims ());
  float *r_data = retval.fortran_vec ();

  const float *y_data = y.data ();

  octave_idx_type nel = y.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      r_data[i] = f (x, y_data[i]);
    }

  return retval;
}

static NDArray
map_m_d (d_dd_fcn f, const NDArray& x, double y)
{
  NDArray retval (x.dims ());
  double *r_data = retval.fortran_vec ();

  const double *x_data = x.data ();

  octave_idx_type nel = x.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      r_data[i] = f (x_data[i], y);
    }

  return retval;
}

static FloatNDArray
map_fm_f (f_ff_fcn f, const FloatNDArray& x, float y)
{
  FloatNDArray retval (x.dims ());
  float *r_data = retval.fortran_vec ();

  const float *x_data = x.data ();

  octave_idx_type nel = x.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      r_data[i] = f (x_data[i], y);
    }

  return retval;
}

static NDArray
map_m_m (d_dd_fcn f, const NDArray& x, const NDArray& y)
{
  assert (x.dims () == y.dims ());

  NDArray retval (x.dims ());
  double *r_data = retval.fortran_vec ();

  const double *x_data = x.data ();
  const double *y_data = y.data ();

  octave_idx_type nel = x.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      r_data[i] = f (x_data[i], y_data[i]);
    }

  return retval;
}

static FloatNDArray
map_fm_fm (f_ff_fcn f, const FloatNDArray& x, const FloatNDArray& y)
{
  assert (x.dims () == y.dims ());

  FloatNDArray retval (x.dims ());
  float *r_data = retval.fortran_vec ();

  const float *x_data = x.data ();
  const float *y_data = y.data ();

  octave_idx_type nel = x.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      r_data[i] = f (x_data[i], y_data[i]);
    }

  return retval;
}

static SparseMatrix
map_d_s (d_dd_fcn f, double x, const SparseMatrix& y)
{
  octave_idx_type nr = y.rows ();
  octave_idx_type nc = y.columns ();
  SparseMatrix retval;
  double f_zero = f (x, 0.);

  if (f_zero != 0)
    {
      retval = SparseMatrix (nr, nc, f_zero);

      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = y.cidx (j); i < y.cidx (j+1); i++)
	  {
	    OCTAVE_QUIT;
	    retval.data (y.ridx(i) + j * nr) = f (x, y.data (i));
	  } 

      retval.maybe_compress (true);
    }
  else
    {
      octave_idx_type nz = y.nnz ();
      retval = SparseMatrix (nr, nc, nz);
      octave_idx_type ii = 0;
      retval.cidx (ii) = 0;

      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = y.cidx (j); i < y.cidx (j+1); i++)
	    {
	      OCTAVE_QUIT;
	      double val = f (x, y.data (i));

	      if (val != 0.0)
		{
		  retval.data (ii) = val;
		  retval.ridx (ii++) = y.ridx (i);
		}
	    } 
	  retval.cidx (j + 1) = ii;
	}

      retval.maybe_compress (false);
    }

  return retval;
}

static SparseMatrix
map_s_d (d_dd_fcn f, const SparseMatrix& x, double y)
{
  octave_idx_type nr = x.rows ();
  octave_idx_type nc = x.columns ();
  SparseMatrix retval;
  double f_zero = f (0., y);

  if (f_zero != 0)
    {
      retval = SparseMatrix (nr, nc, f_zero);

      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = x.cidx (j); i < x.cidx (j+1); i++)
	  {
	    OCTAVE_QUIT;
	    retval.data (x.ridx(i) + j * nr) = f (x.data (i), y);
	  } 

      retval.maybe_compress (true);
    }
  else
    {
      octave_idx_type nz = x.nnz ();
      retval = SparseMatrix (nr, nc, nz);
      octave_idx_type ii = 0;
      retval.cidx (ii) = 0;

      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = x.cidx (j); i < x.cidx (j+1); i++)
	    {
	      OCTAVE_QUIT;
	      double val = f (x.data (i), y);

	      if (val != 0.0)
		{
		  retval.data (ii) = val;
		  retval.ridx (ii++) = x.ridx (i);
		}
	    } 
	  retval.cidx (j + 1) = ii;
	}

      retval.maybe_compress (false);
    }

  return retval;
}

static SparseMatrix
map_s_s (d_dd_fcn f, const SparseMatrix& x, const SparseMatrix& y)
{
  octave_idx_type nr = x.rows ();
  octave_idx_type nc = x.columns ();

  octave_idx_type y_nr = y.rows ();
  octave_idx_type y_nc = y.columns ();

  assert (nr == y_nr && nc == y_nc);

  SparseMatrix retval;
  double f_zero = f (0., 0.);

  if (f_zero != 0)
    {
      retval = SparseMatrix (nr, nc, f_zero);
      octave_idx_type k1 = 0, k2 = 0;

      for (octave_idx_type j = 0; j < nc; j++)
	{
	  while (k1 < x.cidx(j+1) && k2 < y.cidx(j+1))
	    {
	      OCTAVE_QUIT;
	      if (k1 >= x.cidx(j+1))
		{
		  retval.data (y.ridx(k2) + j * nr) = f (0.0, y.data (k2));
		  k2++;
		}
	      else if (k2 >= y.cidx(j+1))
		{
		  retval.data (x.ridx(k1) + j * nr) = f (x.data (k1), 0.0);
		  k1++;
		}
	      else
		{
		  octave_idx_type rx = x.ridx(k1);
		  octave_idx_type ry = y.ridx(k2);

		  if (rx < ry)
		    {
		      retval.data (rx + j * nr) = f (x.data (k1), 0.0);
		      k1++;
		    }
		  else if (rx > ry)
		    {
		      retval.data (ry + j * nr) = f (0.0, y.data (k2));
		      k2++;
		    }
		  else
		    {
		      retval.data (ry + j * nr) = f (x.data (k1), y.data (k2));
		      k1++;
		      k2++;
		    }
		}
	    }
	}

      retval.maybe_compress (true);
    }
  else
    {
      octave_idx_type nz = x.nnz () + y.nnz ();
      retval = SparseMatrix (nr, nc, nz);
      octave_idx_type ii = 0;
      retval.cidx (ii) = 0;
      octave_idx_type k1 = 0, k2 = 0;

      for (octave_idx_type j = 0; j < nc; j++)
	{
	  while (k1 < x.cidx(j+1) && k2 < y.cidx(j+1))
	    {
	      OCTAVE_QUIT;
	      double val;
	      octave_idx_type r;
	      if (k1 >= x.cidx(j+1))
		{
		  r = y.ridx (k2);
		  val = f (0.0, y.data (k2++));
		}
	      else if (k2 >= y.cidx(j+1))
		{
		  r = x.ridx (k1);
		  val = f (x.data (k1++), 0.0);
		}
	      else
		{
		  octave_idx_type rx = x.ridx(k1);
		  octave_idx_type ry = y.ridx(k2);

		  if (rx < ry)
		    {
		      r = x.ridx (k1);
		      val = f (x.data (k1++), 0.0);
		    }
		  else if (rx > ry)
		    {
		      r = y.ridx (k2);
		      val = f (0.0, y.data (k2++));
		    }
		  else
		    {
		      r = y.ridx (k2);
		      val = f (x.data (k1++), y.data (k2++));
		    }
		}
	      if (val != 0.0)
		{
		  retval.data (ii) = val;
		  retval.ridx (ii++) = r;
		}
	    }
	  retval.cidx (j + 1) = ii;
	}

      retval.maybe_compress (false);
    }

  return retval;
}

DEFUN (atan2, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atan2 (@var{y}, @var{x})\n\
Compute atan (@var{y} / @var{x}) for corresponding elements of @var{y}\n\
and @var{x}.  The result is in the range -pi to pi.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    {
      if (args(0).is_integer_type () || args(1).is_integer_type ())
	error ("atan2: not defined for integer types");
      else
	{
	  octave_value arg_y = args(0);
	  octave_value arg_x = args(1);

	  dim_vector y_dims = arg_y.dims ();
	  dim_vector x_dims = arg_x.dims ();

	  bool y_is_scalar = y_dims.all_ones ();
	  bool x_is_scalar = x_dims.all_ones ();

	  bool is_float = arg_y.is_single_type () || arg_x.is_single_type ();

	  if (y_is_scalar && x_is_scalar)
	    {
	      if (is_float)
		{
		  float y = arg_y.float_value ();

		  if (! error_state)
		    {
		      float x = arg_x.float_value ();

		      if (! error_state)
			retval = atan2f (y, x);
		    }
		}
	      else
		{
		  double y = arg_y.double_value ();

		  if (! error_state)
		    {
		      double x = arg_x.double_value ();

		      if (! error_state)
			retval = atan2 (y, x);
		    }
		}
	    }
	  else if (y_is_scalar)
	    {
	      if (is_float)
		{
		  float y = arg_y.float_value ();

		  if (! error_state)
		    {
		      // Even if x is sparse return a full matrix here
		      FloatNDArray x = arg_x.float_array_value ();

		      if (! error_state)
			retval = map_f_fm (atan2f, y, x);
		    }
		}
	      else
		{
		  double y = arg_y.double_value ();

		  if (! error_state)
		    {
		      // Even if x is sparse return a full matrix here
		      NDArray x = arg_x.array_value ();

		      if (! error_state)
			retval = map_d_m (atan2, y, x);
		    }
		}
	    }
	  else if (x_is_scalar)
	    {
	      if (arg_y.is_sparse_type ())
		{
		  SparseMatrix y = arg_y.sparse_matrix_value ();

		  if (! error_state)
		    {
		      double x = arg_x.double_value ();
		      
		      if (! error_state)
			retval = map_s_d (atan2, y, x);
		    }
		}
	      else if (is_float)
		{
		  FloatNDArray y = arg_y.float_array_value ();
		  
		  if (! error_state)
		    {
		      float x = arg_x.float_value ();

		      if (! error_state)
			retval = map_fm_f (atan2f, y, x);
		    }
		}
	      else
		{
		  NDArray y = arg_y.array_value ();

		  if (! error_state)
		    {
		      double x = arg_x.double_value ();

		      if (! error_state)
			retval = map_m_d (atan2, y, x);
		    }
		}
	    }
	  else if (y_dims == x_dims)
	    {
	      // Even if y is sparse return a full matrix here
	      if (arg_x.is_sparse_type ())
		{
		  SparseMatrix y = arg_y.sparse_matrix_value ();

		  if (! error_state)
		    {
		      SparseMatrix x = arg_x.sparse_matrix_value ();

		      if (! error_state)
			retval = map_s_s (atan2, y, x);
		    }
		}
	      else if (is_float)
		{
		  FloatNDArray y = arg_y.array_value ();

		  if (! error_state)
		    {
		      FloatNDArray x = arg_x.array_value ();

		      if (! error_state)
			retval = map_fm_fm (atan2f, y, x);
		    }
		}
	      else
		{
		  NDArray y = arg_y.array_value ();

		  if (! error_state)
		    {
		      NDArray x = arg_x.array_value ();

		      if (! error_state)
			retval = map_m_m (atan2, y, x);
		    }
		}
	    }
	  else
	    error ("atan2: nonconformant matrices");
	}
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (size (atan2 (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (atan2 (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (atan2 (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (atan2 (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (atan2 (1, 2)), [1, 1])

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
%! y = [0, rt3, 1, rt3, -rt3, -1, -rt3, 0];
%! x = [1, 3, 1, 1, 1, 1, 3, 1];
%! assert(atan2 (y, x), v, sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = single([0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0]);
%! y = single([0, rt3, 1, rt3, -rt3, -1, -rt3, 0]);
%! x = single([1, 3, 1, 1, 1, 1, 3, 1]);
%! assert(atan2 (y, x), v, sqrt (eps('single')));

%!error <Invalid call to atan2.*> atan2 ();
%!error <Invalid call to atan2.*> atan2 (1, 2, 3);

*/



DEFUN (hypot, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} hypot (@var{x}, @var{y})\n\
Compute square-root of the squares of @var{x} and @var{y}\n\
element-by-element. This equivalent to @code{sqrt (@var{x}.^ 2 + @var{y}\n\
.^ 2)}, but calculated in a manner that avoids overflows for large\n\
values of @var{x} or @var{y}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    {
      if (args(0).is_integer_type () || args(1).is_integer_type ())
	error ("hypot: not defined for integer types");
      else
	{
	  octave_value arg_x = args(0);
	  octave_value arg_y = args(1);

	  dim_vector x_dims = arg_x.dims ();
	  dim_vector y_dims = arg_y.dims ();

	  bool x_is_scalar = x_dims.all_ones ();
	  bool y_is_scalar = y_dims.all_ones ();

	  bool is_float = arg_y.is_single_type () || arg_x.is_single_type ();

	  if (y_is_scalar && x_is_scalar)
	    {
	      if (is_float)
		{
		  float x;
		  if (arg_x.is_complex_type ())
		    x = abs (arg_x.float_complex_value ());
		  else
		    x = arg_x.float_value ();

		  if (! error_state)
		    {
		      float y;
		      if (arg_y.is_complex_type ())
			y = abs (arg_y.float_complex_value ());
		      else
			y = arg_y.float_value ();

		      if (! error_state)
			retval = hypotf (x, y);
		    }
		}
	      else
		{
		  double x;
		  if (arg_x.is_complex_type ())
		    x = abs (arg_x.complex_value ());
		  else
		    x = arg_x.double_value ();

		  if (! error_state)
		    {
		      double y;
		      if (arg_y.is_complex_type ())
			y = abs (arg_y.complex_value ());
		      else
			y = arg_y.double_value ();

		      if (! error_state)
			retval = hypot (x, y);
		    }
		}
	    }
	  else if (y_is_scalar)
	    {
	      if (is_float)
		{
		  FloatNDArray x;
		  if (arg_x.is_complex_type ())
		    x = arg_x.float_complex_array_value ().abs ();
		  else
		    x = arg_x.float_array_value ();

		  if (! error_state)
		    {
		      float y;
		      if (arg_y.is_complex_type ())
			y = abs (arg_y.float_complex_value ());
		      else
			y = arg_y.float_value ();

		      if (! error_state)
			retval = map_fm_f (hypotf, x, y);
		    }
		}
	      else
		{
		  NDArray x;
		  if (arg_x.is_complex_type ())
		    x = arg_x.complex_array_value ().abs ();
		  else
		    x = arg_x.array_value ();

		  if (! error_state)
		    {
		      double y;
		      if (arg_y.is_complex_type ())
			y = abs (arg_y.complex_value ());
		      else
			y = arg_y.double_value ();

		      if (! error_state)
			retval = map_m_d (hypot, x, y);
		    }
		}
	    }
	  else if (x_is_scalar)
	    {
	      if (is_float)
		{
		  float x;
		  if (arg_x.is_complex_type ())
		    x = abs (arg_x.float_complex_value ());
		  else
		    x = arg_x.float_value ();

		  if (! error_state)
		    {
		      FloatNDArray y;
		      if (arg_y.is_complex_type ())
			y = arg_y.float_complex_array_value ().abs ();
		      else
			y = arg_y.float_array_value ();

		      if (! error_state)
			retval = map_f_fm (hypotf, x, y);
		    }
		}
	      else
		{
		  double x;
		  if (arg_x.is_complex_type ())
		    x = abs (arg_x.complex_value ());
		  else
		    x = arg_x.double_value ();

		  if (! error_state)
		    {
		      NDArray y;
		      if (arg_y.is_complex_type ())
			y = arg_y.complex_array_value ().abs ();
		      else
			y = arg_y.array_value ();

		      if (! error_state)
			retval = map_d_m (hypot, x, y);
		    }
		}
	    }
	  else if (y_dims == x_dims)
	    {
	      if (arg_x.is_sparse_type () && arg_y.is_sparse_type ())
		{
		  SparseMatrix x;
		  if (arg_x.is_complex_type ())
		    x = arg_x.sparse_complex_matrix_value ().abs ();
		  else
		    x = arg_x.sparse_matrix_value ();

		  if (! error_state)
		    {
		      SparseMatrix y;
		      if (arg_y.is_complex_type ())
			y = arg_y.sparse_complex_matrix_value ().abs ();
		      else
			y = arg_y.sparse_matrix_value ();

		      if (! error_state)
			retval = map_s_s (hypot, x, y);
		    }
		}
	      else if (is_float)
		{
		  FloatNDArray x;
		  if (arg_x.is_complex_type ())
		    x = arg_x.float_complex_array_value ().abs ();
		  else
		    x = arg_x.float_array_value ();

		  if (! error_state)
		    {
		      FloatNDArray y;
		      if (arg_y.is_complex_type ())
			y = arg_y.float_complex_array_value ().abs ();
		      else
			y = arg_y.float_array_value ();

		      if (! error_state)
			retval = map_fm_fm (hypotf, x, y);
		    }
		}
	      else
		{
		  NDArray x;
		  if (arg_x.is_complex_type ())
		    x = arg_x.complex_array_value ().abs ();
		  else
		    x = arg_x.array_value ();

		  if (! error_state)
		    {
		      NDArray y;
		      if (arg_y.is_complex_type ())
			y = arg_y.complex_array_value ().abs ();
		      else
			y = arg_y.array_value ();

		      if (! error_state)
			retval = map_m_m (hypot, x, y);
		    }
		}
	    }
	  else
	    error ("hypot: nonconformant matrices");
	}
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (size (hypot (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (hypot (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (hypot (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (hypot (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (hypot (1, 2)), [1, 1])
%!assert (hypot (1:10, 1:10), sqrt(2) * [1:10], 16*eps)
%!assert (hypot (single(1:10), single(1:10)), single(sqrt(2) * [1:10]));
*/

template<typename T, typename ET>
void 
map_2_xlog2 (const Array<T>& x, Array<T>& f, Array<ET>& e)
{
  f = Array<T>(x.dims ());
  e = Array<ET>(x.dims ());
  for (octave_idx_type i = 0; i < x.numel (); i++)
    {
      int exp;
      f.xelem (i) = xlog2 (x(i), exp);
      e.xelem (i) = exp;
    }
}

DEFUN (log2, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log2 (@var{x})\n\
@deftypefnx {Mapping Function} {[@var{f}, @var{e}] = } log2 (@var{x})\n\
Compute the base-2 logarithm for each element of @var{x}.\n\
If called with two output arguments, split @var{x} to\n\
binary mantissa and exponent so that @code{1/2 <= abs(f) < 1} and\n\
@var{e} is an integer. If @code{x = 0}, @code{f = e = 0}.\n\
@seealso{log, log10, log2, exp}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      if (nargout < 2)
        retval(0) = args(0).log2 ();
      else if (args(0).is_single_type ())
	{
	  if (args(0).is_real_type ())
	    {
	      FloatNDArray f;
	      FloatNDArray x = args(0).float_array_value ();
	      // FIXME -- should E be an int value?
	      FloatMatrix e;
	      map_2_xlog2 (x, f, e);
	      retval (1) = e;
	      retval (0) = f;
	    }
	  else if (args(0).is_complex_type ())
	    {
	      FloatComplexNDArray f;
	      FloatComplexNDArray x = args(0).float_complex_array_value ();
	      // FIXME -- should E be an int value?
	      FloatNDArray e;
	      map_2_xlog2 (x, f, e);
	      retval (1) = e;
	      retval (0) = f;
	    }
	}
      else if (args(0).is_real_type ())
        {
          NDArray f;
          NDArray x = args(0).array_value ();
          // FIXME -- should E be an int value?
          Matrix e;
          map_2_xlog2 (x, f, e);
          retval (1) = e;
          retval (0) = f;
        }
      else if (args(0).is_complex_type ())
        {
          ComplexNDArray f;
          ComplexNDArray x = args(0).complex_array_value ();
          // FIXME -- should E be an int value?
          NDArray e;
          map_2_xlog2 (x, f, e);
          retval (1) = e;
          retval (0) = f;
        }
      else
        gripe_wrong_type_arg ("log2", args(0));
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert(log2 ([1/4, 1/2, 1, 2, 4]), [-2, -1, 0, 1, 2]);
%!assert(log2(Inf), Inf);
%!assert(isnan(log2(NaN)));
%!assert(log2(4*i), 2 + log2(1*i));
%!assert(log2(complex(0,Inf)), Inf + log2(i));

%!test
%! [f, e] = log2 ([0,-1; 2,-4; Inf,-Inf]);
%! assert (f, [0,-0.5; 0.5,-0.5; Inf,-Inf]);
%! assert (e, [0,1;2,3;0,0])

%!test
%! [f, e] = log2 (complex (zeros (3, 2), [0,-1; 2,-4; Inf,-Inf]));
%! assert (f, complex (zeros (3, 2), [0,-0.5; 0.5,-0.5; Inf,-Inf]));
%! assert (e, [0,1; 2,3; 0,0]);
*/

DEFUN (fmod, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} fmod (@var{x}, @var{y})\n\
Compute the floating point remainder of dividing @var{x} by @var{y}\n\
using the C library function @code{fmod}.  The result has the same\n\
sign as @var{x}.  If @var{y} is zero, the result is implementation-defined.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    {
      octave_value arg_y = args(0);
      octave_value arg_x = args(1);

      dim_vector y_dims = arg_y.dims ();
      dim_vector x_dims = arg_x.dims ();

      bool y_is_scalar = y_dims.all_ones ();
      bool x_is_scalar = x_dims.all_ones ();

      bool is_float = arg_y.is_single_type () || arg_x.is_single_type ();

      if (y_is_scalar && x_is_scalar)
	{
	  if (is_float)
	    {
	      float y = arg_y.float_value ();

	      if (! error_state)
		{
		  float x = arg_x.float_value ();

		  if (! error_state)
		    retval = fmod (x, y);
		}
	    }
	  else
	    {
	      double y = arg_y.double_value ();

	      if (! error_state)
		{
		  double x = arg_x.double_value ();

		  if (! error_state)
		    retval = fmod (x, y);
		}
	    }
	}
      else if (y_is_scalar)
	{
	  if (is_float)
	    {
	      float y = arg_y.float_value ();

	      if (! error_state)
		{
		  FloatNDArray x = arg_x.float_array_value ();

		  if (! error_state)
		    retval = map_fm_f (fmodf, x, y);
		}
	    }
	  else
	    {
	      double y = arg_y.double_value ();

	      if (! error_state)
		{
		  if (arg_x.is_sparse_type ())
		    {
		      SparseMatrix x = arg_x.sparse_matrix_value ();

		      if (! error_state)
			retval = map_s_d (fmod, x, y);
		    }
		  else
		    {
		      NDArray x = arg_x.array_value ();

		      if (! error_state)
			retval = map_m_d (fmod, x, y);
		    }
		}
	    }
	}
      else if (x_is_scalar)
	{
	  if (arg_y.is_sparse_type ())
	    {
	      SparseMatrix y = arg_y.sparse_matrix_value ();

	      if (! error_state)
		{
		  double x = arg_x.double_value ();

		  if (! error_state)
		    retval = map_d_s (fmod, x, y);
		}
	    }
	  else if (is_float)
	    {
	      FloatNDArray y = arg_y.float_array_value ();

	      if (! error_state)
		{
		  float x = arg_x.float_value ();

		  if (! error_state)
		    retval = map_f_fm (fmodf, x, y);
		}
	    }
	  else
	    {
	      NDArray y = arg_y.array_value ();

	      if (! error_state)
		{
		  double x = arg_x.double_value ();

		  if (! error_state)
		    retval = map_d_m (fmod, x, y);
		}
	    }
	}
      else if (y_dims == x_dims)
	{
	  if (arg_y.is_sparse_type () || arg_x.is_sparse_type ())
	    {
	      SparseMatrix y = arg_y.sparse_matrix_value ();

	      if (! error_state)
		{
		  SparseMatrix x = arg_x.sparse_matrix_value ();

		  if (! error_state)
		    retval = map_s_s (fmod, x, y);
		}
	    }
	  else if (is_float)
	    {
	      FloatNDArray y = arg_y.float_array_value ();

	      if (! error_state)
		{
		  FloatNDArray x = arg_x.float_array_value ();

		  if (! error_state)
		    retval = map_fm_fm (fmodf, x, y);
		}
	    }
	  else
	    {
	      NDArray y = arg_y.array_value ();

	      if (! error_state)
		{
		  NDArray x = arg_x.array_value ();

		  if (! error_state)
		    retval = map_m_m (fmod, x, y);
		}
	    }
	}
      else
	error ("fmod: nonconformant matrices");
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (size (fmod (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (fmod (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (fmod (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (fmod (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (fmod (1, 2)), [1, 1])
*/

// FIXME Need to convert the reduction functions of this file for single precision

#define NATIVE_REDUCTION_1(FCN, TYPE, DIM) \
  (arg.is_ ## TYPE ## _type ()) \
    { \
      TYPE ## NDArray tmp = arg. TYPE ##_array_value (); \
      \
      if (! error_state) \
        retval = tmp.FCN (DIM); \
    }

#define NATIVE_REDUCTION(FCN) \
 \
  octave_value retval; \
 \
  int nargin = args.length (); \
 \
  bool isnative = false; \
  bool isdouble = false; \
  \
  if (nargin > 1 && args(nargin - 1).is_string ()) \
    { \
      std::string str = args(nargin - 1).string_value (); \
      \
      if (! error_state) \
	{ \
	  if (str == "native") \
	    isnative = true; \
	  else if (str == "double") \
            isdouble = true; \
          else \
	    error ("sum: unrecognized string argument"); \
          nargin --; \
	} \
    } \
  \
  if (nargin == 1 || nargin == 2) \
    { \
      octave_value arg = args(0); \
 \
      int dim = (nargin == 1 ? -1 : args(1).int_value (true) - 1); \
 \
      if (! error_state) \
	{ \
	  if (dim >= -1) \
	    { \
	      if (arg.is_sparse_type ()) \
		{ \
		  if (arg.is_real_type ()) \
		    { \
		      SparseMatrix tmp = arg.sparse_matrix_value (); \
		      \
		      if (! error_state) \
			retval = tmp.FCN (dim); \
		    } \
		  else \
		    { \
		      SparseComplexMatrix tmp = arg.sparse_complex_matrix_value (); \
                      \
		      if (! error_state) \
			retval = tmp.FCN (dim); \
		    } \
		} \
	      else \
		{ \
		  if (isnative)	\
		    { \
		      if NATIVE_REDUCTION_1 (FCN, uint8, dim) \
		      else if NATIVE_REDUCTION_1 (FCN, uint16, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, uint32, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, uint64, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, int8, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, int16, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, int32, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, int64, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, bool, dim) \
                      else if (arg.is_char_matrix ()) \
                        { \
			  error (#FCN, ": invalid char type"); \
			} \
		      else if (!isdouble && arg.is_single_type ()) \
                        { \
	                  if (arg.is_complex_type ()) \
		            { \
		              FloatComplexNDArray tmp = \
				arg.float_complex_array_value (); \
                              \
		              if (! error_state) \
		                retval = tmp.FCN (dim); \
		            } \
	                  else if (arg.is_real_type ()) \
		            { \
		              FloatNDArray tmp = arg.float_array_value (); \
                              \
		              if (! error_state) \
		                retval = tmp.FCN (dim); \
		            } \
			} \
	              else if (arg.is_complex_type ()) \
		        { \
		          ComplexNDArray tmp = arg.complex_array_value (); \
                          \
		          if (! error_state) \
		            retval = tmp.FCN (dim); \
		        } \
	              else if (arg.is_real_type ()) \
		        { \
		          NDArray tmp = arg.array_value (); \
                          \
		          if (! error_state) \
		            retval = tmp.FCN (dim); \
		        } \
                      else \
		        { \
		          gripe_wrong_type_arg (#FCN, arg); \
		          return retval; \
		        } \
                    } \
		  else if (!isdouble && arg.is_single_type ()) \
		    { \
	              if (arg.is_real_type ()) \
		        { \
		          FloatNDArray tmp = arg.float_array_value (); \
                          \
		          if (! error_state) \
		            retval = tmp.FCN (dim); \
		        } \
	              else if (arg.is_complex_type ()) \
		        { \
		          FloatComplexNDArray tmp = \
			    arg.float_complex_array_value (); \
                          \
		          if (! error_state) \
		            retval = tmp.FCN (dim); \
		        } \
		    } \
	          else if (arg.is_real_type ()) \
		    { \
		      NDArray tmp = arg.array_value (); \
                      \
		      if (! error_state) \
		        retval = tmp.FCN (dim); \
		    } \
	          else if (arg.is_complex_type ()) \
		    { \
		      ComplexNDArray tmp = arg.complex_array_value (); \
                      \
		      if (! error_state) \
		        retval = tmp.FCN (dim); \
		    } \
	          else \
		    { \
		      gripe_wrong_type_arg (#FCN, arg); \
		      return retval; \
		    } \
		} \
	    } \
	  else \
	    error (#FCN ": invalid dimension argument = %d", dim + 1); \
	} \
      \
    } \
  else \
    print_usage (); \
 \
  return retval

#define DATA_REDUCTION(FCN) \
 \
  octave_value retval; \
 \
  int nargin = args.length (); \
 \
  if (nargin == 1 || nargin == 2) \
    { \
      octave_value arg = args(0); \
 \
      int dim = (nargin == 1 ? -1 : args(1).int_value (true) - 1); \
 \
      if (! error_state) \
	{ \
	  if (dim >= -1) \
	    { \
	      if (arg.is_real_type ()) \
		{ \
		  if (arg.is_sparse_type ()) \
		    { \
		      SparseMatrix tmp = arg.sparse_matrix_value (); \
 \
		      if (! error_state) \
			retval = tmp.FCN (dim); \
		    } \
		  else if (arg.is_single_type ()) \
		    { \
		      FloatNDArray tmp = arg.float_array_value (); \
 \
		      if (! error_state) \
			retval = tmp.FCN (dim); \
		    } \
		  else \
		    { \
		      NDArray tmp = arg.array_value (); \
 \
		      if (! error_state) \
			retval = tmp.FCN (dim); \
		    } \
		} \
	      else if (arg.is_complex_type ()) \
		{ \
		  if (arg.is_sparse_type ()) \
		    { \
		      SparseComplexMatrix tmp = arg.sparse_complex_matrix_value (); \
 \
		      if (! error_state) \
			retval = tmp.FCN (dim); \
		    } \
		  else if (arg.is_single_type ()) \
		    { \
		      FloatComplexNDArray tmp = arg.float_complex_array_value (); \
 \
		      if (! error_state) \
			retval = tmp.FCN (dim); \
		    } \
		  else \
		    { \
		      ComplexNDArray tmp = arg.complex_array_value (); \
 \
		      if (! error_state) \
			retval = tmp.FCN (dim); \
		    } \
		} \
	      else \
		{ \
		  gripe_wrong_type_arg (#FCN, arg); \
		  return retval; \
		} \
	    } \
	  else \
	    error (#FCN ": invalid dimension argument = %d", dim + 1); \
	} \
    } \
  else \
    print_usage (); \
 \
  return retval

DEFUN (cumprod, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cumprod (@var{x}, @var{dim})\n\
Cumulative product of elements along dimension @var{dim}.  If\n\
@var{dim} is omitted, it defaults to 1 (column-wise cumulative\n\
products).\n\
\n\
As a special case, if @var{x} is a vector and @var{dim} is omitted,\n\
return the cumulative product of the elements as a vector with the\n\
same orientation as @var{x}.\n\
@end deftypefn")
{
  DATA_REDUCTION (cumprod);
}

/*

%!assert (cumprod ([1, 2, 3]), [1, 2, 6]);
%!assert (cumprod ([-1; -2; -3]), [-1; 2; -6]);
%!assert (cumprod ([i, 2+i, -3+2i, 4]), [i, -1+2i, -1-8i, -4-32i]);
%!assert (cumprod ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [1, 2, 3; i, 4i, 9i; -1+i, -8+8i, -27+27i]);

%!assert (cumprod (single([1, 2, 3])), single([1, 2, 6]));
%!assert (cumprod (single([-1; -2; -3])), single([-1; 2; -6]));
%!assert (cumprod (single([i, 2+i, -3+2i, 4])), single([i, -1+2i, -1-8i, -4-32i]));
%!assert (cumprod (single([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), single([1, 2, 3; i, 4i, 9i; -1+i, -8+8i, -27+27i]));

%!error <Invalid call to cumprod.*> cumprod ();

%!assert (cumprod ([2, 3; 4, 5], 1), [2, 3; 8, 15]);
%!assert (cumprod ([2, 3; 4, 5], 2), [2, 6; 4, 20]);

%!assert (cumprod (single([2, 3; 4, 5]), 1), single([2, 3; 8, 15]));
%!assert (cumprod (single([2, 3; 4, 5]), 2), single([2, 6; 4, 20]));

 */

DEFUN (cumsum, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cumsum (@var{x}, @var{dim})\n\
Cumulative sum of elements along dimension @var{dim}.  If @var{dim}\n\
is omitted, it defaults to 1 (column-wise cumulative sums).\n\
\n\
As a special case, if @var{x} is a vector and @var{dim} is omitted,\n\
return the cumulative sum of the elements as a vector with the\n\
same orientation as @var{x}.\n\
@end deftypefn")
{
  DATA_REDUCTION (cumsum);
}

/*

%!assert (cumsum ([1, 2, 3]), [1, 3, 6]);
%!assert (cumsum ([-1; -2; -3]), [-1; -3; -6]);
%!assert (cumsum ([i, 2+i, -3+2i, 4]), [i, 2+2i, -1+4i, 3+4i]);
%!assert (cumsum ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [1, 2, 3; 1+i, 2+2i, 3+3i; 2+2i, 4+4i, 6+6i]);

%!assert (cumsum (single([1, 2, 3])), single([1, 3, 6]));
%!assert (cumsum (single([-1; -2; -3])), single([-1; -3; -6]));
%!assert (cumsum (single([i, 2+i, -3+2i, 4])), single([i, 2+2i, -1+4i, 3+4i]));
%!assert (cumsum (single([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), single([1, 2, 3; 1+i, 2+2i, 3+3i; 2+2i, 4+4i, 6+6i]));

%!error <Invalid call to cumsum.*> cumsum ();

%!assert (cumsum ([1, 2; 3, 4], 1), [1, 2; 4, 6]);
%!assert (cumsum ([1, 2; 3, 4], 2), [1, 3; 3, 7]);

%!assert (cumsum (single([1, 2; 3, 4]), 1), single([1, 2; 4, 6]));
%!assert (cumsum (single([1, 2; 3, 4]), 2), single([1, 3; 3, 7]));

 */

DEFUN (diag, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} diag (@var{v}, @var{k})\n\
Return a diagonal matrix with vector @var{v} on diagonal @var{k}.  The\n\
second argument is optional.  If it is positive, the vector is placed on\n\
the @var{k}-th super-diagonal.  If it is negative, it is placed on the\n\
@var{-k}-th sub-diagonal.  The default value of @var{k} is 0, and the\n\
vector is placed on the main diagonal.  For example,\n\
\n\
@example\n\
@group\n\
diag ([1, 2, 3], 1)\n\
     @result{}  0  1  0  0\n\
         0  0  2  0\n\
         0  0  0  3\n\
         0  0  0  0\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Given a matrix argument, instead of a vector, @code{diag} extracts the\n\
@var{k}-th diagonal of the matrix.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).diag();
  else if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    {
      octave_idx_type k = args(1).int_value ();

      if (error_state)
	error ("diag: invalid second argument");      
      else
	retval = args(0).diag(k);
    }
  else if (nargin == 3)
    {
      octave_value arg0 = args(0);
      if (arg0.ndims () == 2 && (args(0).rows () == 1 || args(0).columns () == 1))
        {
          octave_idx_type m = args(1).int_value (), n = args(2).int_value ();
          if (! error_state)
            retval = arg0.diag ().resize (dim_vector (m, n));
          else
            error ("diag: invalid dimensions");
        }
      else
        error ("diag: first argument must be a vector");
    }
  else
    print_usage ();

  return retval;
}

/*

%!assert(full (diag ([1; 2; 3])), [1, 0, 0; 0, 2, 0; 0, 0, 3]);
%!assert(diag ([1; 2; 3], 1), [0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]);
%!assert(diag ([1; 2; 3], 2), [0, 0, 1, 0, 0; 0, 0, 0, 2, 0; 0, 0, 0, 0, 3; 0, 0, 0, 0, 0; 0, 0, 0, 0, 0]);
%!assert(diag ([1; 2; 3],-1), [0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]);
%!assert(diag ([1; 2; 3],-2), [0, 0, 0, 0, 0; 0, 0, 0, 0, 0; 1, 0, 0, 0, 0; 0, 2, 0, 0, 0; 0, 0, 3, 0, 0]);

%!assert(diag ([1, 0, 0; 0, 2, 0; 0, 0, 3]), [1; 2; 3]);
%!assert(diag ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0], 1), [1; 2; 3]);
%!assert(diag ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0], -1), [1; 2; 3]);

%!assert(full (diag (single([1; 2; 3]))), single([1, 0, 0; 0, 2, 0; 0, 0, 3]));
%!assert(diag (single([1; 2; 3]), 1), single([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]));
%!assert(diag (single([1; 2; 3]), 2), single([0, 0, 1, 0, 0; 0, 0, 0, 2, 0; 0, 0, 0, 0, 3; 0, 0, 0, 0, 0; 0, 0, 0, 0, 0]));
%!assert(diag (single([1; 2; 3]),-1), single([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]));
%!assert(diag (single([1; 2; 3]),-2), single([0, 0, 0, 0, 0; 0, 0, 0, 0, 0; 1, 0, 0, 0, 0; 0, 2, 0, 0, 0; 0, 0, 3, 0, 0]));

%!assert(diag (single([1, 0, 0; 0, 2, 0; 0, 0, 3])), single([1; 2; 3]));
%!assert(diag (single([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]), 1), single([1; 2; 3]));
%!assert(diag (single([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]), -1), single([1; 2; 3]));

%!assert(diag (int8([1; 2; 3])), int8([1, 0, 0; 0, 2, 0; 0, 0, 3]));
%!assert(diag (int8([1; 2; 3]), 1), int8([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]));
%!assert(diag (int8([1; 2; 3]), 2), int8([0, 0, 1, 0, 0; 0, 0, 0, 2, 0; 0, 0, 0, 0, 3; 0, 0, 0, 0, 0; 0, 0, 0, 0, 0]));
%!assert(diag (int8([1; 2; 3]),-1), int8([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]));
%!assert(diag (int8([1; 2; 3]),-2), int8([0, 0, 0, 0, 0; 0, 0, 0, 0, 0; 1, 0, 0, 0, 0; 0, 2, 0, 0, 0; 0, 0, 3, 0, 0]));

%!assert(diag (int8([1, 0, 0; 0, 2, 0; 0, 0, 3])), int8([1; 2; 3]));
%!assert(diag (int8([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]), 1), int8([1; 2; 3]));
%!assert(diag (int8([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]), -1), int8([1; 2; 3]));

%!error <Invalid call to diag.*> diag ();

 */

DEFUN (prod, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} prod (@var{x}, @var{dim})\n\
Product of elements along dimension @var{dim}.  If @var{dim} is\n\
omitted, it defaults to 1 (column-wise products).\n\
\n\
As a special case, if @var{x} is a vector and @var{dim} is omitted,\n\
return the product of the elements.\n\
@end deftypefn")
{
  DATA_REDUCTION (prod);
}

/*

%!assert (prod ([1, 2, 3]), 6);
%!assert (prod ([-1; -2; -3]), -6);
%!assert (prod ([i, 2+i, -3+2i, 4]), -4 - 32i);
%!assert (prod ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [-1+i, -8+8i, -27+27i]);

%!assert (prod (single([1, 2, 3])), single(6));
%!assert (prod (single([-1; -2; -3])), single(-6));
%!assert (prod (single([i, 2+i, -3+2i, 4])), single(-4 - 32i));
%!assert (prod (single([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), single([-1+i, -8+8i, -27+27i]));

%!error <Invalid call to prod.*> prod ();

%!assert (prod ([1, 2; 3, 4], 1), [3, 8]);
%!assert (prod ([1, 2; 3, 4], 2), [2; 12]);
%!assert (prod (zeros (1, 0)), 1);
%!assert (prod (zeros (1, 0), 1), zeros (1, 0));
%!assert (prod (zeros (1, 0), 2), 1);
%!assert (prod (zeros (0, 1)), 1);
%!assert (prod (zeros (0, 1), 1), 1);
%!assert (prod (zeros (0, 1), 2), zeros (0, 1));
%!assert (prod (zeros (2, 0)), zeros (1, 0));
%!assert (prod (zeros (2, 0), 1), zeros (1, 0));
%!assert (prod (zeros (2, 0), 2), [1; 1]);
%!assert (prod (zeros (0, 2)), [1, 1]);
%!assert (prod (zeros (0, 2), 1), [1, 1]);
%!assert (prod (zeros (0, 2), 2), zeros(0, 1));

%!assert (prod (single([1, 2; 3, 4]), 1), single([3, 8]));
%!assert (prod (single([1, 2; 3, 4]), 2), single([2; 12]));
%!assert (prod (zeros (1, 0, 'single')), single(1));
%!assert (prod (zeros (1, 0, 'single'), 1), zeros (1, 0, 'single'));
%!assert (prod (zeros (1, 0, 'single'), 2), single(1));
%!assert (prod (zeros (0, 1, 'single')), single(1));
%!assert (prod (zeros (0, 1, 'single'), 1), single(1));
%!assert (prod (zeros (0, 1, 'single'), 2), zeros (0, 1, 'single'));
%!assert (prod (zeros (2, 0, 'single')), zeros (1, 0, 'single'));
%!assert (prod (zeros (2, 0, 'single'), 1), zeros (1, 0, 'single'));
%!assert (prod (zeros (2, 0, 'single'), 2), single([1; 1]));
%!assert (prod (zeros (0, 2, 'single')), single([1, 1]));
%!assert (prod (zeros (0, 2, 'single'), 1), single([1, 1]));
%!assert (prod (zeros (0, 2, 'single'), 2), zeros(0, 1, 'single'));

 */

#define SINGLE_TYPE_CONCAT(TYPE, EXTRACTOR) \
  do \
    { \
      int dv_len = dv.length (); \
      Array<octave_idx_type> ra_idx (dv_len > 1 ? dv_len : 2, 0); \
      \
      for (int j = 1; j < n_args; j++) \
	{ \
	  OCTAVE_QUIT; \
	  \
	  TYPE ra = args(j).EXTRACTOR ();	\
	  \
	  if (! error_state) \
	    { \
	      result.insert (ra, ra_idx); \
	      \
	      if (error_state) \
	        return retval; \
	      \
	      dim_vector dv_tmp = args (j).dims (); \
	      \
	      if (dim >= dv_len) \
	        { \
		  if (j > 1) \
		    error ("%s: indexing error", fname.c_str ()); \
		  break; \
		} \
	      else \
		ra_idx (dim) += (dim < dv_tmp.length () ? dv_tmp (dim) : 1); \
	    } \
	} \
    } \
 while (0)

#define DO_SINGLE_TYPE_CONCAT(TYPE, EXTRACTOR) \
  do \
    { \
      TYPE result (dv); \
      \
      SINGLE_TYPE_CONCAT(TYPE, EXTRACTOR); \
      \
      retval = result; \
    } \
 while (0)

static octave_value
do_cat (const octave_value_list& args, std::string fname)
{
  octave_value retval;

  int n_args = args.length (); 

  if (n_args == 1)
    retval = Matrix ();
  else if (n_args == 2)
    retval = args(1);
  else if (n_args > 2)
    {
      octave_idx_type dim = args(0).int_value () - 1;

      if (error_state)
	{
	  error ("cat: expecting first argument to be a integer");
	  return retval;
	}
  
      if (dim >= 0)
	{
 	  
 	  dim_vector  dv = args(1).dims ();
	  std::string result_type = args(1).class_name ();
	  
	  bool all_sq_strings_p = args(1).is_sq_string ();
	  bool all_dq_strings_p = args(1).is_dq_string ();
	  bool all_real_p = args(1).is_real_type ();
	  bool any_sparse_p = args(1).is_sparse_type();

 	  for (int i = 2; i < args.length (); i++)
  	    {
 	      // add_dims constructs a dimension vector which holds the
	      // dimensions of the final array after concatenation.

	      if (! dv.concat (args(i).dims (), dim))
		{
		  // Dimensions do not match. 
		  error ("cat: dimension mismatch");
		  return retval;
		}
	      
	      result_type = 
		get_concat_class (result_type, args(i).class_name ());

	      if (all_sq_strings_p && ! args(i).is_sq_string ())
		all_sq_strings_p = false;
	      if (all_dq_strings_p && ! args(i).is_dq_string ())
		all_dq_strings_p = false;
	      if (all_real_p && ! args(i).is_real_type ())
		all_real_p = false;
	      if (!any_sparse_p && args(i).is_sparse_type ())
		any_sparse_p = true;
	    }

	  if (result_type == "double")
	    {
	      if (any_sparse_p)
		{	    
		  if (all_real_p)
		    DO_SINGLE_TYPE_CONCAT (SparseMatrix, sparse_matrix_value);
		  else
		    DO_SINGLE_TYPE_CONCAT (SparseComplexMatrix, sparse_complex_matrix_value);
		}
	      else
		{
		  if (all_real_p)
		    DO_SINGLE_TYPE_CONCAT (NDArray, array_value);
		  else
		    DO_SINGLE_TYPE_CONCAT (ComplexNDArray, complex_array_value);
		}
	    }
	  else if (result_type == "single")
	    {
	      if (all_real_p)
		DO_SINGLE_TYPE_CONCAT (FloatNDArray, float_array_value);
	      else
		DO_SINGLE_TYPE_CONCAT (FloatComplexNDArray, 
				       float_complex_array_value);
	    }
	  else if (result_type == "char")
	    {
	      char type = all_dq_strings_p ? '"' : '\'';

	      maybe_warn_string_concat (all_dq_strings_p, all_sq_strings_p);

	      charNDArray result (dv, Vstring_fill_char);

	      SINGLE_TYPE_CONCAT (charNDArray, char_array_value);

	      retval = octave_value (result, true, type);
	    }
	  else if (result_type == "logical")
	    {
	      if (any_sparse_p)
		DO_SINGLE_TYPE_CONCAT (SparseBoolMatrix, sparse_bool_matrix_value);
	      else
		DO_SINGLE_TYPE_CONCAT (boolNDArray, bool_array_value);
	    }
	  else if (result_type == "int8")
	    DO_SINGLE_TYPE_CONCAT (int8NDArray, int8_array_value);
	  else if (result_type == "int16")
	    DO_SINGLE_TYPE_CONCAT (int16NDArray, int16_array_value);
	  else if (result_type == "int32")
	    DO_SINGLE_TYPE_CONCAT (int32NDArray, int32_array_value);
	  else if (result_type == "int64")
	    DO_SINGLE_TYPE_CONCAT (int64NDArray, int64_array_value);
	  else if (result_type == "uint8")
	    DO_SINGLE_TYPE_CONCAT (uint8NDArray, uint8_array_value);
	  else if (result_type == "uint16")
	    DO_SINGLE_TYPE_CONCAT (uint16NDArray, uint16_array_value);
	  else if (result_type == "uint32")
	    DO_SINGLE_TYPE_CONCAT (uint32NDArray, uint32_array_value);
	  else if (result_type == "uint64")
	    DO_SINGLE_TYPE_CONCAT (uint64NDArray, uint64_array_value);
	  else
	    {
	      // The lines below might seem crazy, since we take a copy
	      // of the first argument, resize it to be empty and then resize
	      // it to be full. This is done since it means that there is no
	      // recopying of data, as would happen if we used a single resize.
	      // It should be noted that resize operation is also significantly 
	      // slower than the do_cat_op function, so it makes sense to have
	      // an empty matrix and copy all data.
	      //
	      // We might also start with a empty octave_value using
	      //   tmp = octave_value_typeinfo::lookup_type 
	      //                                (args(1).type_name());
	      // and then directly resize. However, for some types there might
	      // be some additional setup needed, and so this should be avoided.

	      octave_value tmp = args (1);
	      tmp = tmp.resize (dim_vector (0,0)).resize (dv);

	      if (error_state)
		return retval;

	      int dv_len = dv.length ();
	      Array<octave_idx_type> ra_idx (dv_len, 0);

	      for (int j = 1; j < n_args; j++)
		{
		  // Can't fast return here to skip empty matrices as something
		  // like cat(1,[],single([])) must return an empty matrix of
		  // the right type.
		  tmp = do_cat_op (tmp, args (j), ra_idx);

		  if (error_state)
		    return retval;

		  dim_vector dv_tmp = args (j).dims ();

		  if (dim >= dv_len)
		    {
		      if (j > 1)
			error ("%s: indexing error", fname.c_str ());
		      break;
		    }
		  else
		    ra_idx (dim) += (dim < dv_tmp.length () ? 
				     dv_tmp (dim) : 1);
		}
	      retval = tmp;
	    }

	  if (! error_state)
	    {
	      // Reshape, chopping trailing singleton dimensions
	      dv.chop_trailing_singletons ();
	      retval = retval.reshape (dv);
	    }
	}
      else
	error ("%s: invalid dimension argument", fname.c_str ());
    }
  else
    print_usage ();

  return retval;
}

DEFUN (horzcat, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} horzcat (@var{array1}, @var{array2}, @dots{}, @var{arrayN})\n\
Return the horizontal concatenation of N-d array objects, @var{array1},\n\
@var{array2}, @dots{}, @var{arrayN} along dimension 2.\n\
@seealso{cat, vertcat}\n\
@end deftypefn")
{
  octave_value_list args_tmp = args;
  
  int dim = 2;
  
  octave_value d (dim);
  
  args_tmp.prepend (d);
  
  return do_cat (args_tmp, "horzcat");
}

DEFUN (vertcat, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} vertcat (@var{array1}, @var{array2}, @dots{}, @var{arrayN})\n\
Return the vertical concatenation of N-d array objects, @var{array1},\n\
@var{array2}, @dots{}, @var{arrayN} along dimension 1.\n\
@seealso{cat, horzcat}\n\
@end deftypefn")
{
  octave_value_list args_tmp = args;
  
  int dim = 1;
  
  octave_value d (dim);
  
  args_tmp.prepend (d);
  
  return do_cat (args_tmp, "vertcat");
}

DEFUN (cat, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cat (@var{dim}, @var{array1}, @var{array2}, @dots{}, @var{arrayN})\n\
Return the concatenation of N-d array objects, @var{array1},\n\
@var{array2}, @dots{}, @var{arrayN} along dimension @var{dim}.\n\
\n\
@example\n\
@group\n\
A = ones (2, 2);\n\
B = zeros (2, 2);\n\
cat (2, A, B)\n\
@result{} ans =\n\
\n\
     1 1 0 0\n\
     1 1 0 0\n\
@end group\n\
@end example\n\
\n\
Alternatively, we can concatenate @var{A} and @var{B} along the\n\
second dimension the following way:\n\
\n\
@example\n\
@group\n\
[A, B].\n\
@end group\n\
@end example\n\
\n\
@var{dim} can be larger than the dimensions of the N-d array objects\n\
and the result will thus have @var{dim} dimensions as the\n\
following example shows:\n\
@example\n\
@group\n\
cat (4, ones(2, 2), zeros (2, 2))\n\
@result{} ans =\n\
\n\
   ans(:,:,1,1) =\n\
\n\
     1 1\n\
     1 1\n\
\n\
   ans(:,:,1,2) =\n\
     0 0\n\
     0 0\n\
@end group\n\
@end example\n\
@seealso{horzcat, vertcat}\n\
@end deftypefn")
{
  return do_cat (args, "cat");
}

/*

%!function ret = testcat (t1, t2, tr, cmplx)
%! assert (cat (1, cast ([], t1), cast([], t2)), cast ([], tr));
%!
%! assert (cat (1, cast (1, t1), cast (2, t2)), cast ([1; 2], tr));
%! assert (cat (1, cast (1, t1), cast ([2; 3], t2)), cast ([1; 2; 3], tr));
%! assert (cat (1, cast ([1; 2], t1), cast (3, t2)), cast ([1; 2; 3], tr));
%! assert (cat (1, cast ([1; 2], t1), cast ([3; 4], t2)), cast ([1; 2; 3; 4], tr));
%! assert (cat (2, cast (1, t1), cast (2, t2)), cast ([1, 2], tr));
%! assert (cat (2, cast (1, t1), cast ([2, 3], t2)), cast ([1, 2, 3], tr));
%! assert (cat (2, cast ([1, 2], t1), cast (3, t2)), cast ([1, 2, 3], tr));
%! assert (cat (2, cast ([1, 2], t1), cast ([3, 4], t2)), cast ([1, 2, 3, 4], tr));
%! 
%! assert ([cast(1, t1); cast(2, t2)], cast ([1; 2], tr));
%! assert ([cast(1, t1); cast([2; 3], t2)], cast ([1; 2; 3], tr));
%! assert ([cast([1; 2], t1); cast(3, t2)], cast ([1; 2; 3], tr));
%! assert ([cast([1; 2], t1); cast([3; 4], t2)], cast ([1; 2; 3; 4], tr));
%! assert ([cast(1, t1), cast(2, t2)], cast ([1, 2], tr));
%! assert ([cast(1, t1), cast([2, 3], t2)], cast ([1, 2, 3], tr));
%! assert ([cast([1, 2], t1), cast(3, t2)], cast ([1, 2, 3], tr));
%! assert ([cast([1, 2], t1), cast([3, 4], t2)], cast ([1, 2, 3, 4], tr));
%!
%! if (nargin == 3 || cmplx)
%!   assert (cat (1, cast (1i, t1), cast (2, t2)), cast ([1i; 2], tr));
%!   assert (cat (1, cast (1i, t1), cast ([2; 3], t2)), cast ([1i; 2; 3], tr));
%!   assert (cat (1, cast ([1i; 2], t1), cast (3, t2)), cast ([1i; 2; 3], tr));
%!   assert (cat (1, cast ([1i; 2], t1), cast ([3; 4], t2)), cast ([1i; 2; 3; 4], tr));
%!   assert (cat (2, cast (1i, t1), cast (2, t2)), cast ([1i, 2], tr));
%!   assert (cat (2, cast (1i, t1), cast ([2, 3], t2)), cast ([1i, 2, 3], tr));
%!   assert (cat (2, cast ([1i, 2], t1), cast (3, t2)), cast ([1i, 2, 3], tr));
%!   assert (cat (2, cast ([1i, 2], t1), cast ([3, 4], t2)), cast ([1i, 2, 3, 4], tr));
%! 
%!   assert ([cast(1i, t1); cast(2, t2)], cast ([1i; 2], tr));
%!   assert ([cast(1i, t1); cast([2; 3], t2)], cast ([1i; 2; 3], tr));
%!   assert ([cast([1i; 2], t1); cast(3, t2)], cast ([1i; 2; 3], tr));
%!   assert ([cast([1i; 2], t1); cast([3; 4], t2)], cast ([1i; 2; 3; 4], tr));
%!   assert ([cast(1i, t1), cast(2, t2)], cast ([1i, 2], tr));
%!   assert ([cast(1i, t1), cast([2, 3], t2)], cast ([1i, 2, 3], tr));
%!   assert ([cast([1i, 2], t1), cast(3, t2)], cast ([1i, 2, 3], tr));
%!   assert ([cast([1i, 2], t1), cast([3, 4], t2)], cast ([1i, 2, 3, 4], tr));
%!
%!   assert (cat (1, cast (1, t1), cast (2i, t2)), cast ([1; 2i], tr));
%!   assert (cat (1, cast (1, t1), cast ([2i; 3], t2)), cast ([1; 2i; 3], tr));
%!   assert (cat (1, cast ([1; 2], t1), cast (3i, t2)), cast ([1; 2; 3i], tr));
%!   assert (cat (1, cast ([1; 2], t1), cast ([3i; 4], t2)), cast ([1; 2; 3i; 4], tr));
%!   assert (cat (2, cast (1, t1), cast (2i, t2)), cast ([1, 2i], tr));
%!   assert (cat (2, cast (1, t1), cast ([2i, 3], t2)), cast ([1, 2i, 3], tr));
%!   assert (cat (2, cast ([1, 2], t1), cast (3i, t2)), cast ([1, 2, 3i], tr));
%!   assert (cat (2, cast ([1, 2], t1), cast ([3i, 4], t2)), cast ([1, 2, 3i, 4], tr));
%! 
%!   assert ([cast(1, t1); cast(2i, t2)], cast ([1; 2i], tr));
%!   assert ([cast(1, t1); cast([2i; 3], t2)], cast ([1; 2i; 3], tr));
%!   assert ([cast([1; 2], t1); cast(3i, t2)], cast ([1; 2; 3i], tr));
%!   assert ([cast([1; 2], t1); cast([3i; 4], t2)], cast ([1; 2; 3i; 4], tr));
%!   assert ([cast(1, t1), cast(2i, t2)], cast ([1, 2i], tr));
%!   assert ([cast(1, t1), cast([2i, 3], t2)], cast ([1, 2i, 3], tr));
%!   assert ([cast([1, 2], t1), cast(3i, t2)], cast ([1, 2, 3i], tr));
%!   assert ([cast([1, 2], t1), cast([3i, 4], t2)], cast ([1, 2, 3i, 4], tr));
%!
%!   assert (cat (1, cast (1i, t1), cast (2i, t2)), cast ([1i; 2i], tr));
%!   assert (cat (1, cast (1i, t1), cast ([2i; 3], t2)), cast ([1i; 2i; 3], tr));
%!   assert (cat (1, cast ([1i; 2], t1), cast (3i, t2)), cast ([1i; 2; 3i], tr));
%!   assert (cat (1, cast ([1i; 2], t1), cast ([3i; 4], t2)), cast ([1i; 2; 3i; 4], tr));
%!   assert (cat (2, cast (1i, t1), cast (2i, t2)), cast ([1i, 2i], tr));
%!   assert (cat (2, cast (1i, t1), cast ([2i, 3], t2)), cast ([1i, 2i, 3], tr));
%!   assert (cat (2, cast ([1i, 2], t1), cast (3i, t2)), cast ([1i, 2, 3i], tr));
%!   assert (cat (2, cast ([1i, 2], t1), cast ([3i, 4], t2)), cast ([1i, 2, 3i, 4], tr));
%! 
%!   assert ([cast(1i, t1); cast(2i, t2)], cast ([1i; 2i], tr));
%!   assert ([cast(1i, t1); cast([2i; 3], t2)], cast ([1i; 2i; 3], tr));
%!   assert ([cast([1i; 2], t1); cast(3i, t2)], cast ([1i; 2; 3i], tr));
%!   assert ([cast([1i; 2], t1); cast([3i; 4], t2)], cast ([1i; 2; 3i; 4], tr));
%!   assert ([cast(1i, t1), cast(2i, t2)], cast ([1i, 2i], tr));
%!   assert ([cast(1i, t1), cast([2i, 3], t2)], cast ([1i, 2i, 3], tr));
%!   assert ([cast([1i, 2], t1), cast(3i, t2)], cast ([1i, 2, 3i], tr));
%!   assert ([cast([1i, 2], t1), cast([3i, 4], t2)], cast ([1i, 2, 3i, 4], tr));
%! endif
%! ret = true;

%!assert (testcat('double', 'double', 'double'));
%!assert (testcat('single', 'double', 'single'));
%!assert (testcat('double', 'single', 'single'));
%!assert (testcat('single', 'single', 'single'));

%!assert (testcat('double', 'int8', 'int8', false));
%!assert (testcat('int8', 'double', 'int8', false));
%!assert (testcat('single', 'int8', 'int8', false));
%!assert (testcat('int8', 'single', 'int8', false));
%!assert (testcat('int8', 'int8', 'int8', false));
%!assert (testcat('double', 'int16', 'int16', false));
%!assert (testcat('int16', 'double', 'int16', false));
%!assert (testcat('single', 'int16', 'int16', false));
%!assert (testcat('int16', 'single', 'int16', false));
%!assert (testcat('int16', 'int16', 'int16', false));
%!assert (testcat('double', 'int32', 'int32', false));
%!assert (testcat('int32', 'double', 'int32', false));
%!assert (testcat('single', 'int32', 'int32', false));
%!assert (testcat('int32', 'single', 'int32', false));
%!assert (testcat('int32', 'int32', 'int32', false));
%!assert (testcat('double', 'int64', 'int64', false));
%!assert (testcat('int64', 'double', 'int64', false));
%!assert (testcat('single', 'int64', 'int64', false));
%!assert (testcat('int64', 'single', 'int64', false));
%!assert (testcat('int64', 'int64', 'int64', false));

%!assert (testcat('double', 'uint8', 'uint8', false));
%!assert (testcat('uint8', 'double', 'uint8', false));
%!assert (testcat('single', 'uint8', 'uint8', false));
%!assert (testcat('uint8', 'single', 'uint8', false));
%!assert (testcat('uint8', 'uint8', 'uint8', false));
%!assert (testcat('double', 'uint16', 'uint16', false));
%!assert (testcat('uint16', 'double', 'uint16', false));
%!assert (testcat('single', 'uint16', 'uint16', false));
%!assert (testcat('uint16', 'single', 'uint16', false));
%!assert (testcat('uint16', 'uint16', 'uint16', false));
%!assert (testcat('double', 'uint32', 'uint32', false));
%!assert (testcat('uint32', 'double', 'uint32', false));
%!assert (testcat('single', 'uint32', 'uint32', false));
%!assert (testcat('uint32', 'single', 'uint32', false));
%!assert (testcat('uint32', 'uint32', 'uint32', false));
%!assert (testcat('double', 'uint64', 'uint64', false));
%!assert (testcat('uint64', 'double', 'uint64', false));
%!assert (testcat('single', 'uint64', 'uint64', false));
%!assert (testcat('uint64', 'single', 'uint64', false));
%!assert (testcat('uint64', 'uint64', 'uint64', false));

*/

static octave_value
do_permute (const octave_value_list& args, bool inv)
{
  octave_value retval;

  if (args.length () == 2 && args(1).length () >= args(1).ndims ())
    {
      Array<int> vec = args(1).int_vector_value ();

      // FIXME -- maybe we should create an idx_vector object
      // here and pass that to permute?

      int n = vec.length ();

      for (int i = 0; i < n; i++)
	vec(i)--;

      octave_value ret = args(0).permute (vec, inv);

      if (! error_state)
	retval = ret;
    }
  else
    print_usage ();

  return retval;
}

DEFUN (permute, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} permute (@var{a}, @var{perm})\n\
Return the generalized transpose for an N-d array object @var{a}.\n\
The permutation vector @var{perm} must contain the elements\n\
@code{1:ndims(a)} (in any order, but each element must appear just once).\n\
@seealso{ipermute}\n\
@end deftypefn")
{
  return do_permute (args, false);
}

DEFUN (ipermute, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ipermute (@var{a}, @var{iperm})\n\
The inverse of the @code{permute} function.  The expression\n\
\n\
@example\n\
ipermute (permute (a, perm), perm)\n\
@end example\n\
returns the original array @var{a}.\n\
@seealso{permute}\n\
@end deftypefn")
{
  return do_permute (args, true);
}

DEFUN (length, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} length (@var{a})\n\
Return the `length' of the object @var{a}.  For matrix objects, the\n\
length is the number of rows or columns, whichever is greater (this\n\
odd definition is used for compatibility with @sc{Matlab}).\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      int len = args(0).length ();

      if (! error_state)
	retval = len;
    }
  else
    print_usage ();

  return retval;
}

DEFUN (ndims, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ndims (@var{a})\n\
Returns the number of dimensions of array @var{a}.\n\
For any array, the result will always be larger than or equal to 2.\n\
Trailing singleton dimensions are not counted.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      int n_dims = args(0).ndims ();

      if (! error_state)
	retval = n_dims;
    }
  else
    print_usage ();

  return retval;
}

DEFUN (numel, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} numel (@var{a})\n\
Returns the number of elements in the object @var{a}.\n\
@seealso{size}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      int numel = args(0).numel ();

      if (! error_state)
	{
	  if (numel < 0)
	    numel = 0;

	  retval = numel;
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (size, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} size (@var{a}, @var{n})\n\
Return the number rows and columns of @var{a}.\n\
\n\
With one input argument and one output argument, the result is returned\n\
in a row vector.  If there are multiple output arguments, the number of\n\
rows is assigned to the first, and the number of columns to the second,\n\
etc.  For example,\n\
\n\
@example\n\
@group\n\
size ([1, 2; 3, 4; 5, 6])\n\
     @result{} [ 3, 2 ]\n\
\n\
[nr, nc] = size ([1, 2; 3, 4; 5, 6])\n\
     @result{} nr = 3\n\
     @result{} nc = 2\n\
@end group\n\
@end example\n\
\n\
If given a second argument, @code{size} will return the size of the\n\
corresponding dimension.  For example\n\
\n\
@example\n\
size ([1, 2; 3, 4; 5, 6], 2)\n\
     @result{} 2\n\
@end example\n\
\n\
@noindent\n\
returns the number of columns in the given matrix.\n\
@seealso{numel}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      dim_vector dimensions = args(0).dims ();

      int ndims = dimensions.length ();

      Matrix m (1, ndims);

      if (nargout > 1)
	{
	  for (int i = nargout-1; i >= ndims; i--)
	    retval(i) = 1;

	  if (ndims > nargout)
	    {
	      octave_idx_type d = 1;

	      while (ndims >= nargout)
		d *= dimensions(--ndims);
	      
	      retval(ndims) = d;
	    }

	  while (ndims--)
	    retval(ndims) = dimensions(ndims);
	}
      else
	{
	  for (int i = 0; i < ndims; i++)
	    m(0, i) = dimensions(i);

	  retval(0) = m;
	}
    }
  else if (nargin == 2 && nargout < 2)
    {
      octave_idx_type nd = args(1).int_value (true);

      if (error_state)
	error ("size: expecting scalar as second argument");
      else
	{
	  dim_vector dv = args(0).dims ();

	  if (nd > 0)
	    {
	      if (nd <= dv.length ())
		retval(0) = dv(nd-1);
	      else 
		retval(0) = 1;
	    }
	  else
	    error ("size: requested dimension (= %d) out of range", nd);
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (size_equal, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} size_equal (@var{a}, @var{b}, @dots{})\n\
Return true if the dimensions of all arguments agree.\n\
Trailing singleton dimensions are ignored.\n\
@seealso{size, numel}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin >= 2)
    {
      retval = true;

      dim_vector a_dims = args(0).dims ();
      a_dims.chop_trailing_singletons ();

      for (int i = 1; i < nargin; ++i)
        {
          dim_vector b_dims = args(i).dims ();
          b_dims.chop_trailing_singletons ();

          if (a_dims != b_dims)
	    {
	      retval = false;
	      break;
	    }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (nnz, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{scalar} =} nnz (@var{a})\n\
Returns the number of non zero elements in @var{a}.\n\
@seealso{sparse}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).nnz ();
  else
    print_usage ();

  return retval;
}

DEFUN (nzmax, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{scalar} =} nzmax (@var{SM})\n\
Return the amount of storage allocated to the sparse matrix @var{SM}.\n\
Note that Octave tends to crop unused memory at the first opportunity\n\
for sparse objects. There are some cases of user created sparse objects\n\
where the value returned by @dfn{nzmaz} will not be the same as @dfn{nnz},\n\
but in general they will give the same result.\n\
@seealso{sparse, spalloc}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length() == 1)
    retval = args(0).nzmax ();
  else
    print_usage ();

  return retval;
}

DEFUN (rows, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} rows (@var{a})\n\
Return the number of rows of @var{a}.\n\
@seealso{size, numel, columns, length, isscalar, isvector, ismatrix}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).rows ();
  else
    print_usage ();

  return retval;
}

DEFUN (columns, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} columns (@var{a})\n\
Return the number of columns of @var{a}.\n\
@seealso{size, numel, rows, length, isscalar, isvector, ismatrix}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).columns ();
  else
    print_usage ();

  return retval;
}

DEFUN (sum, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} sum (@var{x}, @var{dim})\n\
@deftypefnx {Built-in Function} {} sum (@dots{}, 'native')\n\
Sum of elements along dimension @var{dim}.  If @var{dim} is\n\
omitted, it defaults to 1 (column-wise sum).\n\
\n\
As a special case, if @var{x} is a vector and @var{dim} is omitted,\n\
return the sum of the elements.\n\
\n\
If the optional argument 'native' is given, then the sum is performed\n\
in the same type as the original argument, rather than in the default\n\
double type. For example\n\
\n\
@example\n\
sum ([true, true])\n\
  @result{} 2\n\
sum ([true, true], 'native')\n\
  @result{} true\n\
@end example\n\
@end deftypefn")
{
  NATIVE_REDUCTION (sum);
}

/*

%!assert (sum([true,true]), 2)
%!assert (sum([true,true],'native'), true)
%!assert (sum(int8([127,10,-20])), 117);
%!assert (sum(int8([127,10,-20]),'native'), int8(107));

%!assert(sum ([1, 2, 3]), 6)
%!assert(sum ([-1; -2; -3]), -6);
%!assert(sum ([i, 2+i, -3+2i, 4]), 3+4i);
%!assert(sum ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [2+2i, 4+4i, 6+6i]);

%!assert(sum (single([1, 2, 3])), single(6))
%!assert(sum (single([-1; -2; -3])), single(-6));
%!assert(sum (single([i, 2+i, -3+2i, 4])), single(3+4i));
%!assert(sum (single([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), single([2+2i, 4+4i, 6+6i]));

%!error <Invalid call to sum.*> sum ();

%!assert (sum ([1, 2; 3, 4], 1), [4, 6]);
%!assert (sum ([1, 2; 3, 4], 2), [3; 7]);
%!assert (sum (zeros (1, 0)), 0);
%!assert (sum (zeros (1, 0), 1), zeros(1, 0));
%!assert (sum (zeros (1, 0), 2), 0);
%!assert (sum (zeros (0, 1)), 0);
%!assert (sum (zeros (0, 1), 1), 0);
%!assert (sum (zeros (0, 1), 2), zeros(0, 1));
%!assert (sum (zeros (2, 0)),  zeros(1, 0));
%!assert (sum (zeros (2, 0), 1), zeros(1, 0));
%!assert (sum (zeros (2, 0), 2),  [0; 0]);
%!assert (sum (zeros (0, 2)), [0, 0]);
%!assert (sum (zeros (0, 2), 1), [0, 0]);
%!assert (sum (zeros (0, 2), 2), zeros(0, 1));
%!assert (sum (zeros (2, 2, 0, 3)), zeros(1, 2, 0, 3));
%!assert (sum (zeros (2, 2, 0, 3), 2), zeros(2, 1, 0, 3));
%!assert (sum (zeros (2, 2, 0, 3), 3), zeros(2, 2, 1, 3));
%!assert (sum (zeros (2, 2, 0, 3), 4), zeros(2, 2, 0));
%!assert (sum (zeros (2, 2, 0, 3), 7), zeros(2, 2, 0, 3));

%!assert (sum (single([1, 2; 3, 4]), 1), single([4, 6]));
%!assert (sum (single([1, 2; 3, 4]), 2), single([3; 7]));
%!assert (sum (zeros (1, 0, 'single')), single(0));
%!assert (sum (zeros (1, 0, 'single'), 1), zeros(1, 0, 'single'));
%!assert (sum (zeros (1, 0, 'single'), 2), single(0));
%!assert (sum (zeros (0, 1, 'single')), single(0));
%!assert (sum (zeros (0, 1, 'single'), 1), single(0));
%!assert (sum (zeros (0, 1, 'single'), 2), zeros(0, 1, 'single'));
%!assert (sum (zeros (2, 0, 'single')),  zeros(1, 0, 'single'));
%!assert (sum (zeros (2, 0, 'single'), 1), zeros(1, 0, 'single'));
%!assert (sum (zeros (2, 0, 'single'), 2),  single([0; 0]));
%!assert (sum (zeros (0, 2, 'single')), single([0, 0]));
%!assert (sum (zeros (0, 2, 'single'), 1), single([0, 0]));
%!assert (sum (zeros (0, 2, 'single'), 2), zeros(0, 1, 'single'));
%!assert (sum (zeros (2, 2, 0, 3, 'single')), zeros(1, 2, 0, 3, 'single'));
%!assert (sum (zeros (2, 2, 0, 3, 'single'), 2), zeros(2, 1, 0, 3, 'single'));
%!assert (sum (zeros (2, 2, 0, 3, 'single'), 3), zeros(2, 2, 1, 3, 'single'));
%!assert (sum (zeros (2, 2, 0, 3, 'single'), 4), zeros(2, 2, 0, 'single'));
%!assert (sum (zeros (2, 2, 0, 3, 'single'), 7), zeros(2, 2, 0, 3, 'single'));

*/

DEFUN (sumsq, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} sumsq (@var{x}, @var{dim})\n\
Sum of squares of elements along dimension @var{dim}.  If @var{dim}\n\
is omitted, it defaults to 1 (column-wise sum of squares).\n\
\n\
As a special case, if @var{x} is a vector and @var{dim} is omitted,\n\
return the sum of squares of the elements.\n\
\n\
This function is conceptually equivalent to computing\n\
@example\n\
sum (x .* conj (x), dim)\n\
@end example\n\
but it uses less memory and avoids calling conj if @var{x} is real.\n\
@end deftypefn")
{
  DATA_REDUCTION (sumsq);
}

/*

%!assert(sumsq ([1, 2, 3]), 14)
%!assert(sumsq ([-1; -2; 4i]), 21);
%!assert(sumsq ([1, 2, 3; 2, 3, 4; 4i, 6i, 2]), [21, 49, 29]);

%!assert(sumsq (single([1, 2, 3])), single(14))
%!assert(sumsq (single([-1; -2; 4i])), single(21));
%!assert(sumsq (single([1, 2, 3; 2, 3, 4; 4i, 6i, 2])), single([21, 49, 29]));

%!error <Invalid call to sumsq.*> sumsq ();

%!assert (sumsq ([1, 2; 3, 4], 1), [10, 20]);
%!assert (sumsq ([1, 2; 3, 4], 2), [5; 25]);

%!assert (sumsq (single([1, 2; 3, 4]), 1), single([10, 20]));
%!assert (sumsq (single([1, 2; 3, 4]), 2), single([5; 25]));

 */

DEFUN (islogical, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} islogical (@var{x})\n\
Return true if @var{x} is a logical object.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_bool_type ();
  else
    print_usage ();

  return retval;
}

DEFALIAS (isbool, islogical);

/*

%!assert (islogical(true), true)
%!assert (islogical(false), true)
%!assert (islogical([true, false]), true)
%!assert (islogical(1), false)
%!assert (islogical(1i), false)
%!assert (islogical([1,1]), false)
%!assert (islogical(single(1)), false)
%!assert (islogical(single(1i)), false)
%!assert (islogical(single([1,1])), false)

 */

DEFUN (isinteger, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isinteger (@var{x})\n\
Return true if @var{x} is an integer object (int8, uint8, int16, etc.).\n\
Note that @code{isinteger (14)} is false because numeric constants in\n\
Octave are double precision floating point values.\n\
@seealso{isreal, isnumeric, class, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_integer_type ();
  else
    print_usage ();

  return retval;
}

DEFUN (iscomplex, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscomplex (@var{x})\n\
Return true if @var{x} is a complex-valued numeric object.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_complex_type ();
  else
    print_usage ();

  return retval;
}

DEFUN (isfloat, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isfloat (@var{x})\n\
Return true if @var{x} is a floating-point numeric object.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_float_type ();
  else
    print_usage ();

  return retval;
}

// FIXME -- perhaps this should be implemented with an
// octave_value member function?

DEFUN (complex, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} complex (@var{val})\n\
@deftypefnx {Built-in Function} {} complex (@var{re}, @var{im})\n\
Convert @var{x} to a complex value.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

      if (arg.is_complex_type ())
	retval = arg;
      else
	{
	  if (arg.is_sparse_type ())
	    {
	      SparseComplexMatrix val = arg.sparse_complex_matrix_value ();

	      if (! error_state)
		retval = octave_value (new octave_sparse_complex_matrix (val));
	    }
	  else if (arg.is_single_type ())
	    {
	      if (arg.numel () == 1)
		{
		  FloatComplex val = arg.float_complex_value ();

		  if (! error_state)
		    retval = octave_value (new octave_float_complex (val));
		}
	      else
		{
		  FloatComplexNDArray val = arg.float_complex_array_value ();

		  if (! error_state)
		    retval = octave_value (new octave_float_complex_matrix (val));
		}
	    }
	  else
	    {
	      if (arg.numel () == 1)
		{
		  Complex val = arg.complex_value ();

		  if (! error_state)
		    retval = octave_value (new octave_complex (val));
		}
	      else
		{
		  ComplexNDArray val = arg.complex_array_value ();

		  if (! error_state)
		    retval = octave_value (new octave_complex_matrix (val));
		}
	    }

	  if (error_state)
	    error ("complex: invalid conversion");
	}
    }
  else if (nargin == 2)
    {
      octave_value re = args(0);
      octave_value im = args(1);

      if (re.is_sparse_type () && im.is_sparse_type ())
	{
	  const SparseMatrix re_val = re.sparse_matrix_value ();
	  const SparseMatrix im_val = im.sparse_matrix_value ();

	  if (!error_state)
	    {
	      if (re.numel () == 1)
		{
		  SparseComplexMatrix result;
		  if (re_val.nnz () == 0)
		    result = Complex(0, 1) * SparseComplexMatrix (im_val);
		  else
		    {
		      result = SparseComplexMatrix (im_val.dims (), re_val (0));
		      octave_idx_type nr = im_val.rows ();
		      octave_idx_type nc = im_val.cols ();

		      for (octave_idx_type j = 0; j < nc; j++)
			{
			  octave_idx_type off = j * nr;
			  for (octave_idx_type i = im_val.cidx(j); 
			       i < im_val.cidx(j + 1); i++)
			    result.data (im_val.ridx(i) + off) =  
			      result.data (im_val.ridx(i) + off) + 
			      Complex (0, im_val.data (i));
			}
		    }
		  retval = octave_value (new octave_sparse_complex_matrix (result));
		}
	      else if (im.numel () == 1)
		{
		  SparseComplexMatrix result;
		  if (im_val.nnz () == 0)
		    result = SparseComplexMatrix (re_val);
		  else
		    {
		      result = SparseComplexMatrix (re_val.rows(), re_val.cols(), Complex(0, im_val (0)));
		      octave_idx_type nr = re_val.rows ();
		      octave_idx_type nc = re_val.cols ();

		      for (octave_idx_type j = 0; j < nc; j++)
			{
			  octave_idx_type off = j * nr;
			  for (octave_idx_type i = re_val.cidx(j); 
			       i < re_val.cidx(j + 1); i++)
			    result.data (re_val.ridx(i) + off) =  
			      result.data (re_val.ridx(i) + off) + 
			      re_val.data (i);
			}
		    }
		  retval = octave_value (new octave_sparse_complex_matrix (result));
		}
	      else
		{
		  if (re_val.dims () == im_val.dims ())
		    {
		      SparseComplexMatrix result = SparseComplexMatrix(re_val) 
			+ Complex(0, 1) * SparseComplexMatrix (im_val);
		      retval = octave_value (new octave_sparse_complex_matrix (result));
		    }
		  else
		    error ("complex: dimension mismatch");
		}
	    }
	}
      else if (re.is_single_type () || im.is_single_type ())
	{
	  if (re.numel () == 1)
	    {
	      float re_val = re.float_value ();

	      if (im.numel () == 1)
		{
		  float im_val = im.double_value ();

		  if (! error_state)
		    retval = octave_value (new octave_float_complex (FloatComplex (re_val, im_val)));
		}
	      else
		{
		  const FloatNDArray im_val = im.float_array_value ();

		  if (! error_state)
		    {
		      FloatComplexNDArray result (im_val.dims (), FloatComplex ());

		      for (octave_idx_type i = 0; i < im_val.numel (); i++)
			result.xelem (i) = FloatComplex (re_val, im_val(i));

		      retval = octave_value (new octave_float_complex_matrix (result));
		    }
		}
	    }
	  else
	    {
	      const FloatNDArray re_val = re.float_array_value ();

	      if (im.numel () == 1)
		{
		  float im_val = im.float_value ();

		  if (! error_state)
		    {
		      FloatComplexNDArray result (re_val.dims (), FloatComplex ());

		      for (octave_idx_type i = 0; i < re_val.numel (); i++)
			result.xelem (i) = FloatComplex (re_val(i), im_val);

		      retval = octave_value (new octave_float_complex_matrix (result));
		    }
		}
	      else
		{
		  const FloatNDArray im_val = im.float_array_value ();

		  if (! error_state)
		    {
		      if (re_val.dims () == im_val.dims ())
			{
			  FloatComplexNDArray result (re_val.dims (), FloatComplex ());

			  for (octave_idx_type i = 0; i < re_val.numel (); i++)
			    result.xelem (i) = FloatComplex (re_val(i), im_val(i));

			  retval = octave_value (new octave_float_complex_matrix (result));
			}
		      else
			error ("complex: dimension mismatch");
		    }
		}
	    }
	}
      else if (re.numel () == 1)
	{
	  double re_val = re.double_value ();

	  if (im.numel () == 1)
	    {
	      double im_val = im.double_value ();

	      if (! error_state)
		retval = octave_value (new octave_complex (Complex (re_val, im_val)));
	    }
	  else
	    {
	      const NDArray im_val = im.array_value ();

	      if (! error_state)
		{
		  ComplexNDArray result (im_val.dims (), Complex ());

		  for (octave_idx_type i = 0; i < im_val.numel (); i++)
		    result.xelem (i) = Complex (re_val, im_val(i));

		  retval = octave_value (new octave_complex_matrix (result));
		}
	    }
	}
      else
	{
	  const NDArray re_val = re.array_value ();

	  if (im.numel () == 1)
	    {
	      double im_val = im.double_value ();

	      if (! error_state)
		{
		  ComplexNDArray result (re_val.dims (), Complex ());

		  for (octave_idx_type i = 0; i < re_val.numel (); i++)
		    result.xelem (i) = Complex (re_val(i), im_val);

		  retval = octave_value (new octave_complex_matrix (result));
		}
	    }
	  else
	    {
	      const NDArray im_val = im.array_value ();

	      if (! error_state)
		{
		  if (re_val.dims () == im_val.dims ())
		    {
		      ComplexNDArray result (re_val.dims (), Complex ());

		      for (octave_idx_type i = 0; i < re_val.numel (); i++)
			result.xelem (i) = Complex (re_val(i), im_val(i));

		      retval = octave_value (new octave_complex_matrix (result));
		    }
		  else
		    error ("complex: dimension mismatch");
		}
	    }
	}

      if (error_state)
	error ("complex: invalid conversion");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (isreal, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isreal (@var{x})\n\
Return true if @var{x} is a real-valued numeric object.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_real_type ();
  else
    print_usage ();

  return retval;
}

DEFUN (isempty, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isempty (@var{a})\n\
Return 1 if @var{a} is an empty matrix (either the number of rows, or\n\
the number of columns, or both are zero).  Otherwise, return 0.\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    retval = args(0).is_empty ();
  else
    print_usage ();

  return retval;
}

DEFUN (isnumeric, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isnumeric (@var{x})\n\
Return nonzero if @var{x} is a numeric object.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_numeric_type ();
  else
    print_usage ();

  return retval;
}

DEFUN (islist, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} islist (@var{x})\n\
Return nonzero if @var{x} is a list.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_list ();
  else
    print_usage ();

  return retval;
}

DEFUN (ismatrix, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ismatrix (@var{a})\n\
Return 1 if @var{a} is a matrix.  Otherwise, return 0.\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      octave_value arg = args(0);

      if (arg.is_scalar_type () || arg.is_range ())
	retval = true;
      else if (arg.is_matrix_type ())
	retval = (arg.rows () >= 1 && arg.columns () >= 1);
    }
  else
    print_usage ();

  return retval;
}

/*

%!assert(ismatrix (1));
%!assert(ismatrix ([1, 2, 3]));
%!assert(ismatrix ([1, 2; 3, 4]));

%% Yes, this is right, ismatrix() checks for non-empty matrices.
%!assert(ismatrix ([]), false);

%!assert(ismatrix (single(1)));
%!assert(ismatrix (single([1, 2, 3])));
%!assert(ismatrix (single([1, 2; 3, 4])));

%% Yes, this is right, ismatrix() checks for non-empty matrices.
%!assert(ismatrix (single([])), false);

%!assert(ismatrix ("t"), false);
%!assert(ismatrix ("test"), false);
%!assert(ismatrix (["test"; "ing"]), false);

%!test
%! s.a = 1;
%! assert(ismatrix (s), false);

%!error <Invalid call to ismatrix.*> ismatrix ();
%!error <Invalid call to ismatrix.*> ismatrix ([1, 2; 3, 4], 2);

 */

static octave_value
fill_matrix (const octave_value_list& args, int val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);
  
  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
	return retval;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
	dims.resize (nargin);

	for (int i = 0; i < nargin; i++)
	  {
	    dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

	    if (error_state)
	      {
		error ("%s: expecting scalar integer arguments", fcn);
		break;
	      }
	  }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // FIXME -- perhaps this should be made extensible by
      // using the class name to lookup a function to call to create
      // the new value.

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
	{
	  switch (dt)
	    {
	    case oct_data_conv::dt_int8:
	      retval = int8NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_uint8:
	      retval = uint8NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_int16:
	      retval = int16NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_uint16:
	      retval = uint16NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_int32:
	      retval = int32NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_uint32:
	      retval = uint32NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_int64:
	      retval = int64NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_uint64:
	      retval = uint64NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_single:
	      retval = FloatNDArray (dims, val);
	      break;

	    case oct_data_conv::dt_double:
	      retval = NDArray (dims, val);
	      break;

	    case oct_data_conv::dt_logical:
	      retval = boolNDArray (dims, val);
	      break;

	    default:
	      error ("%s: invalid class name", fcn);
	      break;
	    }
	}
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, double val, float fval, 
	     const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);
  
  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
	return retval;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
	dims.resize (nargin);

	for (int i = 0; i < nargin; i++)
	  {
	    dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

	    if (error_state)
	      {
		error ("%s: expecting scalar integer arguments", fcn);
		break;
	      }
	  }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
	{
	  switch (dt)
	    {
	    case oct_data_conv::dt_single:
	      retval = FloatNDArray (dims, fval);
	      break;

	    case oct_data_conv::dt_double:
	      retval = NDArray (dims, val);
	      break;

	    default:
	      error ("%s: invalid class name", fcn);
	      break;
	    }
	}
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, double val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);
  
  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
	return retval;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
	dims.resize (nargin);

	for (int i = 0; i < nargin; i++)
	  {
	    dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

	    if (error_state)
	      {
		error ("%s: expecting scalar integer arguments", fcn);
		break;
	      }
	  }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
	{
	  switch (dt)
	    {
	    case oct_data_conv::dt_single:
	      retval = FloatNDArray (dims, static_cast <float> (val));
	      break;

	    case oct_data_conv::dt_double:
	      retval = NDArray (dims, val);
	      break;

	    default:
	      error ("%s: invalid class name", fcn);
	      break;
	    }
	}
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, const Complex& val,
	     const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);
  
  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
	return retval;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
	dims.resize (nargin);

	for (int i = 0; i < nargin; i++)
	  {
	    dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

	    if (error_state)
	      {
		error ("%s: expecting scalar integer arguments", fcn);
		break;
	      }
	  }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
	{
	  switch (dt)
	    {
	    case oct_data_conv::dt_single:
	      retval = FloatComplexNDArray (dims, static_cast<FloatComplex> (val));
	      break;

	    case oct_data_conv::dt_double:
	      retval = ComplexNDArray (dims, val);
	      break;

	    default:
	      error ("%s: invalid class name", fcn);
	      break;
	    }
	}
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, bool val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  dim_vector dims (1, 1);
  
  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
	dims.resize (nargin);

	for (int i = 0; i < nargin; i++)
	  {
	    dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

	    if (error_state)
	      {
		error ("%s: expecting scalar integer arguments", fcn);
		break;
	      }
	  }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
	retval = boolNDArray (dims, val);
    }

  return retval;
}

DEFUN (ones, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ones (@var{x})\n\
@deftypefnx {Built-in Function} {} ones (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} ones (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} ones (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all 1.\n\
The arguments are handled the same as the arguments for @code{eye}.\n\
\n\
If you need to create a matrix whose values are all the same, you should\n\
use an expression like\n\
\n\
@example\n\
val_matrix = val * ones (n, m)\n\
@end example\n\
\n\
The optional argument @var{class}, allows @code{ones} to return an array of\n\
the specified type, for example\n\
\n\
@example\n\
val = ones (n,m, \"uint8\")\n\
@end example\n\
@end deftypefn")
{
  return fill_matrix (args, 1, "ones");
}

/*

%!assert(ones (3), [1, 1, 1; 1, 1, 1; 1, 1, 1]);
%!assert(ones (2, 3), [1, 1, 1; 1, 1, 1]);
%!assert(ones (3, 2), [1, 1; 1, 1; 1, 1]);
%!assert(size (ones (3, 4, 5)),  [3, 4, 5]);

%!assert(ones (3,'single'), single([1, 1, 1; 1, 1, 1; 1, 1, 1]));
%!assert(ones (2, 3,'single'), single([1, 1, 1; 1, 1, 1]));
%!assert(ones (3, 2,'single'), single([1, 1; 1, 1; 1, 1]));
%!assert(size (ones (3, 4, 5, 'single')),  [3, 4, 5]);

%!assert(ones (3,'int8'), int8([1, 1, 1; 1, 1, 1; 1, 1, 1]));
%!assert(ones (2, 3,'int8'), int8([1, 1, 1; 1, 1, 1]));
%!assert(ones (3, 2,'int8'), int8([1, 1; 1, 1; 1, 1]));
%!assert(size (ones (3, 4, 5, 'int8')),  [3, 4, 5]);

 */

DEFUN (zeros, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} zeros (@var{x})\n\
@deftypefnx {Built-in Function} {} zeros (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} zeros (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} zeros (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all 0.\n\
The arguments are handled the same as the arguments for @code{eye}.\n\
\n\
The optional argument @var{class}, allows @code{zeros} to return an array of\n\
the specified type, for example\n\
\n\
@example\n\
val = zeros (n,m, \"uint8\")\n\
@end example\n\
@end deftypefn")
{
  return fill_matrix (args, 0, "zeros");
}

/*

%!assert(zeros (3), [0, 0, 0; 0, 0, 0; 0, 0, 0]);
%!assert(zeros (2, 3), [0, 0, 0; 0, 0, 0]);
%!assert(zeros (3, 2), [0, 0; 0, 0; 0, 0]);
%!assert(size (zeros (3, 4, 5)),  [3, 4, 5]);

%!assert(zeros (3,'single'), single([0, 0, 0; 0, 0, 0; 0, 0, 0]));
%!assert(zeros (2, 3,'single'), single([0, 0, 0; 0, 0, 0]));
%!assert(zeros (3, 2,'single'), single([0, 0; 0, 0; 0, 0]));
%!assert(size (zeros (3, 4, 5, 'single')),  [3, 4, 5]);

%!assert(zeros (3,'int8'), int8([0, 0, 0; 0, 0, 0; 0, 0, 0]));
%!assert(zeros (2, 3,'int8'), int8([0, 0, 0; 0, 0, 0]));
%!assert(zeros (3, 2,'int8'), int8([0, 0; 0, 0; 0, 0]));
%!assert(size (zeros (3, 4, 5, 'int8')),  [3, 4, 5]);

 */

DEFUN (Inf, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} Inf (@var{x})\n\
@deftypefnx {Built-in Function} {} Inf (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} Inf (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} Inf (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all Infinity.\n\
The arguments are handled the same as the arguments for @code{eye}.\n\
The optional argument @var{class} may be either @samp{\"single\"} or\n\
@samp{\"double\"}.  The default is @samp{\"double\"}.\n\
@end deftypefn")
{
  return fill_matrix (args, lo_ieee_inf_value (), 
		      lo_ieee_float_inf_value (), "Inf");
}

DEFALIAS (inf, Inf);

/*

%!assert(inf (3), [Inf, Inf, Inf; Inf, Inf, Inf; Inf, Inf, Inf]);
%!assert(inf (2, 3), [Inf, Inf, Inf; Inf, Inf, Inf]);
%!assert(inf (3, 2), [Inf, Inf; Inf, Inf; Inf, Inf]);
%!assert(size (inf (3, 4, 5)),  [3, 4, 5]);

%!assert(inf (3,'single'), single([Inf, Inf, Inf; Inf, Inf, Inf; Inf, Inf, Inf]));
%!assert(inf (2, 3,'single'), single([Inf, Inf, Inf; Inf, Inf, Inf]));
%!assert(inf (3, 2,'single'), single([Inf, Inf; Inf, Inf; Inf, Inf]));
%!assert(size (inf (3, 4, 5, 'single')),  [3, 4, 5]);

%!error(inf (3,'int8'));
%!error(inf (2, 3,'int8'));
%!error(inf (3, 2,'int8'));
%!error(inf (3, 4, 5, 'int8'));

 */

DEFUN (NaN, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} NaN (@var{x})\n\
@deftypefnx {Built-in Function} {} NaN (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} NaN (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} NaN (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all NaN\n\
(Not a Number).  The value NaN is the result of an operation like\n\
@iftex\n\
@tex\n\
$0/0$, or $\\infty - \\infty$,\n\
@end tex\n\
@end iftex\n\
@ifnottex\n\
0/0, or @samp{Inf - Inf},\n\
@end ifnottex\n\
or any operation with a NaN.\n\
\n\
Note that NaN always compares not equal to NaN.  This behavior is\n\
specified by the IEEE standard for floating point arithmetic.  To\n\
find NaN values, you must use the @code{isnan} function.\n\
\n\
The arguments are handled the same as the arguments for @code{eye}.\n\
The optional argument @var{class} may be either @samp{\"single\"} or\n\
@samp{\"double\"}.  The default is @samp{\"double\"}.\n\
@end deftypefn")
{
  return fill_matrix (args, lo_ieee_nan_value (), 
		      lo_ieee_float_nan_value (), "NaN");
}

DEFALIAS (nan, NaN);

/* 
%!assert(NaN (3), [NaN, NaN, NaN; NaN, NaN, NaN; NaN, NaN, NaN]);
%!assert(NaN (2, 3), [NaN, NaN, NaN; NaN, NaN, NaN]);
%!assert(NaN (3, 2), [NaN, NaN; NaN, NaN; NaN, NaN]);
%!assert(size (NaN (3, 4, 5)),  [3, 4, 5]);

%!assert(NaN (3,'single'), single([NaN, NaN, NaN; NaN, NaN, NaN; NaN, NaN, NaN]));
%!assert(NaN (2, 3,'single'), single([NaN, NaN, NaN; NaN, NaN, NaN]));
%!assert(NaN (3, 2,'single'), single([NaN, NaN; NaN, NaN; NaN, NaN]));
%!assert(size (NaN (3, 4, 5, 'single')),  [3, 4, 5]);

%!error(NaN (3,'int8'));
%!error(NaN (2, 3,'int8'));
%!error(NaN (3, 2,'int8'));
%!error(NaN (3, 4, 5, 'int8'));

 */

DEFUN (e, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} e (@var{x})\n\
@deftypefnx {Built-in Function} {} e (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} e (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} e (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all equal\n\
to the base of natural logarithms.  The constant\n\
@iftex\n\
@tex\n\
 $e$\n\
@end tex\n\
@end iftex\n\
@ifnottex\n\
 @var{e}\n\
@end ifnottex\n\
 satisfies the equation\n\
@iftex\n\
@tex\n\
 $\\log (e) = 1$.\n\
@end tex\n\
@end iftex\n\
@ifnottex\n\
 @code{log} (@var{e}) = 1.\n\
@end ifnottex\n\
@end deftypefn")
{
#if defined (M_E)
  double e_val = M_E;
#else
  double e_val = exp (1.0);
#endif

  return fill_matrix (args, e_val, "e");
}

DEFUN (eps, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} eps (@var{x})\n\
@deftypefnx {Built-in Function} {} eps (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} eps (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} eps (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all eps,\n\
the machine precision.  More precisely, @code{eps} is the largest\n\
relative spacing between any two adjacent numbers in the machine's\n\
floating point system.  This number is obviously system-dependent.  On\n\
machines that support 64-bit IEEE floating point arithmetic, @code{eps}\n\
is approximately\n\
@ifnottex\n\
 2.2204e-16.\n\
@end ifnottex\n\
@iftex\n\
@tex\n\
 $2.2204\\times10^{-16}$.\n\
@end tex\n\
@end iftex\n\
for double precision and\n\
@ifnottex\n\
 1.1921e-07.\n\
@end ifnottex\n\
@iftex\n\
@tex\n\
 $1.1921\\times10^{-7}$.\n\
@end tex\n\
@end iftex\n\
for single precision. Given a single argument @var{x}, return the\n\
distance between @var{x} and the next largest value.\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin == 1 && ! args(0).is_string ())
    {
      if (args(0).is_single_type ())
	{
	  float val = args(0).float_value ();

	  if (! error_state)
	    {
	      val  = ::fabsf(val);
	      if (xisnan (val) || xisinf (val))
		retval = fill_matrix (octave_value ("single"), 
				      lo_ieee_nan_value (), 
				      lo_ieee_float_nan_value (), "eps");
	      else if (val < FLT_MIN)
		retval = fill_matrix (octave_value ("single"), 0e0, 
				      powf (2.0, -149e0), "eps");
	      else
		{
		  int expon;
		  frexpf (val, &expon);
		  val = std::pow (static_cast <float> (2.0), 
				  static_cast <float> (expon - 24));
		  retval = fill_matrix (octave_value ("single"), DBL_EPSILON, 
					val, "eps");
		}
	    }
	}
      else
	{
	  double val = args(0).double_value ();

	  if (! error_state)
	    {
	      val  = ::fabs(val);
	      if (xisnan (val) || xisinf (val))
		retval = fill_matrix (octave_value_list (), 
				      lo_ieee_nan_value (), 
				      lo_ieee_float_nan_value (), "eps");
	      else if (val < DBL_MIN)
		retval = fill_matrix (octave_value_list (),
				      pow (2.0, -1074e0), 0e0, "eps");
	      else
		{
		  int expon;
		  frexp (val, &expon);
		  val = std::pow (static_cast <double> (2.0), 
				  static_cast <double> (expon - 53));
		  retval = fill_matrix (octave_value_list (), val, 
					FLT_EPSILON, "eps");
		}
	    }
	}
    }
  else
    retval = fill_matrix (args, DBL_EPSILON, FLT_EPSILON, "eps");

  return retval;
}

/*

%!assert(eps(1/2),2^(-53))
%!assert(eps(1),2^(-52))
%!assert(eps(2),2^(-51))
%!assert(eps(realmax),2^971)
%!assert(eps(0),2^(-1074))
%!assert(eps(realmin/2),2^(-1074))
%!assert(eps(realmin/16),2^(-1074))
%!assert(eps(Inf),NaN)
%!assert(eps(NaN),NaN)
%!assert(eps(single(1/2)),single(2^(-24)))
%!assert(eps(single(1)),single(2^(-23)))
%!assert(eps(single(2)),single(2^(-22)))
%!assert(eps(realmax('single')),single(2^104))
%!assert(eps(single(0)),single(2^(-149)))
%!assert(eps(realmin('single')/2),single(2^(-149)))
%!assert(eps(realmin('single')/16),single(2^(-149)))
%!assert(eps(single(Inf)),single(NaN))
%!assert(eps(single(NaN)),single(NaN))

*/


DEFUN (pi, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} pi (@var{x})\n\
@deftypefnx {Built-in Function} {} pi (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} pi (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} pi (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all equal\n\
to the ratio of the circumference of a circle to its diameter.\n\
Internally, @code{pi} is computed as @samp{4.0 * atan (1.0)}.\n\
@end deftypefn")
{
#if defined (M_PI)
  double pi_val = M_PI;
#else
  double pi_val = 4.0 * atan (1.0);
#endif

  return fill_matrix (args, pi_val, "pi");
}

DEFUN (realmax, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} realmax (@var{x})\n\
@deftypefnx {Built-in Function} {} realmax (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} realmax (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} realmax (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all equal\n\
to the largest floating point number that is representable.  The actual\n\
value is system-dependent.  On machines that support 64-bit IEEE\n\
floating point arithmetic, @code{realmax} is approximately\n\
@ifnottex\n\
 1.7977e+308\n\
@end ifnottex\n\
@iftex\n\
@tex\n\
 $1.7977\\times10^{308}$.\n\
@end tex\n\
@end iftex\n\
@seealso{realmin}\n\
@end deftypefn")
{
  return fill_matrix (args, DBL_MAX, FLT_MAX, "realmax");
}

DEFUN (realmin, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} realmin (@var{x})\n\
@deftypefnx {Built-in Function} {} realmin (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} realmin (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} realmin (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all equal\n\
to the smallest normalized floating point number that is representable.\n\
The actual value is system-dependent.  On machines that support\n\
64-bit IEEE floating point arithmetic, @code{realmin} is approximately\n\
@ifnottex\n\
 2.2251e-308\n\
@end ifnottex\n\
@iftex\n\
@tex\n\
 $2.2251\\times10^{-308}$.\n\
@end tex\n\
@end iftex\n\
@seealso{realmax}\n\
@end deftypefn")
{
  return fill_matrix (args, DBL_MIN, FLT_MIN, "realmin");
}

DEFUN (I, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} I (@var{x})\n\
@deftypefnx {Built-in Function} {} I (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} I (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} I (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all equal\n\
to the pure imaginary unit, defined as\n\
@iftex\n\
@tex\n\
  $\\sqrt{-1}$.\n\
@end tex\n\
@end iftex\n\
@ifnottex\n\
  @code{sqrt (-1)}.\n\
@end ifnottex\n\
Since I (also i, J, and j) is a function, you can use the name(s) for\n\
other purposes.\n\
@end deftypefn")
{
  return fill_matrix (args, Complex (0.0, 1.0), "I");
}

DEFALIAS (i, I);
DEFALIAS (J, I);
DEFALIAS (j, I);

DEFUN (NA, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} NA (@var{x})\n\
@deftypefnx {Built-in Function} {} NA (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} NA (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} NA (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all equal\n\
to the special constant used to designate missing values.\n\
@end deftypefn")
{
  return fill_matrix (args, lo_ieee_na_value (), 
		      lo_ieee_float_na_value (), "NA");
}

/*

%!assert(single(NA('double')),NA('single'))
%!assert(double(NA('single')),NA('double'))

 */

DEFUN (false, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} false (@var{x})\n\
@deftypefnx {Built-in Function} {} false (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} false (@var{n}, @var{m}, @var{k}, @dots{})\n\
Return a matrix or N-dimensional array whose elements are all logical 0.\n\
The arguments are handled the same as the arguments for @code{eye}.\n\
@end deftypefn")
{
  return fill_matrix (args, false, "false");
}

DEFUN (true, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} true (@var{x})\n\
@deftypefnx {Built-in Function} {} true (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} true (@var{n}, @var{m}, @var{k}, @dots{})\n\
Return a matrix or N-dimensional array whose elements are all logical 1.\n\
The arguments are handled the same as the arguments for @code{eye}.\n\
@end deftypefn")
{
  return fill_matrix (args, true, "true");
}

template <class MT>
octave_value
identity_matrix (int nr, int nc)
{
  octave_value retval;

  typename octave_array_type_traits<MT>::element_type one (1);

  if (nr == 1 && nc == 1)
    retval = one;
  else
    {
      dim_vector dims (nr, nc);

      typename octave_array_type_traits<MT>::element_type zero (0);

      MT m (dims, zero);

      if (nr > 0 && nc > 0)
	{
	  int n = std::min (nr, nc);

	  for (int i = 0; i < n; i++)
	    m(i,i) = one;
	}

      retval = m;
    }

  return retval;
}

#define INSTANTIATE_EYE(T) \
  template octave_value identity_matrix<T> (int, int)

INSTANTIATE_EYE (int8NDArray);
INSTANTIATE_EYE (uint8NDArray);
INSTANTIATE_EYE (int16NDArray);
INSTANTIATE_EYE (uint16NDArray);
INSTANTIATE_EYE (int32NDArray);
INSTANTIATE_EYE (uint32NDArray);
INSTANTIATE_EYE (int64NDArray);
INSTANTIATE_EYE (uint64NDArray);
INSTANTIATE_EYE (FloatNDArray);
INSTANTIATE_EYE (NDArray);
INSTANTIATE_EYE (boolNDArray);

static octave_value
identity_matrix (int nr, int nc, oct_data_conv::data_type dt)
{
  octave_value retval;

  // FIXME -- perhaps this should be made extensible by using
  // the class name to lookup a function to call to create the new
  // value.

  if (! error_state)
    {
      switch (dt)
	{
	case oct_data_conv::dt_int8:
	  retval = identity_matrix<int8NDArray> (nr, nc);
	  break;

	case oct_data_conv::dt_uint8:
	  retval = identity_matrix<uint8NDArray> (nr, nc);
	  break;

	case oct_data_conv::dt_int16:
	  retval = identity_matrix<int16NDArray> (nr, nc);
	  break;

	case oct_data_conv::dt_uint16:
	  retval = identity_matrix<uint16NDArray> (nr, nc);
	  break;

	case oct_data_conv::dt_int32:
	  retval = identity_matrix<int32NDArray> (nr, nc);
	  break;

	case oct_data_conv::dt_uint32:
	  retval = identity_matrix<uint32NDArray> (nr, nc);
	  break;

	case oct_data_conv::dt_int64:
	  retval = identity_matrix<int64NDArray> (nr, nc);
	  break;

	case oct_data_conv::dt_uint64:
	  retval = identity_matrix<uint64NDArray> (nr, nc);
	  break;

	case oct_data_conv::dt_single:
	  retval = FloatDiagMatrix (nr, nc, 1.0f);
	  break;

	case oct_data_conv::dt_double:
	  retval = DiagMatrix (nr, nc, 1.0);
	  break;

	case oct_data_conv::dt_logical:
	  retval = identity_matrix<boolNDArray> (nr, nc);
	  break;

	default:
	  error ("eye: invalid class name");
	  break;
	}
    }

  return retval;
}

#undef INT_EYE_MATRIX

DEFUN (eye, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} eye (@var{x})\n\
@deftypefnx {Built-in Function} {} eye (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} eye (@dots{}, @var{class})\n\
Return an identity matrix.  If invoked with a single scalar argument,\n\
@code{eye} returns a square matrix with the dimension specified.  If you\n\
supply two scalar arguments, @code{eye} takes them to be the number of\n\
rows and columns.  If given a vector with two elements, @code{eye} uses\n\
the values of the elements as the number of rows and columns,\n\
respectively.  For example,\n\
\n\
@example\n\
@group\n\
eye (3)\n\
     @result{}  1  0  0\n\
         0  1  0\n\
         0  0  1\n\
@end group\n\
@end example\n\
\n\
The following expressions all produce the same result:\n\
\n\
@example\n\
@group\n\
eye (2)\n\
@equiv{}\n\
eye (2, 2)\n\
@equiv{}\n\
eye (size ([1, 2; 3, 4])\n\
@end group\n\
@end example\n\
\n\
The optional argument @var{class}, allows @code{eye} to return an array of\n\
the specified type, like\n\
\n\
@example\n\
val = zeros (n,m, \"uint8\")\n\
@end example\n\
\n\
Calling @code{eye} with no arguments is equivalent to calling it\n\
with an argument of 1.  This odd definition is for compatibility\n\
with @sc{Matlab}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  // Check for type information.

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
	return retval;
    }

  switch (nargin)
    {
    case 0:
      retval = identity_matrix (1, 1, dt);
      break;

    case 1:
      {
	octave_idx_type nr, nc;
	get_dimensions (args(0), "eye", nr, nc);

	if (! error_state)
	  retval = identity_matrix (nr, nc, dt);
      }
      break;

    case 2:
      {
	octave_idx_type nr, nc;
	get_dimensions (args(0), args(1), "eye", nr, nc);

	if (! error_state)
	  retval = identity_matrix (nr, nc, dt);
      }
      break;

    default:
      print_usage ();
      break;
    }

  return retval;
}


/*

%!assert (full (eye(3)), [1, 0, 0; 0, 1, 0; 0, 0, 1]);
%!assert (full (eye(2, 3)), [1, 0, 0; 0, 1, 0]);

%!assert (full (eye(3,'single')), single([1, 0, 0; 0, 1, 0; 0, 0, 1]));
%!assert (full (eye(2, 3,'single')), single([1, 0, 0; 0, 1, 0]));

%!assert (eye(3,'int8'), int8([1, 0, 0; 0, 1, 0; 0, 0, 1]));
%!assert (eye(2, 3,'int8'), int8([1, 0, 0; 0, 1, 0]));

%!error <Invalid call to eye.*> eye (1, 2, 3);

 */

DEFUN (linspace, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} linspace (@var{base}, @var{limit}, @var{n})\n\
Return a row vector with @var{n} linearly spaced elements between\n\
@var{base} and @var{limit}.  If the number of elements is greater than one,\n\
then the @var{base} and @var{limit} are always included in\n\
the range.  If @var{base} is greater than @var{limit}, the elements are\n\
stored in decreasing order.  If the number of points is not specified, a\n\
value of 100 is used.\n\
\n\
The @code{linspace} function always returns a row vector.\n\
\n\
For compatibility with @sc{Matlab}, return the second argument if\n\
fewer than two values are requested.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  octave_idx_type npoints = 100;

  if (nargin != 2 && nargin != 3)
    {
      print_usage ();
      return retval;
    }

  if (nargin == 3)
    npoints = args(2).idx_type_value ();

  if (! error_state)
    {
      octave_value arg_1 = args(0);
      octave_value arg_2 = args(1);

      if (arg_1.is_single_type () || arg_2.is_single_type ())
	{
	  if (arg_1.is_complex_type () || arg_2.is_complex_type ())
	    {
	      FloatComplex x1 = arg_1.float_complex_value ();
	      FloatComplex x2 = arg_2.float_complex_value ();

	      if (! error_state)
		{
		  FloatComplexRowVector rv = linspace (x1, x2, npoints);

		  if (! error_state)
		    retval = rv;
		}
	    }
	  else
	    {
	      float x1 = arg_1.float_value ();
	      float x2 = arg_2.float_value ();

	      if (! error_state)
		{
		  FloatRowVector rv = linspace (x1, x2, npoints);

		  if (! error_state)
		    retval = rv;
		}
	    }
	}
      else
	{
	  if (arg_1.is_complex_type () || arg_2.is_complex_type ())
	    {
	      Complex x1 = arg_1.complex_value ();
	      Complex x2 = arg_2.complex_value ();

	      if (! error_state)
		{
		  ComplexRowVector rv = linspace (x1, x2, npoints);

		  if (! error_state)
		    retval = rv;
		}
	    }
	  else
	    {
	      double x1 = arg_1.double_value ();
	      double x2 = arg_2.double_value ();

	      if (! error_state)
		{
		  RowVector rv = linspace (x1, x2, npoints);

		  if (! error_state)
		    retval = rv;
		}
	    }
	}
    }
  else
    error ("linspace: expecting third argument to be an integer");

  return retval;
}


/*

%!test
%! x1 = linspace (1, 2);
%! x2 = linspace (1, 2, 10);
%! x3 = linspace (1, -2, 10);
%! assert((size (x1) == [1, 100] && x1(1) == 1 && x1(100) == 2
%! && size (x2) == [1, 10] && x2(1) == 1 && x2(10) == 2
%! && size (x3) == [1, 10] && x3(1) == 1 && x3(10) == -2));


% assert(linspace ([1, 2; 3, 4], 5, 6), linspace (1, 5, 6));

%!error <Invalid call to linspace.*> linspace ();
%!error <Invalid call to linspace.*> linspace (1, 2, 3, 4);

%!test
%! fail("linspace ([1, 2; 3, 4], 5, 6)","warning");

*/

// FIXME -- should accept dimensions as separate args for N-d
// arrays as well as 1-d and 2-d arrays.

DEFUN (resize, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} resize (@var{x}, @var{m})\n\
@deftypefnx {Built-in Function} {} resize (@var{x}, @var{m}, @var{n})\n\
Destructively resize @var{x}.\n\
\n\
@strong{Values in @var{x} are not preserved as they are with\n\
@code{reshape}.}\n\
\n\
If only @var{m} is supplied and it is a scalar, the dimension of the\n\
result is @var{m}-by-@var{m}.  If @var{m} is a vector, then the\n\
dimensions of the result are given by the elements of @var{m}.\n\
If both @var{m} and @var{n} are scalars, then the dimensions of\n\
the result are @var{m}-by-@var{n}.\n\
@seealso{reshape, postpad}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin == 2)
    {
      Array<double> vec = args(1).vector_value ();
      int ndim = vec.length ();
      if (ndim == 1)
	{
	  octave_idx_type m = static_cast<octave_idx_type> (vec(0));
	  retval = args(0);
	  retval = retval.resize (dim_vector (m, m), true);
	}
      else
	{
	  dim_vector dv;
	  dv.resize (ndim);
	  for (int i = 0; i < ndim; i++)
	    dv(i) = static_cast<octave_idx_type> (vec(i));
	  retval = args(0);
	  retval = retval.resize (dv, true);
	}
    }
  else if (nargin == 3)
    {
      octave_idx_type m = static_cast<octave_idx_type> 
	(args(1).scalar_value());
      octave_idx_type n = static_cast<octave_idx_type> 
	(args(2).scalar_value());
      if (!error_state)
	{
	  retval = args(0);
	  retval = retval.resize (dim_vector (m, n), true);
	}
    }
  else
    print_usage ();
  return retval;
}

// FIXME -- should use octave_idx_type for dimensions.

DEFUN (reshape, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} reshape (@var{a}, @var{m}, @var{n}, @dots{})\n\
@deftypefnx {Built-in Function} {} reshape (@var{a}, @var{siz})\n\
Return a matrix with the given dimensions whose elements are taken\n\
from the matrix @var{a}.  The elements of the matrix are accessed in\n\
column-major order (like Fortran arrays are stored).\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
reshape ([1, 2, 3, 4], 2, 2)\n\
     @result{}  1  3\n\
         2  4\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Note that the total number of elements in the original\n\
matrix must match the total number of elements in the new matrix.\n\
\n\
A single dimension of the return matrix can be unknown and is flagged\n\
by an empty argument.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  Array<int> new_size;

  if (nargin == 2)
    new_size = args(1).int_vector_value ();
  else if (nargin > 2)
    {
      new_size.resize (nargin-1);
      int empty_dim = -1;
      
      for (int i = 1; i < nargin; i++)
	{
	  if (args(i).is_empty ())
	    if (empty_dim > 0)
	      {
		error ("reshape: only a single dimension can be unknown");
		break;
	      }
	    else
	      {
		empty_dim = i;
		new_size(i-1) = 1;
	      }
	  else
	    {
	      new_size(i-1) = args(i).idx_type_value ();

	      if (error_state)
		break;
	    }
	}

      if (! error_state && (empty_dim > 0))
	{
	  int nel = 1;
	  for (int i = 0; i < nargin - 1; i++)
	    nel *= new_size(i);

	  if (nel == 0)
	    new_size(empty_dim-1) = 0;
	  else
	    {
	      int size_empty_dim = args(0).numel () / nel;
	      
	      if (args(0).numel () != size_empty_dim * nel)
		error ("reshape: size is not divisble by the product of known dimensions (= %d)", nel);
	      else
		new_size(empty_dim-1) = size_empty_dim;
	    }
	}
    }
  else
    {
      print_usage ();
      return retval;
    }

  if (error_state)
    {
      error ("reshape: invalid arguments");
      return retval;
    }

  // Remove trailing singletons in new_size, but leave at least 2
  // elements.

  int n = new_size.length ();

  while (n > 2)
    {
      if (new_size(n-1) == 1)
	n--;
      else
	break;
    }

  new_size.resize (n);

  if (n < 2)
    {
      error ("reshape: expecting size to be vector with at least 2 elements");
      return retval;
    }

  dim_vector new_dims;

  new_dims.resize (n);

  for (octave_idx_type i = 0; i < n; i++)
    new_dims(i) = new_size(i);

  octave_value arg = args(0);

  dim_vector dims = arg.dims ();

  if (new_dims.numel () == dims.numel ())
    retval = (new_dims == dims) ? arg : arg.reshape (new_dims);
  else
    {
      std::string dims_str = dims.str ();
      std::string new_dims_str = new_dims.str ();

      error ("reshape: can't reshape %s array to %s array",
	     dims_str.c_str (), new_dims_str.c_str ());
    }

  return retval;
}

/*

%!assert(size (reshape (ones (4, 4), 2, 8)), [2, 8])
%!assert(size (reshape (ones (4, 4), 8, 2)), [8, 2])
%!assert(size (reshape (ones (15, 4), 1, 60)), [1, 60])
%!assert(size (reshape (ones (15, 4), 60, 1)), [60, 1])

%!assert(size (reshape (ones (4, 4, 'single'), 2, 8)), [2, 8])
%!assert(size (reshape (ones (4, 4, 'single'), 8, 2)), [8, 2])
%!assert(size (reshape (ones (15, 4, 'single'), 1, 60)), [1, 60])
%!assert(size (reshape (ones (15, 4, 'single'), 60, 1)), [60, 1])

%!test
%! s.a = 1;
%! fail("reshape (s, 2, 3)");

%!error <Invalid call to reshape.*> reshape ();
%!error reshape (1, 2, 3, 4);

 */

DEFUN (squeeze, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} squeeze (@var{x})\n\
Remove singleton dimensions from @var{x} and return the result.\n\
Note that for compatibility with @sc{Matlab}, all objects have\n\
a minimum of two dimensions and row vectors are left unchanged.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).squeeze ();
  else
    print_usage ();    

  return retval;
}

DEFUN (full, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{FM} =} full (@var{SM})\n\
 returns a full storage matrix from a sparse, diagonal, permutation matrix or a range.\n\
@seealso{sparse}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).full_value ();
  else
    print_usage ();    

  return retval;
}

// Compute various norms of the vector X.

DEFUN (norm, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} norm (@var{a}, @var{p}, @var{opt})\n\
Compute the p-norm of the matrix @var{a}.  If the second argument is\n\
missing, @code{p = 2} is assumed.\n\
\n\
If @var{a} is a matrix (or sparse matrix):\n\
\n\
@table @asis\n\
@item @var{p} = @code{1}\n\
1-norm, the largest column sum of the absolute values of @var{a}.\n\
\n\
@item @var{p} = @code{2}\n\
Largest singular value of @var{a}.\n\
\n\
@item @var{p} = @code{Inf} or @code{\"inf\"}\n\
@cindex infinity norm\n\
Infinity norm, the largest row sum of the absolute values of @var{a}.\n\
\n\
@item @var{p} = @code{\"fro\"}\n\
@cindex Frobenius norm\n\
Frobenius norm of @var{a}, @code{sqrt (sum (diag (@var{a}' * @var{a})))}.\n\
\n\
@item other @var{p}, @code{@var{p} > 1}\n\
@cindex general p-norm \n\
maximum @code{norm (A*x, p)} such that @code{norm (x, p) == 1}\n\
@end table\n\
\n\
If @var{a} is a vector or a scalar:\n\
\n\
@table @asis\n\
@item @var{p} = @code{Inf} or @code{\"inf\"}\n\
@code{max (abs (@var{a}))}.\n\
\n\
@item @var{p} = @code{-Inf}\n\
@code{min (abs (@var{a}))}.\n\
\n\
@item @var{p} = @code{\"fro\"}\n\
Frobenius norm of @var{a}, @code{sqrt (sumsq (abs (a)))}.\n\
\n\
@item @var{p} = 0\n\
Hamming norm - the number of nonzero elements.\n\
\n\
@item other @var{p}, @code{@var{p} > 1}\n\
p-norm of @var{a}, @code{(sum (abs (@var{a}) .^ @var{p})) ^ (1/@var{p})}.\n\
\n\
@item other @var{p} @code{@var{p} < 1}\n\
the p-pseudonorm defined as above.\n\
@end table\n\
\n\
If @code{\"rows\"} is given as @var{opt}, the norms of all rows of the matrix @var{a} are\n\
returned as a column vector. Similarly, if @code{\"columns\"} or @code{\"cols\"} is passed\n\
column norms are computed.\n\
@seealso{cond, svd}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin >= 1 && nargin <= 3)
    {
      octave_value x_arg = args(0);

      if (x_arg.is_empty ())
	{
	  if (x_arg.is_single_type ())
	    retval(0) = static_cast<float>(0.0);
	  else
	    retval(0) = 0.0;
	}
      else if (x_arg.ndims () == 2)
	{
          enum { sfmatrix, sfcols, sfrows, sffrob, sfinf } strflag = sfmatrix;
          if (nargin > 1 && args(nargin-1).is_string ())
            {
              std::string str = args(nargin-1).string_value ();
              if (str == "cols" || str == "columns")
                strflag = sfcols;
              else if (str == "rows")
                strflag = sfrows;
              else if (str == "fro")
                strflag = sffrob;
              else if (str == "inf")
                strflag = sfinf;
              else
                error ("norm: unrecognized option: %s", str.c_str ());
              // we've handled the last parameter, so act as if it was removed
              nargin --;
            }
          else if (nargin > 1 && ! args(1).is_scalar_type ())
            gripe_wrong_type_arg ("norm", args(1), true);

          if (! error_state)
            {
              octave_value p_arg = (nargin > 1) ? args(1) : octave_value (2);
              switch (strflag)
                {
                case sfmatrix:
                  retval(0) = xnorm (x_arg, p_arg);
                  break;
                case sfcols:
                  retval(0) = xcolnorms (x_arg, p_arg);
                  break;
                case sfrows:
                  retval(0) = xrownorms (x_arg, p_arg);
                  break;
                case sffrob:
                  retval(0) = xfrobnorm (x_arg);
                  break;
                case sfinf:
                  retval(0) = xnorm (x_arg, octave_Inf);
                  break;
                }
            }
	}
      else
	error ("norm: only valid for 2-D objects");
    }
  else
    print_usage ();

  return retval;
}

/*
%!shared x
%! x = [1, -3, 4, 5, -7];
%!assert(norm(x,1), 20);
%!assert(norm(x,2), 10);
%!assert(norm(x,3), 8.24257059961711, -4*eps);
%!assert(norm(x,Inf), 7);
%!assert(norm(x,-Inf), 1);
%!assert(norm(x,"inf"), 7);
%!assert(norm(x,"fro"), 10, -eps);
%!assert(norm(x), 10);
%!assert(norm([1e200, 1]), 1e200);
%!assert(norm([3+4i, 3-4i, sqrt(31)]), 9, -4*eps);
%!shared m
%! m = magic (4);
%!assert(norm(m,1), 34);
%!assert(norm(m,2), 34, -eps);
%!assert(norm(m,Inf), 34);
%!assert(norm(m,"inf"), 34);
%!shared m2, flo, fhi
%! m2 = [1,2;3,4];
%! flo = 1e-300;
%! fhi = 1e+300;
%!assert (norm(flo*m2,"fro"), sqrt(30)*flo, -eps)
%!assert (norm(fhi*m2,"fro"), sqrt(30)*fhi, -eps)

%!shared x
%! x = single([1, -3, 4, 5, -7]);
%!assert(norm(x,1), single(20));
%!assert(norm(x,2), single(10));
%!assert(norm(x,3), single(8.24257059961711), -4*eps('single'));
%!assert(norm(x,Inf), single(7));
%!assert(norm(x,-Inf), single(1));
%!assert(norm(x,"inf"), single(7));
%!assert(norm(x,"fro"), single(10), -eps('single'));
%!assert(norm(x), single(10));
%!assert(norm(single([1e200, 1])), single(1e200));
%!assert(norm(single([3+4i, 3-4i, sqrt(31)])), single(9), -4*eps('single'));
%!shared m
%! m = single(magic (4));
%!assert(norm(m,1), single(34));
%!assert(norm(m,2), single(34), -eps('single'));
%!assert(norm(m,Inf), single(34));
%!assert(norm(m,"inf"), single(34));
%!shared m2, flo, fhi
%! m2 = single([1,2;3,4]);
%! flo = single(1e-300);
%! fhi = single(1e+300);
%!assert (norm(flo*m2,"fro"), single(sqrt(30)*flo), -eps('single'))
%!assert (norm(fhi*m2,"fro"), single(sqrt(30)*fhi), -eps('single'))
*/

#define UNARY_OP_DEFUN_BODY(F) \
 \
  octave_value retval; \
 \
  if (args.length () == 1) \
    retval = F (args(0)); \
  else \
    print_usage (); \
 \
  return retval

DEFUN (not, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} not (@var{x})\n\
This function is equivalent to @code{! x}.\n\
@end deftypefn")
{
  UNARY_OP_DEFUN_BODY (op_not);
}

DEFUN (uplus, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} uplus (@var{x})\n\
This function is equivalent to @code{+ x}.\n\
@end deftypefn")
{
  UNARY_OP_DEFUN_BODY (op_uplus);
}

DEFUN (uminus, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} uminus (@var{x})\n\
This function is equivalent to @code{- x}.\n\
@end deftypefn")
{
  UNARY_OP_DEFUN_BODY (op_uminus);
}

DEFUN (transpose, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} transpose (@var{x})\n\
This function is equivalent to @code{x.'}.\n\
@end deftypefn")
{
  UNARY_OP_DEFUN_BODY (op_transpose);
}

/*

%!assert (2.', 2);
%!assert (2i.',2i);
%!assert ([1:4].',[1;2;3;4]);
%!assert ([1;2;3;4].',[1:4]);
%!assert ([1,2;3,4].',[1,3;2,4]);
%!assert ([1,2i;3,4].',[1,3;2i,4]);

%!assert (transpose ([1,2;3,4]),[1,3;2,4]);

%!assert (single(2).', single(2));
%!assert (single(2i).',single(2i));
%!assert (single([1:4]).',single([1;2;3;4]));
%!assert (single([1;2;3;4]).',single([1:4]));
%!assert (single([1,2;3,4]).',single([1,3;2,4]));
%!assert (single([1,2i;3,4]).',single([1,3;2i,4]));

%!assert (transpose (single([1,2;3,4])),single([1,3;2,4]));

*/

DEFUN (ctranspose, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ctranspose (@var{x})\n\
This function is equivalent to @code{x'}.\n\
@end deftypefn")
{
  UNARY_OP_DEFUN_BODY (op_hermitian);
}

/*

%!assert (2', 2);
%!assert (2i',-2i);
%!assert ([1:4]',[1;2;3;4]);
%!assert ([1;2;3;4]',[1:4]);
%!assert ([1,2;3,4]',[1,3;2,4]);
%!assert ([1,2i;3,4]',[1,3;-2i,4]);

%!assert (ctranspose ([1,2i;3,4]),[1,3;-2i,4]);

%!assert (single(2)', single(2));
%!assert (single(2i)',single(-2i));
%!assert (single([1:4])',single([1;2;3;4]));
%!assert (single([1;2;3;4])',single([1:4]));
%!assert (single([1,2;3,4])',single([1,3;2,4]));
%!assert (single([1,2i;3,4])',single([1,3;-2i,4]));

%!assert (ctranspose (single([1,2i;3,4])),single([1,3;-2i,4]));

*/

#define BINARY_OP_DEFUN_BODY(F) \
 \
  octave_value retval; \
 \
  if (args.length () == 2) \
    retval = F (args(0), args(1)); \
  else \
    print_usage (); \
 \
  return retval

DEFUN (plus, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} plus (@var{x}, @var{y})\n\
This function is equivalent to @code{x + y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_add);
}

DEFUN (minus, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} minus (@var{x}, @var{y})\n\
This function is equivalent to @code{x - y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_sub);
}

DEFUN (mtimes, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mtimes (@var{x}, @var{y})\n\
This function is equivalent to @code{x * y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_mul);
}

DEFUN (mrdivide, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mrdivide (@var{x}, @var{y})\n\
This function is equivalent to @code{x / y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_div);
}

DEFUN (mpower, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mpower (@var{x}, @var{y})\n\
This function is equivalent to @code{x ^ y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_pow);
}

DEFUN (mldivide, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mldivide (@var{x}, @var{y})\n\
This function is equivalent to @code{x \\ y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_ldiv);
}

DEFUN (lt, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} lt (@var{x}, @var{y})\n\
This function is equivalent to @code{x < y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_lt);
}

DEFUN (le, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} le (@var{x}, @var{y})\n\
This function is equivalent to @code{x <= y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_le);
}

DEFUN (eq, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} eq (@var{x}, @var{y})\n\
This function is equivalent to @code{x == y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_eq);
}

DEFUN (ge, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ge (@var{x}, @var{y})\n\
This function is equivalent to @code{x >= y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_ge);
}

DEFUN (gt, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} gt (@var{x}, @var{y})\n\
This function is equivalent to @code{x > y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_gt);
}

DEFUN (ne, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ne (@var{x}, @var{y})\n\
This function is equivalent to @code{x != y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_ne);
}

DEFUN (times, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} times (@var{x}, @var{y})\n\
This function is equivalent to @code{x .* y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_el_mul);
}

DEFUN (rdivide, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} rdivide (@var{x}, @var{y})\n\
This function is equivalent to @code{x ./ y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_el_div);
}

DEFUN (power, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} power (@var{x}, @var{y})\n\
This function is equivalent to @code{x .^ y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_el_pow);
}

DEFUN (ldivide, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ldivide (@var{x}, @var{y})\n\
This function is equivalent to @code{x .\\ y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_el_ldiv);
}

DEFUN (and, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} and (@var{x}, @var{y})\n\
This function is equivalent to @code{x & y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_el_and);
}

DEFUN (or, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} or (@var{x}, @var{y})\n\
This function is equivalent to @code{x | y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_el_or);
}

static double tic_toc_timestamp = -1.0;

DEFUN (tic, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} tic ()\n\
@deftypefnx {Built-in Function} {} toc ()\n\
Set or check a wall-clock timer.  Calling @code{tic} without an\n\
output argument sets the timer.  Subsequent calls to @code{toc}\n\
return the number of seconds since the timer was set.  For example,\n\
\n\
@example\n\
tic ();\n\
# many computations later...\n\
elapsed_time = toc ();\n\
@end example\n\
\n\
@noindent\n\
will set the variable @code{elapsed_time} to the number of seconds since\n\
the most recent call to the function @code{tic}.\n\
\n\
If called with one output argument then this function returns a scalar\n\
of type @code{uint64} and the wall-clock timer is not started.\n\
\n\
@example\n\
@group\n\
t = tic; sleep (5); (double (tic ()) - double (t)) * 1e-6\n\
     @result{} 5\n\
@end group\n\
@end example\n\
\n\
Nested timing with @code{tic} and @code{toc} is not supported.\n\
Therefore @code{toc} will always return the elapsed time from the most\n\
recent call to @code{tic}.\n\
\n\
If you are more interested in the CPU time that your process used, you\n\
should use the @code{cputime} function instead.  The @code{tic} and\n\
@code{toc} functions report the actual wall clock time that elapsed\n\
between the calls.  This may include time spent processing other jobs or\n\
doing nothing at all.  For example,\n\
\n\
@example\n\
@group\n\
tic (); sleep (5); toc ()\n\
     @result{} 5\n\
t = cputime (); sleep (5); cputime () - t\n\
     @result{} 0\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
(This example also illustrates that the CPU timer may have a fairly\n\
coarse resolution.)\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 0)
    warning ("tic: ignoring extra arguments");

  octave_time now;

  double tmp = now.double_value ();

  if (nargout > 0)
    retval = static_cast<octave_uint64> (1e6 * tmp);
  else
    tic_toc_timestamp = tmp;
      
  return retval;
}

DEFUN (toc, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} toc ()\n\
See tic.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 0)
    warning ("tic: ignoring extra arguments");

  if (tic_toc_timestamp < 0)
    {
      warning ("toc called before timer set");
      if (nargout > 0)
	retval = Matrix ();
    }
  else
    {
      octave_time now;

      double tmp = now.double_value () - tic_toc_timestamp;

      if (nargout > 0)
	retval = tmp;
      else
	octave_stdout << "Elapsed time is " << tmp << " seconds.\n";
    }
    
  return retval;
}

DEFUN (cputime, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{total}, @var{user}, @var{system}] =} cputime ();\n\
Return the CPU time used by your Octave session.  The first output is\n\
the total time spent executing your process and is equal to the sum of\n\
second and third outputs, which are the number of CPU seconds spent\n\
executing in user mode and the number of CPU seconds spent executing in\n\
system mode, respectively.  If your system does not have a way to report\n\
CPU time usage, @code{cputime} returns 0 for each of its output values.\n\
Note that because Octave used some CPU time to start, it is reasonable\n\
to check to see if @code{cputime} works by checking to see if the total\n\
CPU time used is nonzero.\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();
  double usr = 0.0;
  double sys = 0.0;

  if (nargin != 0)
    warning ("tic: ignoring extra arguments");

#if defined (HAVE_GETRUSAGE)

  struct rusage ru;

  getrusage (RUSAGE_SELF, &ru);

  usr = static_cast<double> (ru.ru_utime.tv_sec) +
    static_cast<double> (ru.ru_utime.tv_usec) * 1e-6;

  sys = static_cast<double> (ru.ru_stime.tv_sec) +
    static_cast<double> (ru.ru_stime.tv_usec) * 1e-6;

#elif defined (HAVE_TIMES) && defined (HAVE_SYS_TIMES_H)

  struct tms t;

  times (&t);

  unsigned long ticks;
  unsigned long seconds;
  unsigned long fraction;

  ticks = t.tms_utime + t.tms_cutime;
  fraction = ticks % HZ;
  seconds = ticks / HZ;

  usr = static_cast<double> (seconds) + static_cast<double>(fraction) /
    static_cast<double>(HZ);

  ticks = t.tms_stime + t.tms_cstime;
  fraction = ticks % HZ;
  seconds = ticks / HZ;

  sys = static_cast<double> (seconds) + static_cast<double>(fraction) /
    static_cast<double>(HZ);

#elif defined (__WIN32__)

  HANDLE hProcess = GetCurrentProcess ();
  FILETIME ftCreation, ftExit, ftUser, ftKernel;
  GetProcessTimes (hProcess, &ftCreation, &ftExit, &ftKernel, &ftUser);

  int64_t itmp = *(reinterpret_cast<int64_t *> (&ftUser));
  usr = static_cast<double> (itmp) * 1e-7;

  itmp = *(reinterpret_cast<int64_t *> (&ftKernel));
  sys = static_cast<double> (itmp) * 1e-7;

#endif

  retval (2) = sys;
  retval (1) = usr;
  retval (0) = sys + usr;

  return retval;
}

DEFUN (sort, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{s}, @var{i}] =} sort (@var{x})\n\
@deftypefnx {Loadable Function} {[@var{s}, @var{i}] =} sort (@var{x}, @var{dim})\n\
@deftypefnx {Loadable Function} {[@var{s}, @var{i}] =} sort (@var{x}, @var{mode})\n\
@deftypefnx {Loadable Function} {[@var{s}, @var{i}] =} sort (@var{x}, @var{dim}, @var{mode})\n\
Return a copy of @var{x} with the elements arranged in increasing\n\
order.  For matrices, @code{sort} orders the elements in each column.\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
sort ([1, 2; 2, 3; 3, 1])\n\
     @result{}  1  1\n\
         2  2\n\
         3  3\n\
@end group\n\
@end example\n\
\n\
The @code{sort} function may also be used to produce a matrix\n\
containing the original row indices of the elements in the sorted\n\
matrix.  For example,\n\
\n\
@example\n\
@group\n\
[s, i] = sort ([1, 2; 2, 3; 3, 1])\n\
     @result{} s = 1  1\n\
            2  2\n\
            3  3\n\
     @result{} i = 1  3\n\
            2  1\n\
            3  2\n\
@end group\n\
@end example\n\
\n\
If the optional argument @var{dim} is given, then the matrix is sorted\n\
along the dimension defined by @var{dim}. The optional argument @code{mode}\n\
defines the order in which the values will be sorted. Valid values of\n\
@code{mode} are `ascend' or `descend'.\n\
\n\
For equal elements, the indices are such that the equal elements are listed\n\
in the order that appeared in the original list.\n\
\n\
The @code{sort} function may also be used to sort strings and cell arrays\n\
of strings, in which case the dictionary order of the strings is used.\n\
\n\
The algorithm used in @code{sort} is optimized for the sorting of partially\n\
ordered lists.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();
  sortmode smode = ASCENDING;

  if (nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  bool return_idx = nargout > 1;

  octave_value arg = args(0);

  int dim = 0;
  if (nargin > 1)
    {
      if (args(1).is_string ())
	{
	  std::string mode = args(1).string_value();
	  if (mode == "ascend")
	    smode = ASCENDING;
	  else if (mode == "descend")
	    smode = DESCENDING;
	  else
	    {
	      error ("sort: mode must be either \"ascend\" or \"descend\"");
	      return retval;
	    }
	}
      else
	dim = args(1).nint_value () - 1;
    }

  if (nargin > 2)
    {
      if (args(1).is_string ())
	{
	  print_usage ();
	  return retval;
	}

      if (! args(2).is_string ())
	{
	  error ("sort: mode must be a string");
	  return retval;
	}
      std::string mode = args(2).string_value();
      if (mode == "ascend")
	smode = ASCENDING;
      else if (mode == "descend")
	smode = DESCENDING;
      else
	{
	  error ("sort: mode must be either \"ascend\" or \"descend\"");
	  return retval;
	}
    }

  dim_vector dv = arg.dims ();
  if (error_state)
    {
      gripe_wrong_type_arg ("sort", arg);
      return retval;
    }
  if (nargin == 1 || args(1).is_string ())
    {
      // Find first non singleton dimension
      for (int i = 0; i < dv.length (); i++)
	if (dv(i) > 1)
	  {
	    dim = i;
	    break;
	  }
    }
  else
    {
      if (dim < 0 || dim > dv.length () - 1)
	{
	  error ("sort: dim must be a valid dimension");
	  return retval;
	}
    }

  if (return_idx)
    {
      Array<octave_idx_type> sidx;

      retval (0) = arg.sort (sidx, dim, smode);
      retval (1) = NDArray (sidx, true);
    }
  else
    retval(0) = arg.sort (dim, smode);

  return retval;
}

/*

%% Double
%!assert (sort ([NaN, 1, -1, 2, Inf]), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], 1), [NaN, 1, -1, 2, Inf])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2), [-1, 1, 2, Inf, NaN])
%!error (sort ([NaN, 1, -1, 2, Inf], 3))
%!assert (sort ([NaN, 1, -1, 2, Inf], "ascend"), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2, "ascend"), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], "descend"), [NaN, Inf, 2, 1, -1])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2, "descend"), [NaN, Inf, 2, 1, -1])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4]), [3, 1, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4], 1), [3, 1, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4], 2), [1, 3, 5, 7; 2, 4, 6, 8])
%!assert (sort (1), 1)

%!test
%! [v, i] = sort ([NaN, 1, -1, Inf, 1]);
%! assert (v, [-1, 1, 1, Inf, NaN])
%! assert (i, [3, 2, 5, 4, 1])

%% Complex
%!assert (sort ([NaN, 1i, -1, 2, Inf]), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 1), [NaN, 1i, -1, 2, Inf])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2), [1i, -1, 2, Inf, NaN])
%!error (sort ([NaN, 1i, -1, 2, Inf], 3))
%!assert (sort ([NaN, 1i, -1, 2, Inf], "ascend"), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2, "ascend"), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], "descend"), [NaN, Inf, 2, -1, 1i])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2, "descend"), [NaN, Inf, 2, -1, 1i])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4]), [3, 1i, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4], 1), [3, 1i, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4], 2), [1i, 3, 5, 7; 2, 4, 6, 8])
%!assert (sort (1i), 1i)

%!test
%! [v, i] = sort ([NaN, 1i, -1, Inf, 1, 1i]);
%! assert (v, [1, 1i, 1i, -1, Inf, NaN])
%! assert (i, [5, 2, 6, 3, 4, 1])

%% Single
%!assert (sort (single([NaN, 1, -1, 2, Inf])), single([-1, 1, 2, Inf, NaN]))
%!assert (sort (single([NaN, 1, -1, 2, Inf]), 1), single([NaN, 1, -1, 2, Inf]))
%!assert (sort (single([NaN, 1, -1, 2, Inf]), 2), single([-1, 1, 2, Inf, NaN]))
%!error (sort (single([NaN, 1, -1, 2, Inf]), 3))
%!assert (sort (single([NaN, 1, -1, 2, Inf]), "ascend"), single([-1, 1, 2, Inf, NaN]))
%!assert (sort (single([NaN, 1, -1, 2, Inf]), 2, "ascend"), single([-1, 1, 2, Inf, NaN]))
%!assert (sort (single([NaN, 1, -1, 2, Inf]), "descend"), single([NaN, Inf, 2, 1, -1]))
%!assert (sort (single([NaN, 1, -1, 2, Inf]), 2, "descend"), single([NaN, Inf, 2, 1, -1]))
%!assert (sort (single([3, 1, 7, 5; 8, 2, 6, 4])), single([3, 1, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single([3, 1, 7, 5; 8, 2, 6, 4]), 1), single([3, 1, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single([3, 1, 7, 5; 8, 2, 6, 4]), 2), single([1, 3, 5, 7; 2, 4, 6, 8]))
%!assert (sort (single(1)), single(1))

%!test
%! [v, i] = sort (single([NaN, 1, -1, Inf, 1]));
%! assert (v, single([-1, 1, 1, Inf, NaN]))
%! assert (i, [3, 2, 5, 4, 1])

%% Single Complex
%!assert (sort (single([NaN, 1i, -1, 2, Inf])), single([1i, -1, 2, Inf, NaN]))
%!assert (sort (single([NaN, 1i, -1, 2, Inf]), 1), single([NaN, 1i, -1, 2, Inf]))
%!assert (sort (single([NaN, 1i, -1, 2, Inf]), 2), single([1i, -1, 2, Inf, NaN]))
%!error (sort (single([NaN, 1i, -1, 2, Inf]), 3))
%!assert (sort (single([NaN, 1i, -1, 2, Inf]), "ascend"), single([1i, -1, 2, Inf, NaN]))
%!assert (sort (single([NaN, 1i, -1, 2, Inf]), 2, "ascend"), single([1i, -1, 2, Inf, NaN]))
%!assert (sort (single([NaN, 1i, -1, 2, Inf]), "descend"), single([NaN, Inf, 2, -1, 1i]))
%!assert (sort (single([NaN, 1i, -1, 2, Inf]), 2, "descend"), single([NaN, Inf, 2, -1, 1i]))
%!assert (sort (single([3, 1i, 7, 5; 8, 2, 6, 4])), single([3, 1i, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single([3, 1i, 7, 5; 8, 2, 6, 4]), 1), single([3, 1i, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single([3, 1i, 7, 5; 8, 2, 6, 4]), 2), single([1i, 3, 5, 7; 2, 4, 6, 8]))
%!assert (sort (single(1i)),single( 1i))

%!test
%! [v, i] = sort (single([NaN, 1i, -1, Inf, 1, 1i]));
%! assert (v, single([1, 1i, 1i, -1, Inf, NaN]))
%! assert (i, [5, 2, 6, 3, 4, 1])

%% Bool
%!assert (sort ([true, false, true, false]), [false, false, true, true])
%!assert (sort ([true, false, true, false], 1), [true, false, true, false])
%!assert (sort ([true, false, true, false], 2), [false, false, true, true])
%!error (sort ([true, false, true, false], 3))
%!assert (sort ([true, false, true, false], "ascend"), [false, false, true, true])
%!assert (sort ([true, false, true, false], 2, "ascend"), [false, false, true, true])
%!assert (sort ([true, false, true, false], "descend"), [true, true, false, false])
%!assert (sort ([true, false, true, false], 2, "descend"), [true, true, false, false])
%!assert (sort (true), true)

%!test
%! [v, i] = sort ([true, false, true, false]);
%! assert (v, [false, false, true, true])
%! assert (i, [2, 4, 1, 3])

%% Sparse Double
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf])), sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 1), sparse ([0, NaN, 1, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2), sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!error (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 3))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), "ascend"), sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2, "ascend"), sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), "descend"), sparse ([NaN, Inf, 2, 1, 0, 0, -1]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2, "descend"), sparse ([NaN, Inf, 2, 1, 0, 0, -1]))

%!shared a
%! a = randn (10, 10);
%! a (a < 0) = 0;
%!assert (sort (sparse (a)), sparse (sort (a)))
%!assert (sort (sparse (a), 1), sparse (sort (a, 1)))
%!assert (sort (sparse (a), 2), sparse (sort (a, 2)))
%!test
%! [v, i] = sort (a);
%! [vs, is] = sort (sparse (a));
%! assert (vs, sparse (v))
%! assert (is, i)

%% Sparse Complex
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf])), sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 1), sparse ([0, NaN, 1i, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2), sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!error (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 3))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), "ascend"), sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2, "ascend"), sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), "descend"), sparse ([NaN, Inf, 2, -1, 1i, 0, 0]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2, "descend"), sparse ([NaN, Inf, 2, -1, 1i, 0, 0]))

%!shared a
%! a = randn (10, 10); 
%! a (a < 0) = 0;
%! a = 1i * a;
%!assert (sort (sparse (a)), sparse (sort (a)))
%!assert (sort (sparse (a), 1), sparse (sort (a, 1)))
%!assert (sort (sparse (a), 2), sparse (sort (a, 2)))
%!test
%! [v, i] = sort (a);
%! [vs, is] = sort (sparse (a));
%! assert (vs, sparse (v))
%! assert (is, i)

%% Sparse Bool
%!assert (sort (sparse ([true, false, true, false])), sparse ([false, false, true, true]))
%!assert (sort (sparse([true, false, true, false]), 1), sparse ([true, false, true, false]))
%!assert (sort (sparse ([true, false, true, false]), 2), sparse ([false, false, true, true]))
%!error (sort (sparse ([true, false, true, false], 3)))
%!assert (sort (sparse ([true, false, true, false]), "ascend"), sparse([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), 2, "ascend"), sparse([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), "descend"), sparse ([true, true, false, false]))
%!assert (sort (sparse ([true, false, true, false]), 2, "descend"), sparse([true, true, false, false]))

%!test
%! [v, i] = sort (sparse([true, false, true, false]));
%! assert (v, sparse([false, false, true, true]))
%! assert (i, [2, 4, 1, 3])

%% Cell string array
%!shared a, b, c
%! a = {"Alice", "Cecile", "Eric", "Barry", "David"};
%! b = {"Alice", "Barry", "Cecile", "David", "Eric"};
%! c = {"Eric", "David", "Cecile", "Barry", "Alice"};
%!assert (sort (a), b);
%!assert (sort (a, 1), a)
%!assert (sort (a, 2), b)
%!error (sort (a, 3))
%!assert (sort (a, "ascend"), b)
%!assert (sort (a, 2, "ascend"), b)
%!assert (sort (a, "descend"), c)
%!assert (sort (a, 2, "descend"), c)

%!test
%! [v, i] = sort (a);
%! assert (i, [1, 4, 2, 5, 3])

%!error <Invalid call to sort.*> sort ();
%!error <Invalid call to sort.*> sort (1, 2, 3, 4);

*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
