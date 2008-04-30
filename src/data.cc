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
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "parse.h"
#include "pt-mat.h"
#include "utils.h"
#include "variables.h"
#include "pager.h"

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

// These mapping functions may also be useful in other places, eh?

typedef double (*d_dd_fcn) (double, double);

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
and @var{x}.  The result is in range -pi to pi.\n\
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

	  if (y_is_scalar && x_is_scalar)
	    {
	      double y = arg_y.double_value ();

	      if (! error_state)
		{
		  double x = arg_x.double_value ();

		  if (! error_state)
		    retval = atan2 (y, x);
		}
	    }
	  else if (y_is_scalar)
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

	  if (y_is_scalar && x_is_scalar)
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
	  else if (y_is_scalar)
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
	  else if (x_is_scalar)
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

      if (y_is_scalar && x_is_scalar)
	{
	  double y = arg_y.double_value ();

	  if (! error_state)
	    {
	      double x = arg_x.double_value ();

	      if (! error_state)
		retval = fmod (x, y);
	    }
	}
      else if (y_is_scalar)
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
  \
  if (nargin > 1 && args(nargin - 1).is_string ()) \
    { \
      std::string str = args(nargin - 1).string_value (); \
      \
      if (! error_state) \
	{ \
	  if (str == "native") \
	    isnative = true; \
	  else if (str != "double") /* Ignore double as no single type */ \
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
  else
    print_usage ();

  return retval;
}

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
	    }

	  // The lines below might seem crazy, since we take a copy
	  // of the first argument, resize it to be empty and then resize
	  // it to be full. This is done since it means that there is no
	  // recopying of data, as would happen if we used a single resize.
	  // It should be noted that resize operation is also significantly 
	  // slower than the do_cat_op function, so it makes sense to have an
	  // empty matrix and copy all data.
	  //
	  // We might also start with a empty octave_value using
	  //   tmp = octave_value_typeinfo::lookup_type (args(1).type_name());
	  // and then directly resize. However, for some types there might be
	  // some additional setup needed, and so this should be avoided.

	  octave_value tmp;

	  int i;
          for (i = 1; i < n_args; i++)
	    {
	      if (! args (i).all_zero_dims ())
		{
		  tmp = args (i);
		  break;
		}
	    }

	  if (i == n_args)
	    retval = Matrix ();
	  else
	    {
	      tmp = tmp.resize (dim_vector (0,0)).resize (dv);

	      if (error_state)
		return retval;

	      int dv_len = dv.length ();
	      Array<octave_idx_type> ra_idx (dv_len, 0);

	      for (int j = i; j < n_args; j++)
		{
		  if (args (j). dims (). any_zero ())
		    continue;

		  tmp = do_cat_op (tmp, args (j), ra_idx);

		  if (error_state)
		    return retval;

		  dim_vector dv_tmp = args (j).dims ();

		  if (dim >= dv_len)
		    {
		      if (j > i)
			error ("%s: indexing error", fname.c_str ());
		      break;
		    }
		  else
		    ra_idx (dim) += (dim < dv_tmp.length () ? 
				     dv_tmp (dim) : 1);
		}

	      retval = tmp;
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
@seealso{size, numel, rows, length, isscalar, isvector, and ismatrix}\n\
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

DEFUN (isinteger, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isinteger (@var{x})\n\
Return true if @var{x} is an integer object (int8, uint8, int16, etc.).\n\
Note that @code{isinteger (14)} is false because numeric constants in\n\
are double precision floating point values.\n\
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

	  if (error_state)
	    error ("complex: invalid conversion");
	}
    }
  else if (nargin == 2)
    {
      octave_value re = args(0);
      octave_value im = args(1);

      if (re.numel () == 1)
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

	    case oct_data_conv::dt_single: // FIXME
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
	    case oct_data_conv::dt_single: // FIXME
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
	    case oct_data_conv::dt_single: // FIXME
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
  return fill_matrix (args, lo_ieee_inf_value (), "Inf");
}

DEFALIAS (inf, Inf);

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
@ifinfo\n\
0/0, or @samp{Inf - Inf},\n\
@end ifinfo\n\
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
  return fill_matrix (args, lo_ieee_nan_value (), "NaN");
}

DEFALIAS (nan, NaN);

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
@ifinfo\n\
 @var{e}\n\
@end ifinfo\n\
 satisfies the equation\n\
@iftex\n\
@tex\n\
 $\\log (e) = 1$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
 @code{log} (@var{e}) = 1.\n\
@end ifinfo\n\
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
machines that support 64 bit IEEE floating point arithmetic, @code{eps}\n\
is approximately\n\
@ifinfo\n\
 2.2204e-16.\n\
@end ifinfo\n\
@iftex\n\
@tex\n\
 $2.2204\\times10^{-16}$.\n\
@end tex\n\
@end iftex\n\
@end deftypefn")
{
  return fill_matrix (args, DBL_EPSILON, "eps");
}

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
@ifinfo\n\
 1.7977e+308\n\
@end ifinfo\n\
@iftex\n\
@tex\n\
 $1.7977\\times10^{308}$.\n\
@end tex\n\
@end iftex\n\
@seealso{realmin}\n\
@end deftypefn")
{
  return fill_matrix (args, DBL_MAX, "realmax");
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
@ifinfo\n\
 2.2251e-308\n\
@end ifinfo\n\
@iftex\n\
@tex\n\
 $2.2251\\times10^{-308}$.\n\
@end tex\n\
@end iftex\n\
@seealso{realmax}\n\
@end deftypefn")
{
  return fill_matrix (args, DBL_MIN, "realmin");
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
@ifinfo\n\
  @code{sqrt (-1)}.\n\
@end ifinfo\n\
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
  return fill_matrix (args, lo_ieee_na_value (), "NA");
}

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

	case oct_data_conv::dt_single: // FIXME
	case oct_data_conv::dt_double:
	  retval = identity_matrix<NDArray> (nr, nc);
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
  else
    error ("linspace: expecting third argument to be an integer");

  return retval;
}

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
@seealso{reshape}\n\
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

  if (new_dims.numel () == arg.numel ())
    retval = (new_dims == arg.dims ()) ? arg : arg.reshape (new_dims);
  else
    error ("reshape: size mismatch");

  return retval;
}

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
*/

// Compute various norms of the vector X.

DEFUN (norm, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} norm (@var{a}, @var{p})\n\
Compute the p-norm of the matrix @var{a}.  If the second argument is\n\
missing, @code{p = 2} is assumed.\n\
\n\
If @var{a} is a matrix:\n\
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
@item other\n\
p-norm of @var{a}, @code{(sum (abs (@var{a}) .^ @var{p})) ^ (1/@var{p})}.\n\
@end table\n\
@seealso{cond, svd}\n\
@end deftypefn")
{
  // Currently only handles vector norms for full double/complex
  // vectors internally.  Other cases are handled by __norm__.m.

  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_value x_arg = args(0);

      if (x_arg.is_empty ())
	retval(0) = 0.0;
      else if (x_arg.ndims () == 2)
	{
	  if ((x_arg.rows () == 1 || x_arg.columns () == 1)
	      && ! (x_arg.is_sparse_type () || x_arg.is_integer_type ()))
	    {
	      double p_val = 2;

	      if (nargin == 2)
		{
		  octave_value p_arg = args(1);

		  if (p_arg.is_string ())
		    {
		      std::string p = args(1).string_value ();

		      if (p == "inf")
			p_val = octave_Inf;
		      else if (p == "fro")
			p_val = -1;
		      else
			error ("norm: unrecognized norm `%s'", p.c_str ());
		    }
		  else
		    {
		      p_val = p_arg.double_value ();

		      if (error_state)
			error ("norm: unrecognized norm value");
		    }
		}

	      if (! error_state)
		{
		  if (x_arg.is_real_type ())
		    {
		      MArray<double> x (x_arg.array_value ());

		      if (! error_state)
			retval(0) = x.norm (p_val);
		      else
			error ("norm: expecting real vector");
		    }
		  else
		    {
		      MArray<Complex> x (x_arg.complex_array_value ());

		      if (! error_state)
			retval(0) = x.norm (p_val);
		      else
			error ("norm: expecting complex vector");
		    }
		}
	    }
	  else
	    retval = feval ("__norm__", args);
	}
      else
	error ("norm: only valid for 2-D objects");
    }
  else
    print_usage ();

  // Should not return a sparse type
  if (retval(0).is_sparse_type ())
    {
      if (retval(0).type_name () == "sparse matrix") 
	retval(0) = retval(0).matrix_value ();
      else if (retval(0).type_name () == "sparse complex matrix")
	retval(0) = retval(0).complex_matrix_value ();
      else if (retval(0).type_name () == "sparse bool matrix")
	retval(0) = retval(0).bool_matrix_value ();
    }

  return retval;
}

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

DEFUN (ctranspose, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ctranspose (@var{x})\n\
This function is equivalent to @code{x'}.\n\
@end deftypefn")
{
  UNARY_OP_DEFUN_BODY (op_hermitian);
}

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

      octave_idx_type *ps = sidx.fortran_vec ();
      NDArray midx (sidx.dims ());
      double *pm = midx.fortran_vec ();

      for (octave_idx_type i = 0; i < sidx.numel (); i++)
	pm [i] = static_cast<double> 
	  (ps [i] + static_cast<octave_idx_type> (1));

      retval (1) = midx;
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

*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
