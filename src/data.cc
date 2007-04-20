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

#include <cfloat>
#include <cmath>

#include <string>

#include "lo-ieee.h"
#include "str-vec.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "ov.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "variables.h"
#include "oct-obj.h"
#include "utils.h"
#include "Cell.h"
#include "oct-map.h"
#include "pt-mat.h"

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

static Matrix
map_d_m (d_dd_fcn f, double x, const Matrix& y)
{
  octave_idx_type nr = y.rows ();
  octave_idx_type nc = y.columns ();

  Matrix retval (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	retval (i, j) = f (x, y (i, j));
      }

  return retval;
}

static Matrix
map_m_d (d_dd_fcn f, const Matrix& x, double y)
{
  octave_idx_type nr = x.rows ();
  octave_idx_type nc = x.columns ();

  Matrix retval (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	retval (i, j) = f (x (i, j), y);
      }

  return retval;
}

static Matrix
map_m_m (d_dd_fcn f, const Matrix& x, const Matrix& y)
{
  octave_idx_type x_nr = x.rows ();
  octave_idx_type x_nc = x.columns ();

  octave_idx_type y_nr = y.rows ();
  octave_idx_type y_nc = y.columns ();

  assert (x_nr == y_nr && x_nc == y_nc);

  Matrix retval (x_nr, x_nc);

  for (octave_idx_type j = 0; j < x_nc; j++)
    for (octave_idx_type i = 0; i < x_nr; i++)
      {
	OCTAVE_QUIT;
	retval (i, j) = f (x (i, j), y (i, j));
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
      octave_value arg_y = args(0);
      octave_value arg_x = args(1);

      octave_idx_type y_nr = arg_y.rows ();
      octave_idx_type y_nc = arg_y.columns ();

      octave_idx_type x_nr = arg_x.rows ();
      octave_idx_type x_nc = arg_x.columns ();

      int arg_y_empty = empty_arg ("atan2", y_nr, y_nc);
      int arg_x_empty = empty_arg ("atan2", x_nr, x_nc);

      if (arg_y_empty > 0 && arg_x_empty > 0)
	return octave_value (Matrix ());
      else if (arg_y_empty || arg_x_empty)
	return retval;

      octave_idx_type y_is_scalar = (y_nr == 1 && y_nc == 1);
      octave_idx_type x_is_scalar = (x_nr == 1 && x_nc == 1);

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
	      Matrix x = arg_x.matrix_value ();

	      if (! error_state)
		retval = map_d_m (atan2, y, x);
	    }
	}
      else if (x_is_scalar)
	{
	  Matrix y = arg_y.matrix_value ();

	  if (! error_state)
	    {
	      double x = arg_x.double_value ();

	      if (! error_state)
		retval = map_m_d (atan2, y, x);
	    }
	}
      else if (y_nr == x_nr && y_nc == x_nc)
	{
	  Matrix y = arg_y.matrix_value ();

	  if (! error_state)
	    {
	      Matrix x = arg_x.matrix_value ();

	      if (! error_state)
		retval = map_m_m (atan2, y, x);
	    }
	}
      else
	error ("atan2: nonconformant matrices");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fmod, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} fmod (@var{x}, @var{y})\n\
Compute the floating point remainder of dividing @var{x} by @var{y}\n\
using the C library function @code{fmod}.  The result has the same\n\
sign as @var{x}.  If @var{y} is zero, the result implementation-defined.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    {
      octave_value arg_x = args(0);
      octave_value arg_y = args(1);

      octave_idx_type y_nr = arg_y.rows ();
      octave_idx_type y_nc = arg_y.columns ();

      octave_idx_type x_nr = arg_x.rows ();
      octave_idx_type x_nc = arg_x.columns ();

      int arg_y_empty = empty_arg ("fmod", y_nr, y_nc);
      int arg_x_empty = empty_arg ("fmod", x_nr, x_nc);

      if (arg_y_empty > 0 && arg_x_empty > 0)
	return octave_value (Matrix ());
      else if (arg_y_empty || arg_x_empty)
	return retval;

      octave_idx_type y_is_scalar = (y_nr == 1 && y_nc == 1);
      octave_idx_type x_is_scalar = (x_nr == 1 && x_nc == 1);

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
	      Matrix x = arg_x.matrix_value ();

	      if (! error_state)
		retval = map_m_d (fmod, x, y);
	    }
	}
      else if (x_is_scalar)
	{
	  Matrix y = arg_y.matrix_value ();

	  if (! error_state)
	    {
	      double x = arg_x.double_value ();

	      if (! error_state)
		retval = map_d_m (fmod, x, y);
	    }
	}
      else if (y_nr == x_nr && y_nc == x_nc)
	{
	  Matrix y = arg_y.matrix_value ();

	  if (! error_state)
	    {
	      Matrix x = arg_x.matrix_value ();

	      if (! error_state)
		retval = map_m_m (fmod, x, y);
	    }
	}
      else
	error ("fmod: nonconformant matrices");
    }
  else
    print_usage ();

  return retval;
}

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

// FIXME -- we could eliminate some duplicate code here with
// some template functions or macros.

static octave_value
make_diag (const Matrix& v, octave_idx_type k)
{
  octave_idx_type nr = v.rows ();
  octave_idx_type nc = v.columns ();
  assert (nc == 1 || nr == 1);

  octave_value retval;

  octave_idx_type roff = 0;
  octave_idx_type coff = 0;
  if (k > 0)
    {
      roff = 0;
      coff = k;
    }
  else if (k < 0)
    {
      roff = -k;
      coff = 0;
    }

  if (nr == 1)
    {
      octave_idx_type n = nc + std::abs (k);
      Matrix m (n, n, 0.0);
      for (octave_idx_type i = 0; i < nc; i++)
	m (i+roff, i+coff) = v (0, i);
      retval = m;
    }
  else
    {
      octave_idx_type n = nr + std::abs (k);
      Matrix m (n, n, 0.0);
      for (octave_idx_type i = 0; i < nr; i++)
	m (i+roff, i+coff) = v (i, 0);
      retval = m;
    }

  return retval;
}

static octave_value
make_diag (const ComplexMatrix& v, octave_idx_type k)
{
  octave_idx_type nr = v.rows ();
  octave_idx_type nc = v.columns ();
  assert (nc == 1 || nr == 1);

  octave_value retval;

  octave_idx_type roff = 0;
  octave_idx_type coff = 0;
  if (k > 0)
    {
      roff = 0;
      coff = k;
    }
  else if (k < 0)
    {
      roff = -k;
      coff = 0;
    }

  if (nr == 1)
    {
      octave_idx_type n = nc + std::abs (k);
      ComplexMatrix m (n, n, 0.0);
      for (octave_idx_type i = 0; i < nc; i++)
	m (i+roff, i+coff) = v (0, i);
      retval = m;
    }
  else
    {
      octave_idx_type n = nr + std::abs (k);
      ComplexMatrix m (n, n, 0.0);
      for (octave_idx_type i = 0; i < nr; i++)
	m (i+roff, i+coff) = v (i, 0);
      retval = m;
    }

  return retval;
}

static octave_value
make_diag (const octave_value& arg)
{
  octave_value retval;

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  octave_idx_type nr = m.rows ();
	  octave_idx_type nc = m.columns ();

	  if (nr == 0 || nc == 0)
	    retval = Matrix ();
	  else if (nr == 1 || nc == 1)
	    retval = make_diag (m, 0);
	  else
	    {
	      ColumnVector v = m.diag ();
	      if (v.numel () > 0)
		retval = v;
	    }
	}
      else
	gripe_wrong_type_arg ("diag", arg);
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix cm = arg.complex_matrix_value ();

      if (! error_state)
	{
	  octave_idx_type nr = cm.rows ();
	  octave_idx_type nc = cm.columns ();

	  if (nr == 0 || nc == 0)
	    retval = Matrix ();
	  else if (nr == 1 || nc == 1)
	    retval = make_diag (cm, 0);
	  else
	    {
	      ComplexColumnVector v = cm.diag ();
	      if (v.numel () > 0)
		retval = v;
	    }
	}
      else
	gripe_wrong_type_arg ("diag", arg);
    }
  else
    gripe_wrong_type_arg ("diag", arg);

  return retval;
}

static octave_value
make_diag (const octave_value& a, const octave_value& b)
{
  octave_value retval;

  octave_idx_type k = b.int_value ();

  if (error_state)
    {
      error ("diag: invalid second argument");      
      return retval;
    }

  if (a.is_real_type ())
    {
      Matrix m = a.matrix_value ();

      if (! error_state)
	{
	  octave_idx_type nr = m.rows ();
	  octave_idx_type nc = m.columns ();

	  if (nr == 1 || nc == 1)
	    retval = make_diag (m, k);
	  else if (nr == 0 || nc == 0)
	    retval = Matrix ();
	  else
	    {
	      ColumnVector d = m.diag (k);
	      retval = d;
	    }
	}
    }
  else if (a.is_complex_type ())
    {
      ComplexMatrix cm = a.complex_matrix_value ();

      if (! error_state)
	{
	  octave_idx_type nr = cm.rows ();
	  octave_idx_type nc = cm.columns ();

	  if (nr == 1 || nc == 1)
	    retval = make_diag (cm, k);
	  else if (nr == 0 || nc == 0)
	    retval = Matrix ();
	  else
	    {
	      ComplexColumnVector d = cm.diag (k);
	      retval = d;
	    }
	}
    }
  else
    gripe_wrong_type_arg ("diag", a);

  return retval;
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
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = make_diag (args(0));
  else if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    retval = make_diag (args(0), args(1));
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

	      Array<int> ra_idx (dv.length (), 0);

	      for (int j = i; j < n_args; j++)
		{
		  tmp = do_cat_op (tmp, args (j), ra_idx);

		  if (error_state)
		    return retval;

		  dim_vector dv_tmp = args (j).dims ();

		  ra_idx (dim) += (dim < dv_tmp.length () ? dv_tmp (dim) : 1);
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
do_permute (const octave_value_list& args, bool inv, const std::string& fname)
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
  return do_permute (args, false, "permute");
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
  return do_permute (args, true, "ipermute");
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
@deftypefn {Built-in Function} {} size_equal (@var{a}, @var{b})\n\
Return true if the dimensions of @var{a} and @var{b} agree.\n\
Trailing singleton dimensions are ignored.\n\
@seealso{size, numel}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      dim_vector a_dims = args(0).dims ();
      dim_vector b_dims = args(1).dims ();

      a_dims.chop_trailing_singletons ();
      b_dims.chop_trailing_singletons ();

      retval = a_dims == b_dims;
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
Note that Octave tends to crop unused memory at the first oppurtunity\n\
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
Sum of elements along dimension @var{dim}.  If @var{dim} is\n\
omitted, it defaults to 1 (column-wise sum).\n\
\n\
As a special case, if @var{x} is a vector and @var{dim} is omitted,\n\
return the sum of the elements.\n\
@end deftypefn")
{
  DATA_REDUCTION (sum);
}

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

DEFUN (isbool, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Functio} {} isbool (@var{x})\n\
Return true if @var{x} is a boolean object.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_bool_type ();
  else
    print_usage ();

  return retval;
}

DEFALIAS (islogical, isbool);

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
Since I (also i, J, and J) is a function, you can use the name(s) for\n\
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
@var{base} and @var{limit}.  The number of elements, @var{n}, must be\n\
greater than 1.  The @var{base} and @var{limit} are always included in\n\
the range.  If @var{base} is greater than @var{limit}, the elements are\n\
stored in decreasing order.  If the number of points is not specified, a\n\
value of 100 is used.\n\
\n\
The @code{linspace} function always returns a row vector.\n\
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
@deftypefn {Function File} {} reshape (@var{a}, @var{m}, @var{n}, @dots{})\n\
@deftypefnx {Function File} {} reshape (@var{a}, @var{siz})\n\
Return a matrix with the given dimensions whose elements are taken\n\
from the matrix @var{a}.  The elements of the matrix are access in\n\
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
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).squeeze ();
  else
    print_usage ();    

  return retval;
}

DEFUN (__vnorm__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __vnorm__ (@var{x}, @var{p})\n\
Compute various norms of the vector @var{x}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      double p_val;

      octave_value p_arg;

      if (nargin == 1)
	p_arg = 2;
      else
	p_arg = args(1);

      if (p_arg.is_string ())
	{
	  std::string p = args(1).string_value ();

	  if (p == "inf")
	    p_val = octave_Inf;
	  else if (p == "fro")
	    p_val = -1;
	  else
	    {
	      error ("norm: unrecognized norm `%s'", p.c_str ());
	      return retval;
	    }
	}
      else
	{
	  p_val = args(1).double_value ();

	  if (error_state)
	    {
	      error ("norm: unrecognized norm value");
	      return retval;
	    }
	}

      octave_value x_arg = args(0);

      if (x_arg.is_real_type ())
	{
	  ColumnVector x (x_arg.vector_value ());

	  if (! error_state)
	    retval = x.norm (p_val);
	  else
	    error ("norm: expecting real vector");
	}
      else
	{
	  ComplexColumnVector x (x_arg.complex_vector_value ());

	  if (! error_state)
	    retval = x.norm (p_val);
	  else
	    error ("norm: expecting complex vector");
	}
    }
  else
    print_usage ();

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
This function is equivalent to @code{x .\\ y}.\n\
@end deftypefn")
{
  BINARY_OP_DEFUN_BODY (op_el_pow);
}

DEFUN (ldivide, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ldivide (@var{x}, @var{y})\n\
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
