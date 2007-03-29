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

// Based on Tony Richardson's filter.m.
//
// Originally translated to C++ by KH (Kurt.Hornik@wu-wien.ac.at)
// with help from Fritz Leisch and Andreas Weingessel on Oct 20, 1994.
//
// Rewritten to use templates to handle both real and complex cases by
// jwe, Wed Nov  1 19:15:29 1995.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern MArrayN<double>
filter (MArray<double>&, MArray<double>&, MArrayN<double>&, int dim);

extern MArrayN<Complex>
filter (MArray<Complex>&, MArray<Complex>&, MArrayN<Complex>&, int dim);
#endif

template <class T>
MArrayN<T>
filter (MArray<T>& b, MArray<T>& a, MArrayN<T>& x, MArrayN<T>& si, 
	int dim = 0)
{
  MArrayN<T> y;

  octave_idx_type a_len  = a.length ();
  octave_idx_type b_len  = b.length ();

  octave_idx_type ab_len = a_len > b_len ? a_len : b_len;

  b.resize (ab_len, 0.0);
  if (a_len > 1)
    a.resize (ab_len, 0.0);

  T norm = a (0);

  if (norm == 0.0)
    {
      error ("filter: the first element of a must be non-zero");
      return y;
    }

  dim_vector x_dims = x.dims ();
  if (dim < 0 || dim > x_dims.length ())
    {
      error ("filter: filtering over invalid dimension");
      return y;
    }

  octave_idx_type x_len = x_dims(dim);

  dim_vector si_dims = si.dims ();
  octave_idx_type si_len = si_dims(0);

  if (si_len != ab_len - 1)
    {
      error ("filter: first dimension of si must be of length max (length (a), length (b)) - 1");
      return y;
    }

  if (si_dims.length () != x_dims.length ())
    {
      error ("filter: dimensionality of si and x must agree");
      return y;
    }

  octave_idx_type si_dim = 0;
  for (octave_idx_type i = 0; i < x_dims.length (); i++)
    {
      if (i == dim)
	continue;
     
      if (x_dims(i) == 1)
	continue;
 
      if (si_dims(++si_dim) != x_dims(i))
	{
	  error ("filter: dimensionality of si and x must agree");
	  return y;
	}
    }

  if (norm != 1.0)
    {
      a = a / norm;
      b = b / norm;
    }

  if (a_len <= 1 && si_len <= 0)
    return b(0) * x;

  y.resize (x_dims, 0.0);

  int x_stride = 1;
  for (int i = 0; i < dim; i++)
    x_stride *= x_dims(i);

  octave_idx_type x_num = x_dims.numel () / x_len;
  for (octave_idx_type num = 0; num < x_num; num++)
    {
      octave_idx_type x_offset;
      if (x_stride == 1)
	x_offset = num * x_len;
      else
	{
	  octave_idx_type x_offset2 = 0;
	  x_offset = num;
	  while (x_offset >= x_stride)
	    {
	      x_offset -= x_stride;
	      x_offset2++;
	    }
	  x_offset += x_offset2 * x_stride * x_len;
	}
      octave_idx_type si_offset = num * si_len;

      if (a_len > 1)
	{
	  T *py = y.fortran_vec ();
	  T *psi = si.fortran_vec ();

	  const T *pa = a.data ();
	  const T *pb = b.data ();
	  const T *px = x.data ();

	  psi += si_offset;

	  for (octave_idx_type i = 0, idx = x_offset; i < x_len; i++, idx += x_stride)
	    {
	      py[idx] = psi[0] + pb[0] * px[idx];

	      if (si_len > 0)
		{
		  for (octave_idx_type j = 0; j < si_len - 1; j++)
		    {
		      OCTAVE_QUIT;

		      psi[j] = psi[j+1] - pa[j+1] * py[idx] + pb[j+1] * px[idx];
		    }

		  psi[si_len-1] = pb[si_len] * px[idx] - pa[si_len] * py[idx];
		}
	      else
		{
		  OCTAVE_QUIT;

		  psi[0] = pb[si_len] * px[idx] - pa[si_len] * py[idx];
		}
	    }
	}
      else if (si_len > 0)
	{
	  T *py = y.fortran_vec ();
	  T *psi = si.fortran_vec ();

	  const T *pb = b.data ();
	  const T *px = x.data ();

	  psi += si_offset;

	  for (octave_idx_type i = 0, idx = x_offset; i < x_len; i++, idx += x_stride)
	    {
	      py[idx] = psi[0] + pb[0] * px[idx];

	      if (si_len > 1)
		{
		  for (octave_idx_type j = 0; j < si_len - 1; j++)
		    {
		      OCTAVE_QUIT;

		      psi[j] = psi[j+1] + pb[j+1] * px[idx];
		    }

		  psi[si_len-1] = pb[si_len] * px[idx];
		}
	      else
		{
		  OCTAVE_QUIT;

		  psi[0] = pb[1] * px[idx];
		}
	    }
	}
    }
  
  return y;
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern MArrayN<double>
filter (MArray<double>&, MArray<double>&, MArrayN<double>&,
	MArrayN<double>&, int dim);

extern MArrayN<Complex>
filter (MArray<Complex>&, MArray<Complex>&, MArrayN<Complex>&,
	MArrayN<Complex>&, int dim);
#endif

template <class T>
MArrayN<T>
filter (MArray<T>& b, MArray<T>& a, MArrayN<T>& x, int dim = -1)
{
  dim_vector x_dims = x.dims();

  if (dim < 0)
    {
      // Find first non-singleton dimension
      while (dim < x_dims.length () && x_dims(dim) <= 1)
	dim++;
  
      // All dimensions singleton, pick first dimension
      if (dim == x_dims.length ())
	dim = 0;
    }
  else
    if (dim < 0 || dim > x_dims.length ())
      {
	error ("filter: filtering over invalid dimension");
	return MArrayN<T> ();
      }

  octave_idx_type a_len = a.length ();
  octave_idx_type b_len = b.length ();

  octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;
  dim_vector si_dims = x.dims ();
  for (int i = dim; i > 0; i--)
    si_dims(i) = si_dims(i-1);
  si_dims(0) = si_len;
  
  MArrayN<T> si (si_dims, T (0.0));

  return filter (b, a, x, si, dim);
}

DEFUN_DLD (filter, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {y =} filter (@var{b}, @var{a}, @var{x})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, @var{si})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, [], @var{dim})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, @var{si}, @var{dim})\n\
Return the solution to the following linear, time-invariant difference\n\
equation:\n\
@iftex\n\
@tex\n\
$$\n\
\\sum_{k=0}^N a_{k+1} y_{n-k} = \\sum_{k=0}^M b_{k+1} x_{n-k}, \\qquad\n\
 1 \\le n \\le P\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@smallexample\n\
   N                   M\n\
  SUM a(k+1) y(n-k) = SUM b(k+1) x(n-k)      for 1<=n<=length(x)\n\
  k=0                 k=0\n\
@end smallexample\n\
@end ifinfo\n\
\n\
@noindent\n\
where\n\
@ifinfo\n\
 N=length(a)-1 and M=length(b)-1.\n\
@end ifinfo\n\
@iftex\n\
@tex\n\
 $a \\in \\Re^{N-1}$, $b \\in \\Re^{M-1}$, and $x \\in \\Re^P$.\n\
@end tex\n\
@end iftex\n\
over the first non-singleton dimension of @var{x} or over @var{dim} if\n\
supplied. An equivalent form of this equation is:\n\
@iftex\n\
@tex\n\
$$\n\
y_n = -\\sum_{k=1}^N c_{k+1} y_{n-k} + \\sum_{k=0}^M d_{k+1} x_{n-k}, \\qquad\n\
 1 \\le n \\le P\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@smallexample\n\
            N                   M\n\
  y(n) = - SUM c(k+1) y(n-k) + SUM d(k+1) x(n-k)  for 1<=n<=length(x)\n\
           k=1                 k=0\n\
@end smallexample\n\
@end ifinfo\n\
\n\
@noindent\n\
where\n\
@ifinfo\n\
 c = a/a(1) and d = b/a(1).\n\
@end ifinfo\n\
@iftex\n\
@tex\n\
$c = a/a_1$ and $d = b/a_1$.\n\
@end tex\n\
@end iftex\n\
\n\
If the fourth argument @var{si} is provided, it is taken as the\n\
initial state of the system and the final state is returned as\n\
@var{sf}.  The state vector is a column vector whose length is\n\
equal to the length of the longest coefficient vector minus one.\n\
If @var{si} is not supplied, the initial state vector is set to all\n\
zeros.\n\
\n\
In terms of the z-transform, y is the result of passing the discrete-\n\
time signal x through a system characterized by the following rational\n\
system function:\n\
@iftex\n\
@tex\n\
$$\n\
H(z) = {\\displaystyle\\sum_{k=0}^M d_{k+1} z^{-k}\n\
        \\over 1 + \\displaystyle\\sum_{k+1}^N c_{k+1} z^{-k}}\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
             M\n\
            SUM d(k+1) z^(-k)\n\
            k=0\n\
  H(z) = ----------------------\n\
               N\n\
          1 + SUM c(k+1) z^(-k)\n\
              k=1\n\
@end example\n\
@end ifinfo\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin  = args.length ();

  if (nargin < 3 || nargin > 5)
    {
      print_usage ();
      return retval;
    }

  const char *errmsg = "filter: arguments a and b must be vectors";

  int dim;
  dim_vector x_dims = args(2).dims ();

  if (nargin == 5)
    {
      dim = args(4).nint_value() - 1;
      if (dim < 0 || dim >= x_dims.length ())
	{
	  error ("filter: filtering over invalid dimension");
	  return retval;
	}
    }
  else
    {
      // Find first non-singleton dimension
      dim = 0;
      while (dim < x_dims.length () && x_dims(dim) <= 1)
	dim++;
  
      // All dimensions singleton, pick first dimension
      if (dim == x_dims.length ())
	dim = 0;
    }

  if (args(0).is_complex_type ()
      || args(1).is_complex_type ()
      || args(2).is_complex_type ()
      || (nargin >= 4 && args(3).is_complex_type ()))
    {
      ComplexColumnVector b (args(0).complex_vector_value ());
      ComplexColumnVector a (args(1).complex_vector_value ());

      ComplexNDArray x (args(2).complex_array_value ());

      if (! error_state)
	{
	  ComplexNDArray si;

	  if (nargin == 3 || args(3).is_empty ())
	    {
	      octave_idx_type a_len = a.length ();
	      octave_idx_type b_len = b.length ();

	      octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;

	      dim_vector si_dims = x.dims ();
	      for (int i = dim; i > 0; i--)
		si_dims(i) = si_dims(i-1);
	      si_dims(0) = si_len;

	      si.resize (si_dims, 0.0);
	    }
	  else
	    {
	      dim_vector si_dims = args (3).dims ();
	      bool si_is_vector = true;
	      for (int i = 0; i < si_dims.length (); i++)
		if (si_dims(i) != 1 && si_dims(i) < si_dims.numel ())
		  {
		    si_is_vector = false;
		    break;
		  }

	      si = args(3).complex_array_value ();

	      if (si_is_vector)
		si = si.reshape (dim_vector (si.numel (), 1));
	    }

	  if (! error_state)
	    {
	      ComplexNDArray y (filter (b, a, x, si, dim));

	      if (nargout == 2)
		retval(1) = si;

	      retval(0) = y;
	    }
	  else
	    error (errmsg);
	}
      else
	error (errmsg);
    }
  else
    {
      ColumnVector b (args(0).vector_value ());
      ColumnVector a (args(1).vector_value ());

      NDArray x (args(2).array_value ());

      if (! error_state)
	{
	  NDArray si;

	  if (nargin == 3 || args(3).is_empty ())
	    {
	      octave_idx_type a_len = a.length ();
	      octave_idx_type b_len = b.length ();

	      octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;

	      dim_vector si_dims = x.dims ();
	      for (int i = dim; i > 0; i--)
		si_dims(i) = si_dims(i-1);
	      si_dims(0) = si_len;

	      si.resize (si_dims, 0.0);
	    }
	  else
	    {
	      dim_vector si_dims = args (3).dims ();
	      bool si_is_vector = true;
	      for (int i = 0; i < si_dims.length (); i++)
		if (si_dims(i) != 1 && si_dims(i) < si_dims.numel ())
		  {
		    si_is_vector = false;
		    break;
		  }

	      si = args(3).array_value ();

	      if (si_is_vector)
		si = si.reshape (dim_vector (si.numel (), 1));
	    }

	  if (! error_state)
	    {
	      NDArray y (filter (b, a, x, si, dim));

	      if (nargout == 2)
		retval(1) = si;

	      retval(0) = y;
	    }
	  else
	    error (errmsg);
	}
      else
	error (errmsg);
    }

  return retval;
}

template MArrayN<double>
filter (MArray<double>&, MArray<double>&, MArrayN<double>&,
	MArrayN<double>&, int dim);

template MArrayN<double>
filter (MArray<double>&, MArray<double>&, MArrayN<double>&, int dim);

template MArrayN<Complex>
filter (MArray<Complex>&, MArray<Complex>&, MArrayN<Complex>&,
	MArrayN<Complex>&, int dim);

template MArrayN<Complex>
filter (MArray<Complex>&, MArray<Complex>&, MArrayN<Complex>&, int dim);

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/  
