/*

Copyright (C) 1996, 1997, 1999, 2000, 2002, 2004, 2005, 2006, 2007,
              2008, 2009 John W. Eaton

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
extern MArray<double>
filter (MArray<double>&, MArray<double>&, MArray<double>&, int dim);

extern MArray<Complex>
filter (MArray<Complex>&, MArray<Complex>&, MArray<Complex>&, int dim);

extern MArray<float>
filter (MArray<float>&, MArray<float>&, MArray<float>&, int dim);

extern MArray<FloatComplex>
filter (MArray<FloatComplex>&, MArray<FloatComplex>&, MArray<FloatComplex>&, int dim);
#endif

template <class T>
MArray<T>
filter (MArray<T>& b, MArray<T>& a, MArray<T>& x, MArray<T>& si, 
        int dim = 0)
{
  MArray<T> y;

  octave_idx_type a_len  = a.length ();
  octave_idx_type b_len  = b.length ();

  octave_idx_type ab_len = a_len > b_len ? a_len : b_len;

  b.resize (ab_len, 1, 0.0);
  if (a_len > 1)
    a.resize (ab_len, 1, 0.0);

  T norm = a (0);

  if (norm == static_cast<T>(0.0))
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

  if (x_len == 0)
    return x;

  if (norm != static_cast<T>(1.0))
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
extern MArray<double>
filter (MArray<double>&, MArray<double>&, MArray<double>&,
        MArray<double>&, int dim);

extern MArray<Complex>
filter (MArray<Complex>&, MArray<Complex>&, MArray<Complex>&,
        MArray<Complex>&, int dim);

extern MArray<float>
filter (MArray<float>&, MArray<float>&, MArray<float>&,
        MArray<float>&, int dim);

extern MArray<FloatComplex>
filter (MArray<FloatComplex>&, MArray<FloatComplex>&, MArray<FloatComplex>&,
        MArray<FloatComplex>&, int dim);
#endif

template <class T>
MArray<T>
filter (MArray<T>& b, MArray<T>& a, MArray<T>& x, int dim = -1)
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
        return MArray<T> ();
      }

  octave_idx_type a_len = a.length ();
  octave_idx_type b_len = b.length ();

  octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;
  dim_vector si_dims = x.dims ();
  for (int i = dim; i > 0; i--)
    si_dims(i) = si_dims(i-1);
  si_dims(0) = si_len;
  
  MArray<T> si (si_dims, T (0.0));

  return filter (b, a, x, si, dim);
}

DEFUN_DLD (filter, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {y =} filter (@var{b}, @var{a}, @var{x})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, @var{si})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, [], @var{dim})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, @var{si}, @var{dim})\n\
Return the solution to the following linear, time-invariant difference\n\
equation:\n\
@tex\n\
$$\n\
\\sum_{k=0}^N a_{k+1} y_{n-k} = \\sum_{k=0}^M b_{k+1} x_{n-k}, \\qquad\n\
 1 \\le n \\le P\n\
$$\n\
@end tex\n\
@ifnottex\n\
@c Set example in small font to prevent overfull line\n\
\n\
@smallexample\n\
@group\n\
   N                   M\n\
  SUM a(k+1) y(n-k) = SUM b(k+1) x(n-k)      for 1<=n<=length(x)\n\
  k=0                 k=0\n\
@end group\n\
@end smallexample\n\
\n\
@end ifnottex\n\
\n\
@noindent\n\
where\n\
@ifnottex\n\
 N=length(a)-1 and M=length(b)-1.\n\
@end ifnottex\n\
@tex\n\
 $a \\in \\Re^{N-1}$, $b \\in \\Re^{M-1}$, and $x \\in \\Re^P$.\n\
@end tex\n\
over the first non-singleton dimension of @var{x} or over @var{dim} if\n\
supplied.  An equivalent form of this equation is:\n\
@tex\n\
$$\n\
y_n = -\\sum_{k=1}^N c_{k+1} y_{n-k} + \\sum_{k=0}^M d_{k+1} x_{n-k}, \\qquad\n\
 1 \\le n \\le P\n\
$$\n\
@end tex\n\
@ifnottex\n\
@c Set example in small font to prevent overfull line\n\
\n\
@smallexample\n\
@group\n\
            N                   M\n\
  y(n) = - SUM c(k+1) y(n-k) + SUM d(k+1) x(n-k)  for 1<=n<=length(x)\n\
           k=1                 k=0\n\
@end group\n\
@end smallexample\n\
\n\
@end ifnottex\n\
\n\
@noindent\n\
where\n\
@ifnottex\n\
 c = a/a(1) and d = b/a(1).\n\
@end ifnottex\n\
@tex\n\
$c = a/a_1$ and $d = b/a_1$.\n\
@end tex\n\
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
@tex\n\
$$\n\
H(z) = {\\displaystyle\\sum_{k=0}^M d_{k+1} z^{-k}\n\
        \\over 1 + \\displaystyle\\sum_{k+1}^N c_{k+1} z^{-k}}\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
             M\n\
            SUM d(k+1) z^(-k)\n\
            k=0\n\
  H(z) = ----------------------\n\
               N\n\
          1 + SUM c(k+1) z^(-k)\n\
              k=1\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
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

  bool isfloat = (args(0).is_single_type ()
                  || args(1).is_single_type ()
                  || args(2).is_single_type ()
                  || (nargin >= 4 && args(3).is_single_type ()));
    
  if (args(0).is_complex_type ()
      || args(1).is_complex_type ()
      || args(2).is_complex_type ()
      || (nargin >= 4 && args(3).is_complex_type ()))
    {
      if (isfloat)
        {
          FloatComplexColumnVector b (args(0).float_complex_vector_value ());
          FloatComplexColumnVector a (args(1).float_complex_vector_value ());

          FloatComplexNDArray x (args(2).float_complex_array_value ());

          if (! error_state)
            {
              FloatComplexNDArray si;

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

                  si = args(3).float_complex_array_value ();

                  if (si_is_vector)
                    si = si.reshape (dim_vector (si.numel (), 1));
                }

              if (! error_state)
                {
                  FloatComplexNDArray y (filter (b, a, x, si, dim));

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
    }
  else
    {
      if (isfloat)
        {
          FloatColumnVector b (args(0).float_vector_value ());
          FloatColumnVector a (args(1).float_vector_value ());

          FloatNDArray x (args(2).float_array_value ());

          if (! error_state)
            {
              FloatNDArray si;

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

                  si = args(3).float_array_value ();

                  if (si_is_vector)
                    si = si.reshape (dim_vector (si.numel (), 1));
                }

              if (! error_state)
                {
                  FloatNDArray y (filter (b, a, x, si, dim));

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
    }

  return retval;
}

template MArray<double>
filter (MArray<double>&, MArray<double>&, MArray<double>&,
        MArray<double>&, int dim);

template MArray<double>
filter (MArray<double>&, MArray<double>&, MArray<double>&, int dim);

template MArray<Complex>
filter (MArray<Complex>&, MArray<Complex>&, MArray<Complex>&,
        MArray<Complex>&, int dim);

template MArray<Complex>
filter (MArray<Complex>&, MArray<Complex>&, MArray<Complex>&, int dim);

template MArray<float>
filter (MArray<float>&, MArray<float>&, MArray<float>&,
        MArray<float>&, int dim);

template MArray<float>
filter (MArray<float>&, MArray<float>&, MArray<float>&, int dim);

template MArray<FloatComplex>
filter (MArray<FloatComplex>&, MArray<FloatComplex>&, MArray<FloatComplex>&,
        MArray<FloatComplex>&, int dim);

template MArray<FloatComplex>
filter (MArray<FloatComplex>&, MArray<FloatComplex>&, MArray<FloatComplex>&, int dim);

/*
%!shared a, b, x, r
%!test
%!  a = [1 1];
%!  b = [1 1];
%!  x = zeros(1,10); x(1) = 1;
%!  assert(all(filter(b,   [1], x  ) == [1 1 0 0 0 0 0 0 0 0]  ))
%!  assert(all(filter(b,   [1], x.') == [1 1 0 0 0 0 0 0 0 0].'))
%!  assert(all(filter(b.', [1], x  ) == [1 1 0 0 0 0 0 0 0 0]  ))
%!  assert(all(filter(b.', [1], x.') == [1 1 0 0 0 0 0 0 0 0].'))
%!  assert(all(filter([1], a,   x  ) == [+1 -1 +1 -1 +1 -1 +1 -1 +1 -1]  ))
%!  assert(all(filter([1], a,   x.') == [+1 -1 +1 -1 +1 -1 +1 -1 +1 -1].'))
%!  assert(all(filter([1], a.', x  ) == [+1 -1 +1 -1 +1 -1 +1 -1 +1 -1]  ))
%!  assert(all(filter([1], a.', x.') == [+1 -1 +1 -1 +1 -1 +1 -1 +1 -1].'))
%!  assert(all(filter(b,   a,   x  ) == [1 0 0 0 0 0 0 0 0 0]  ))
%!  assert(all(filter(b.', a,   x  ) == [1 0 0 0 0 0 0 0 0 0]  ))
%!  assert(all(filter(b,   a.', x  ) == [1 0 0 0 0 0 0 0 0 0]  ))
%!  assert(all(filter(b.', a,   x  ) == [1 0 0 0 0 0 0 0 0 0]  ))
%!  assert(all(filter(b,   a,   x.') == [1 0 0 0 0 0 0 0 0 0].'))
%!  assert(all(filter(b.', a,   x.') == [1 0 0 0 0 0 0 0 0 0].'))
%!  assert(all(filter(b,   a.', x.') == [1 0 0 0 0 0 0 0 0 0].'))
%!  assert(all(filter(b.', a,   x.') == [1 0 0 0 0 0 0 0 0 0].'))
%!
%!test
%!  r = sqrt(1/2)*(1+i);
%!  a = a*r;
%!  b = b*r;
%!  assert(all(filter(b, [1], x   ) == r*[1 1 0 0 0 0 0 0 0 0]   ))
%!  assert(all(filter(b, [1], r*x ) == r*r*[1 1 0 0 0 0 0 0 0 0] ))
%!  assert(all(filter(b, [1], x.' ) == r*[1 1 0 0 0 0 0 0 0 0].' ))
%!  assert(all(filter(b, a,   x   ) ==   [1 0 0 0 0 0 0 0 0 0]   ))
%!  assert(all(filter(b, a,   r*x ) == r*[1 0 0 0 0 0 0 0 0 0]   ))
%!
%!shared a, b, x, y, so
%!test
%!  a = [1,1]; b=[1,1];
%!  x = zeros(1,10); x(1) = 1;
%!  [y, so] = filter(b, [1], x, [-1]);
%!  assert(all(y == [0 1 0 0 0 0 0 0 0 0]))
%!  assert(so,0)
%!
%!test
%!  x  = zeros(10,3); x(1,1)=-1; x(1,2)=1;
%!  y0 = zeros(10,3); y0(1:2,1)=-1; y0(1:2,2)=1;
%!  y = filter(b,[1],x);
%!  assert(all(all(y==y0)))
%!
%!test
%!  a = [1,1]; b=[1,1];
%!  x = zeros(4,4,2); x(1,1:4,1) = +1; x(1,1:4,2) = -1;
%!  y0 = zeros(4,4,2); y0(1:2,1:4,1) = +1; y0(1:2,1:4,2) = -1;
%!  y = filter(b, [1], x);
%!  assert(all(all(all(y==y0))))
%!
%!assert(filter(1,ones(10,1)/10,[]), [])
%!assert(filter(1,ones(10,1)/10,zeros(0,10)), zeros(0,10))

%%  Should put some tests of the "DIM" parameter in here.

*/
