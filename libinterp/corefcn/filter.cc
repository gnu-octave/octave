////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

// Based on Tony Richardson's filter.m.
//
// Originally translated to C++ by KH (Kurt.Hornik@wu-wien.ac.at)
// with help from Fritz Leisch and Andreas Weingessel on Oct 20, 1994.
//
// Rewritten to use templates to handle both real and complex cases by
// jwe, Wed Nov  1 19:15:29 1995.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "quit.h"

#include "defun.h"
#include "error.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename T>
MArray<T>
filter (MArray<T>& b, MArray<T>& a, MArray<T>& x, MArray<T>& si,
        int dim = 0)
{
  MArray<T> y;

  octave_idx_type a_len = a.numel ();
  octave_idx_type b_len = b.numel ();

  octave_idx_type ab_len = (a_len > b_len ? a_len : b_len);

  // FIXME: The two lines below should be unnecessary because
  //        this template is called with a and b as column vectors
  //        already.  However the a.resize line is currently (2011/04/26)
  //        necessary to stop bug #33164.
  b.resize (dim_vector (ab_len, 1), 0.0);
  if (a_len > 1)
    a.resize (dim_vector (ab_len, 1), 0.0);

  T norm = a (0);

  if (norm == static_cast<T> (0.0))
    error ("filter: the first element of A must be nonzero");

  dim_vector x_dims = x.dims ();
  if (dim < 0 || dim > x_dims.ndims ())
    error ("filter: DIM must be a valid dimension");

  octave_idx_type x_len = x_dims(dim);

  dim_vector si_dims = si.dims ();
  octave_idx_type si_len = si_dims(0);

  if (si_len != ab_len - 1)
    error ("filter: first dimension of SI must be of length max (length (a), length (b)) - 1");

  if (si_dims.ndims () != x_dims.ndims ())
    error ("filter: dimensionality of SI and X must agree");

  for (octave_idx_type i = 1; i < dim; i++)
    {
      if (si_dims(i) != x_dims(i-1))
        error ("filter: dimensionality of SI and X must agree");
    }
  for (octave_idx_type i = dim+1; i < x_dims.ndims (); i++)
    {
      if (si_dims(i) != x_dims(i))
        error ("filter: dimensionality of SI and X must agree");
    }

  if (x_len == 0)
    return x;

  if (norm != static_cast<T> (1.0))
    {
      a /= norm;
      b /= norm;
    }

  if (a_len <= 1 && si_len <= 0)
    return b(0) * x;

  // Here onwards, either a_len > 1 or si_len >= 1 or both.

  y.resize (x_dims, 0.0);

  octave_idx_type x_stride = 1;
  for (int i = 0; i < dim; i++)
    x_stride *= x_dims(i);

  octave_idx_type x_num = x_dims.numel () / x_len;
  // For deconv and fftfilt, x_num seems to always be 1.
  // For directly calling filter, it can be more than 1.

  for (octave_idx_type num = 0; num < x_num; num++)
    {
      octave_idx_type x_offset = (x_stride == 1) ? num * x_len
                                 : num + (num / x_stride) * x_stride * (x_len - 1);

      octave_idx_type si_offset = num * si_len;

      // Try to achieve a balance between speed and interruptibility.
      //
      // One extreme is to not check for interruptions at all, which gives
      // good speed but the user cannot use Ctrl-C for the whole duration.
      // The other end is to check frequently from inside an inner loop,
      // which slows down performance by 5X or 6X.
      //
      // Putting any sort of check in an inner loop seems to prevent the
      // compiler from optimizing the loop, so we cannot say "check for
      // interruptions every M iterations" using an if-statement.
      //
      // This is a compromise approach to split the total numer of loop
      // executions into num_outer and num_inner, to provide periodic checks
      // for interruptions without writing a conditional inside a tight loop.
      //
      // To make it more interruptible and run more slowly, reduce num_inner.
      // To speed it up but make it less interruptible, increase it.
      // May need to increase it slowly over time as computers get faster.
      // The aim is to not lose Ctrl-C ability for longer than about 2 seconds.
      //
      // In December 2021, num_inner = 100000 is acceptable.

      octave_idx_type num_execs = si_len-1; // 0 to num_execs-1
      octave_idx_type num_inner = 100000;
      octave_idx_type num_outer = num_execs / num_inner;

      // The following if-else block depends on a_len and si_len,
      // both of which are loop invariants in this 0 <= num < x_num loop.
      // But x_num is so small in practice that using the if-else inside
      // the loop has more benefits than duplicating the outer for-loop,
      // even though the checks are on loop invariants.

      // We cannot have a_len <= 1 AND si_len <= 0 because that case already
      // returned above. This means exactly one of the following blocks
      // inside the if-conditional will be obeyed: it is not possible for the
      // if-block and the else-block to *both* skip. Therefore any code that
      // is common to both branches can be pulled out here without affecting
      // correctness or speed.

      T *py = y.fortran_vec ();
      T *psi = si.fortran_vec ();
      const T *pb = b.data ();
      const T *px = x.data ();
      psi += si_offset;

      if (a_len > 1)
        {
          const T *pa = a.data ();

          // Usually the last element to be written will be si_len-1
          // but if si_len is 0, then we need the 0th element to be written.
          // Pulling this check out of the for-loop makes it run faster.
          octave_idx_type iidx = (si_len > 0) ? si_len-1 : 0;

          for (octave_idx_type i = 0, idx = x_offset;
               i < x_len;
               i++, idx += x_stride)
            {
              py[idx] = psi[0] + pb[0] * px[idx];

              // Outer and inner loops for interruption management
              for (octave_idx_type u = 0; u <= num_outer; u++)
                {
                  octave_idx_type lo = u * num_inner;
                  octave_idx_type hi = (lo + num_inner < num_execs-1)
                                       ? lo + num_inner : num_execs-1;

                  // Inner loop, no interruption
                  for (octave_idx_type j = lo; j <= hi; j++)
                    psi[j] = psi[j+1] - pa[j+1] * py[idx] + pb[j+1] * px[idx];

                  octave_quit();  // Check for interruptions
                }

              psi[iidx] = pb[si_len] * px[idx] - pa[si_len] * py[idx];
            }
        }
      else // a_len <= 1 ==> si_len MUST be > 0
        {
          // This else-block is almost the same as the above if-block,
          // except for the absence of variable pa.

          for (octave_idx_type i = 0, idx = x_offset;
               i < x_len;
               i++, idx += x_stride)
            {
              py[idx] = psi[0] + pb[0] * px[idx];

              // Outer and inner loops for interruption management
              for (octave_idx_type u = 0; u <= num_outer; u++)
                {
                  octave_idx_type lo = u * num_inner;
                  octave_idx_type hi = (lo + num_inner < num_execs-1)
                                       ? lo + num_inner : num_execs-1;

                  // Inner loop, no interruption
                  for (octave_idx_type j = lo; j <= hi; j++)
                    psi[j] = psi[j+1] + pb[j+1] * px[idx];

                  octave_quit();  // Check for interruptions
                }

              psi[si_len-1] = pb[si_len] * px[idx];
            }
        }
    }

  return y;
}

template <typename T>
MArray<T>
filter (MArray<T>& b, MArray<T>& a, MArray<T>& x, int dim = -1)
{
  dim_vector x_dims = x.dims ();

  if (dim < 0)
    dim = x_dims.first_non_singleton ();
  else if (dim > x_dims.ndims ())
    error ("filter: DIM must be a valid dimension");

  octave_idx_type a_len = a.numel ();
  octave_idx_type b_len = b.numel ();

  octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;
  dim_vector si_dims = x.dims ();
  for (int i = dim; i > 0; i--)
    si_dims(i) = si_dims(i-1);
  si_dims(0) = si_len;

  MArray<T> si (si_dims, T (0.0));

  return filter (b, a, x, si, dim);
}

DEFUN (filter, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} filter (@var{b}, @var{a}, @var{x})
@deftypefnx {} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, @var{si})
@deftypefnx {} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, [], @var{dim})
@deftypefnx {} {[@var{y}, @var{sf}] =} filter (@var{b}, @var{a}, @var{x}, @var{si}, @var{dim})
Apply a 1-D digital filter to the data @var{x}.

@code{filter} returns the solution to the following linear, time-invariant
difference equation:
@tex
$$
\sum_{k=0}^N a_{k+1} y_{n-k} = \sum_{k=0}^M b_{k+1} x_{n-k}, \qquad
 1 \le n \le P
$$
@end tex
@ifnottex
@c Set example in small font to prevent overfull line

@smallexample
@group
 N                   M
SUM a(k+1) y(n-k) = SUM b(k+1) x(n-k)    for 1<=n<=length(x)
k=0                 k=0
@end group
@end smallexample

@end ifnottex

@noindent
where
@ifnottex
N=length(a)-1 and M=length(b)-1.
@end ifnottex
@tex
$a \in \Re^{N-1}$, $b \in \Re^{M-1}$, and $x \in \Re^P$.
@end tex
The result is calculated over the first non-singleton dimension of @var{x}
or over @var{dim} if supplied.

An equivalent form of the equation is:
@tex
$$
y_n = -\sum_{k=1}^N c_{k+1} y_{n-k} + \sum_{k=0}^M d_{k+1} x_{n-k}, \qquad
 1 \le n \le P
$$
@end tex
@ifnottex
@c Set example in small font to prevent overfull line

@smallexample
@group
          N                   M
y(n) = - SUM c(k+1) y(n-k) + SUM d(k+1) x(n-k)  for 1<=n<=length(x)
         k=1                 k=0
@end group
@end smallexample

@end ifnottex

@noindent
where
@ifnottex
 c = a/a(1) and d = b/a(1).
@end ifnottex
@tex
$c = a/a_1$ and $d = b/a_1$.
@end tex

If the fourth argument @var{si} is provided, it is taken as the
initial state of the system and the final state is returned as
@var{sf}.  The state vector is a column vector whose length is
equal to the length of the longest coefficient vector minus one.
If @var{si} is not supplied, the initial state vector is set to all
zeros.

In terms of the Z Transform, @var{y} is the result of passing the
discrete-time signal @var{x} through a system characterized by the following
rational system function:
@tex
$$
H(z) = {\displaystyle\sum_{k=0}^M d_{k+1} z^{-k}
        \over 1 + \displaystyle\sum_{k+1}^N c_{k+1} z^{-k}}
$$
@end tex
@ifnottex

@example
@group
          M
         SUM d(k+1) z^(-k)
         k=0
H(z) = ---------------------
            N
       1 + SUM c(k+1) z^(-k)
           k=1
@end group
@end example

@end ifnottex
@seealso{filter2, fftfilt, freqz}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 3 || nargin > 5)
    print_usage ();

  int dim;
  dim_vector x_dims = args(2).dims ();

  if (nargin == 5)
    {
      dim = args(4).nint_value () - 1;
      if (dim < 0 || dim >= x_dims.ndims ())
        error ("filter: DIM must be a valid dimension");
    }
  else
    dim = x_dims.first_non_singleton ();

  octave_value_list retval;

  const char *a_b_errmsg = "filter: A and B must be vectors";
  const char *x_si_errmsg = "filter: X and SI must be arrays";

  bool isfloat = (args(0).is_single_type ()
                  || args(1).is_single_type ()
                  || args(2).is_single_type ()
                  || (nargin >= 4 && args(3).is_single_type ()));

  if (args(0).iscomplex ()
      || args(1).iscomplex ()
      || args(2).iscomplex ()
      || (nargin >= 4 && args(3).iscomplex ()))
    {
      if (isfloat)
        {
          FloatComplexColumnVector b = args(0).xfloat_complex_vector_value (a_b_errmsg);
          FloatComplexColumnVector a = args(1).xfloat_complex_vector_value (a_b_errmsg);
          FloatComplexNDArray x = args(2).xfloat_complex_array_value (x_si_errmsg);

          FloatComplexNDArray si;

          if (nargin == 3 || args(3).isempty ())
            {
              octave_idx_type a_len = a.numel ();
              octave_idx_type b_len = b.numel ();

              octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;

              dim_vector si_dims = x.dims ();
              for (int i = dim; i > 0; i--)
                si_dims(i) = si_dims(i-1);
              si_dims(0) = si_len;

              si.resize (si_dims, 0.0);
            }
          else
            {
              si = args(3).xfloat_complex_array_value (x_si_errmsg);

              if (si.isvector () && x.isvector ())
                si = si.reshape (dim_vector (si.numel (), 1));
            }

          FloatComplexNDArray y (filter (b, a, x, si, dim));

          retval = ovl (y, si);
        }
      else
        {
          ComplexColumnVector b = args(0).xcomplex_vector_value (a_b_errmsg);
          ComplexColumnVector a = args(1).xcomplex_vector_value (a_b_errmsg);

          ComplexNDArray x = args(2).xcomplex_array_value (x_si_errmsg);

          ComplexNDArray si;

          if (nargin == 3 || args(3).isempty ())
            {
              octave_idx_type a_len = a.numel ();
              octave_idx_type b_len = b.numel ();

              octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;

              dim_vector si_dims = x.dims ();
              for (int i = dim; i > 0; i--)
                si_dims(i) = si_dims(i-1);
              si_dims(0) = si_len;

              si.resize (si_dims, 0.0);
            }
          else
            {
              si = args(3).xcomplex_array_value (x_si_errmsg);

              if (si.isvector () && x.isvector ())
                si = si.reshape (dim_vector (si.numel (), 1));
            }

          ComplexNDArray y (filter (b, a, x, si, dim));

          retval = ovl (y, si);
        }
    }
  else
    {
      if (isfloat)
        {
          FloatColumnVector b = args(0).xfloat_vector_value (a_b_errmsg);
          FloatColumnVector a = args(1).xfloat_vector_value (a_b_errmsg);

          FloatNDArray x = args(2).xfloat_array_value (x_si_errmsg);

          FloatNDArray si;

          if (nargin == 3 || args(3).isempty ())
            {
              octave_idx_type a_len = a.numel ();
              octave_idx_type b_len = b.numel ();

              octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;

              dim_vector si_dims = x.dims ();
              for (int i = dim; i > 0; i--)
                si_dims(i) = si_dims(i-1);
              si_dims(0) = si_len;

              si.resize (si_dims, 0.0);
            }
          else
            {
              si = args(3).xfloat_array_value (x_si_errmsg);

              if (si.isvector () && x.isvector ())
                si = si.reshape (dim_vector (si.numel (), 1));
            }

          FloatNDArray y (filter (b, a, x, si, dim));

          retval = ovl (y, si);
        }
      else
        {
          ColumnVector b = args(0).xvector_value (a_b_errmsg);
          ColumnVector a = args(1).xvector_value (a_b_errmsg);

          NDArray x = args(2).xarray_value (x_si_errmsg);

          NDArray si;

          if (nargin == 3 || args(3).isempty ())
            {
              octave_idx_type a_len = a.numel ();
              octave_idx_type b_len = b.numel ();

              octave_idx_type si_len = (a_len > b_len ? a_len : b_len) - 1;

              dim_vector si_dims = x.dims ();
              for (int i = dim; i > 0; i--)
                si_dims(i) = si_dims(i-1);
              si_dims(0) = si_len;

              si.resize (si_dims, 0.0);
            }
          else
            {
              si = args(3).xarray_value (x_si_errmsg);

              if (si.isvector () && x.isvector ())
                si = si.reshape (dim_vector (si.numel (), 1));
            }

          NDArray y (filter (b, a, x, si, dim));

          retval = ovl (y, si);
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
filter (MArray<FloatComplex>&, MArray<FloatComplex>&, MArray<FloatComplex>&,
        int dim);

/*
%!shared a, b, x, r
%!test
%! a = [1 1];
%! b = [1 1];
%! x = zeros (1,10);  x(1) = 1;
%! assert (filter (b,   [1], x  ), [1 1 0 0 0 0 0 0 0 0]);
%! assert (filter (b,   [1], x.'), [1 1 0 0 0 0 0 0 0 0].');
%! assert (filter (b.', [1], x  ), [1 1 0 0 0 0 0 0 0 0]  );
%! assert (filter (b.', [1], x.'), [1 1 0 0 0 0 0 0 0 0].');
%! assert (filter ([1], a,   x  ), [+1 -1 +1 -1 +1 -1 +1 -1 +1 -1]  );
%! assert (filter ([1], a,   x.'), [+1 -1 +1 -1 +1 -1 +1 -1 +1 -1].');
%! assert (filter ([1], a.', x  ), [+1 -1 +1 -1 +1 -1 +1 -1 +1 -1]  );
%! assert (filter ([1], a.', x.'), [+1 -1 +1 -1 +1 -1 +1 -1 +1 -1].');
%! assert (filter (b,   a,   x  ), [1 0 0 0 0 0 0 0 0 0]  );
%! assert (filter (b.', a,   x  ), [1 0 0 0 0 0 0 0 0 0]  );
%! assert (filter (b,   a.', x  ), [1 0 0 0 0 0 0 0 0 0]  );
%! assert (filter (b.', a,   x  ), [1 0 0 0 0 0 0 0 0 0]  );
%! assert (filter (b,   a,   x.'), [1 0 0 0 0 0 0 0 0 0].');
%! assert (filter (b.', a,   x.'), [1 0 0 0 0 0 0 0 0 0].');
%! assert (filter (b,   a.', x.'), [1 0 0 0 0 0 0 0 0 0].');
%! assert (filter (b.', a,   x.'), [1 0 0 0 0 0 0 0 0 0].');

%!test
%! r = sqrt (1/2) * (1+i);
%! a = a*r;
%! b = b*r;
%! assert (filter (b, [1], x   ), r*[1 1 0 0 0 0 0 0 0 0]   );
%! assert (filter (b, [1], r*x ), r*r*[1 1 0 0 0 0 0 0 0 0] );
%! assert (filter (b, [1], x.' ), r*[1 1 0 0 0 0 0 0 0 0].' );
%! assert (filter (b, a,   x   ),   [1 0 0 0 0 0 0 0 0 0]   );
%! assert (filter (b, a,   r*x ), r*[1 0 0 0 0 0 0 0 0 0]   );

%!shared a, b, x, y, so
%!test
%! a = [1,1];
%! b = [1,1];
%! x = zeros (1,10);  x(1) = 1;
%! [y, so] = filter (b, [1], x, [-1]);
%! assert (y, [0 1 0 0 0 0 0 0 0 0]);
%! assert (so, 0);

%!test
%! x  = zeros (10,3);  x(1,1) = -1;  x(1,2) = 1;
%! y0 = zeros (10,3); y0(1:2,1) = -1;  y0(1:2,2) = 1;
%! y = filter (b, [1], x);
%! assert (y, y0);

%!test
%! a = [1,1];
%! b=[1,1];
%! x = zeros (4,4,2);  x(1,1:4,1) = +1;  x(1,1:4,2) = -1;
%! y0 = zeros (4,4,2);  y0(1:2,1:4,1) = +1;  y0(1:2,1:4,2) = -1;
%! y = filter (b, [1], x);
%! assert (y, y0);

%!assert (filter (1, ones (10,1) / 10, []), [])
%!assert (filter (1, ones (10,1) / 10, zeros (0,10)), zeros (0,10))
%!assert (filter (1, ones (10,1) / 10, single (1:5)),
%!        repmat (single (10), 1, 5))

## Test using initial conditions
%!assert (filter ([1, 1, 1], [1, 1], [1 2], [1, 1]), [2 2])
%!assert (filter ([1, 1, 1], [1, 1], [1 2], [1, 1]'), [2 2])
%!assert (filter ([1, 3], [1], [1 2; 3 4; 5 6], [4, 5]), [5 7; 6 10; 14 18])
%!error filter ([1, 3], [1], [1 2; 3 4; 5 6], [4, 5]')
%!assert (filter ([1, 3, 2], [1], [1 2; 3 4; 5 6], [1 0 0; 1 0 0], 2),
%!        [2 6; 3 13; 5 21])

## Test of DIM parameter
%!test
%! x = ones (2, 1, 3, 4);
%! x(1,1,:,:) = [1 2 3 4; 5 6 7 8; 9 10 11 12];
%! y0 = [1 1 6 2 15 3 2 1 8 2 18 3 3 1 10 2 21 3 4 1 12 2 24 3];
%! y0 = reshape (y0, size (x));
%! y = filter ([1 1 1], 1, x, [], 3);
%! assert (y, y0);
*/

OCTAVE_END_NAMESPACE(octave)
