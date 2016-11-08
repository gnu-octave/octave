/*

Copyright (C) 2009-2016 VZLU Prague

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lo-blas-proto.h"
#include "mx-base.h"
#include "error.h"
#include "defun.h"
#include "parse.h"

static void
get_red_dims (const dim_vector& x, const dim_vector& y, int dim,
              dim_vector& z, octave_idx_type& m, octave_idx_type& n,
              octave_idx_type& k)
{
  int nd = x.ndims ();
  assert (nd == y.ndims ());
  z = dim_vector::alloc (nd);
  m = 1, n = 1, k = 1;
  for (int i = 0; i < nd; i++)
    {
      if (i < dim)
        {
          z(i) = x(i);
          m *= x(i);
        }
      else if (i > dim)
        {
          z(i) = x(i);
          n *= x(i);
        }
      else
        {
          k = x(i);
          z(i) = 1;
        }
    }
}

DEFUN (dot, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} dot (@var{x}, @var{y}, @var{dim})
Compute the dot product of two vectors.

If @var{x} and @var{y} are matrices, calculate the dot products along the
first non-singleton dimension.

If the optional argument @var{dim} is given, calculate the dot products
along this dimension.

This is equivalent to
@code{sum (conj (@var{X}) .* @var{Y}, @var{dim})},
but avoids forming a temporary array and is faster.  When @var{X} and
@var{Y} are column vectors, the result is equivalent to
@code{@var{X}' * @var{Y}}.
@seealso{cross, divergence}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  octave_value retval;
  octave_value argx = args(0);
  octave_value argy = args(1);

  if (! argx.is_numeric_type () || ! argy.is_numeric_type ())
    error ("dot: X and Y must be numeric");

  dim_vector dimx = argx.dims ();
  dim_vector dimy = argy.dims ();
  bool match = dimx == dimy;
  if (! match && nargin == 2 && dimx.is_vector () && dimy.is_vector ())
    {
      // Change to column vectors.
      dimx = dimx.redim (1);
      argx = argx.reshape (dimx);
      dimy = dimy.redim (1);
      argy = argy.reshape (dimy);
      match = dimx == dimy;
    }

  if (! match)
    error ("dot: sizes of X and Y must match");

  int dim;
  if (nargin == 2)
    dim = dimx.first_non_singleton ();
  else
    dim = args(2).int_value (true) - 1;

  if (dim < 0)
    error ("dot: DIM must be a valid dimension");

  octave_idx_type m, n, k;
  dim_vector dimz;
  if (argx.is_complex_type () || argy.is_complex_type ())
    {
      if (argx.is_single_type () || argy.is_single_type ())
        {
          FloatComplexNDArray x = argx.float_complex_array_value ();
          FloatComplexNDArray y = argy.float_complex_array_value ();
          get_red_dims (dimx, dimy, dim, dimz, m, n, k);
          FloatComplexNDArray z (dimz);

          F77_XFCN (cdotc3, CDOTC3, (m, n, k,
                                     F77_CONST_CMPLX_ARG (x.data ()), F77_CONST_CMPLX_ARG (y.data ()),
                                     F77_CMPLX_ARG (z.fortran_vec ())));
          retval = z;
        }
      else
        {
          ComplexNDArray x = argx.complex_array_value ();
          ComplexNDArray y = argy.complex_array_value ();
          get_red_dims (dimx, dimy, dim, dimz, m, n, k);
          ComplexNDArray z (dimz);

          F77_XFCN (zdotc3, ZDOTC3, (m, n, k,
                                     F77_CONST_DBLE_CMPLX_ARG (x.data ()), F77_CONST_DBLE_CMPLX_ARG (y.data ()),
                                     F77_DBLE_CMPLX_ARG (z.fortran_vec ())));
          retval = z;
        }
    }
  else if (argx.is_float_type () && argy.is_float_type ())
    {
      if (argx.is_single_type () || argy.is_single_type ())
        {
          FloatNDArray x = argx.float_array_value ();
          FloatNDArray y = argy.float_array_value ();
          get_red_dims (dimx, dimy, dim, dimz, m, n, k);
          FloatNDArray z (dimz);

          F77_XFCN (sdot3, SDOT3, (m, n, k, x.data (), y.data (),
                                   z.fortran_vec ()));
          retval = z;
        }
      else
        {
          NDArray x = argx.array_value ();
          NDArray y = argy.array_value ();
          get_red_dims (dimx, dimy, dim, dimz, m, n, k);
          NDArray z (dimz);

          F77_XFCN (ddot3, DDOT3, (m, n, k, x.data (), y.data (),
                                   z.fortran_vec ()));
          retval = z;
        }
    }
  else
    {
      // Non-optimized evaluation.
      octave_value_list tmp;
      tmp(1) = dim + 1;
      tmp(0) = do_binary_op (octave_value::op_el_mul, argx, argy);

      tmp = feval ("sum", tmp, 1);
      if (! tmp.empty ())
        retval = tmp(0);
    }

  return retval;
}

/*
%!assert (dot ([1, 2], [2, 3]), 8)

%!test
%! x = [2, 1; 2, 1];
%! y = [-0.5, 2; 0.5, -2];
%! assert (dot (x, y), [0 0]);
%! assert (dot (single (x), single (y)), single ([0 0]));

%!test
%! x = [1+i, 3-i; 1-i, 3-i];
%! assert (dot (x, x), [4, 20]);
%! assert (dot (single (x), single (x)), single ([4, 20]));

%!test
%! x = int8 ([1 2]);
%! y = int8 ([2 3]);
%! assert (dot (x, y), 8);

%!test
%! x = int8 ([1 2; 3 4]);
%! y = int8 ([5 6; 7 8]);
%! assert (dot (x, y), [26 44]);
%! assert (dot (x, y, 2), [17; 53]);
%! assert (dot (x, y, 3), [5 12; 21 32]);

## Test input validation
%!error dot ()
%!error dot (1)
%!error dot (1,2,3,4)
%!error <X and Y must be numeric> dot ({1,2}, [3,4])
%!error <X and Y must be numeric> dot ([1,2], {3,4})
%!error <sizes of X and Y must match> dot ([1 2], [1 2 3])
%!error <sizes of X and Y must match> dot ([1 2]', [1 2 3]')
%!error <sizes of X and Y must match> dot (ones (2,2), ones (2,3))
%!error <DIM must be a valid dimension> dot ([1 2], [1 2], 0)
*/

DEFUN (blkmm, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} blkmm (@var{A}, @var{B})
Compute products of matrix blocks.

The blocks are given as 2-dimensional subarrays of the arrays @var{A},
@var{B}.  The size of @var{A} must have the form @code{[m,k,@dots{}]} and
size of @var{B} must be @code{[k,n,@dots{}]}.  The result is then of size
@code{[m,n,@dots{}]} and is computed as follows:

@example
@group
for i = 1:prod (size (@var{A})(3:end))
  @var{C}(:,:,i) = @var{A}(:,:,i) * @var{B}(:,:,i)
endfor
@end group
@end example
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value retval;

  octave_value argx = args(0);
  octave_value argy = args(1);

  if (! argx.is_numeric_type () || ! argy.is_numeric_type ())
    error ("blkmm: A and B must be numeric");

  const dim_vector dimx = argx.dims ();
  const dim_vector dimy = argy.dims ();
  int nd = dimx.ndims ();
  octave_idx_type m = dimx(0);
  octave_idx_type k = dimx(1);
  octave_idx_type n = dimy(1);
  octave_idx_type np = 1;
  bool match = dimy(0) == k && nd == dimy.ndims ();
  dim_vector dimz = dim_vector::alloc (nd);
  dimz(0) = m;
  dimz(1) = n;
  for (int i = 2; match && i < nd; i++)
    {
      match = match && dimx(i) == dimy(i);
      dimz(i) = dimx(i);
      np *= dimz(i);
    }

  if (! match)
    error ("blkmm: A and B dimensions don't match: (%s) and (%s)",
           dimx.str ().c_str (), dimy.str ().c_str ());

  if (argx.is_complex_type () || argy.is_complex_type ())
    {
      if (argx.is_single_type () || argy.is_single_type ())
        {
          FloatComplexNDArray x = argx.float_complex_array_value ();
          FloatComplexNDArray y = argy.float_complex_array_value ();
          FloatComplexNDArray z (dimz);

          F77_XFCN (cmatm3, CMATM3, (m, n, k, np,
                                     F77_CONST_CMPLX_ARG (x.data ()), F77_CONST_CMPLX_ARG (y.data ()),
                                     F77_CMPLX_ARG (z.fortran_vec ())));
          retval = z;
        }
      else
        {
          ComplexNDArray x = argx.complex_array_value ();
          ComplexNDArray y = argy.complex_array_value ();
          ComplexNDArray z (dimz);

          F77_XFCN (zmatm3, ZMATM3, (m, n, k, np,
                                     F77_CONST_DBLE_CMPLX_ARG (x.data ()), F77_CONST_DBLE_CMPLX_ARG (y.data ()),
                                     F77_DBLE_CMPLX_ARG (z.fortran_vec ())));
          retval = z;
        }
    }
  else
    {
      if (argx.is_single_type () || argy.is_single_type ())
        {
          FloatNDArray x = argx.float_array_value ();
          FloatNDArray y = argy.float_array_value ();
          FloatNDArray z (dimz);

          F77_XFCN (smatm3, SMATM3, (m, n, k, np,
                                     x.data (), y.data (),
                                     z.fortran_vec ()));
          retval = z;
        }
      else
        {
          NDArray x = argx.array_value ();
          NDArray y = argy.array_value ();
          NDArray z (dimz);

          F77_XFCN (dmatm3, DMATM3, (m, n, k, np,
                                     x.data (), y.data (),
                                     z.fortran_vec ()));
          retval = z;
        }
    }

  return retval;
}

/*
%!test
%! x(:,:,1) = [1 2; 3 4];
%! x(:,:,2) = [1 1; 1 1];
%! z(:,:,1) = [7 10; 15 22];
%! z(:,:,2) = [2 2; 2 2];
%! assert (blkmm (x,x), z);
%! assert (blkmm (single (x), single (x)), single (z));
%! assert (blkmm (x, single (x)), single (z));

%!test
%! x(:,:,1) = [1 2; 3 4];
%! x(:,:,2) = [1i 1i; 1i 1i];
%! z(:,:,1) = [7 10; 15 22];
%! z(:,:,2) = [-2 -2; -2 -2];
%! assert (blkmm (x,x), z);
%! assert (blkmm (single (x), single (x)), single (z));
%! assert (blkmm (x, single (x)), single (z));

## Test input validation
%!error blkmm ()
%!error blkmm (1)
%!error blkmm (1,2,3)
%!error <A and B dimensions don't match> blkmm (ones (2,2), ones (3,3))
%!error <A and B must be numeric> blkmm ({1,2}, [3,4])
%!error <A and B must be numeric> blkmm ([3,4], {1,2})
*/

