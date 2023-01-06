////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lo-blas-proto.h"
#include "mx-base.h"

#include "builtin-defun-decls.h"
#include "defun.h"
#include "error.h"
#include "parse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// FIXME: input 'y' is no longer necessary (2/5/2022).
//        At some point it would be better to change all occurrences of
//        get_red_dims to eliminate this input parameter.
static void
get_red_dims (const dim_vector& x, const dim_vector& /* y */, int dim,
              dim_vector& z, F77_INT& m, F77_INT& n, F77_INT& k)
{
  int nd = x.ndims ();
  z = dim_vector::alloc (nd);
  octave_idx_type tmp_m = 1;
  octave_idx_type tmp_n = 1;
  octave_idx_type tmp_k = 1;
  for (int i = 0; i < nd; i++)
    {
      if (i < dim)
        {
          z(i) = x(i);
          tmp_m *= x(i);
        }
      else if (i > dim)
        {
          z(i) = x(i);
          tmp_n *= x(i);
        }
      else
        {
          z(i) = 1;
          tmp_k = x(i);
        }
    }

  m = to_f77_int (tmp_m);
  n = to_f77_int (tmp_n);
  k = to_f77_int (tmp_k);
}

DEFUN (dot, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{z} =} dot (@var{x}, @var{y})
@deftypefnx {} {@var{z} =} dot (@var{x}, @var{y}, @var{dim})
Compute the dot product of two vectors.

If @var{x} and @var{y} are matrices, calculate the dot products along the
first non-singleton dimension.

If the optional argument @var{dim} is given, calculate the dot products
along this dimension.

Implementation Note: This is equivalent to
@code{sum (conj (@var{X}) .* @var{Y}, @var{dim})}, but avoids forming a
temporary array and is faster.  When @var{X} and @var{Y} are column vectors,
the result is equivalent to @code{@var{X}' * @var{Y}}.  Although, @code{dot}
is defined for integer arrays, the output may differ from the expected result
due to the limited range of integer objects.
@seealso{cross, divergence}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  octave_value retval;
  octave_value argx = args(0);
  octave_value argy = args(1);

  if (! argx.isnumeric () || ! argy.isnumeric ())
    error ("dot: X and Y must be numeric");

  dim_vector dimx = argx.dims ();
  dim_vector dimy = argy.dims ();
  bool match = dimx == dimy;
  if (! match && nargin == 2 && dimx.isvector () && dimy.isvector ())
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

  F77_INT m, n, k;
  dim_vector dimz;
  if (argx.iscomplex () || argy.iscomplex ())
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
  else if (argx.isfloat () && argy.isfloat ())
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
      // FIXME: This may *not* do what the user expects.
      // It might be more useful to issue a warning, or even an error, instead
      // of calculating possibly garbage results.
      // Think of the dot product of two int8 vectors where the multiplications
      // exceed intmax.
      octave_value_list tmp;
      tmp(1) = dim + 1;
      tmp(0) = binary_op (octave_value::op_el_mul, argx, argy);

      tmp = Fsum (tmp, 1);
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
%! x = int8 ([1, 2]);
%! y = int8 ([2, 3]);
%! assert (dot (x, y), 8);

%!test
%! x = int8 ([1, 2; 3, 4]);
%! y = int8 ([5, 6; 7, 8]);
%! assert (dot (x, y), [26 44]);
%! assert (dot (x, y, 2), [17; 53]);
%! assert (dot (x, y, 3), [5 12; 21 32]);

## This is, perhaps, surprising.  Integer maximums and saturation mechanics
## prevent accurate value from being calculated.
%!test
%! x = int8 ([127]);
%! assert (dot (x, x), 127);

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

template <typename T>
static void
blkmm_internal (const T& x, const T& y, T& z,
                F77_INT m, F77_INT n, F77_INT k, F77_INT np);

template <>
void
blkmm_internal (const FloatComplexNDArray& x, const FloatComplexNDArray& y,
                FloatComplexNDArray& z,
                F77_INT m, F77_INT n, F77_INT k, F77_INT np)
{
  F77_XFCN (cmatm3, CMATM3, (m, n, k, np,
                             F77_CONST_CMPLX_ARG (x.data ()),
                             F77_CONST_CMPLX_ARG (y.data ()),
                             F77_CMPLX_ARG (z.fortran_vec ())));
}

template <>
void
blkmm_internal (const ComplexNDArray& x, const ComplexNDArray& y,
                ComplexNDArray& z,
                F77_INT m, F77_INT n, F77_INT k, F77_INT np)
{
  F77_XFCN (zmatm3, ZMATM3, (m, n, k, np,
                             F77_CONST_DBLE_CMPLX_ARG (x.data ()),
                             F77_CONST_DBLE_CMPLX_ARG (y.data ()),
                             F77_DBLE_CMPLX_ARG (z.fortran_vec ())));
}

template <>
void
blkmm_internal (const FloatNDArray& x, const FloatNDArray& y, FloatNDArray& z,
                F77_INT m, F77_INT n, F77_INT k, F77_INT np)
{
  F77_XFCN (smatm3, SMATM3, (m, n, k, np,
                             x.data (), y.data (),
                             z.fortran_vec ()));
}

template <>
void
blkmm_internal (const NDArray& x, const NDArray& y, NDArray& z,
                F77_INT m, F77_INT n, F77_INT k, F77_INT np)
{
  F77_XFCN (dmatm3, DMATM3, (m, n, k, np,
                             x.data (), y.data (),
                             z.fortran_vec ()));
}

static void
get_blkmm_dims (const dim_vector& dimx, const dim_vector& dimy,
                F77_INT& m, F77_INT& n, F77_INT& k, F77_INT& np,
                dim_vector& dimz)
{
  int nd = dimx.ndims ();

  m = to_f77_int (dimx(0));
  k = to_f77_int (dimx(1));
  n = to_f77_int (dimy(1));

  octave_idx_type tmp_np = 1;

  bool match = ((dimy(0) == k) && (nd == dimy.ndims ()));

  dimz = dim_vector::alloc (nd);

  dimz(0) = m;
  dimz(1) = n;
  for (int i = 2; match && i < nd; i++)
    {
      match = (dimx(i) == dimy(i));
      dimz(i) = dimx(i);
      tmp_np *= dimz(i);
    }

  np = to_f77_int (tmp_np);

  if (! match)
    error ("blkmm: A and B dimensions don't match: (%s) and (%s)",
           dimx.str ().c_str (), dimy.str ().c_str ());
}

template <typename T>
T
do_blkmm (const octave_value& xov, const octave_value& yov)
{
  const T x = octave_value_extract<T> (xov);
  const T y = octave_value_extract<T> (yov);
  F77_INT m, n, k, np;
  dim_vector dimz;

  get_blkmm_dims (x.dims (), y.dims (), m, n, k, np, dimz);

  T z (dimz);

  if (n != 0 && m != 0)
    blkmm_internal<T> (x, y, z, m, n, k, np);

  return z;
}

DEFUN (blkmm, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} blkmm (@var{A}, @var{B})
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

  if (! argx.isnumeric () || ! argy.isnumeric ())
    error ("blkmm: A and B must be numeric");

  if (argx.iscomplex () || argy.iscomplex ())
    {
      if (argx.is_single_type () || argy.is_single_type ())
        retval = do_blkmm<FloatComplexNDArray> (argx, argy);
      else
        retval = do_blkmm<ComplexNDArray> (argx, argy);
    }
  else
    {
      if (argx.is_single_type () || argy.is_single_type ())
        retval = do_blkmm<FloatNDArray> (argx, argy);
      else
        retval = do_blkmm<NDArray> (argx, argy);
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

%!test <*54261>
%! x = ones (0, 3, 3);
%! y = ones (3, 5, 3);
%! z = blkmm (x,y);
%! assert (size (z), [0, 5, 3]);
%! x = ones (1, 3, 3);
%! y = ones (3, 0, 3);
%! z = blkmm (x,y);
%! assert (size (z), [1, 0, 3]);

## Test input validation
%!error blkmm ()
%!error blkmm (1)
%!error blkmm (1,2,3)
%!error <A and B must be numeric> blkmm ({1,2}, [3,4])
%!error <A and B must be numeric> blkmm ([3,4], {1,2})
%!error <A and B dimensions don't match> blkmm (ones (2,2), ones (3,3))
*/

OCTAVE_END_NAMESPACE(octave)
