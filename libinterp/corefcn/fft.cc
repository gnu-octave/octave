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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static octave_value
do_fft (const octave_value_list& args, const char *fcn, int type)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  octave_value retval;
  octave_value arg = args(0);
  octave_idx_type n_points = -1;
  dim_vector dims = arg.dims ();
  int ndims = dims.ndims ();
  int dim = -1;

  if (nargin > 1)
    {
      if (! args(1).isempty ())
        {
          double dval = args(1).double_value ();
          if (math::isnan (dval))
            error ("%s: number of points (N) cannot be NaN", fcn);

          n_points = math::nint_big (dval);
          if (n_points < 0)
            error ("%s: number of points (N) must be greater than zero", fcn);
        }
    }

  if (nargin > 2)
    {
      double dval = args(2).double_value ();
      if (math::isnan (dval))
        error ("%s: DIM cannot be NaN", fcn);
      else if (dval < 1 || dval > ndims)
        error ("%s: DIM must be a valid dimension along which to perform FFT",
               fcn);
      else
        // to be safe, cast it back to int since dim is an int
        dim = math::nint (dval) - 1;
    }

  // FIXME: This seems strange and unnecessary (10/21/16).
  // How would you ever arrive at an octave_value object without correct dims?
  // We certainly don't make this check every other place in Octave.
  for (octave_idx_type i = 0; i < ndims; i++)
    if (dims(i) < 0)
      return retval;

  if (dim < 0)
    {
      dim = dims.first_non_singleton ();

      // And if the first argument is scalar?
      if (dim == ndims)
        dim = 1;
    }

  if (n_points < 0)
    n_points = dims(dim);
  else
    dims(dim) = n_points;

  if (n_points == 0 || dims.any_zero ())
    {
      if (arg.is_single_type ())
        return octave_value (FloatNDArray (dims));
      else
        return octave_value (NDArray (dims));
    }

  if (n_points == 1)
    {
      octave_value_list idx (ndims);
      for (octave_idx_type i = 0; i < ndims; i++)
        idx(i) = idx_vector::colon;
      idx(dim) = idx_vector (0);

      return arg.index_op (idx);
    }

  if (arg.is_single_type ())
    {
      if (arg.isreal ())
        {
          FloatNDArray nda = arg.float_array_value ();

          nda.resize (dims, 0.0);
          retval = (type != 0 ? nda.ifourier (dim) : nda.fourier (dim));
        }
      else
        {
          FloatComplexNDArray cnda = arg.float_complex_array_value ();

          cnda.resize (dims, 0.0);
          retval = (type != 0 ? cnda.ifourier (dim) : cnda.fourier (dim));
        }
    }
  else
    {
      if (arg.isreal ())
        {
          NDArray nda = arg.array_value ();

          nda.resize (dims, 0.0);
          retval = (type != 0 ? nda.ifourier (dim) : nda.fourier (dim));
        }
      else if (arg.iscomplex ())
        {
          ComplexNDArray cnda = arg.complex_array_value ();

          cnda.resize (dims, 0.0);
          retval = (type != 0 ? cnda.ifourier (dim) : cnda.fourier (dim));
        }
      else
        err_wrong_type_arg (fcn, arg);
    }

  return retval;
}

/*
%!testif HAVE_FFTW
%! assert (fft ([]), [])
%!testif HAVE_FFTW
%! assert (fft (zeros (10,0)), zeros (10,0))
%!testif HAVE_FFTW
%! assert (fft (zeros (0,10)), zeros (0,10))
%!testif HAVE_FFTW
%! assert (fft (0), 0)
%!testif HAVE_FFTW
%! assert (fft (1), 1)
%!testif HAVE_FFTW
%! assert (fft (ones (2,2)), [2,2; 0,0])
%!testif HAVE_FFTW
%! assert (fft (eye (2,2)), [1,1; 1,-1])

%!testif HAVE_FFTW
%! assert (fft (single ([])), single ([]))
%!testif HAVE_FFTW
%! assert (fft (zeros (10,0,"single")), zeros (10,0,"single"))
%!testif HAVE_FFTW
%! assert (fft (zeros (0,10,"single")), zeros (0,10,"single"))
%!testif HAVE_FFTW
%! assert (fft (single (0)), single (0))
%!testif HAVE_FFTW
%! assert (fft (single (1)), single (1))
%!testif HAVE_FFTW
%! assert (fft (ones (2,2,"single")), single ([2,2; 0,0]))
%!testif HAVE_FFTW
%! assert (fft (eye (2,2,"single")), single ([1,1; 1,-1]))

%!error fft ()
*/


DEFUN (fft, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} fft (@var{x})
@deftypefnx {} {@var{y} =} fft (@var{x}, @var{n})
@deftypefnx {} {@var{y} =} fft (@var{x}, @var{n}, @var{dim})
Compute the discrete Fourier transform of @var{x} using
a Fast Fourier Transform (FFT) algorithm.

The FFT is calculated along the first non-singleton dimension of the
array.  Thus if @var{x} is a matrix, @code{fft (@var{x})} computes the
FFT for each column of @var{x}.

If called with two arguments, @var{n} is expected to be an integer
specifying the number of elements of @var{x} to use, or an empty
matrix to specify that its value should be ignored.  If @var{n} is
larger than the dimension along which the FFT is calculated, then
@var{x} is resized and padded with zeros.  Otherwise, if @var{n} is
smaller than the dimension along which the FFT is calculated, then
@var{x} is truncated.

If called with three arguments, @var{dim} is an integer specifying the
dimension of the matrix along which the FFT is performed.
@seealso{ifft, fft2, fftn, fftw}
@end deftypefn */)
{
  return do_fft (args, "fft", 0);
}


DEFUN (ifft, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} ifft (@var{y})
@deftypefnx {} {@var{x} =} ifft (@var{y}, @var{n})
@deftypefnx {} {@var{x} =} ifft (@var{y}, @var{n}, @var{dim})
Compute the inverse discrete Fourier transform of @var{y}
using a Fast Fourier Transform (FFT) algorithm.

The inverse FFT is calculated along the first non-singleton dimension
of the array.  Thus if @var{y} is a matrix, @code{ifft (@var{y})} computes
the inverse FFT for each column of @var{y}.

If called with two arguments, @var{n} is expected to be an integer
specifying the number of elements of @var{y} to use, or an empty
matrix to specify that its value should be ignored.  If @var{n} is
larger than the dimension along which the inverse FFT is calculated, then
@var{y} is resized and padded with zeros.  Otherwise, if @var{n} is
smaller than the dimension along which the inverse FFT is calculated,
then @var{y} is truncated.

If called with three arguments, @var{dim} is an integer specifying the
dimension of the matrix along which the inverse FFT is performed.
@seealso{fft, ifft2, ifftn, fftw}
@end deftypefn */)
{
  return do_fft (args, "ifft", 1);
}

/*
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
%!testif HAVE_FFTW
%! N = 64;
%! n = 4;
%! t = 2*pi*(0:1:N-1)/N;
%! s = cos (n*t);
%! S = fft (s);
%!
%! answer = zeros (size (t));
%! answer(n+1) = N/2;
%! answer(N-n+1) = N/2;
%!
%! assert (S, answer, 4*N*eps);

## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
%!testif HAVE_FFTW
%! N = 64;
%! n = 7;
%! t = 2*pi*(0:1:N-1)/N;
%! s = cos (n*t);
%!
%! S = zeros (size (t));
%! S(n+1) = N/2;
%! S(N-n+1) = N/2;
%!
%! assert (ifft (S), s, 4*N*eps);

## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
%!testif HAVE_FFTW
%! N = 64;
%! n = 4;
%! t = single (2*pi*(0:1:N-1)/N);
%! s = cos (n*t);
%! S = fft (s);
%!
%! answer = zeros (size (t), "single");
%! answer(n+1) = N/2;
%! answer(N-n+1) = N/2;
%!
%! assert (S, answer, 4*N* eps ("single"));

## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
%!testif HAVE_FFTW
%! N = 64;
%! n = 7;
%! t = 2*pi*(0:1:N-1)/N;
%! s = cos (n*t);
%!
%! S = zeros (size (t), "single");
%! S(n+1) = N/2;
%! S(N-n+1) = N/2;
%!
%! assert (ifft (S), s, 4*N* eps ("single"));
*/

OCTAVE_END_NAMESPACE(octave)
