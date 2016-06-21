/*

Copyright (C) 2004-2015 David Bateman

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

#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "utils.h"

// This function should be merged with Fifft.

#if defined (HAVE_FFTW)
#  define FFTSRC "@sc{fftw}"
#else
#  define FFTSRC "@sc{fftpack}"
#endif

static octave_value
do_fftn (const octave_value_list &args, const char *fcn, int type)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value retval;
  octave_value arg = args(0);
  dim_vector dims = arg.dims ();

  for (int i = 0; i < dims.ndims (); i++)
    if (dims(i) < 0)
      return retval;

  if (nargin > 1)
    {
      Matrix val = args(1).xmatrix_value ("%s: SIZE must be a vector of length dim", fcn);

      if (val.rows () > val.columns ())
        val = val.transpose ();

      if (val.columns () != dims.ndims () || val.rows () != 1)
        error ("%s: SIZE must be a vector of length dim", fcn);

      for (int i = 0; i < dims.ndims (); i++)
        {
          if (octave::math::isnan (val(i,0)))
            error ("%s: SIZE has invalid NaN entries", fcn);
          else if (octave::math::nint_big (val(i,0)) < 0)
            error ("%s: all dimensions in SIZE must be greater than zero", fcn);
          else
            dims(i) = octave::math::nint_big(val(i,0));
        }
    }

  if (dims.all_zero ())
    {
      if (arg.is_single_type ())
        return octave_value (FloatMatrix ());
      else
        return octave_value (Matrix ());
    }

  if (arg.is_single_type ())
    {
      if (arg.is_real_type ())
        {
          FloatNDArray nda = arg.float_array_value ();

          nda.resize (dims, 0.0);
          retval = (type != 0 ? nda.ifourierNd () : nda.fourierNd ());
        }
      else
        {
          FloatComplexNDArray cnda = arg.float_complex_array_value ();

          cnda.resize (dims, 0.0);
          retval = (type != 0 ? cnda.ifourierNd () : cnda.fourierNd ());
        }
    }
  else
    {
      if (arg.is_real_type ())
        {
          NDArray nda = arg.array_value ();

          nda.resize (dims, 0.0);
          retval = (type != 0 ? nda.ifourierNd () : nda.fourierNd ());
        }
      else if (arg.is_complex_type ())
        {
          ComplexNDArray cnda = arg.complex_array_value ();

          cnda.resize (dims, 0.0);
          retval = (type != 0 ? cnda.ifourierNd () : cnda.fourierNd ());
        }
      else
        err_wrong_type_arg (fcn, arg);
    }

  return retval;
}

DEFUN (fftn, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} fftn (@var{A})
@deftypefnx {} {} fftn (@var{A}, @var{size})
Compute the N-dimensional discrete Fourier transform of @var{A} using
a Fast Fourier Transform (FFT) algorithm.

The optional vector argument @var{size} may be used specify the dimensions
of the array to be used.  If an element of @var{size} is smaller than the
corresponding dimension of @var{A}, then the dimension of @var{A} is
truncated prior to performing the FFT@.  Otherwise, if an element of
@var{size} is larger than the corresponding dimension then @var{A} is
resized and padded with zeros.
@seealso{ifftn, fft, fft2, fftw}
@end deftypefn */)
{
  return do_fftn (args, "fftn", 0);
}

DEFUN (ifftn, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} ifftn (@var{A})
@deftypefnx {} {} ifftn (@var{A}, @var{size})
Compute the inverse N-dimensional discrete Fourier transform of @var{A}
using a Fast Fourier Transform (FFT) algorithm.

The optional vector argument @var{size} may be used specify the dimensions
of the array to be used.  If an element of @var{size} is smaller than the
corresponding dimension of @var{A}, then the dimension of @var{A} is
truncated prior to performing the inverse FFT@.  Otherwise, if an element of
@var{size} is larger than the corresponding dimension then @var{A} is
resized and padded with zeros.
@seealso{fftn, ifft, ifft2, fftw}
@end deftypefn */)
{
  return do_fftn (args, "ifftn", 1);
}
