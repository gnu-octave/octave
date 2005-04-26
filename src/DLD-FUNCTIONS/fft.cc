/*

Copyright (C) 2004 David Bateman
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

#include "lo-mappers.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

#if defined (HAVE_FFTW3)
#define FFTSRC "@sc{Fftw}"
#else
#define FFTSRC "@sc{Fftpack}"
#endif

static octave_value
do_fft (const octave_value_list &args, const char *fcn, int type)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage (fcn);
      return retval;
    }

  octave_value arg = args(0);
  dim_vector dims = arg.dims ();
  octave_idx_type n_points = -1;
  int dim = -1;
  
  if (nargin > 1)
    {
      if (! args(1).is_empty ())
	{
	  double dval = args(1).double_value ();
	  if (xisnan (dval))
	    error ("%s: NaN is invalid as the N_POINTS", fcn);
	  else
	    {
	      n_points = NINTbig (dval);
	      if (n_points < 0)
		error ("%s: number of points must be greater than zero", fcn);
	    }
	}
    }

  if (error_state)
    return retval;

  if (nargin > 2)
    {
      double dval = args(2).double_value ();
      if (xisnan (dval))
	error ("%s: NaN is invalid as the N_POINTS", fcn);
      else if (dval < 1 || dval > dims.length ())
	error ("%s: invalid dimension along which to perform fft", fcn);
      else
	// to be safe, cast it back to int since dim is an int
	dim = NINT (dval) - 1;
    }

  if (error_state)
    return retval;

  for (octave_idx_type i = 0; i < dims.length (); i++)
    if (dims(i) < 0)
      return retval;

  if (dim < 0)
    {
      for (octave_idx_type i = 0; i < dims.length (); i++)
	if ( dims(i) > 1)
	  {
	    dim = i;
	    break;
	  }

      // And if the first argument is scalar?
      if (dim < 0)
	dim = 1;
    }

  if (n_points < 0)
    n_points = dims (dim);
  else
    dims (dim) = n_points;

  if (dims.all_zero () || n_points == 0)
    return octave_value (Matrix ());

  if (arg.is_real_type ())
    {
      NDArray nda = arg.array_value ();

      if (! error_state)
	{
	  nda.resize (dims, 0.0);
	  retval = (type != 0 ? nda.ifourier (dim) : nda.fourier (dim));
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexNDArray cnda = arg.complex_array_value ();

      if (! error_state)
	{
	  cnda.resize (dims, 0.0);
	  retval = (type != 0 ? cnda.ifourier (dim) : cnda.fourier (dim));
	}
    }
  else
    {
      gripe_wrong_type_arg (fcn, arg);
    }

  return retval;
}


DEFUN_DLD (fft, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} fft (@var{a}, @var{n}, @var{dim})\n\
Compute the FFT of @var{a} using subroutines from\n"
FFTSRC
". The FFT is calculated along the first non-singleton dimension of the\n\
array. Thus if @var{a} is a matrix, @code{fft (@var{a})} computes the\n\
FFT for each column of @var{a}.\n\
\n\
If called with two arguments, @var{n} is expected to be an integer\n\
specifying the number of elements of @var{a} to use, or an empty\n\
matrix to specify that its value should be ignored. If @var{n} is\n\
larger than the dimension along which the FFT is calculated, then\n\
@var{a} is resized and padded with zeros. Otherwise, if@var{n} is\n\
smaller than the dimension along which the FFT is calculated, then\n\
@var{a} is truncated.\n\
\n\
If called with three agruments, @var{dim} is an integer specifying the\n\
dimension of the matrix along which the FFT is performed\n\
@end deftypefn\n\
@seealso {ifft, fft2, fftn, fftw_wisdom}")
{
  return do_fft (args, "fft", 0);
}


DEFUN_DLD (ifft, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} ifft (@var{a}, @var{n}, @var{dim})\n\
Compute the inverse FFT of @var{a} using subroutines from\n"
FFTSRC
". The inverse FFT is calculated along the first non-singleton dimension\n\
of the array. Thus if @var{a} is a matrix, @code{fft (@var{a})} computes\n\
the inverse FFT for each column of @var{a}.\n\
\n\
If called with two arguments, @var{n} is expected to be an integer\n\
specifying the number of elements of @var{a} to use, or an empty\n\
matrix to specify that its value should be ignored. If @var{n} is\n\
larger than the dimension along which the inverse FFT is calculated, then\n\
@var{a} is resized and padded with zeros. Otherwise, if@var{n} is\n\
smaller than the dimension along which the inverse FFT is calculated,\n\
then @var{a} is truncated.\n\
\n\
If called with three agruments, @var{dim} is an integer specifying the\n\
dimension of the matrix along which the inverse FFT is performed\n\
@end deftypefn\n\
@seealso {fft, ifft2, ifftn, fftw_wisdom}")
{
  return do_fft (args, "ifft", 1);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
