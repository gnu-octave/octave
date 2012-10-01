## Copyright (C) 1995-2012 Andreas Weingessel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{y}, @var{c}] =} stft (@var{x}, @var{win_size}, @var{inc}, @var{num_coef}, @var{win_type})
## Compute the short-time Fourier transform of the vector @var{x} with
## @var{num_coef} coefficients by applying a window of @var{win_size} data
## points and an increment of @var{inc} points.
##
## Before computing the Fourier transform, one of the following windows
## is applied:
##
## @table @asis
## @item @nospell{hanning}
## win_type = 1
##
## @item @nospell{hamming}
## win_type = 2
##
## @item rectangle
## win_type = 3
## @end table
##
## The window names can be passed as strings or by the @var{win_type} number.
##
## If not all arguments are specified, the following defaults are used:
## @var{win_size} = 80, @var{inc} = 24, @var{num_coef} = 64, and
## @var{win_type} = 1.
##
## @code{@var{y} = stft (@var{x}, @dots{})} returns the absolute values
## of the Fourier coefficients according to the @var{num_coef} positive
## frequencies.
##
## @code{[@var{y}, @var{c}] = stft (@code{x}, @dots{})} returns the
## entire STFT-matrix @var{y} and a 3-element vector @var{c} containing
## the window size, increment, and window type, which is needed by the
## synthesis function.
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Description: Short-Time Fourier Transform

function [y, c] = stft(x, win_size, inc, num_coef, win_type)

  ## Default values of unspecified arguments.
  if (nargin < 5)
    win_type = 1;
    if (nargin < 4)
      num_coef = 64;
      if (nargin < 3)
        inc = 24;
        if (nargin < 2)
          win_size = 80;
        endif
      endif
    endif
  elseif (nargin == 5)
    if (ischar (win_type))
      if (strcmp (win_type, "hanning"))
        win_type = 1;
      elseif (strcmp (win_type, "hamming"))
        win_type = 2;
      elseif (strcmp (win_type, "rectangle"))
        win_type = 3;
      else
        error ("stft: unknown window type '%s'", win_type);
      endif
    endif
  else
    print_usage ();
  endif

  ## Check whether X is a vector.
  [nr, nc] = size (x);
  if (nc != 1)
    if (nr == 1)
      x = x';
      nr = nc;
    else
      error ("stft: X must be a vector");
    endif
  endif

  ncoef = 2 * num_coef;
  if (win_size > ncoef)
    win_size = ncoef;
    printf ("stft: window size adjusted to %f\n", win_size);
  endif
  num_win = fix ((nr - win_size) / inc);

  ## compute the window coefficients
  if (win_type == 3)
    ## Rectangular window.
    win_coef = ones (win_size, 1);
  elseif (win_type == 2)
    ## Hamming window.
    win_coef = hamming (win_size);
  else
    ## Hanning window.
    win_coef = hanning (win_size);
  endif

  ## Create a matrix Z whose columns contain the windowed time-slices.
  z = zeros (ncoef, num_win + 1);
  start = 1;
  for i = 0:num_win
    z(1:win_size, i+1) = x(start:start+win_size-1) .* win_coef;
    start = start + inc;
  endfor

  y = fft (z);

  if (nargout == 1)
    y = abs (y(1:num_coef, :));
  else
    c = [win_size, inc, win_type];
  endif

endfunction
