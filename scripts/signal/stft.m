## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2005, 2006, 2007, 2009
##               Andreas Weingessel
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
## @deftypefn {Function File} {[@var{y}, @var{c}] =} stft (@var{x}, @var{win_size}, @var{inc}, @var{num_coef}, @var{w_type})
## Compute the short-time Fourier transform of the vector @var{x} with
## @var{num_coef} coefficients by applying a window of @var{win_size} data
## points and an increment of @var{inc} points.
##
## Before computing the Fourier transform, one of the following windows
## is applied:
##
## @table @asis
## @item @nospell{hanning}
## w_type = 1
##
## @item @nospell{hamming}
## w_type = 2
##
## @item rectangle
## w_type = 3
## @end table
##
## The window names can be passed as strings or by the @var{w_type} number.
##
## If not all arguments are specified, the following defaults are used:
## @var{win_size} = 80, @var{inc} = 24, @var{num_coef} = 64, and
## @var{w_type} = 1.
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

function [Y, c] = stft(X, win, inc, coef, w_type)

  ## Default values of unspecified arguments.
  if (nargin < 5)
    w_type = 1;
    if (nargin < 4)
      coef = 64;
      if (nargin < 3)
        inc = 24;
        if (nargin < 2)
          win = 80;
        endif
      endif
    endif
  elseif (nargin == 5)
    if (ischar (w_type))
      if (strcmp (w_type, "hanning"))
        w_type = 1;
      elseif (strcmp (w_type, "hamming"))
        w_type = 2;
      elseif (strcmp (w_type, "rectangle"))
        w_type = 3;
      else
        error ("stft: unknown window type `%s'", w_type);
      endif
    endif
  else
    print_usage ();
  endif

  ## Check whether X is a vector.
  [nr, nc] = size (X);
  if (nc != 1)
    if (nr == 1)
      X = X';
      nr = nc;
    else
      error ("stft: X must be a vector");
    endif
  endif

  num_coef = 2 * coef;
  if (win > num_coef)
    win = num_coef;
    printf ("stft: window size adjusted to %f\n", win);
  endif
  num_win = fix ((nr - win) / inc);

  ## compute the window coefficients
  if (w_type == 3)
    ## Rectangular window.
    WIN_COEF = ones (win, 1);
  elseif (w_type == 2)
    ## Hamming window.
    WIN_COEF = hamming (win);
  else
    ## Hanning window.
    WIN_COEF = hanning (win);
  endif

  ## Create a matrix Z whose columns contain the windowed time-slices.
  Z = zeros (num_coef, num_win + 1);
  start = 1;
  for i = 0:num_win
    Z(1:win, i+1) = X(start:start+win-1) .* WIN_COEF;
    start = start + inc;
  endfor

  Y = fft (Z);

  if (nargout == 1)
    Y = abs (Y(1:coef, :));
  else
    c = [win, inc, w_type];
  endif

endfunction
