## Copyright (C) 1995, 1996, 1997  Andreas Weingessel
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details. 
## 
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## usage:  [Y, c] = stft (X [, win_size [, inc [, num_coef [, w_type]]]])
##
## Computes the short-term Fourier transform of the vector X with
## "num_coef" coefficients by applying a window of "win_size" data
## points and an increment of "inc" points.
##
## Before computing the Fourier transform, one of the following windows
## is applied:  "hanning" (w_type = 1), "hamming" (w_type = 2),
## "rectangle" (w_type = 3).  The window names can be passed as strings
## or by the w_type number.
##
## If not all arguments are specified, the following defaults are used:
## win_size = 80, inc = 24, num_coef = 64, w_type = 1.
##
## Y = stft (X [, ...]) returns the absolute values of the Fourier
## coefficients according to the num_coef positive frequencies.
## [Y, c] = stft (X [, ...]) returns the entire STFT-matrix Y and a
## vector c = [win_size, inc, w_type] which is needed by the synthesis
## function.

## Author:  AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Description:  Short-term Fourier transform

function [Y, c] = stft(X, win, inc, coef, w_type)
  
  ## default values of unspecified arguments
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
    if (isstr (w_type))
      if (strcmp (w_type, "hanning"))
	w_type = 1;
      elseif (strcmp (w_type, "hamming"))
	w_type = 2;
      elseif (strcmp (w_type, "rectangle"))
	w_type = 3;
      else
	error (["stft:  unknown window type `", w_type, "'"])
      endif
    endif
  else
    usage ("[Y [, c]] = ", ...
	   "stft(X [, win_size [, inc [, num_coef [, w_type]]]])");
  endif

  ## check whether X is a vector
  [nr, nc] = size (X);
  if (nc != 1)
    if (nr == 1)
      X = X'; 
      nr = nc;
    else
      error ("stft:  X must be a vector");
    endif
  endif

  num_coef = 2 * coef;
  if (win > num_coef)
    win = num_coef;
    printf ("stft:  window size adjusted to %f\n", win);
  endif
  num_win = fix ((nr - win) / inc);

  ## compute the window coefficients
  if (w_type == 3)		# rectangular window 
    WIN_COEF = ones (win, 1);
  elseif (w_type == 2)		# Hamming window
    WIN_COEF = hamming (win);
  else				# Hanning window
    WIN_COEF = hanning (win);
  endif
  
  ## create a matrix Z whose columns contain the windowed time-slices
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
