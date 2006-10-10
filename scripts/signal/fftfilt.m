## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} fftfilt (@var{b}, @var{x}, @var{n})
##
## With two arguments, @code{fftfilt} filters @var{x} with the FIR filter
## @var{b} using the FFT.
##
## Given the optional third argument, @var{n}, @code{fftfilt} uses the
## overlap-add method to filter @var{x} with @var{b} using an N-point FFT.
##
## If @var{x} is a matrix, filter each column of the matrix.
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Created: 3 September 1994
## Adapted-By: jwe

function y = fftfilt (b, x, N)

  ## If N is not specified explicitly, we do not use the overlap-add
  ## method at all because loops are really slow.  Otherwise, we only
  ## ensure that the number of points in the FFT is the smallest power
  ## of two larger than N and length(b).  This could result in length
  ## one blocks, but if the user knows better ...

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  transpose = (rows (x) == 1);

  if (transpose)
    x = x.';
  endif

  [r_x, c_x] = size (x);
  [r_b, c_b] = size (b);

  if min ([r_b, c_b]) != 1
    error ("fftfilt: b should be a vector");
  endif

  l_b = r_b * c_b;
  b = reshape (b, l_b, 1);

  if (nargin == 2)
    ## Use FFT with the smallest power of 2 which is >= length (x) +
    ## length (b) - 1 as number of points ...
    N = 2 ^ (ceil (log (r_x + l_b - 1) / log (2)));
    B = fft (b, N);
    y = ifft (fft (x, N) .* B(:,ones (1, c_x)));
  else
    ## Use overlap-add method ...
    if (! (isscalar (N)))
      error ("fftfilt: N has to be a scalar");
    endif
    N = 2 ^ (ceil (log (max ([N, l_b])) / log (2)));
    L = N - l_b + 1;
    B = fft (b, N);
    B = B(:,ones (c_x,1));
    R = ceil (r_x / L);
    y = zeros (r_x, c_x);
    for r = 1:R;
      lo = (r - 1) * L + 1;
      hi = min (r * L, r_x);
      tmp = zeros (N, c_x);
      tmp(1:(hi-lo+1),:) = x(lo:hi,:);
      tmp = ifft (fft (tmp) .* B);
      hi  = min (lo+N-1, r_x);
      y(lo:hi,:) = y(lo:hi,:) + tmp(1:(hi-lo+1),:);
    endfor
  endif

  y = y(1:r_x,:);
  if (transpose)
    y = y.';
  endif

  ## Final cleanups: if both x and b are real respectively integer, y
  ## should also be

  if (isreal (b) && isreal (x))
    y = real (y);
  endif
  if (! any (b - round (b)))
    idx = !any (x - round (x));
    y(:,idx) = round (y(:,idx));
  endif

endfunction
