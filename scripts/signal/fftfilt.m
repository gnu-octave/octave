## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn {Function File} {} fftfilt (@var{b}, @var{x}, @var{n})
##
## With two arguments, @code{fftfilt} filters @var{x} with the FIR filter
## @var{b} using the FFT.
##
## Given the optional third argument, @var{n}, @code{fftfilt} uses the
## overlap-add method to filter @var{x} with @var{b} using an N-point FFT.
##
## If @var{x} is a matrix, filter each column of the matrix.
## @seealso{filter, filter2}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Created: 3 September 1994
## Adapted-By: jwe

function y = fftfilt (b, x, n)

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

  if (! isvector (b))
    error ("fftfilt: B must be a vector");
  endif

  if (ndims (x) != 2)
    error ("fftfilt: X must be a 1-D or 2-D array");
  endif

  l_b = r_b * c_b;
  b = reshape (b, l_b, 1);

  if (nargin == 2)
    ## Use FFT with the smallest power of 2 which is >= length (x) +
    ## length (b) - 1 as number of points ...
    n = 2 ^ nextpow2 (r_x + l_b - 1);
    B = fft (b, n);
    y = ifft (fft (x, n) .* B(:, ones (1, c_x)));
  else
    ## Use overlap-add method ...
    if (! (isscalar (n)))
      error ("fftfilt: N has to be a scalar");
    endif
    n = 2 ^ nextpow2 (max ([n, l_b]));
    L = n - l_b + 1;
    B = fft (b, n);
    B = B(:, ones (c_x,1));
    R = ceil (r_x / L);
    y = zeros (r_x, c_x);
    for r = 1:R;
      lo = (r - 1) * L + 1;
      hi = min (r * L, r_x);
      tmp = zeros (n, c_x);
      tmp(1:(hi-lo+1),:) = x(lo:hi,:);
      tmp = ifft (fft (tmp) .* B);
      hi  = min (lo+n-1, r_x);
      y(lo:hi,:) = y(lo:hi,:) + tmp(1:(hi-lo+1),:);
    endfor
  endif

  y = y(1:r_x, :);
  if (transpose)
    y = y.';
  endif

  ## Final cleanups: If both x and b are real, y should be real.
  ## If both x and b are integer, y should be integer.

  if (isreal (b) && isreal (x))
    y = real (y);
  endif
  if (! any (b - fix (b)))
    idx = !any (x - fix (x));
    y(:, idx) = round (y(:, idx));
  endif

endfunction


%!shared b, x, r
%!test
%!  b = [1 1];
%!  x = [1, zeros(1,9)];
%!  assert(fftfilt(b,  x  ), [1 1 0 0 0 0 0 0 0 0]  , eps);
%!  assert(fftfilt(b,  x.'), [1 1 0 0 0 0 0 0 0 0].', eps);
%!  assert(fftfilt(b.',x  ), [1 1 0 0 0 0 0 0 0 0]  , eps);
%!  assert(fftfilt(b.',x.'), [1 1 0 0 0 0 0 0 0 0].', eps);

%!test
%!  r = sqrt(1/2) * (1+i);
%!  b = b*r;
%!  assert(fftfilt(b, x  ), r*[1 1 0 0 0 0 0 0 0 0]  , eps);
%!  assert(fftfilt(b, r*x), r*r*[1 1 0 0 0 0 0 0 0 0], eps);
%!  assert(fftfilt(b, x.'), r*[1 1 0 0 0 0 0 0 0 0].', eps);

%!test
%!  b = [1 1];
%!  x = zeros (10,3); x(1,1)=-1; x(1,2)=1;
%!  y0 = zeros (10,3); y0(1:2,1)=-1; y0(1:2,2)=1;
%!  y = fftfilt (b, x);
%!  assert (y,y0);

%!test
%!  b  = rand (10, 1);
%!  x  = rand (10, 1);
%!  y0 = filter (b, 1, x);
%!  y  = filter (b, 1, x);
%!  assert (y, y0);

%% Test input validation
%!error fftfilt (1)
%!error fftfilt (1, 2, 3, 4)
%!error fftfilt (ones (2), 1)
%!error fftfilt (2, ones (3,3,3))
%!error fftfilt (2, 1, ones (2))

