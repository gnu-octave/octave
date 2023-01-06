########################################################################
##
## Copyright (C) 2001-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{y} =} interpft (@var{x}, @var{n})
## @deftypefnx {} {@var{y} =} interpft (@var{x}, @var{n}, @var{dim})
##
## Fourier interpolation.
##
## If @var{x} is a vector then @var{x} is resampled with @var{n} points.  The
## data in @var{x} is assumed to be equispaced.  If @var{x} is a matrix or an
## N-dimensional array, the interpolation is performed on each column of
## @var{x}.
##
## If @var{dim} is specified, then interpolate along the dimension @var{dim}.
##
## @code{interpft} assumes that the interpolated function is periodic, and so
## assumptions are made about the endpoints of the interpolation.
## @seealso{interp1}
## @end deftypefn

function y = interpft (x, n, dim)

  if (nargin < 2)
    print_usage ();
  endif

  if (! (isscalar (n) && n == fix (n)))
    error ("interpft: N must be a scalar integer");
  endif

  if (nargin == 2)
    if (isrow (x))
      dim = 2;
    else
      dim = 1;
    endif
  endif

  nd = ndims (x);

  if (dim < 1 || dim > nd)
    error ("interpft: invalid dimension DIM");
  endif

  perm = [dim:nd, 1:(dim-1)];
  x = permute (x, perm);
  m = rows (x);

  inc = ceil (m/n);
  xfft = fft (x) / m;
  k = ceil (m / 2);
  sz = size (x);
  sz(1) = n * inc - m;

  idx = repmat ({':'}, nd, 1);
  idx{1} = 1:k;
  y = cat (1, xfft(idx{:}), zeros (sz));
  idx{1} = k+1:m;
  y = cat (1, y, xfft(idx{:}));

  ## When m is an even number of rows, the FFT has a single Nyquist bin.
  ## If zero-padded above, distribute the value of the Nyquist bin evenly
  ## between the new corresponding positive and negative frequency bins.
  if (sz(1) > 0 && k == m/2)
    idx{1} = n * inc - k + 1;
    tmp = y(idx{:}) / 2;
    y(idx{:}) = tmp;
    idx{1} = k + 1;
    y(idx{:}) = tmp;
  endif

  y = n * ifft (y);

  if (inc != 1)
    sz(1) = n;
    y = inc * reshape (y(1:inc:end), sz);
  endif

  y = ipermute (y, perm);

endfunction


%!demo
%! clf;
%! t = 0 : 0.3 : pi;  dt = t(2)-t(1);
%! n = length (t);  k = 100;
%! ti = t(1) + [0 : k-1]*dt*n/k;
%! y = sin (4*t + 0.3) .* cos (3*t - 0.1);
%! yp = sin (4*ti + 0.3) .* cos (3*ti - 0.1);
%! plot (ti, yp, 'g', ti, interp1 (t, y, ti, "spline"), 'b', ...
%!       ti, interpft (y, k), 'c', t, y, "r+");
%! legend ("sin (4t+0.3)cos (3t-0.1)", "spline", "interpft", "data");

%!shared n,y
%! x = [0:10]';  y = sin(x);  n = length (x);
%!testif HAVE_FFTW
%! assert (interpft (y, n), y, 20*eps);
%!testif HAVE_FFTW
%! assert (interpft (y', n), y', 20*eps);
%!testif HAVE_FFTW
%! assert (interpft ([y,y],n), [y,y], 20*eps);

## Test case with complex input
%!testif HAVE_FFTW <*39566>
%! x = (1 + j) * [1:4]';
%! y = ifft ([15 + 15*j; -6; -1.5 - 1.5*j; 0; -1.5 - 1.5*j; -6*j]);
%! assert (interpft (x, 6), y, 10*eps);

## Test for correct spectral symmetry with even/odd lengths
%!testif HAVE_FFTW
%! assert (max (abs (imag (interpft ([1:8], 20)))), 0, 20*eps);
%!testif HAVE_FFTW
%! assert (max (abs (imag (interpft ([1:8], 21)))), 0, 21*eps);
%!testif HAVE_FFTW
%! assert (max (abs (imag (interpft ([1:9], 20)))), 0, 20*eps);
%!testif HAVE_FFTW
%! assert (max (abs (imag (interpft ([1:9], 21)))), 0, 21*eps);

## Test input validation
%!error <Invalid call> interpft ()
%!error <Invalid call> interpft (1)
%!error <N must be a scalar integer> interpft (1,[2,2])
%!error <N must be a scalar integer> interpft (1,2.1)
%!error <invalid dimension DIM> interpft (1,2,0)
%!error <invalid dimension DIM> interpft (1,2,3)
