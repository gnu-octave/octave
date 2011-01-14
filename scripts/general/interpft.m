## Copyright (C) 2001-2011 Paul Kienzle
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
## @deftypefn  {Function File} {} interpft (@var{x}, @var{n})
## @deftypefnx {Function File} {} interpft (@var{x}, @var{n}, @var{dim})
##
## Fourier interpolation.  If @var{x} is a vector, then @var{x} is
## resampled with @var{n} points.  The data in @var{x} is assumed to be
## equispaced.  If @var{x} is an array, then operate along each column of
## the array separately.  If @var{dim} is specified, then interpolate
## along the dimension @var{dim}.
##
## @code{interpft} assumes that the interpolated function is periodic,
## and so assumptions are made about the end points of the interpolation.
##
## @seealso{interp1}
## @end deftypefn

## Author: Paul Kienzle
## 2001-02-11
##    * initial version
## 2002-03-17 aadler
##    * added code to work on matrices as well 
## 2006-05-25 dbateman
##    * Make it matlab compatiable, cutting out the 2-D interpolation

function z = interpft (x, n, dim)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 2)
    if (isvector (x) && size (x, 1) == 1)
      dim = 2;
    else
      dim = 1;
    endif
  endif

  if (! isscalar (n))
    error ("interpft: N must be an integer scalar");
  endif

  nd = ndims (x);

  if (dim < 1 || dim > nd)
    error ("interpft: integrating over invalid dimension");
  endif

  perm = [dim:nd, 1:(dim-1)];
  x = permute (x, perm);
  m = size (x, 1);

  inc = 1;
  while (inc*n < m)
    inc++;
  endwhile
  y = fft (x) / m;
  k = floor (m / 2);
  sz = size (x);
  sz(1) = n * inc - m;
  idx = cell (nd, 1);
  for i = 2:nd
    idx{i} = 1:sz(i);
  endfor
  idx{1} = 1:k;
  z = cat (1, y(idx{:}), zeros (sz));
  idx{1} = k+1:m;
  z = cat (1, z, y(idx{:}));
  z = n * ifft (z);

  if (inc != 1)
    sz(1) = n;
    z = inc * reshape (z(1:inc:end), sz);
  endif

  z = ipermute (z, perm);
endfunction

%!demo
%! t = 0 : 0.3 : pi; dt = t(2)-t(1);
%! n = length (t); k = 100;
%! ti = t(1) + [0 : k-1]*dt*n/k;
%! y = sin (4*t + 0.3) .* cos (3*t - 0.1);
%! yp = sin (4*ti + 0.3) .* cos (3*ti - 0.1);
%! plot (ti, yp, 'g', ti, interp1(t, y, ti, 'spline'), 'b', ...
%!       ti, interpft (y, k), 'c', t, y, 'r+');
%! legend ('sin(4t+0.3)cos(3t-0.1','spline','interpft','data');

%!shared n,y
%! x = [0:10]'; y = sin(x); n = length (x);
%!assert (interpft(y, n), y, 20*eps);
%!assert (interpft(y', n), y', 20*eps);
%!assert (interpft([y,y],n), [y,y], 20*eps);

%!error (interpft(y,n,0))
%!error (interpft(y,[n,n]))
