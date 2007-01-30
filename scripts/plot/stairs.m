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
## @deftypefn {Function File} {} stairs (@var{x}, @var{y})
## Given two vectors of x-y data, bar produces a `stairstep' plot.
##
## If only one argument is given, it is taken as a vector of y-values
## and the x coordinates are taken to be the indices of the elements.
##
## If two output arguments are specified, the data are generated but
## not plotted.  For example,
##
## @example
## stairs (x, y);
## @end example
##
## @noindent
## and
##
## @example
## [xs, ys] = stairs (x, y);
## plot (xs, ys);
## @end example
##
## @noindent
## are equivalent.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, replot, xlabel, ylabel, title}
## @end deftypefn

## Author: jwe

function [xs, ys] = stairs (x, y)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    if (ismatrix (x))
      if (isvector (x))
	x = x(:);
      endif
      y = x;
      x = 1:rows (y);
    endif
  endif

  if (ndims (x) > 2 || ndims (y) > 2)
    error ("stairs: expecting 2-d arguments");
  endif

  vec_x = isvector (x);

  if (vec_x)
    x = x(:);
  endif

  if (isvector (y))
    y = y(:);
  endif

  if (ismatrix (y))
    [nr, nc] = size (y);
    if (vec_x)
      x = repmat (x, [1, nc]);
    else
      [x_nr, x_nc] = size (x);
      if (x_nr != nr || x_nc != nc)
	error ("stairs: argument size mismatch");
      endif
    endif
  endif

  len = 2*nr - 1;

  tmp_xs = tmp_ys = zeros (len, nc);

  tmp_xs(1,:) = x(1,:);
  tmp_ys(1,:) = y(1,:);

  tmp_x = x(2:nr,:);
  ridx = 2:2:len-1;
  tmp_xs(ridx,:) = tmp_x;
  tmp_ys(ridx,:) = y(1:nr-1,:);

  ridx = 3:2:len;
  tmp_xs(ridx,:) = tmp_x;
  tmp_ys(ridx,:) = y(2:nr,:);

  if (nargout == 0)
    plot (tmp_xs, tmp_ys);
  else
    xs = tmp_xs;
    ys = tmp_ys;
  endif

endfunction
