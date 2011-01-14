## Copyright (C) 2000-2011 Kai Habel
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
## @deftypefn  {Function File} {@var{H} =} convhull (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{H} =} convhull (@var{x}, @var{y}, @var{opt})
## Returns the index vector to the points of the enclosing convex hull.  The
## data points are defined by the x and y vectors.
##
## A third optional argument, which must be a string, contains extra options
## passed to the underlying qhull command.  See the documentation for the 
## Qhull library for details.
##
## @seealso{delaunay, convhulln}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function H = convhull (x, y, opt)

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif

  if (isvector (x) && isvector (y) && length (x) == length (y))
    if (nargin == 2)
      i = convhulln ([x(:), y(:)]);
    elseif (ischar (opt) || iscell (opt))
      i = convhulln ([x(:), y(:)], opt);
    else
      error ("convhull: third argument must be a string or cell array of strings");
    endif
  else
    error ("convhull: first two input arguments must be vectors of same size");
  endif

  n = rows (i);
  i = i'(:);
  H = zeros (n + 1, 1);

  H(1) = i(1);
  next_i = i(2);
  i(2) = 0;
  for k = 2:n
    next_idx = find (i == next_i);

    if (rem (next_idx, 2) == 0)
      H(k) = i(next_idx);
      next_i = i(next_idx - 1);
      i(next_idx - 1) = 0;
    else
      H(k) = i(next_idx);
      next_i = i(next_idx + 1);
      i(next_idx + 1) = 0;
    endif
  endfor

  H(n + 1) = H(1);
endfunction

%!testif HAVE_QHULL
%! x = -3:0.5:3;
%! y = abs (sin (x));
%! assert (convhull (x, y, {"s","Qci","Tcv","Pp"}), [1;7;13;12;11;10;4;3;2;1])

%!demo
%! x = -3:0.05:3;
%! y = abs (sin (x));
%! k = convhull (x, y);
%! plot (x(k),y(k),'r-',x,y,'b+');
%! axis ([-3.05, 3.05, -0.05, 1.05]);
