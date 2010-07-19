## Copyright (C) 1999, 2000, 2007, 2008, 2009 Kai Habel
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
## @deftypefn  {Function File} {@var{tri} =} delaunay (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{tri} =} delaunay (@var{x}, @var{y}, @var{opt})
## The return matrix of size [n, 3] contains a set triangles which are
## described by the indices to the data point x and y vector.
## The triangulation satisfies the Delaunay circum-circle criterion.
## No other data point is in the circum-circle of the defining triangle.
##
## A third optional argument, which must be a string, contains extra options
## passed to the underlying qhull command.  See the documentation for the 
## Qhull library for details.
##
## @example
## @group
## x = rand (1, 10);
## y = rand (size (x));
## T = delaunay (x, y);
## X = [x(T(:,1)); x(T(:,2)); x(T(:,3)); x(T(:,1))];
## Y = [y(T(:,1)); y(T(:,2)); y(T(:,3)); y(T(:,1))];
## axis ([0,1,0,1]);
## plot (X, Y, "b", x, y, "r*");
## @end group
## @end example
## @seealso{voronoi, delaunay3, delaunayn}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function ret = delaunay (x, y, opt)

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif
  
  if (isvector (x) && isvector (y) && length (x) == length (y))
    if (nargin == 2)
      tri = delaunayn ([x(:), y(:)]);
    elseif (ischar (opt) || iscellstr (opt))
      tri = delaunayn ([x(:), y(:)], opt);
    else
      error ("delaunay: third argument must be a string");
    endif
  else
    error ("delaunay: first two input arguments must be vectors of same size");
  endif

  if (nargout == 0)
    x = x(:).';
    y = y(:).';
    X = [x(tri(:,1)); x(tri(:,2)); x(tri(:,3)); x(tri(:,1))];
    Y = [y(tri(:,1)); y(tri(:,2)); y(tri(:,3)); y(tri(:,1))];
    plot(X, Y, 'b', x, y, 'r*');
  else
    ret = tri;
  endif
endfunction

%!testif HAVE_QHULL
%! x = [-1, 0, 1, 0, 0];
%! y = [0, 1, 0, -1, 0];
%! assert (sortrows (sort (delaunay (x, y), 2)), [1,2,5;1,4,5;2,3,5;3,4,5])

%!demo
%! rand ('state', 1);
%! x = rand(1,10);
%! y = rand(size(x));
%! T = delaunay(x,y);
%! X = [ x(T(:,1)); x(T(:,2)); x(T(:,3)); x(T(:,1)) ];
%! Y = [ y(T(:,1)); y(T(:,2)); y(T(:,3)); y(T(:,1)) ];
%! axis([0,1,0,1]);
%! plot(X,Y,'b',x,y,'r*');
