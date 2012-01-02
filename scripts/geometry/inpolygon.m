## Copyright (C) 2006-2012 Frederick (Rick) A Niles
##               and S�ren Hauberg
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
## @deftypefn {Function File} {[@var{in}, @var{on}] =} inpolygon (@var{x}, @var{y}, @var{xv}, @var{yv})
##
## For a polygon defined by vertex points @code{(@var{xv}, @var{yv})}, determine
## if the points @code{(@var{x}, @var{y})} are inside or outside the polygon.
## The variables @var{x}, @var{y}, must have the same dimension.  The optional
## output @var{on} gives the points that are on the polygon.
##
## @end deftypefn

## Author: Frederick (Rick) A Niles <niles@rickniles.com>
## Created: 14 November 2006

## Vectorized by S�ren Hauberg <soren@hauberg.org>

## The method for determining if a point is in in a polygon is based on
## the algorithm shown on
## http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/ and is
## credited to Randolph Franklin.

function [in, on] = inpolygon (x, y, xv, yv)

  if (nargin != 4)
    print_usage ();
  endif

  if (! (isreal (x) && isreal (y) && ismatrix (y) && ismatrix (y)
         && size_equal (x, y)))
    error ("inpolygon: first two arguments must be real matrices of same size");
  elseif (! (isreal (xv) && isreal (yv) && isvector (xv) && isvector (yv)
             && size_equal (xv, yv)))
    error ("inpolygon: last two arguments must be real vectors of same size");
  endif

  npol = length (xv);
  do_boundary = (nargout >= 2);

  in = zeros (size(x), "logical");
  if (do_boundary)
    on = zeros (size(x), "logical");
  endif

  j = npol;
  for i = 1 : npol
    delta_xv = xv(j) - xv(i);
    delta_yv = yv(j) - yv(i);
    ## distance = [distance from (x,y) to edge] * length(edge)
    distance = delta_xv .* (y - yv(i)) - (x - xv(i)) .* delta_yv;
    ##
    ## is y between the y-values of edge i,j
    ##        AND (x,y) on the left of the edge ?
    idx1 = (((yv(i) <= y & y < yv(j)) | (yv(j) <= y & y < yv(i)))
            & 0 < distance.*delta_yv);
    in (idx1) = !in (idx1);

    ## Check if (x,y) are actually on the boundary of the polygon.
    if (do_boundary)
       idx2 = (((yv(i) <= y & y <= yv(j)) | (yv(j) <= y & y <= yv(i)))
               & ((xv(i) <= x & x <= xv(j)) | (xv(j) <= x & x <= xv(i)))
               & (0 == distance | !delta_xv));
       on (idx2) = true;
    endif
    j = i;
  endfor

endfunction

%!demo
%!  xv=[ 0.05840, 0.48375, 0.69356, 1.47478, 1.32158, \
%!       1.94545, 2.16477, 1.87639, 1.18218, 0.27615, \
%!       0.05840 ];
%!  yv=[ 0.60628, 0.04728, 0.50000, 0.50000, 0.02015, \
%!       0.18161, 0.78850, 1.13589, 1.33781, 1.04650, \
%!       0.60628 ];
%! xa=[0:0.1:2.3];
%! ya=[0:0.1:1.4];
%! [x,y]=meshgrid(xa,ya);
%! [in,on]=inpolygon(x,y,xv,yv);
%!
%! inside=in & !on;
%! plot(xv,yv)
%! hold on
%! plot(x(inside),y(inside),"@g")
%! plot(x(~in),y(~in),"@m")
%! plot(x(on),y(on),"@b")
%! hold off
%! disp("Green points are inside polygon, magenta are outside,");
%! disp("and blue are on boundary.");

%!demo
%!  xv=[ 0.05840, 0.48375, 0.69356, 1.47478, 1.32158, \
%!       1.94545, 2.16477, 1.87639, 1.18218, 0.27615, \
%!       0.05840, 0.73295, 1.28913, 1.74221, 1.16023, \
%!       0.73295, 0.05840 ];
%!  yv=[ 0.60628, 0.04728, 0.50000, 0.50000, 0.02015, \
%!       0.18161, 0.78850, 1.13589, 1.33781, 1.04650, \
%!       0.60628, 0.82096, 0.67155, 0.96114, 1.14833, \
%!       0.82096, 0.60628];
%! xa=[0:0.1:2.3];
%! ya=[0:0.1:1.4];
%! [x,y]=meshgrid(xa,ya);
%! [in,on]=inpolygon(x,y,xv,yv);
%!
%! inside=in & ~ on;
%! plot(xv,yv)
%! hold on
%! plot(x(inside),y(inside),"@g")
%! plot(x(~in),y(~in),"@m")
%! plot(x(on),y(on),"@b")
%! hold off
%! disp("Green points are inside polygon, magenta are outside,");
%! disp("and blue are on boundary.");

%!error inpolygon ();
%!error inpolygon (1, 2);
%!error inpolygon (1, 2, 3);

%!error inpolygon (1, [1,2], [3, 4], [5, 6]);
%!error inpolygon ([1,2], [3, 4], [5, 6], 1);

%!test
%! [in, on] = inpolygon ([1, 0], [1, 0], [-1, -1, 1, 1], [-1, 1, 1, -1]);
%! assert (in, [false, true]);
%! assert (on, [true, false]);
