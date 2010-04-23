## Copyright (C) 2006, 2007, 2008, 2009 Frederick (Rick) A Niles
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
## @deftypefn {Function File} {[@var{in}, @var{on}] =} inpolygon (@var{x}, @var{y}, @var{xv}, @var{xy})
##
## For a polygon defined by @code{(@var{xv}, @var{yv})} points, determine
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

function [IN, ON] = inpolygon (X, Y, xv, yv)

  if (nargin != 4)
    print_usage ();
  endif

  if (! (isreal (X) && isreal (Y) && ismatrix (Y) && ismatrix (Y)
         && size_equal (X, Y)))
    error ("inpolygon: first two arguments must be real matrices of same size");
  elseif (! (isreal (xv) && isreal (yv) && isvector (xv) && isvector (yv)
             && size_equal (xv, yv)))
    error ("inpolygon: last two arguments must be real vectors of same size");
  endif

  npol = length (xv);
  do_boundary = (nargout >= 2);
  
  IN = zeros (size(X), "logical");
  if (do_boundary) 
    ON = zeros (size(X), "logical"); 
  endif
  
  j = npol;
  for i = 1 : npol
    delta_xv = xv(j) - xv(i);
    delta_yv = yv(j) - yv(i);
    ## distance = [distance from (X,Y) to edge] * length(edge)
    distance = delta_xv .* (Y - yv(i)) - (X - xv(i)) .* delta_yv;
    ##
    ## is Y between the y-values of edge i,j
    ##        AND (X,Y) on the left of the edge ?
    idx1 = (((yv(i) <= Y & Y < yv(j)) | (yv(j) <= Y & Y < yv(i)))
            & 0 < distance.*delta_yv);
    IN (idx1) = !IN (idx1);

    ## Check if (X,Y) are actually ON the boundary of the polygon.
    if (do_boundary)
       idx2 = (((yv(i) <= Y & Y <= yv(j)) | (yv(j) <= Y & Y <= yv(i)))
               & ((xv(i) <= X & X <= xv(j)) | (xv(j) <= X & X <= xv(i)))
               & (0 == distance | !delta_xv));
       ON (idx2) = true;
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
%! [IN,ON]=inpolygon(x,y,xv,yv);
%! 
%! inside=IN & !ON;
%! plot(xv,yv)
%! hold on
%! plot(x(inside),y(inside),"@g")
%! plot(x(~IN),y(~IN),"@m")
%! plot(x(ON),y(ON),"@b")
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
%! [IN,ON]=inpolygon(x,y,xv,yv);
%! 
%! inside=IN & ~ ON;
%! plot(xv,yv)
%! hold on
%! plot(x(inside),y(inside),"@g")
%! plot(x(~IN),y(~IN),"@m")
%! plot(x(ON),y(ON),"@b")
%! hold off
%! disp("Green points are inside polygon, magenta are outside,");
%! disp("and blue are on boundary.");

