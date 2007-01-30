## Copyright (C) 2003 Shai Ayal
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## OctPlot is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with OctPlot; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} {@var{c}} = contour (@var{x},@var{y},@var{z},@var{vv})
## Compute isolines (countour lines) of the matrix @var{z}. 
## parameters @var{x}, @var{y} and @var{vn} are optional.
##
## The return value @var{c} is a 2 by @var{n} matrix containing the
## contour lines in the following format
##
## @example
## @var{c} = [lev1 , x1 , x2 , ... , levn , x1 , x2 , ... 
##      len1   , y1 , y2 , ... , lenn   , y1 , y2 , ...  ]
## @end example
##
## @noindent
## in which contour line @var{n} has a level (height) of @var{levn} and
## length of @var{lenn}.
## 
## If @var{x} and @var{y} are omitted they are taken as the row/column 
## index of @var{z}.  @var{vn} is either a scalar denoting the number of
## lines to compute or a vector containing the values of the lines.  If
## only one value is wanted, set @code{@var{vn} = [val, val]}.  If
## @var{vn} is omitted it defaults to 10.
##
## @example
## @var{c}=contourc (@var{x}, @var{y}, @var{z}, linspace(0,2*pi,10))
## @end example
## @seealso{contourc,line,plot}
## @end deftypefn


## Author: shaia

function retval = contour (varargin)

  [c, lev] = contourc (varargin{:});

  cmap = get (gcf(), "colormap");
  
  levx = linspace (min (lev), max (lev), size (cmap, 1));

  newplot ();

  ## decode contourc output format
  i1 = 1;
  while (i1 < length (c))
    clev = c(1,i1);
    clen = c(2,i1);

    ccr = interp1 (levx, cmap(:,1), clev);
    ccg = interp1 (levx, cmap(:,2), clev);
    ccb = interp1 (levx, cmap(:,3), clev);

    ii = i1+1:i1+clen;
    line (c(1,ii), c(2,ii), "color", [ccr, ccg, ccb, 1]);

    i1 += c(2,i1)+1;
  endwhile
  
  drawnow ();

  ## folowing DM's suggestion to surpress output if none asked for
  if (nargout > 0)
    retval = c;
  endif

endfunction


