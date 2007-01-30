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
## @deftypefn {Function File} {} [@var{c}, @var{lev}] =  contourc (@var{x}, @var{y}, @var{z}, @var{vv})
## Compute isolines (countour lines) of the matrix @var{z}. 
## Parameters @var{x}, @var{y} and @var{vn} are optional.
##
## The return value @var{lev} is a vector of the contour levels.
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
## index of @var{z}.  @var{vn} is either a scalar denoting the number of lines 
## to compute or a vector containing the values of the lines. If only one 
## value is wanted, set @code{@var{vn} = [val, val]};
## If @var{vn} is omitted it defaults to 10.
##
## @example
## @var{c}=contourc (@var{x}, @var{y}, @var{z}, linspace(0,2*pi,10))
## @end example
## @seealso{contour}
## @end deftypefn

## Author: shaia

function [c, lev] = contourc (varargin)

  if (nargin == 1)
    vn = 10;
    z = varargin{1};
    x = 1:size(z,1);
    y = 1:size(z,2);
  elseif (nargin == 2)
    vn = varargin{2};
    z = varargin{1};
    x = 1:size(z,1);
    y = 1:size(z,2);
  elseif (nargin == 3)
    vn = 10;
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
  elseif (nargin == 4)
    vn = varargin{4};
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
  else
    print_usage ();
  endif

  if (isscalar (vn))
    vv = linspace (min (z(:)), max (z(:)), vn+2)(2:end-1);
  else
    vv = unique (sort (vn));
  endif

  ## Vectorize the x,y vectors, assuming they are output from meshgrid.
  if (! isvector (x))
    x = x(1,:);
  endif

  if (! isvector (y))
    y = y(:,1);
  endif

  ## Make everyone the right dimensions.
  if (size (x, 2) == 1)
    x = x';
  endif
  if (size (y, 2) == 1)
    y = y';
  endif

  ## Now call __contourc__ for the real work...
  c=__contourc__(x,y,z,vv);
  if nargout==2,
    lev = vv;
  endif

endfunction
