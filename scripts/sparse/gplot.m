## Copyright (C) 2005 Ivana Varekova
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} gplot (@var{a}, @var{xy})
## @deftypefnx {Function File} {} gplot (@var{a}, @var{xy}, @var{LineSpec})
## @deftypefnx {Function File} {[@var{x}, @var{y}] =} gplot (@var{a}, @var{xy})
## Plots a graph defined by @var{A} and @var{xy} in the graph theory sense.
## @var{A} is the adjacency matrix of the array to be plotted and @var{xy} is a
## @var{n}-by-2 matrix containing the coordinates of the nodes of the graph.
##
## If defined, @var{LineStyle} defines the output style for the plot. Called 
## with no output arguments the graph is plotted directly. Called with output
## arguments the coordinates of the plot are returned in @var{x} and @var{y}, 
## rather than being plotted.
## @end deftypefn
## @seealso{treeplot,etreeplot,spy}

function [x, y] = gplot (A, xy, LineStyle)

  if (nargin < 2 || nargin > 3 || nargout > 2)
    error ("gplot: wrong number of input/output arguments");
  endif

  if (nargin == 2)
    LineStyle = "1;;";
  endif

  [i,j] = find(A);
  xcoord = [xy(i,1), xy(j,1), NaN * ones(length(i),1)]'(:);
  ycoord = [xy(i,2), xy(j,2), NaN * ones(length(i),1)]'(:);

  if (nargout == 0)
    plot(xcoord,ycoord,LineStyle); 
  else
    x = xcoord;
    y = ycoord;
  endif

endfunction
