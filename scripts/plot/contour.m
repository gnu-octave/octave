## Copyright (C) 2003 Shai Ayal
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
## @deftypefn {Function File} {@var{c} =} contour (@var{z})
## @deftypefnx {Function File} {@var{c} =} contour (@var{z}, @var{vn})
## @deftypefnx {Function File} {@var{c} =} contour (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {@var{c} =} contour (@var{x}, @var{y}, @var{z}, @var{vn})
##
## Plot level curves (contour lines) of the matrix @var{z}, using the
## contour matrix @var{c} computed by @code{contourc} from the same
## arguments; see the latter for their interpretation.  @var{c} is only
## returned if requested.  For example:
##
## @example
## @group
## x = 0:2;
## y = x;
## z = x' * y;
## contour (x, y, z, 2:3)
##
## @end group
## @end example
## @seealso{contourc, line, plot}
## @end deftypefn

## Author: shaia

function retval = contour (varargin)

  [c, lev] = contourc (varargin{:});

  cmap = get (gcf(), "colormap");
  
  levx = linspace (min (lev), max (lev), size (cmap, 1));

  newplot ();

  ## Decode contourc output format.
  i1 = 1;
  while (i1 < length (c))

    clev = c(1,i1);
    clen = c(2,i1);

    ccr = interp1 (levx, cmap(:,1), clev);
    ccg = interp1 (levx, cmap(:,2), clev);
    ccb = interp1 (levx, cmap(:,3), clev);

    ii = i1+1:i1+clen;
    line (c(1,ii), c(2,ii), "color", [ccr, ccg, ccb]);

    i1 += c(2,i1)+1;
  endwhile
  
  if (nargout > 0)
    retval = c;
  endif

endfunction


