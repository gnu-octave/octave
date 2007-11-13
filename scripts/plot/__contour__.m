## Copyright (C) 2007 David Bateman
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

## Undocumented internal function.

function [c, h] = __contour__ (varargin)

  ax = varargin {1};
  z = varargin {2};

  clim = get (ax, "clim");

  [c, lev] = contourc (varargin{3:end});

  ## Decode contourc output format.
  i1 = 1;
  h = [];
  maxlev = max (lev);
  minlev = min (lev);
  while (i1 < length (c))
    clev = c(1,i1);
    clen = c(2,i1);

    ii = i1+1:i1+clen;
    lev = (clev - minlev) * (clim(2) - clim(1)) / (maxlev - minlev) + clim(1);

    if (isnan (z))
      h = [h; patch(ax, c(1,ii), c(2,ii), "facecolor", "none", 
		    "edgecolor", "flat", "cdata", lev)];
    else
      h = [h; patch(ax, c(1,ii), c(2,ii), z*ones(size(ii)), "facecolor",
		    "none", "edgecolor", "flat", "cdata", lev)];
    endif
    i1 += clen+1;
  endwhile
  
endfunction
