## Copyright (C) 1996, 1997, 2007 John W. Eaton
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

## Author: jwe

function tmp = __bars__ (h, vertical, x, y, xb, yb, width, group, have_color_spec, varargin)

  ycols = columns (y);
  clim = get (h, "clim");

  if (vertical)
    tmp = [];
    for i = 1:ycols
      if (! have_color_spec)
	if (ycols == 1)
	  lev = clim(1);
	else
	  lev = (i - 1) * (clim(2) - clim(1)) / (ycols - 1) - clim(1);
	endif
	tmp = [tmp; patch(xb(:,:,i), yb(:,:,i), "FaceColor", "flat", 
			  "cdata", lev, varargin {:})];
      else
	tmp = [tmp; patch(xb(:,:,i), yb(:,:,i), varargin {:})];
      endif
    endfor
  else
    tmp = [];
    for i = 1:ycols
      if (! have_color_spec)
	if (ycols == 1)
	  lev = clim(1)
	else
	  lev = (i - 1) * (clim(2) - clim(1)) / (ycols - 1) - clim(1);
	endif
	tmp = [tmp; patch(yb(:,:,i), xb(:,:,i), "FaceColor", "flat", 
			  "cdata", lev, varargin {:})];
      else
	tmp = [tmp; patch(yb(:,:,i), xb(:,:,i), varargin {:})];
      endif
    endfor
  endif
endfunction
