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

  ax = varargin{1};
  z = varargin{2};

  linespec.linestyle = "-";
  linespec.color = "flat";
  for i = 3 : nargin
    arg = varargin {i};
    if ((ischar (arg) || iscell (arg)))
      [linespec, valid] = __pltopt__ ("quiver", arg, false);
      if (isempty (linespec.color))
	linespec.color = "flat";
      endif
      if (valid)
	have_line_spec = true;
	varargin(i) = [];
	break;
      endif
    endif
  endfor

  if (ischar (z))
    if (strcmp (z, "none"))
      z = NaN;
    elseif (strcmp (z, "base"))
      if (nargin < 3)
	z = varargin{1};
      else
	z = varargin{3};
      endif
      z = 2 * (min (z(:)) - max (z(:)));
    elseif (! strcmp (z, "level"))
      error ("unrecognized z argument");
    endif
  endif

  [c, lev] = contourc (varargin{3:end});

  ## Decode contourc output format.
  i1 = 1;
  h = [];
  while (i1 < length (c))
    clev = c(1,i1);
    clen = c(2,i1);

    if (all (c(:,i1+1) == c(:,i1+clen)))
      p = c(:, i1+1:i1+clen-1);
    else
      p = [c(:, i1+1:i1+clen), NaN(2, 1)];
    endif

    if (isnan (z))
      h = [h; patch(ax, p(1,:), p(2,:), "facecolor", "none", 
		    "edgecolor", linespec.color, "linestyle", 
		    linespec.linestyle, "cdata", clev)];
    elseif (!ischar(z))
      h = [h; patch(ax, p(1,:), p(2,:), z * ones (1, columns (p)), "facecolor",
		    "none", "edgecolor", linespec.color, 
		    "linestyle", linespec.linestyle, "cdata", clev)];
    else
      h = [h; patch(ax, p(1,:), p(2,:), clev * ones (1, columns (p)),
		    "facecolor", "none", "edgecolor", linespec.color, 
		    "linestyle", linespec.linestyle, "cdata", clev)];
    endif
    i1 += clen+1;
  endwhile
  
endfunction
