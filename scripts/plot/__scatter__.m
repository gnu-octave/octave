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

function h = __scatter__ (varargin)

  h = varargin{1};
  nd = varargin{2};
  fcn = varargin{3};
  x = varargin{4}(:);
  y = varargin{5}(:);
  istart = 6;

  if (nd == 3)
    z = varargin{6}(:);
    idx = isnan(x) | isnan (y) | isnan (z);
    x (idx) = [];
    y (idx) = [];
    z (idx) = [];
    istart = 7;
  else
    idx = isnan(x) | isnan (y);
    x (idx) = [];
    y (idx) = [];
    z = zeros (length (x), 0);
  endif

  firstnonnumeric = Inf;
  for i = istart:nargin
    if (! isnumeric (varargin{i}))
      firstnonnumeric = i;
      break;
    endif
  endfor

  if (istart < nargin && firstnonnumeric > istart)
    s = varargin{istart};
    if (isempty (s))
      s = 6;
    endif
  else
    s = 6;
  endif

  if (istart < nargin && firstnonnumeric > istart + 1)
    c = varargin{istart + 1};
    if (isvector (c))
      c = c(:);
    endif
  elseif (firstnonnumeric == istart + 1 && ischar (varargin{istart + 1}))
    c = varargin{istart + 1};
    firstnonnumeric++;
  else
    c = 1 : length(x);
  endif

  newargs = {};
  filled = false;
  have_marker = false;
  marker = "o";
  iarg = firstnonnumeric;
  while (iarg <= nargin)
    arg = varargin{iarg++};
    if (ischar (arg) && strncmp (tolower (arg), "filled", 6))
      filled = true;
    elseif ((isstr (arg) || iscell (arg)) && ! have_marker)
      [linespec, valid] = __pltopt__ ("scatter", arg, false);
      if (valid)
	have_marker = true;
	marker = linespec.marker;
	if (strncmp (marker, "none", 4))
	  marker = "o";
	endif
      else
	error ("scatter: invalid linespec");
      endif
    else
      newargs{end+1} = arg;
      if (iarg <= nargin)
	newargs{end+1} = varagin{iarg++};
      endif
    endif
  endwhile

  if (ischar (c))
    h = patch("faces", [1:length(x)].', "vertices", [x, y, z], "facecolor",
	      "none", "edgecolor", c, "marker", marker, 
	      "markersize", s, "linestyle", "none");
    if (filled)
      set(h, "markerfacecolor", c); 
    endif
  else
    h = patch("faces", [1:length(x)].', "vertices", [x, y, z], "facecolor",
	      "none", "edgecolor", "flat", "cdata", c, "marker", marker, 
	      "markersize", s, "linestyle", "none");
    if (filled)
      set(h, "markerfacecolor", "flat"); 
    endif
    ax = get (h, "parent");
    clim = get (ax, "clim");
    if (min(c(:)) < clim(1))
      clim(1) = min(c(:));
      set (ax, "clim", clim);
    endif
    if (max(c(:)) > clim(2))
      set (ax, "clim", [clim(1), max(c(:))]);
    endif
  endif

endfunction
