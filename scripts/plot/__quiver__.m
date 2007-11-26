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

## Undocumented internal function

function hlist = __quiver__ (varargin)
  h = varargin {1};
  is3d = varargin {2};

  s = 1;
  arrowsize = 0.33;

  firstnonnumeric = Inf;
  for i = 3:nargin
    if (! isnumeric (varargin {i}))
      firstnonnumeric = i;
      break;
    endif
  endfor

  ioff = 3;
  if (nargin < (6 + is3d) || firstnonnumeric < (6 + is3d))
    u = varargin{ioff++};
    v = varargin{ioff++};
    if (is3d)
      w = varargin{ioff++}
      [x, y, z] = meshgrid (1:size(u,1), 1:size(u,2), 1:max(size(w)));
    else
      [x, y] = meshgrid (1:size(u,1), 1:size(u,2));
    endif
    if (nargin >= ioff && isnumeric (varargin{ioff}) && 
	isscalar (varargin{ioff}))
      s = varargin{ioff++};
    endif
  else
    x = varargin{ioff++};
    y = varargin{ioff++};
    if (is3d)
      z = varargin{ioff++};
    endif
    u = varargin{ioff++};
    v = varargin{ioff++}; 
    if (is3d)
      w = varargin{ioff++};
      if (isvector(x) && isvector(y) && isvector(z) && 
	  (!isvector (u) || !isvector (v) || !isvector(w)))
	[x, y, z] = meshgrid (x, y, z);
      endif
    else
      if (isvector(x) && isvector(y) && (!isvector (u) || !isvector (v)))
	[x, y] = meshgrid (x, y);
      endif
    endif
    if (nargin >= ioff && isnumeric (varargin{ioff}) && 
	isscalar (varargin{ioff}))
      s = varargin{ioff++};
    endif
  endif

  have_filled = false;
  have_line_spec = false;
  while (ioff <= nargin)
    arg = varargin {ioff++};
    if (ischar (arg) && strncmp (tolower (arg), "filled", 6))
      have_filled = true;
    elseif ((isstr (arg) || iscell (arg))
	    && ! have_line_spec)
      [linespec, valid] = __pltopt__ ("quiver", arg, false);
      if (valid)
	have_line_spec = true;
	if (strncmp (linespec.linestyle, "none", 4))
	  linespec.linestyle = "-";
	endif
      else
	error ("quiver: invalid linespec");
      endif
    else
      error ("quiver: unrecognized argument");
    endif
  endwhile

  if (s)
    ## Scale the arrows to fit in the grid
    dx = (max(x(:)) - min(x(:))) ./ size (x, 2);
    dy = (max(y(:)) - min(y(:))) ./ size (y, 1);
    if (is3d)
      ## What should this be divided by? The below seems right
      dz = (max(z(:)) - min(z(:))) ./ max (size (z));
      len = max (sqrt (u(:).^2 + dy(:).^2) + dz(:).^2);
    else
      len = max (sqrt (u(:).^2 + dy(:).^2));
      dz = 0;
    endif
    if (len > 0)
      s = s / sqrt (2) * sqrt (dx.^2 + dy.^2 + dz.^2) / len; 
      u = s * u;
      v = s * v;
      if (is3d)
	w = s*w;
      endif
    endif
  endif

  x = x(:);
  y = y(:);
  xend = x + u(:);
  yend = y + v(:);
  if (is3d)
    z = z(:);
    zend = z + w(:);
  endif

  hstate = get (h, "nextplot");
  unwind_protect
    if (have_line_spec)
      if (is3d)
	h1 = plot3 ([x.'; xend.'; NaN(1, length (x))](:),
		    [y.'; yend.'; NaN(1, length (y))](:),
		    [z.'; zend.'; NaN(1, length (z))](:),
		    "linestyle", linespec.linestyle);
      else
	h1 = plot ([x.'; xend.'; NaN(1, length (x))](:),
		   [y.'; yend.'; NaN(1, length (y))](:),
		   "linestyle", linespec.linestyle);
      endif
    else
      if (is3d)
	h1 = plot3 ([x.'; xend.'; NaN(1, length (x))](:),
		    [y.'; yend.'; NaN(1, length (y))](:),
		    [z.'; zend.'; NaN(1, length (z))](:));
      else
	h1 = plot ([x.'; xend.'; NaN(1, length (x))](:),
		   [y.'; yend.'; NaN(1, length (y))](:));
      endif
    endif
    hold on;

    xtmp = x + u(:) .* (1 - arrowsize);
    ytmp = y + v(:) .* (1 - arrowsize);
    xarrw1 = xtmp + (y - yend) * arrowsize / 3;
    xarrw2 = xtmp - (y - yend) * arrowsize / 3;
    yarrw1 = ytmp - (x - xend) * arrowsize / 3;
    yarrw2 = ytmp + (x - xend) * arrowsize / 3;
    if (is3d)
      zarrw1 = zarrw2 = zend - w(:) * arrowsize / 3;
    endif

    if (have_line_spec)
      if (isfield (linespec, "marker") && 
	! strncmp (linespec.marker, "none", 4))
	if (is3d)
	  h2 = plot3 ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		      [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
		      [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:),
		      "linestyle", "none");
	else
	  h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		     [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
		     "linestyle", "none");
	endif
      else
	if (is3d)
	  h2 = plot3 ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		      [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
		      [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:),
		      "linestyle", linespec.linestyle);
	else
	  h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		     [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
		     "linestyle", linespec.linestyle);
	endif
      endif
    elseif (is3d)
      h2 = plot3 ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		  [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
		  [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:));
    else
      h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		 [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:));
    endif

    if (! have_line_spec || (isfield (linespec, "marker") && 
			     strncmp (linespec.marker, "none", 4)))
      if (is3d)
	h3 = plot3 (x, y, z, "linestyle", "none", "marker", "none");
      else
	h3 = plot (x, y, "linestyle", "none", "marker", "none");
      endif
    else
      if (is3d)
	h3 = plot3 (x, y, z, "linestyle", "none", "marker", linespec.marker);
      else

	h3 = plot (x, y, "linestyle", "none", "marker", linespec.marker);
      endif
    endif
    if (have_filled)
      ## FIXME gnuplot doesn't respect the markerfacecolor field
      set(h3, "markerfacecolor", get (h1, "color")); 
    endif
  unwind_protect_cleanup
    set (h, "nextplot", hstate);
  end_unwind_protect

  hlist = [h1; h2; h3];

endfunction
