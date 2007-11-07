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

## -*- texinfo -*-
## @deftypefn {Function File} {} quiver (@var{u}, @var{v})
## @deftypefnx {Function File} {} quiver (@var{x}, @var{y}, @var{u}, @var{v})
## @deftypefnx {Function File} {} quiver (@dots{}, @var{s})
## @deftypefnx {Function File} {} quiver (@dots{}, @var{style})
## @deftypefnx {Function File} {} quiver (@dots{}, 'filled')
## @deftypefnx {Function File} {} quiver (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} quiver (@dots{})
##
## Plot the @code{(@var{u}, @var{v})} components of a vector field in 
## an @code{(@var{x}, @var{y})} meshgrid. If the grid is uniform, you can 
## specify @var{x} and @var{y} as vectors.
##
## If @var{x} and @var{y} are undefined they are assumed to be
## @code{(1:@var{m}, 1:@var{n})} where @code{[@var{m}, @var{n}] = 
## size(@var{u})}.
##
## The variable @var{s} is a scalar defining a scaling factor to use for
##  the arrows of the field relative to the mesh spacing. A value of 0 
## disables all scaling. The default value is 1.
##
## The style to use for the plot can be defined with a line style @var{style}
## in a similar manner to the line styles used with the @code{plot} command.
## If a marker is specified then markers at the grid points of the vectors are
## printed rather than arrows. If the argument 'filled' is given then the
## markers as filled.
##
## The optional return value @var{h} provides a list of handles to the 
## the parts of the vector field (body, arrow and marker).
##
## @example
## @group
## [x, y] = meshgrid (1:2:20);
## quiver (x, y, sin (2*pi*x/10), sin (2*pi*y/10));
## @end group
## @end example
##
## @seealso{plot}
## @end deftypefn

function retval = quiver (varargin)

  if (nargin < 2)
    print_usage ();
  elseif (isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin {1};
    if (! strcmp (get (h, "type"), "axes"))
      error ("quiver: expecting first argument to be an axes object");
    endif
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      tmp = __quiver__ (h, varargin{2:end});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    newplot ();
    tmp = __quiver__ (gca (), varargin{:});
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction

function hlist = __quiver__ (varargin)
  h = varargin {1};

  s = 1;
  arrowsize = 0.33;

  firstnonnumeric = Inf;
  for i = 2:nargin
    if (! isnumeric (varargin {i}))
      firstnonnumeric = i;
      break;
    endif
  endfor

  if (nargin < 5 || firstnonnumeric < 5)
    u = varargin{2};
    v = varargin{3};
    if (nargin == 4 && isnumeric (varargin{4}) && isscalar (varargin{4}))
      s = varargin{4};
      iarg = 5;
    else
      iarg = 4;
    endif
    [x, y] = meshgrid (1:size(u,1), 1:size(u,2));
  else
    x = varargin{2};
    y = varargin{3};
    u = varargin{4};
    v = varargin{5};
    if (isvector(x) && isvector(y) && (!isvector (u) || !isvector (v)))
      [x, y] = meshgrid (x, y);
    endif
    if (nargin > 5 && isnumeric (varargin{6}) && isscalar (varargin{6}))
      s = varargin{6};
      iarg = 7;
    else
      iarg = 6;
    endif
  endif

  have_filled = false;
  have_line_spec = false;
  while (iarg <= nargin)
    arg = varargin {iarg++};
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
    len = max (sqrt (u(:).^2 + dy(:).^2));
    if (len > 0)
      s = s / sqrt (2) * sqrt (dx.^2 + dy.^2) / len; 
      u = s * u;
      v = s * v;
    endif
  endif

  x = x(:);
  y = y(:);
  xend = x + u(:);
  yend = y + v(:);

  hstate = get (h, "nextplot");
  unwind_protect
    if (have_line_spec)
      h1 = plot ([x.'; xend.'; NaN(1, length (x))](:),
		 [y.'; yend.'; NaN(1, length (y))](:),
		 "linestyle", linespec.linestyle);
    else
      h1 = plot ([x.'; xend.'; NaN(1, length (x))](:),
		 [y.'; yend.'; NaN(1, length (y))](:));
    endif
    hold on;

    xtmp = x + u(:) .* (1 - arrowsize);
    ytmp = y + v(:) .* (1 - arrowsize);
    xarrw1 = xtmp + (y - yend) * arrowsize / 3;
    xarrw2 = xtmp - (y - yend) * arrowsize / 3;
    yarrw1 = ytmp + (x - xend) * arrowsize / 3;
    yarrw2 = ytmp - (x - xend) * arrowsize / 3;

    if (have_line_spec)
      if (isfield (linespec, "marker") && 
	! strncmp (linespec.marker, "none", 4))
	h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		   [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
		   "linestyle", "none");
      else
	h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		   [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
		   "linestyle", linespec.linestyle);
      endif
    else
      h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
		 [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:));
    endif

    if (! have_line_spec || (isfield (linespec, "marker") && 
			     strncmp (linespec.marker, "none", 4)))
      h3 = plot (x, y, "linestyle", "none", "marker", "none");
    else
      h3 = plot (x, y, "linestyle", "none", "marker", linespec.marker);
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

%!demo
%! [x,y] = meshgrid(1:2:20);
%! quiver(x,y,sin(2*pi*x/10),sin(2*pi*y/10))

%!demo
%! axis("equal");
%! x=linspace(0,3,80); y=sin(2*pi*x); theta=2*pi*x+pi/2;
%! quiver(x,y,sin(theta)/10,cos(theta)/10);
%! hold on; plot(x,y,"r"); hold off;
