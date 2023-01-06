########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} voronoi (@var{x}, @var{y})
## @deftypefnx {} {} voronoi (@var{x}, @var{y}, @var{options})
## @deftypefnx {} {} voronoi (@dots{}, "linespec")
## @deftypefnx {} {} voronoi (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} voronoi (@dots{})
## @deftypefnx {} {[@var{vx}, @var{vy}] =} voronoi (@dots{})
## Plot the Voronoi diagram of points @code{(@var{x}, @var{y})}.
##
## The Voronoi facets with points at infinity are not drawn.
##
## The @var{options} argument, which must be a string or cell array of strings,
## contains options passed to the underlying qhull command.
## See the documentation for the Qhull library for details
## @url{http://www.qhull.org/html/qh-quick.htm#options}.
##
## If @qcode{"linespec"} is given it is used to set the color and line style of
## the plot.
##
## If an axes graphics handle @var{hax} is supplied then the Voronoi diagram is
## drawn on the specified axes rather than in a new figure.
##
## If a single output argument is requested then the Voronoi diagram will be
## plotted and a graphics handle @var{h} to the plot is returned.
##
## [@var{vx}, @var{vy}] = voronoi (@dots{}) returns the Voronoi vertices
## instead of plotting the diagram.
##
## @example
## @group
## x = rand (10, 1);
## y = rand (size (x));
## h = convhull (x, y);
## [vx, vy] = voronoi (x, y);
## plot (vx, vy, "-b", x, y, "o", x(h), y(h), "-g");
## legend ("", "points", "hull");
## @end group
## @end example
##
## @seealso{voronoin, delaunay, convhull}
## @end deftypefn

function [vx, vy] = voronoi (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  narg = 1;
  hax = NaN;
  if (isscalar (varargin{1}) && ishghandle (varargin{1}))
    hax = varargin{1};
    if (! isaxes (hax))
      error ("voronoi: HAX argument must be an axes object");
    endif
    narg += 1;
  endif

  if (nargin < 1 + narg || nargin > 3 + narg)
    print_usage ();
  endif

  x = varargin{narg++};
  y = varargin{narg++};

  opts = {};
  if (narg <= nargin)
    if (iscell (varargin{narg}))
      opts = varargin(narg++);
    elseif (isnumeric (varargin{narg}))
      ## Accept, but ignore, the triangulation
      narg += 1;
    endif
  endif

  linespec = {"b"};
  if (narg <= nargin && ischar (varargin{narg}))
    linespec = varargin(narg);
  endif

  if (! isvector (x) || ! isvector (y) || numel (x) != numel (y))
    error ("voronoi: X and Y must be vectors of the same length");
  elseif (numel (x) < 2)
    error ("voronoi: minimum of 2 points required");
  endif
  x = x(:);
  y = y(:);

  ## Add box to approximate rays to infinity.  For Voronoi diagrams the
  ## box should be close to the points themselves.  To make the job of
  ## finding the exterior edges easier it should be bigger than the area
  ## enclosed by the points themselves.
  ## NOTE: Octave uses a factor of 2 although we don't have an mathematical
  ## justification for that.

  xmin = min (x);
  xmax = max (x);
  ymin = min (y);
  ymax = max (y);
  ## Factor for size of bounding box
  scale = 2;
  xdelta = xmax - xmin;
  ydelta = ymax - ymin;
  xbox = [xmin - scale * xdelta; xmin - scale * xdelta;
          xmax + scale * xdelta; xmax + scale * xdelta];
  ybox = [ymin - scale * ydelta; ymax + scale * ydelta;
          ymax + scale * ydelta; ymin - scale * ydelta];

  [p, c, infi] = __voronoi__ ("voronoi", [[x; xbox], [y; ybox]], opts{:});

  ## Build list of edges from points in facet.
  c = c(! infi).';
  edges = zeros (2, 0);
  for i = 1:numel (c)
    facet = c{i};
    if (isempty (facet))
      continue;
    endif
    edges = [edges, [facet; [facet(end), facet(1:end-1)]]];
  endfor

  ## Keep only the unique edges of the Voronoi diagram
  edges = sortrows (sort (edges).').';
  edges = edges(:, [any(diff(edges, 1, 2)), true]);

  if (numel (x) > 2)
    ## Eliminate the edges of the diagram representing the box.
    ## Exclude points outside a certain radius from the center of distribution.
    ## FIXME: Factor should be at least 1.0.  Octave uses 1.1 for margin.
    ## There is no theoretical justification for this choice.
    ctr = [(xmax + xmin)/2 , (ymax + ymin)/2];
    radius = 1.1 * sumsq ([xmin, ymin] - ctr);
    dist = sumsq (p - ctr, 2);

    p_inside = (1:rows (p))(dist < radius);
    edge_inside = any (ismember (edges, p_inside));
    edges = edges(:, edge_inside);
  else
    ## look for the edge between the two given points
    for edge = edges
      if (det ([[[1;1],p(edge,1:2)];1,x(1),y(1)])
          * det ([[[1;1],p(edge,1:2)];1,x(2),y(2)]) < 0)
        edges = edge;
        break;
      endif
    endfor
    ## Use larger plot limits to make it more likely single bisector is shown.
    xdelta = ydelta = max (xdelta, ydelta);
  endif

  ## Get points of the diagram
  Vvx = reshape (p(edges, 1), size (edges));
  Vvy = reshape (p(edges, 2), size (edges));

  if (nargout < 2)
    if (isnan (hax))
      hax = gca ();
    endif
    h = plot (hax, Vvx, Vvy, linespec{:}, x, y, '+');
    lim = [xmin, xmax, ymin, ymax];
    axis (lim + 0.1 * [[-1, 1] * xdelta, [-1, 1] * ydelta]);
    if (nargout == 1)
      vx = h;
    endif
  else
    vx = Vvx;
    vy = Vvy;
  endif

endfunction


%!demo
%! voronoi (rand (10,1), rand (10,1));

%!testif HAVE_QHULL
%! phi = linspace (-pi, 3/4*pi, 8);
%! [x,y] = pol2cart (phi, 1);
%! [vx,vy] = voronoi (x,y);
%! assert (vx(2,:), zeros (1, columns (vx)), eps);
%! assert (vy(2,:), zeros (1, columns (vy)), eps);

%!testif HAVE_QHULL <*40996>
%! ## Special case of just 2 points
%! x = [0 1];  y = [1 0];
%! [vx, vy] = voronoi (x,y);
%! assert (vx, [-0.7; 1.7], eps);
%! assert (vy, [-0.7; 1.7], eps);

%!testif HAVE_QHULL <*38295>
%! x = [1,2,3];  y = [2,3,1];
%! [vx, vy] = voronoi (x,y);
%! assert (columns (vx), 3);

%!testif HAVE_QHULL <*37270>
%! ## Duplicate points can cause an internal error
%! x = [1,2,3, 3];  y = [2,3,1, 1];
%! [vx, vy] = voronoi (x,y);


## Input validation tests
%!error <Invalid call> voronoi ()
%!error voronoi (ones (3,1))
%!error voronoi (ones (3,1), ones (3,1), "invalid1", "invalid2", "invalid3")
%!error <HAX argument must be an axes object> voronoi (0, ones (3,1), ones (3,1))
%!error <X and Y must be vectors of the same length> voronoi (ones (3,1), ones (4,1))
%!error <minimum of 2 points required> voronoi (2.5, 3.5)
