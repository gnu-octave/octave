########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn  {} {} streamline (@var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {} streamline (@var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {} streamline (@dots{}, @var{options})
## @deftypefnx {} {} streamline (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} streamline (@dots{})
## Plot streamlines of 2-D or 3-D vector fields.
##
## Plot streamlines of a 2-D or 3-D vector field given by
## @code{[@var{u}, @var{v}]} or @code{[@var{u}, @var{v}, @var{w}]}.  The vector
## field is defined over a rectangular grid given by @code{[@var{x}, @var{y}]}
## or @code{[@var{x}, @var{y}, @var{z}]}.  The streamlines start at the seed
## points @code{[@var{sx}, @var{sy}]} or @code{[@var{sx}, @var{sy}, @var{sz}]}.
##
## The input parameter @var{options} is a 2-D vector of the form
## @code{[@var{stepsize}, @var{max_vertices}]}.  The first parameter
## specifies the step size used for trajectory integration (default 0.1).  A
## negative value is allowed which will reverse the direction of integration.
## The second parameter specifies the maximum number of segments used to
## create a streamline (default 10,000).
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the hggroup
## comprising the field lines.
##
## Example:
##
## @example
## @group
## [x, y] = meshgrid (-1.5:0.2:2, -1:0.2:2);
## u = - x / 4 - y;
## v = x - y / 4;
## streamline (x, y, u, v, 1.7, 1.5);
## @end group
## @end example
##
## @seealso{stream2, stream3, streamribbon, streamtube, ostreamtube}
## @end deftypefn

function h = streamline (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("streamline", varargin{:});

  if (nargin == 0)
    print_usage ();
  endif

  nd = ndims (varargin{1});
  if (nd > 3)
    error ("streamline: input data must be 2-D or 3-D");
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  h = [];
  if (nd == 2)
    xy = stream2 (varargin{:});
    for i = 1 : length (xy)
      sl = xy{i};
      if (! isempty (sl))
        htmp = line (hax, "xdata", sl(:, 1), "ydata", sl(:, 2), "color", "b");
        h = [h; htmp];
      endif
    endfor
  else
    xyz = stream3 (varargin{:});
    for i = 1 : length (xyz)
      sl = xyz{i};
      if (! isempty (sl))
        htmp = line (hax,
                     "xdata", sl(:, 1), "ydata", sl(:, 2), "zdata", sl(:, 3),
                     "color", "b");
        h = [h; htmp];
      endif
    endfor
  endif

endfunction


%!demo
%! clf;
%! [x, y] = meshgrid (-2:0.5:2);
%! u = - y - x / 2;
%! v = x - y / 2;
%! [sx, sy] = meshgrid (-2:2:2);
%! h = streamline (x, y, u, v, sx, sy);
%! set (h, "color", "r");
%! hold on;
%! quiver (x, y, u, v);
%! scatter (sx(:), sy(:), 20, "filled", "o", "markerfacecolor", "r");
%! title ("Spiral Sink");
%! grid on;
%! axis equal;

%!demo
%! clf;
%! [x, y, z] = meshgrid (-3:3);
%! u = - x / 2 - y;
%! v = x - y / 2;
%! w = - z;
%! [sx, sy, sz] = meshgrid (3, 0:1.5:1.5, 0:1.5:3);
%! h = streamline (x, y, z, u, v, w, sx, sy, sz);
%! set (h, "color", "r");
%! hold on;
%! quiver3 (x, y, z, u, v, w);
%! scatter3 (sx(:), sy(:), sz(:), 20, "filled", "o", "markerfacecolor", "r");
%! view (3);
%! title ("Spiral Sink");
%! grid on;
%! axis equal;

%!demo
%! clf;
%! [x, y, z] = meshgrid (-1:0.4:1, -1:0.4:1, -3:0.3:0);
%! a = 0.08;
%! b = 0.04;
%! u = - a * x - y;
%! v = x - a * y;
%! w = - b * ones (size (x));
%! hold on;
%! sx = 1.0;
%! sy = 0.0;
%! sz = 0.0;
%! plot3 (sx, sy, sz, ".r", "markersize", 15);
%! t = linspace (0, 12 * 2 * pi (), 500);
%! tx = exp (-a * t).*cos (t);
%! ty = exp (-a * t).*sin (t);
%! tz = - b * t;
%! plot3 (tx, ty, tz, "-b");
%! h = streamline (x, y, z, u, v, w, sx, sy, sz);
%! set (h, "color", "r");
%! view (3);
%! title ("Heuns Scheme (red) vs. Analytical Solution (blue)");
%! grid on;
%! axis equal tight;

## Test input validation
%!error <Invalid call> streamline ()
%!error <Invalid call to streamline>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   streamline (hax);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
%!error <input data must be 2-D or 3-D> streamline (ones (2,2,2,2))
