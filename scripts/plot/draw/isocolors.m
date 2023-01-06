########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{cdat} =} isocolors (@var{c}, @var{v})
## @deftypefnx {} {@var{cdat} =} isocolors (@var{x}, @var{y}, @var{z}, @var{c}, @var{v})
## @deftypefnx {} {@var{cdat} =} isocolors (@var{x}, @var{y}, @var{z}, @var{r}, @var{g}, @var{b}, @var{v})
## @deftypefnx {} {@var{cdat} =} isocolors (@var{r}, @var{g}, @var{b}, @var{v})
## @deftypefnx {} {@var{cdat} =} isocolors (@dots{}, @var{hp})
## @deftypefnx {} {} isocolors (@dots{}, @var{hp})
##
## Compute isosurface colors.
##
## If called with one output argument, and the first input argument @var{c}
## is a three-dimensional array that contains indexed color values, and the
## second input argument @var{v} are the vertices of an isosurface geometry,
## then return a matrix @var{cdat} with color data information for the geometry
## at computed points @code{[x, y, z] = meshgrid (1:l, 1:m, 1:n)}.  The output
## argument @var{cdat} can be used to manually set the
## @qcode{"FaceVertexCData"} property of an isosurface patch object.
##
## If called with additional input arguments @var{x}, @var{y} and @var{z} which
## are three-dimensional arrays of the same size as @var{c} then the
## color data is taken at those specified points.
##
## Instead of indexed color data @var{c}, @code{isocolors} can also be called
## with RGB values @var{r}, @var{g}, @var{b}.  If input arguments @var{x},
## @var{y}, @var{z} are not given then @code{meshgrid} computed values are
## used.
##
## Optionally, a patch handle @var{hp} can be given as the last input argument
## to all function call variations and the vertex data will be extracted
## from the isosurface patch object.  Finally, if no output argument is given
## then the colors of the patch given by the patch handle @var{hp} are changed.
##
## @seealso{isosurface, isonormals}
## @end deftypefn

function cdat = isocolors (varargin)

  calc_rgb = false;
  switch (nargin)
    case 2
      c = varargin{1};
      vp = varargin{2};
      x = 1:columns (c);
      y = 1:rows (c);
      z = 1:size (c, 3);
    case 4
      calc_rgb = true;
      R = varargin{1};
      G = varargin{2};
      B = varargin{3};
      vp = varargin{4};
      x = 1:rows (R);
      y = 1:columns (R);
      z = 1:size (R, 3);
    case 5
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      c = varargin{4};
      vp = varargin{5};
    case 7
      calc_rgb = true;
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      R = varargin{4};
      G = varargin{5};
      B = varargin{6};
      vp = varargin{7};
    otherwise
      print_usage ();
  endswitch

  if (isnumeric (vp) && columns (vp) == 3)
    hp = [];
    v = vp;
  elseif (isgraphics (vp, "patch"))
    hp = vp;
    v = get (hp, "Vertices");
  else
    error ("isocolors: last argument must be a vertex list or patch handle");
  endif

  if (calc_rgb)
    new_colors = zeros (rows (v), 3);
    new_colors(:,1) = __interp_cube__ ("isocolors", x, y, z, R, v, "values");
    new_colors(:,2) = __interp_cube__ ("isocolors", x, y, z, G, v, "values");
    new_colors(:,3) = __interp_cube__ ("isocolors", x, y, z, B, v, "values");
  else
    new_colors = __interp_cube__ ("isocolors", x, y, z, c, v, "values");
  endif

  if (nargout == 0)
    if (! isempty (hp))
      set (hp, "FaceVertexCData", new_colors);
    endif
  else
    cdat = new_colors;
  endif

endfunction


%!demo
%! function isofinish (p)
%!   set (gca, "PlotBoxAspectRatioMode", "manual", ...
%!             "PlotBoxAspectRatio", [1 1 1]);
%!   set (p, "FaceColor", "interp");
%!   set (p, "FaceLighting", "flat");
%!   light ("Position", [1 1 5]);
%! endfunction
%!
%! N = 15;    # Increase number of vertices in each direction
%! iso = .4;  # Change isovalue to .1 to display a sphere
%! lin = linspace (0, 2, N);
%! [x, y, z] = meshgrid (lin, lin, lin);
%! c = abs ((x-.5).^2 + (y-.5).^2 + (z-.5).^2);
%! clf;
%!
%! subplot (2,2,1);
%!  view (-38, 20);
%!  [f, v] = isosurface (x, y, z, c, iso);
%!  p = patch ("Faces", f, "Vertices", v, "EdgeColor", "none");
%!  cdat = rand (size (c));       # Compute random color data
%!  isocolors (x, y, z, cdat, p); # Directly set colors of patch
%!  title ("random colors for patch handle");
%!  isofinish (p);
%!
%! subplot (2,2,2);
%!  view (-38, 20);
%!  p = patch ("Faces", f, "Vertices", v, "EdgeColor", "none");
%!  [r, g, b] = meshgrid (lin, 2-lin, 2-lin);
%!  cdat = isocolors (x, y, z, c, v); # Compute color data vertices
%!  set (p, "FaceVertexCData", cdat); # Set color data manually
%!  title ('random colors for "FaceVertexCData"');
%!  isofinish (p);
%!
%! subplot (2,2,3);
%!  view (-38, 20);
%!  p = patch ("Faces", f, "Vertices", v, "EdgeColor", "none");
%!  cdat = isocolors (r, g, b, c, p); # Compute color data for patch
%!  set (p, "FaceVertexCData", cdat); # Set color data manually
%!  title ('random RGB colors for "FaceVertexCData"');
%!  isofinish (p);
%!
%! subplot (2,2,4);
%!  view (-38, 20);
%!  p = patch ("Faces", f, "Vertices", v, "EdgeColor", "none");
%!  r = g = b = repmat ([1:N] / N, [N, 1, N]); # Black to white
%!  cdat = isocolors (x, y, z, r, g, b, v);
%!  set (p, "FaceVertexCData", cdat);
%!  title ('gray shades for "FaceVertexCData"');
%!  isofinish (p);

%!test
%! [x, y, z] = meshgrid (0:.5:2, 0:.5:2, 0:.5:2);
%! c = (x-.5).^2 + (y-.5).^2 + (z-.5).^2;
%! [f, v] = isosurface (x, y, z, c, .4);
%! cdat = isocolors (x, y, z, c, v);
%! assert (rows (cdat) == rows (v));

## Test input validation
%!error <Invalid call> isocolors ()
%!error <Invalid call> isocolors (1)
%!error <Invalid call> isocolors (1,2,3)
%!error <Invalid call> isocolors (1,2,3,4,5,6)
%!error <Invalid call> isocolors (1,2,3,4,5,6,7,8)
%!error <last argument must be a vertex list> isocolors (1, {1})
%!error <last argument must be a vertex list> isocolors (1, [1 2 3 4])
%!error <last argument must be a .*patch handle> isocolors (1, 0)
