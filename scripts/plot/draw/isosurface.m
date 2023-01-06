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
## @deftypefn  {} {@var{fv} =} isosurface (@var{v}, @var{isoval})
## @deftypefnx {} {@var{fv} =} isosurface (@var{v})
## @deftypefnx {} {@var{fv} =} isosurface (@var{x}, @var{y}, @var{z}, @var{v}, @var{isoval})
## @deftypefnx {} {@var{fv} =} isosurface (@var{x}, @var{y}, @var{z}, @var{v})
## @deftypefnx {} {@var{fvc} =} isosurface (@dots{}, @var{col})
## @deftypefnx {} {@var{fv} =} isosurface (@dots{}, "noshare")
## @deftypefnx {} {@var{fv} =} isosurface (@dots{}, "verbose")
## @deftypefnx {} {[@var{f}, @var{v}] =} isosurface (@dots{})
## @deftypefnx {} {[@var{f}, @var{v}, @var{c}] =} isosurface (@dots{})
## @deftypefnx {} {} isosurface (@dots{})
##
## Calculate isosurface of 3-D volume data.
##
## An isosurface connects points with the same value and is analogous to a
## contour plot, but in three dimensions.
##
## The input argument @var{v} is a three-dimensional array that contains data
## sampled over a volume.
##
## The input @var{isoval} is a scalar that specifies the value for the
## isosurface.  If @var{isoval} is omitted or empty, a @nospell{"good"} value
## for an isosurface is determined from @var{v}.
##
## When called with a single output argument @code{isosurface} returns a
## structure array @var{fv} that contains the fields @var{faces} and
## @var{vertices} computed at the points
## @code{[@var{x}, @var{y}, @var{z}] = meshgrid (1:l, 1:m, 1:n)} where
## @code{[l, m, n] = size (@var{v})}.  The output @var{fv} can be
## used directly as input to the @code{patch} function.
##
## If called with additional input arguments @var{x}, @var{y}, and @var{z}
## that are three-dimensional arrays with the same size as @var{v} or
## vectors with lengths corresponding to the dimensions of @var{v}, then the
## volume data is taken at the specified points.  If @var{x}, @var{y}, or
## @var{z} are empty, the grid corresponds to the indices (@code{1:n}) in
## the respective direction (@pxref{XREFmeshgrid,,@code{meshgrid}}).
##
## The optional input argument @var{col}, which is a three-dimensional array
## of the same size as @var{v}, specifies coloring of the isosurface.  The
## color data is interpolated, as necessary, to match @var{isoval}.  The
## output structure array, in this case, has the additional field
## @var{facevertexcdata}.
##
## If given the string input argument @qcode{"noshare"}, vertices may be
## returned multiple times for different faces.  The default behavior is to
## eliminate vertices shared by adjacent faces.
##
## The string input argument @qcode{"verbose"} is supported for @sc{matlab}
## compatibility, but has no effect.
##
## Any string arguments must be passed after the other arguments.
##
## If called with two or three output arguments, return the information about
## the faces @var{f}, vertices @var{v}, and color data @var{c} as separate
## arrays instead of a single structure array.
##
## If called with no output argument, the isosurface geometry is directly
## plotted with the @code{patch} command and a light object is added to
## the axes if not yet present.
##
## For example,
##
## @example
## @group
## [x, y, z] = meshgrid (1:5, 1:5, 1:5);
## v = rand (5, 5, 5);
## isosurface (x, y, z, v, .5);
## @end group
## @end example
##
## @noindent
## will directly draw a random isosurface geometry in a graphics window.
##
## An example of an isosurface geometry with different additional coloring:
## @c Set example in small font to prevent overfull line
##
## @smallexample
## N = 15;    # Increase number of vertices in each direction
## iso = .4;  # Change isovalue to .1 to display a sphere
## lin = linspace (0, 2, N);
## [x, y, z] = meshgrid (lin, lin, lin);
## v = abs ((x-.5).^2 + (y-.5).^2 + (z-.5).^2);
## figure ();
##
## subplot (2,2,1); view (-38, 20);
## [f, vert] = isosurface (x, y, z, v, iso);
## p = patch ("Faces", f, "Vertices", vert, "EdgeColor", "none");
## pbaspect ([1 1 1]);
## isonormals (x, y, z, v, p)
## set (p, "FaceColor", "green", "FaceLighting", "gouraud");
## light ("Position", [1 1 5]);
##
## subplot (2,2,2); view (-38, 20);
## p = patch ("Faces", f, "Vertices", vert, "EdgeColor", "blue");
## pbaspect ([1 1 1]);
## isonormals (x, y, z, v, p)
## set (p, "FaceColor", "none", "EdgeLighting", "gouraud");
## light ("Position", [1 1 5]);
##
## subplot (2,2,3); view (-38, 20);
## [f, vert, c] = isosurface (x, y, z, v, iso, y);
## p = patch ("Faces", f, "Vertices", vert, "FaceVertexCData", c, ...
##            "FaceColor", "interp", "EdgeColor", "none");
## pbaspect ([1 1 1]);
## isonormals (x, y, z, v, p)
## set (p, "FaceLighting", "gouraud");
## light ("Position", [1 1 5]);
##
## subplot (2,2,4); view (-38, 20);
## p = patch ("Faces", f, "Vertices", vert, "FaceVertexCData", c, ...
##            "FaceColor", "interp", "EdgeColor", "blue");
## pbaspect ([1 1 1]);
## isonormals (x, y, z, v, p)
## set (p, "FaceLighting", "gouraud");
## light ("Position", [1 1 5]);
## @end smallexample
##
## @seealso{isonormals, isocolors, isocaps, smooth3, reducevolume, reducepatch,
## patch}
## @end deftypefn

## FIXME: Add support for string input argument "verbose"
##        (needs changes to __marching_cube__.m)

function varargout = isosurface (varargin)

  if (nargin < 1 || nargin > 8)
    print_usage ();
  endif

  [x, y, z, v, isoval, colors, noshare, verbose] = ...
      __get_check_isosurface_args__ (nargout, varargin{:});

  calc_colors = ! isempty (colors);
  if (calc_colors)
    [fvc.faces, fvc.vertices, fvc.facevertexcdata] = ...
        __marching_cube__ (x, y, z, v, isoval, colors);
  else
    [fvc.faces, fvc.vertices] = __marching_cube__ (x, y, z, v, isoval);
  endif

  if (isempty (fvc.vertices) || isempty (fvc.faces))
    warning ("isosurface: triangulation is empty");
  endif

  ## remove faces for which at least one of the vertices is NaN
  vert_nan = 1:size (fvc.vertices, 1);
  vert_nan(any (isnan (fvc.vertices), 2)) = NaN;
  fvc.faces = vert_nan(fvc.faces);
  fvc.faces(any (isnan (fvc.faces), 2), :) = [];

  if (! noshare)
    [fvc.faces, fvc.vertices, J] = __unite_shared_vertices__ (fvc.faces,
                                                              fvc.vertices);

    if (calc_colors)
      fvc.facevertexcdata = fvc.facevertexcdata(J);  # share very close vertices
    endif
  endif

  switch (nargout)
    case 0
      ## plot the calculated surface
      if (calc_colors)
        fc = fvc.facevertexcdata;
      else
        fc = isoval;
      endif
      ## Matlab uses "EdgeColor", "none", but that looks odd in gnuplot.
      hax = gca ();
      if (strcmp (get (gcf, "__graphics_toolkit__"), "gnuplot"))
        ec = "k";
      else
        ec = "none";
      endif
      pa = patch ("Faces", fvc.faces, "Vertices", fvc.vertices,
                  "FaceVertexCData", fc,
                  "FaceColor", "flat", "EdgeColor", ec,
                  "FaceLighting", "gouraud");
      if (! ishold ())
        set (hax, "View", [-37.5, 30]);
      endif
      isonormals (x, y, z, v, pa);
      lights = findobj (hax, "Type", "light");
      if (isempty (lights))
        camlight ();
      endif

    case 1
      varargout = {fvc};

    case 2
      varargout = {fvc.faces, fvc.vertices};

    otherwise  # 3 args or more
      varargout = {fvc.faces, fvc.vertices, fvc.facevertexcdata};

  endswitch

endfunction

function [x, y, z, v, isoval, colors, noshare, verbose] = __get_check_isosurface_args__ (nout, varargin)

  ## get arguments from input and check values
  x = y = z = [];
  v = [];
  isoval = [];
  colors = [];

  ## default values
  noshare = false;
  verbose = false;

  nin = length (varargin);
  num_string_inputs = 0;

  ## check whether last 2 input arguments are strings and assign parameters
  for i_arg = (nin:-1:nin-1)
    if (! ischar (varargin{i_arg}) || i_arg < 1)
      break;  # no string arguments at end, exit checking
    endif
    switch (tolower (varargin{i_arg}))
      case {"v", "verbose"}
        verbose = true;
        num_string_inputs++;

      case {"n", "noshare"}
        noshare = true;
        num_string_inputs++;

      case ""
        num_string_inputs++;
        ## silently ignore empty strings

      otherwise
        error ("isosurface: parameter '%s' not supported", varargin{i_arg});

    endswitch
  endfor

  ## assign arguments
  switch (nin - num_string_inputs)
    case 1  # isosurface (v, ...)
      v = varargin{1};

    case 2  # isosurface (v, isoval, ...) or isosurface (v, col, ...)
      v = varargin{1};
      if (isscalar (varargin{2}) || isempty (varargin{2}))
        isoval = varargin{2};
      else
        colors = varargin{2};
      endif

    case 3  # isosurface (v, isoval, col, ...)
      v = varargin{1};
      isoval = varargin{2};
      colors = varargin{3};

    case 4  # isosurface (x, y, z, v, ...)
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      v = varargin{4};

    case 5  # isosurface (x, y, z, v, isoval, ...) or
            # isosurface (x, y, z, v, col, ...)
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      v = varargin{4};
      if (isscalar (varargin{5}) || isempty (varargin{5}))
        isoval = varargin{5};
      else
        colors = varargin{5};
      endif

    case 6  # isosurface (x, y, z, v, isoval, col, ...)
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      v = varargin{4};
      isoval = varargin{5};
      colors = varargin{6};

    otherwise
      error ("isosurface: incorrect number of input arguments");

  endswitch

  ## check dimensions of v
  v_sz = size (v);
  if (ndims (v) != 3 || any (v_sz(1:3) < 2))
    error ("isosurface: V must be a non-singleton 3-dimensional matrix");
  endif

  if (isempty (x))
    x = 1:columns (v);
  endif
  if (isempty (y))
    y = 1:rows (v);
  endif
  if (isempty (z))
    z = 1:size (v, 3);
  endif

  ## check x
  if (isvector (x) && length (x) == v_sz(2))
    x = repmat (x(:)', [v_sz(1) 1 v_sz(3)]);
  elseif (! size_equal (v, x))
    error ("isosurface: X must match the size of V");
  endif

  ## check y
  if (isvector (y) && length (y) == v_sz(1))
    y = repmat (y(:), [1 v_sz(2) v_sz(3)]);
  elseif (! size_equal (v, y))
    error ("isosurface: Y must match the size of V");
  endif

  ## check z
  if (isvector (z) && length (z) == v_sz(3))
    z = repmat (reshape (z(:), [1 1 length(z)]), [v_sz(1) v_sz(2) 1]);
  elseif (! size_equal (v, z))
    error ("isosurface: Z must match the size of V");
  endif

  ## check isoval
  if (isempty (isoval))
    ## calculate "good" isoval value from v
    isoval = __calc_isovalue_from_data__ (v);
  endif

  if (! isscalar (isoval))
    error ("isosurface: ISOVAL must be a scalar");
  endif

  ## check colors
  if (! isempty (colors))
    if (! size_equal (v, colors))
      error ("isosurface: COL must match the size of V");
    endif
    if (nout == 2)
      warning ("isosurface: colors will be calculated, but no output argument to receive it");
    endif
  elseif (nout >= 3)
    error ("isosurface: COL must be passed to return C");
  endif

endfunction


%!demo
%! clf;
%! [x,y,z] = meshgrid (-2:0.5:2, -2:0.5:2, -2:0.5:2);
%! v = x.^2 + y.^2 + z.^2;
%! isosurface (x, y, z, v, 1);
%! axis equal;
%! title ("isosurface() of a sphere");

%!demo
%! clf;
%! [x,y,z] = meshgrid (-2:0.5:2, -2:0.5:2, -2:0.5:2);
%! v = x.^2 + y.^2 + z.^2;
%! isosurface (x, y, z, v, 3);
%! isosurface (x, y, z, v, 5);
%! axis equal;
%! title ("isosurface() of two nested spheres");

%!demo
%! clf;
%! x = 0:2;
%! y = 0:3;
%! z = 0:1;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! v        = [0, 0, 0; 0, 0, 0; 0, 0, 1; 0, 0, 1];
%! v(:,:,2) = [0, 0, 0; 0, 0, 1; 0, 1, 2; 0, 1, 2];
%! iso = 0.8;
%!
%! ## Three arguments, no output
%! subplot (2, 2, 1);
%!  fvc = isosurface (v, iso, yy);
%!  patch (fvc);
%!  shading faceted;
%!  view (110, 40);
%! ## six arguments, no output (x, y, z are vectors)
%! subplot (2, 2, 2);
%!  fvc = isosurface (x, y, z, v, iso, yy);
%!  patch (fvc);
%!  shading faceted;
%!  view (110, 40);
%! ## six arguments, no output (x, y, z are matrices)
%! subplot (2, 2, 3);
%!  fvc = isosurface (xx, yy, zz, v, iso, yy);
%!  patch (fvc);
%!  shading faceted;
%!  view (110, 40);
%! ## six arguments, no output (mixed x, y, z) and option "noshare"
%! subplot (2, 2, 4);
%!  fvc = isosurface (x, yy, z, v, iso, yy, "noshare");
%!  patch (fvc);
%!  shading faceted;
%!  view (110, 40);
%!  annotation ("textbox", [0.41 0.9 0.9 0.1], ...
%!      "String", "isosurface() called 4 ways", ...
%!      "HorizontalAlignment", "center", ...
%!      "FontSize", 12);
%!  annotation ("textbox", [0.1 0.45 0.9 0.1], ...
%!      "String", {["Apart from the first plot having a different scale, " ...
%!                  "all four plots must look the same."],
%!                 ["The last plot might have different colors but must " ...
%!                  "have the same shape."]}, ...
%!      "HorizontalAlignment", "left", ...
%!      "FontSize", 12);

%!shared x, y, z, xx, yy, zz, val, iso
%! x = 0:2;
%! y = 0:3;
%! z = 0:1;
%! [xx, yy, zz]  = meshgrid (x, y, z);
%! val        = [0, 0, 0; 0, 0, 0; 0, 0, 1; 0, 0, 1];
%! val(:,:,2) = [0, 0, 0; 0, 0, 1; 0, 1, 2; 0, 1, 2];
%! iso = 0.8;

## one argument, one output
%!test
%! fv = isosurface (val);
%! assert (isfield (fv, "vertices"), true);
%! assert (isfield (fv, "faces"), true);
%! assert (size (fv.vertices), [5 3]);
%! assert (size (fv.faces), [3 3]);

## two arguments (second is ISO), one output
%!test
%! fv = isosurface (val, iso);
%! assert (isfield (fv, "vertices"), true);
%! assert (isfield (fv, "faces"), true);
%! assert (size (fv.vertices), [11 3]);
%! assert (size (fv.faces), [10 3]);

## two arguments (second is COL), one output
%!test
%! fvc = isosurface (val, yy);
%! assert (isfield (fvc, "vertices"), true);
%! assert (isfield (fvc, "faces"), true);
%! assert (isfield (fvc, "facevertexcdata"), true);
%! assert (size (fvc.vertices), [5 3]);
%! assert (size (fvc.faces), [3 3]);
%! assert (size (fvc.facevertexcdata), [5 1]);

## three arguments, one output
%!test
%! fvc = isosurface (val, iso, yy);
%! assert (isfield (fvc, "vertices"), true);
%! assert (isfield (fvc, "faces"), true);
%! assert (isfield (fvc, "facevertexcdata"), true);
%! assert (size (fvc.vertices), [11 3]);
%! assert (size (fvc.faces), [10 3]);
%! assert (size (fvc.facevertexcdata), [11 1]);

## four arguments, one output
%!test
%! fv = isosurface (x, [], z, val);
%! assert (isfield (fv, "vertices"), true);
%! assert (isfield (fv, "faces"), true);
%! assert (size (fv.vertices), [5 3]);
%! assert (size (fv.faces), [3 3]);

## five arguments (fifth is ISO), one output
%!test
%! fv = isosurface (xx, y, [], val, iso);
%! assert (isfield (fv, "vertices"), true);
%! assert (isfield (fv, "faces"), true);
%! assert (size (fv.vertices), [11 3]);
%! assert (size (fv.faces), [10 3]);

## five arguments (fifth is COL), one output
%!test
%! fvc = isosurface ([], yy, z, val, yy);
%! assert (isfield (fvc, "vertices"), true);
%! assert (isfield (fvc, "faces"), true);
%! assert (isfield (fvc, "facevertexcdata"), true);
%! assert (size (fvc.vertices), [5 3]);
%! assert (size (fvc.faces), [3 3]);
%! assert (size (fvc.facevertexcdata), [5 1]);

## six arguments, one output
%!test
%! fvc = isosurface (xx, yy, zz, val, iso, yy);
%! assert (isfield (fvc, "vertices"), true);
%! assert (isfield (fvc, "faces"), true);
%! assert (isfield (fvc, "facevertexcdata"), true);
%! assert (size (fvc.vertices), [11 3]);
%! assert (size (fvc.faces), [10 3]);
%! assert (size (fvc.facevertexcdata), [11 1]);

## five arguments (fifth is ISO), two outputs
%!test
%! [f, v] = isosurface (x, y, z, val, iso);
%! assert (size (f), [10 3]);
%! assert (size (v), [11 3]);

## six arguments, three outputs
%!test
%! [f, v, c] = isosurface (x, y, z, val, iso, yy);
%! assert (size (f), [10 3]);
%! assert (size (v), [11 3]);
%! assert (size (c), [11 1]);

## two arguments (second is ISO) and one string, one output
%!test
%! fv = isosurface (val, iso, "verbose");
%! assert (isfield (fv, "vertices"), true);
%! assert (isfield (fv, "faces"), true);
%! assert (size (fv.vertices), [11 3]);
%! assert (size (fv.faces), [10 3]);

## six arguments and two strings, one output
%!test
%! fvc = isosurface (xx, yy, zz, val, iso, yy, "v", "noshare");
%! assert (isfield (fvc, "vertices"), true);
%! assert (isfield (fvc, "faces"), true);
%! assert (isfield (fvc, "facevertexcdata"), true);
%! assert (size (fvc.vertices), [20 3]);
%! assert (size (fvc.faces), [10 3]);
%! assert (size (fvc.facevertexcdata), [20 1]);

## five arguments (fifth is COL) and two strings (different order), one output
%!test
%! fvc = isosurface (xx, yy, zz, val, yy, "n", "v");
%! assert (isfield (fvc, "vertices"), true);
%! assert (isfield (fvc, "faces"), true);
%! assert (isfield (fvc, "facevertexcdata"), true);
%! assert (size (fvc.vertices), [7 3]);
%! assert (size (fvc.faces), [3 3]);
%! assert (size (fvc.facevertexcdata), [7 1]);

## test for each error and warning
%!error <Invalid call> isosurface ()
%!error <Invalid call> isosurface (1,2,3,4,5,6,7,8,9)
%!error <parameter 'foobar' not supported>
%! fvc = isosurface (val, iso, "foobar");
%!error <incorrect number of input arguments>
%! fvc = isosurface (xx, yy, zz, val, iso, yy, 5);
%!error <V must be a non-singleton 3-dimensional matrix>
%! v = reshape (1:6*8, [6 8]);
%! fvc = isosurface (v, iso);
%!error <V must be a non-singleton 3-dimensional matrix>
%! v = reshape(1:6*8, [6 1 8]); fvc = isosurface (v, iso);
%!error <X must match the size of V>
%! x = 1:2:24;
%! fvc = isosurface (x, y, z, val, iso);
%!error <Y must match the size of V>
%! y = -14:6:11;
%! fvc = isosurface (x, y, z, val, iso);
%!error <Z must match the size of V>
%! z = linspace (16, 18, 5);
%! fvc = isosurface (x, y, z, val, iso);
%!error <X must match the size of V>
%! x = 1:2:24;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! fvc = isosurface (xx, yy, zz, val, iso);
%!error <X must match the size of V>
%! y = -14:6:11;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! fvc = isosurface (xx, yy, zz, val, iso);
%!error <X must match the size of V>
%! z = linspace (16, 18, 3);
%! [xx, yy, zz] = meshgrid (x, y, z);
%! fvc = isosurface (xx, yy, zz, val, iso);
%!error <ISOVAL must be a scalar> fvc = isosurface (val, [iso iso], yy)
%!error <COL must match the size of V> fvc = isosurface (val, [iso iso])
%!error <COL must be passed to return C> [f, v, c] = isosurface (val, iso)
%!warning <colors will be calculated, but no output argument to receive it>
%! [f, v] = isosurface (val, iso, yy);

## test for __calc_isovalue_from_data__
## FIXME: private function cannot be tested, unless bug #38776 is resolved.
%!test <38776>
%! assert (__calc_isovalue_from_data__ (1:5), 3.02);
