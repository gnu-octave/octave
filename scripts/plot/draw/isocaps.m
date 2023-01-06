########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{fvc} =} isocaps (@var{v}, @var{isoval})
## @deftypefnx {} {@var{fvc} =} isocaps (@var{v})
## @deftypefnx {} {@var{fvc} =} isocaps (@var{x}, @var{y}, @var{z}, @var{v}, @var{isoval})
## @deftypefnx {} {@var{fvc} =} isocaps (@var{x}, @var{y}, @var{z}, @var{v})
## @deftypefnx {} {@var{fvc} =} isocaps (@dots{}, @var{which_caps})
## @deftypefnx {} {@var{fvc} =} isocaps (@dots{}, @var{which_plane})
## @deftypefnx {} {@var{fvc} =} isocaps (@dots{}, @qcode{"verbose"})
## @deftypefnx {} {[@var{faces}, @var{vertices}, @var{fvcdata}] =} isocaps (@dots{})
## @deftypefnx {} {} isocaps (@dots{})
##
## Create end-caps for isosurfaces of 3-D data.
##
## This function places caps at the open ends of isosurfaces.
##
## The input argument @var{v} is a three-dimensional array that contains data
## sampled over a volume.
##
## The input @var{isoval} is a scalar that specifies the value for the
## isosurface.  If @var{isoval} is omitted or empty, a @nospell{"good"} value
## for an isosurface is determined from @var{v}.
##
## When called with a single output argument, @code{isocaps} returns a
## structure array @var{fvc} with the fields: @code{faces}, @code{vertices},
## and @code{facevertexcdata}.  The results are computed at the points
## @code{[@var{x}, @var{y}, @var{z}] = meshgrid (1:l, 1:m, 1:n)} where
## @code{[l, m, n] = size (@var{v})}.  The output @var{fvc} can be used
## directly as input to the @code{patch} function.
##
## If called with additional input arguments @var{x}, @var{y}, and @var{z}
## that are three-dimensional arrays with the same size as @var{v} or
## vectors with lengths corresponding to the dimensions of @var{v}, then the
## volume data is taken at the specified points.  If @var{x}, @var{y}, or
## @var{z} are empty, the grid corresponds to the indices (@code{1:n}) in
## the respective direction (@pxref{XREFmeshgrid,,@code{meshgrid}}).
##
## The optional parameter @var{which_caps} can have one of the following
## string values which defines how the data will be enclosed:
##
## @table @asis
## @item @qcode{"above"}, @qcode{"a"} (default)
## for end-caps that enclose the data above @var{isoval}.
##
## @item @qcode{"below"}, @qcode{"b"}
## for end-caps that enclose the data below @var{isoval}.
## @end table
##
## The optional parameter @var{which_plane} can have one of the following
## string values to define which end-cap should be drawn:
##
## @table @asis
## @item @qcode{"all"} (default)
## for all of the end-caps.
##
## @item @qcode{"xmin"}
## for end-caps at the lower x-plane of the data.
##
## @item @qcode{"xmax"}
## for end-caps at the upper x-plane of the data.
##
## @item @qcode{"ymin"}
## for end-caps at the lower y-plane of the data.
##
## @item @qcode{"ymax"}
## for end-caps at the upper y-plane of the data.
##
## @item @qcode{"zmin"}
## for end-caps at the lower z-plane of the data.
##
## @item @qcode{"zmax"}
## for end-caps at the upper z-plane of the data.
## @end table
##
## The string input argument @qcode{"verbose"} is supported for @sc{matlab}
## compatibility, but has no effect.
##
## If called with two or three output arguments, the data for faces
## @var{faces}, vertices @var{vertices}, and the color data
## @var{facevertexcdata} are returned in separate arrays instead of a single
## structure.
##
## If called with no output argument, the end-caps are drawn directly in the
## current figure with the @code{patch} command.
##
## @seealso{isosurface, isonormals, patch}
## @end deftypefn

function varargout = isocaps (varargin)

  if (nargin < 1 || nargin > 8)
    print_usage ();
  endif

  faces = vertices = fvcdata = [];

  [x, y, z, v, isoval, which_caps, which_plane, verbose] = ...
                                     __get_check_isocaps_args__ (varargin{:});

  ## select type of cap (above or below iso value)
  data_min = min ([v(:); isoval]);
  data_max = max ([v(:); isoval]);
  switch (tolower (which_caps))
    case {"a", "above"}
      pad_val = data_min - 1;

    case {"b", "below"}
      pad_val = data_max + 1;

    otherwise
      error ("isocaps: unknown WHICH_CAPS option '%s'", which_caps);

  endswitch

  ## create patches for caps
  if (strcmpi (which_plane, "all"))
    ## get patches for all planes
    [f_xmin, v_xmin] = __get_isocaps_patches__ (x, y, z, v, isoval, ...
                                                pad_val, "xmin", verbose);
    [f_xmax, v_xmax] = __get_isocaps_patches__ (x, y, z, v, isoval, ...
                                                pad_val, "xmax", verbose);
    [f_ymin, v_ymin] = __get_isocaps_patches__ (x, y, z, v, isoval, ...
                                                pad_val, "ymin", verbose);
    [f_ymax, v_ymax] = __get_isocaps_patches__ (x, y, z, v, isoval, ...
                                                pad_val, "ymax", verbose);
    [f_zmin, v_zmin] = __get_isocaps_patches__ (x, y, z, v, isoval, ...
                                                pad_val, "zmin", verbose);
    [f_zmax, v_zmax] = __get_isocaps_patches__ (x, y, z, v, isoval, ...
                                                pad_val, "zmax", verbose);
    vertices = [v_xmin; v_xmax; v_ymin; v_ymax; v_zmin; v_zmax];
    v_nums = [rows(v_xmin), rows(v_xmax), ...
              rows(v_ymin), rows(v_ymax), rows(v_zmin)];
    f_offset = cumsum (v_nums);
    faces = [f_xmin; f_xmax + f_offset(1); ...
             f_ymin + f_offset(2); f_ymax + f_offset(3); ...
             f_zmin + f_offset(4); f_zmax + f_offset(5)];
  else  # only one plane specified
    [faces, vertices] = __get_isocaps_patches__ (x, y, z, v, isoval,
                                                 pad_val, which_plane,
                                                 verbose);
  endif

  if (! isempty (vertices))
    ## interpolate data at the vertices for coloring of the end-cap
    fvcdata = interp3 (x, y, z, v,
                       vertices(:,1), vertices(:,2), vertices(:,3));
  endif

  switch (nargout)
    case 0
      hp = patch ("Faces", faces, "Vertices", vertices, ...
                  "FaceVertexCData", fvcdata, ...
                  "FaceColor", "interp", "EdgeColor", "none");

    case 1
      vfc.vertices = vertices;
      vfc.faces = faces;
      vfc.facevertexcdata = fvcdata;
      varargout = {vfc};

    otherwise
      varargout{1} = faces;
      varargout{2} = vertices;
      varargout{3} = fvcdata;

  endswitch

endfunction

## get arguments from input and check values
function [x, y, z, v, isoval, which_caps, which_plane, verbose] = ...
                                  __get_check_isocaps_args__ (varargin)

  x = y = z = [];
  v = [];
  isoval = [];

  ## default values
  which_caps = "above";
  which_plane = "all";
  verbose = "";

  ## check whether last 3 input arguments are strings and assign parameters
  num_string_inputs = 0;
  for (i_arg = nargin:-1:nargin-2)
    if (! ischar (varargin{i_arg}) || i_arg < 1)
      break;  # no string arguments at end, exit checking
    endif
    switch (lower (varargin{i_arg}))
      case {"v", "verbose"}
        verbose = "verbose";
        num_string_inputs++;

      case {"all", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"}
        which_plane = lower (varargin{i_arg});
        num_string_inputs++;

      case {"above", "a", "below", "b"}
        which_caps = lower (varargin{i_arg});
        num_string_inputs++;

      otherwise
        error ("isocaps: parameter '%s' not supported", varargin{i_arg});

    endswitch
  endfor

  ## assign arguments
  switch (nargin - num_string_inputs)
    case 1  # isocaps (v, ...)
      v = varargin{1};

    case 2  # isocaps (v, isoval, ...)
      v = varargin{1};
      isoval = varargin{2};

    case 4  # isocaps (x, y, z, v, ...)
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      v = varargin{4};

    case 5  # isocaps (x, y, z, v, isoval, ...)
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      v = varargin{4};
      isoval = varargin{5};

    otherwise
      error ("isocaps: incorrect number of input arguments");

  endswitch

  ## check dimensions of data
  v_sz = size (v);
  if (ndims (v) != 3 || any (v_sz(1:3) < 2))
    error ("isocaps: V must be a non-singleton 3-dimensional matrix");
  endif

  if (isempty (x))
    x = 1:v_sz(2);
  endif
  if (isempty (y))
    y = 1:v_sz(1);
  endif
  if (isempty (z))
    z = 1:v_sz(3);
  endif

  ## check x
  if (isvector (x) && length (x) == v_sz(2))
    x = repmat (x(:).', [v_sz(1) 1 v_sz(3)]);
  elseif (! size_equal (v, x))
    error ("isocaps: X must match the size of V");
  endif

  ## check y
  if (isvector (y) && length (y) == v_sz(1))
    y = repmat (y(:), [1 v_sz(2) v_sz(3)]);
  elseif (! size_equal (v, y))
    error ("isocaps: Y must match the size of V");
  endif

  ## check z
  if (isvector (z) && length (z) == v_sz(3))
    z = repmat (reshape (z(:), [1 1 length(z)]), ...
                [v_sz(1) v_sz(2) 1]);
  elseif (! size_equal (v, z))
    error ("isocaps: Z must match the size of V");
  endif

  ## check isoval
  if (isempty (isoval))
    ## calculate "good" isoval value from v
    isoval = __calc_isovalue_from_data__ (v);
  endif

  if (! isscalar (isoval) || ! isnumeric (isoval))
    error ("isocaps: ISOVAL must be a scalar number");
  endif

endfunction

## calculate patches for end-caps
function [faces, vertices] = __get_isocaps_patches__ (x, y, z, v, isoval,
                                                      pad_val, which_plane,
                                                      verbose)

  v_sz = size (v);
  is_lower_cap = strcmp (which_plane(2:end), "min");
  switch (which_plane(1))
    case "y"
      coor = 2;
      capdata = zeros (2, v_sz(2), v_sz(3));
      if (is_lower_cap)
        cap_idx = 1;
      else
        cap_idx = v_sz(1);
      endif
      coor_val = y(cap_idx,1,1);
      cap_data(2,:,:) = v(cap_idx,:,:);
      cap_data(1,:,:) = pad_val;

    case "x"
      coor = 1;
      cap_data = zeros (v_sz(1), 2, v_sz(3));
      if (is_lower_cap)
        cap_idx = 1;
      else
        cap_idx = v_sz(2);
      endif
      coor_val = x(1,cap_idx,1);
      cap_data(:,2,:) = v(:,cap_idx,:);
      cap_data(:,1,:) = pad_val;

    case "z"
      coor = 3;
      cap_data = zeros (v_sz(1), v_sz(2), 2);
      if (is_lower_cap)
        cap_idx = 1;
      else
        cap_idx = v_sz(3);
      endif
      coor_val = z(1,1,cap_idx);
      cap_data(:,:,2) = v(:,:,cap_idx);
      cap_data(:,:,1) = pad_val;

    otherwise
      error ("isocaps: invalid plane '%s'", which_plane);

  endswitch

  n_cap = size (cap_data);
  x_iso = x(1:n_cap(1),1:n_cap(2),1:n_cap(3));
  y_iso = y(1:n_cap(1),1:n_cap(2),1:n_cap(3));
  z_iso = z(1:n_cap(1),1:n_cap(2),1:n_cap(3));

  [faces, vertices] = isosurface (x_iso, y_iso, z_iso, cap_data, isoval,
                                  verbose);

  if (! isempty (vertices))
    vertices(:,coor) = coor_val;
  endif

endfunction


%!demo
%! isoval = .4;
%! lin = linspace (0, 1.2, 15);
%! [x, y, z] = meshgrid (lin, lin, lin);
%! v = abs ((x-0.45).^2 + (y-0.55).^2 + (z-0.8).^2);
%! hf = clf;
%! ha = axes ();
%! view (3);  box off;
%! fvc_iso = isosurface (x, y, z, v, isoval);
%! cmap = get (hf, "Colormap");
%! p_iso = patch (fvc_iso, "FaceLighting", "gouraud", ...
%!                "FaceColor", cmap(end,:), "EdgeColor", "none");
%! isonormals (x, y, z, v, p_iso);
%! fvc_xmin = isocaps (x, y, z, v, isoval, "xmin", "b");
%! patch (fvc_xmin, "FaceColor", "interp", "EdgeColor", "none", ...
%!        "FaceLighting", "gouraud");
%! fvc_ymin = isocaps (x, y, z, v, isoval, "ymin", "b");
%! patch (fvc_ymin, "FaceColor", "interp", "EdgeColor", "none", ...
%!        "FaceLighting", "gouraud");
%! fvc_zmax = isocaps (x, y, z, v, isoval, "zmax", "b");
%! patch (fvc_zmax, "FaceColor", "interp", "EdgeColor", "none", ...
%!        "FaceLighting", "gouraud");
%! axis equal;
%! light ();
%! title ({"isocaps()", "sphere with 3 end-caps"});

%!demo
%! v = smooth3 (rand (6, 8, 4));
%! isoval = .5;
%! x = 1:3:22;  y = -14:5:11;  z = linspace (16, 18, 4);
%! [xx, yy, zz] = meshgrid (x, y, z);
%! clf;
%! ## two arguments, no output
%! subplot (2, 2, 1);
%!  isocaps (v, isoval);
%!  view (3);
%! ## five arguments, no output (x, y, z are vectors)
%! subplot (2, 2, 2);
%!  isocaps (x, y, z, v, isoval);
%!  view (3);
%! ## five arguments, no output (x, y, z are matrices)
%! subplot (2, 2, 3);
%!  isocaps (xx, yy, zz, v, isoval);
%!  view (3);
%! ## five arguments, no output (mixed x, y, z)
%! subplot (2, 2, 4);
%!  isocaps (x, yy, z, v, isoval);
%!  view (3);
%!
%! annotation ("textbox", [0.41 0.9 0.9 0.1], ...
%!     "String", "isocaps() called 4 ways", ...
%!     "HorizontalAlignment", "center", ...
%!     "FontSize", 12);
%! annotation ("textbox", [0.1 0.47 0.9 0.1], ...
%!     "String", ["Apart from the first plot having a different scale, " ...
%!     "all four plots must look the same."], ...
%!     "HorizontalAlignment", "left", ...
%!     "FontSize", 12);

%!shared x, y, z, xx, yy, zz, val, iso
%! x = 1:3:22;  y = -14:5:11;  z = linspace (16, 18, 4);
%! [xx, yy, zz] = meshgrid (x, y, z);
%! val = rand (6, 8, 4);
%! iso = .5;

## check results for differently shaped input coordinates
%!test
%! fvc_vectors = isocaps (x, y, z, val, iso);
%! fvc_matrices = isocaps (xx, yy, zz, val, iso);
%! fvc_mixed = isocaps (xx, y, zz, val, iso);
%! assert (fvc_vectors, fvc_matrices);
%! assert (fvc_vectors, fvc_mixed);

## two arguments, one output
%!test
%! fvc = isocaps (val, iso);
%! assert (isfield (fvc, "vertices"));
%! assert (isfield (fvc, "faces"));
%! assert (isfield (fvc, "facevertexcdata"));

## one argument (undocumented Matlab)
%!test
%! fvc = isocaps (val);
%! fvc2 = isocaps (val, []);
%! assert (fvc, fvc2);
%! assert (isfield (fvc, "vertices"));
%! assert (isfield (fvc, "faces"));
%! assert (isfield (fvc, "facevertexcdata"));

## four arguments (undocumented Matlab)
%!test
%! fvc = isocaps (x, [], z, val);
%! assert (isfield (fvc, "vertices"));
%! assert (isfield (fvc, "faces"));
%! assert (isfield (fvc, "facevertexcdata"));

## five arguments, two outputs
%!test
%! [faces, vertices] = isocaps ([], y, z, val, iso);
%! assert (columns (faces), 3);
%! assert (columns (vertices), 3);

## five arguments, three outputs
%!test
%! [faces, vertices, fvcdata] = isocaps (x, y, [], val, iso);
%! assert (columns (faces), 3);
%! assert (columns (vertices), 3);
%! assert (columns (fvcdata), 1);
%! assert (rows (vertices), rows (fvcdata));

## two arguments + one string, one output
%!test
%! fvc = isocaps (val, iso, "below");
%! assert (isfield (fvc, "vertices"));
%! assert (isfield (fvc, "faces"));
%! assert (isfield (fvc, "facevertexcdata"));

## two arguments + two strings, one output
%!test
%! fvc = isocaps (val, iso, "b", "ymax");
%! assert (isfield (fvc, "vertices"));
%! assert (isfield (fvc, "faces"));
%! assert (isfield (fvc, "facevertexcdata"));

## two arguments + three strings, one output
%!test
%! fvc = isocaps (val, iso, "a", "ymin", "verbose");
%! assert (isfield (fvc, "vertices"));
%! assert (isfield (fvc, "faces"));
%! assert (isfield (fvc, "facevertexcdata"));

## five arguments + one string, three outputs
%!test
%! [faces, vertices, fvcdata] = isocaps (x, y, z, val, iso, "xmin");
%! assert (columns (faces), 3);
%! assert (columns (vertices), 3);
%! assert (columns (fvcdata), 1);
%! assert (rows (vertices), rows (fvcdata));

## five arguments + one string, three outputs
%!test
%! [faces, vertices, fvcdata] = isocaps (x, y, z, val, iso, "verbose");
%! assert (columns (faces), 3);
%! assert (columns (vertices), 3);
%! assert (columns (fvcdata), 1);
%! assert (rows (vertices), rows (fvcdata));

## five arguments + two strings, three outputs
%!test
%! [faces, vertices, fvcdata] = isocaps (x, y, z, val, iso, "xmax", "above");
%! assert (columns (faces), 3);
%! assert (columns (vertices), 3);
%! assert (columns (fvcdata), 1);
%! assert (rows (vertices), rows (fvcdata));

## five arguments + three strings, three outputs
%!test
%! [faces, vertices, fvcdata] = isocaps (x, y, z, val, iso,
%!                                       "zmin", "b", "verbose");
%! assert (columns (faces), 3);
%! assert (columns (vertices), 3);
%! assert (columns (fvcdata), 1);
%! assert (rows (vertices), rows (fvcdata));

## five arguments + three strings (different order), three outputs
%!test
%! [faces, vertices, fvcdata] = isocaps (x, y, z, val, iso,
%!                                       "below", "v", "zmax");
%! assert (columns (faces), 3);
%! assert (columns (vertices), 3);
%! assert (columns (fvcdata), 1);
%! assert (rows (vertices), rows (fvcdata));

## test for each error
%!error <Invalid call> isocaps ()
%!error <Invalid call> isocaps (1,2,3,4,5,6,7,8,9)
%!error <parameter 'foo' not supported> isocaps (val, iso, "foo")
%!error <incorrect number of input arguments> isocaps (x, val, iso)
%!error <incorrect number of input arguments> isocaps (xx, yy, zz, val, iso, 5)
%!error <V must be a non-singleton 3-dimensional matrix>
%! v2 = reshape (1:6*8, [6 8]);
%! fvc = isocaps (v2, iso);
%!error <V must be a non-singleton 3-dimensional matrix>
%! v3 = reshape (1:6*8, [6 1 8]);
%! fvc = isocaps (v3, iso);
%!error <X must match the size of V>
%! x = 1:2:24;
%! fvc = isocaps (x, y, z, val, iso);
%!error <Y must match the size of V>
%! y = -14:6:11;
%! fvc = isocaps (x, y, z, val, iso);
%!error <Z must match the size of V>
%! z = linspace (16, 18, 5);
%! fvc = isocaps (x, y, z, val, iso);
%!error <X must match the size of V>
%! x = 1:2:24;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! fvc = isocaps (xx, yy, zz, val, iso);
%!error <X must match the size of V>
%! y = -14:6:11;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! fvc = isocaps (xx, yy, zz, val, iso);
%!error <X must match the size of V>
%! z = linspace (16, 18, 3);
%! [xx, yy, zz] = meshgrid (x, y, z);
%! fvc = isocaps (xx, yy, zz, val, iso);
%!error <ISOVAL must be a scalar> isocaps (val, [iso iso])
%!error <ISOVAL must be a scalar> isocaps (val, {iso})
