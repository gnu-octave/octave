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
## @deftypefn  {} {@var{vn} =} isonormals (@var{val}, @var{vert})
## @deftypefnx {} {@var{vn} =} isonormals (@var{val}, @var{hp})
## @deftypefnx {} {@var{vn} =} isonormals (@var{x}, @var{y}, @var{z}, @var{val}, @var{vert})
## @deftypefnx {} {@var{vn} =} isonormals (@var{x}, @var{y}, @var{z}, @var{val}, @var{hp})
## @deftypefnx {} {@var{vn} =} isonormals (@dots{}, "negate")
## @deftypefnx {} {} isonormals (@var{val}, @var{hp})
## @deftypefnx {} {} isonormals (@var{x}, @var{y}, @var{z}, @var{val}, @var{hp})
## @deftypefnx {} {} isonormals (@dots{}, "negate")
##
## Calculate normals to an isosurface.
##
## The vertex normals @var{vn} are calculated from the gradient of the
## 3-dimensional array @var{val} (size: lxmxn) containing the data for an
## isosurface geometry.  The normals point towards smaller values in @var{val}.
##
## If called with one output argument @var{vn}, and the second input argument
## @var{vert} holds the vertices of an isosurface, then the normals @var{vn}
## are calculated at the vertices @var{vert} on a grid given by
## @code{[x, y, z] = meshgrid (1:l, 1:m, 1:n)}.  The output argument
## @var{vn} has the same size as @var{vert} and can be used to set the
## @qcode{"VertexNormals"} property of the corresponding patch.
##
## If called with additional input arguments @var{x}, @var{y}, and @var{z},
## which are 3-dimensional arrays with the same size as @var{val},
## then the volume data is taken at these points.  Instead of the vertex data
## @var{vert}, a patch handle @var{hp} can be passed to the function.
##
## If the last input argument is the string @qcode{"negate"}, compute the
## reverse vector normals of an isosurface geometry (i.e., pointed towards
## larger values in @var{val}).
##
## If no output argument is given, the property @qcode{"VertexNormals"} of
## the patch associated with the patch handle @var{hp} is changed directly.
##
## @seealso{isosurface, isocolors, smooth3}
## @end deftypefn

function vn = isonormals (varargin)

  narg = nargin;
  negate = false;
  if (nargin > 2)
    if (ischar (varargin{end}))
      if (strcmpi (varargin{end}, "negate"))
        negate = true;
        narg -= 1;
      else
        error ("isonormals: Unknown option '%s'", varargin{end});
      endif
    endif
  endif

  switch (narg)
    case 2
      val = varargin{1};
      vp = varargin{2};
      x = 1:columns (val);
      y = 1:rows (val);
      z = 1:size (val, 3);

    case 5
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      val = varargin{4};
      vp = varargin{5};

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
    error ("isonormals: input must be a list of vertices or a patch handle");
  endif

  if (negate)
    normals = __interp_cube__ ("isonormals", x, y, z, val, v, "normals");
  else
    normals = -__interp_cube__ ("isonormals", x, y, z, val, v, "normals");
  endif

  if (nargout == 0)
    if (! isempty (hp))
      set (hp, "VertexNormals", normals);
    endif
  else
    vn = normals;
  endif

endfunction


%!demo
%! function isofinish (hp)
%!   axis equal;
%!   set (hp, "VertexNormals", -get (hp, "VertexNormals"));  # Revert normals
%!   shading interp;
%!   lighting gouraud;
%!   set (hp, "BackFaceLighting", "lit");
%!   light ();
%! endfunction
%!
%! N = 15;    # Increase number of vertices in each direction
%! iso = .4;  # Change isovalue to .1 to display a sphere
%! lin = linspace (0, 2, N);
%! [x, y, z] = meshgrid (lin, lin, lin);
%! val = (x-.5).^2 + (y-.5).^2 + (z-.5).^2;
%! clf;
%!
%! subplot (2,2,1);
%!  view (-38, 20);
%!  [fac, vert, cdat] = isosurface (x, y, z, val, iso, y);
%!  hp = patch ("Faces", fac, "Vertices", vert, "FaceVertexCData", cdat);
%!  title ("without isonormals");
%!  isofinish (hp);
%!  set (hp, "VertexNormalsMode", "auto");  # for Matlab compatibility
%!
%! subplot (2,2,2);
%!  view (-38, 20);
%!  hp = patch ("Faces", fac, "Vertices", vert, "FaceVertexCData", cdat);
%!  title ("patch modified by isonormals");
%!  isonormals (x, y, z, val, hp);  # Directly modify patch
%!  isofinish (hp);
%!
%! subplot (2,2,3);
%!  view (-38, 20);
%!  hp = patch ("Faces", fac, "Vertices", vert, "FaceVertexCData", cdat);
%!  vn = isonormals (x, y, z, val, vert);  # Compute normals of isosurface
%!  set (hp, "VertexNormals", vn);         # Manually set vertex normals
%!  title ('set "VertexNormals" from isonormals');
%!  isofinish (hp);
%!
%! subplot (2,2,4);
%!  view (-38, 20);
%!  hp = patch ("Faces", fac, "Vertices", vert, "FaceVertexCData", cdat);
%!  isonormals (x, y, z, val, hp, "negate");  # Use reverse directly
%!  title ('patch modified by isonormals (..., "negate")');
%!  isofinish (hp);

%!shared x,y,z,val,vert
%! [x, y, z] = meshgrid (0:.5:2, 0:.5:2, 0:.5:2);
%! val = abs ((x-.5).^2 + (y-.3).^2 + (z-.4).^2);
%! [fac, vert, cdat] = isosurface (x, y, z, val, .4, y);

%!test
%! vn = isonormals (x, y, z, val, vert);
%! assert (size (vert), size (vn));

%!test
%! np = isonormals (x, y, z, val, vert);
%! nn = isonormals (x, y, z, val, vert, "negate");
%! assert (np, -nn);

%!test
%! [x,y,z] = meshgrid (-2:1:2, -2:1:2, -2:1:2);
%! val = x.^2 + y.^2 + z.^2;
%! [f,vert] = isosurface (x, y, z, val, 1);
%! vn = isonormals (x, y, z, val, vert);
%! dirn = vert ./ vn;
%! assert (all (dirn(isfinite (dirn)) <= 0));

## Test input validation
%!error <Invalid call> isonormals ()
%!error <Invalid call> isonormals (1)
%!error <Invalid call> isonormals (1,2,3)
%!error <Invalid call> isonormals (1,2,3,4)
%!error <Invalid call> isonormals (1,2,3,4,5,6)
%!error <Unknown option 'foo'> isonormals (x, y, z, val, vert, "foo")
%!error <must be a list of vertices> isonormals (1, {1})
%!error <must be a list of vertices> isonormals (1, [1 2 3 4])
%!error <must be a .* patch handle> isonormals (x, y, z, val, x)
## Test validation of private function __interp_cube__
%!error <X, Y, Z have unequal dimensions> isonormals ({x}, y, z, val, vert)
%!error <X, Y, Z have unequal dimensions> isonormals (x, {y}, z, val, vert)
%!error <X, Y, Z have unequal dimensions> isonormals (x, y, {z}, val, vert)
%!error <X, Y, Z have unequal dimensions> isonormals (x, y, z(1), val, vert)
%!error <X, Y, Z have unequal dimensions> isonormals (x(:), y(:), z, val, vert)
%!error <VAL dimensions must match those of X, Y, and Z> isonormals (1, 2, 3, val, vert)
