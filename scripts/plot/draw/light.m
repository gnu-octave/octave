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
## @deftypefn  {} {} light ()
## @deftypefnx {} {} light (@dots{}, "@var{prop}", @var{val}, @dots{})
## @deftypefnx {} {} light (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} light (@dots{})
## Create a light object in the current axes or for axes @var{hax}.
##
## When a light object is present in an axes object, and the properties
## @qcode{"EdgeLighting"} or @qcode{"FaceLighting"} of a @code{patch} or
## @code{surface} object are set to a value other than @qcode{"none"}, these
## objects are drawn with lighting effects.  Supported values for Lighting
## properties are @qcode{"none"} (no lighting effects), @qcode{"flat"} (faceted
## look of the objects), and @qcode{"gouraud"} (linear interpolation of the
## lighting effects between the vertices).  If the lighting mode is set to
## @qcode{"flat"}, the @qcode{"FaceNormals"} property is used for lighting.
## For @qcode{"gouraud"}, the @qcode{"VertexNormals"} property is used.
##
## Up to eight light objects are supported per axes.  (Implementation
## dependent)
##
## Lighting is only supported for OpenGL graphic toolkits (i.e., @qcode{"fltk"}
## and @qcode{"qt"}).
##
## A light object has the following properties which alter the appearance of
## the plot.
##
## @table @asis
## @item @qcode{"Color":} The color of the light can be passed as an
## RGB-vector (e.g., @code{[1 0 0]} for red) or as a string (e.g., @qcode{"r"}
## for red).  The default color is white (@code{[1 1 1]}).
##
## @item @qcode{"Position":} The direction from which the light emanates as a
## 1x3-vector.  The default direction is @code{[1 0 1]}.
##
## @item @qcode{"Style":} This string defines whether the light emanates from a
## light source at infinite distance (@qcode{"infinite"}) or from a local point
## source (@qcode{"local"}).  The default is @qcode{"infinite"}.
## @end table
##
## If the first argument @var{hax} is an axes handle, then add the light object
## to this axes, rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created light
## object.
##
## Programming Note: The full list of properties is documented at
## @ref{Light Properties}.
## @seealso{lighting, material, patch, surface}
## @end deftypefn

function h = light (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("light", varargin{:});

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  htmp = __go_light__ (hax, varargin{:});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! ## Demonstrate effects of lighting
%! clf;
%! ## patches
%! h_axes1 = subplot (2, 2, 1);
%!  [x,y,z] = meshgrid (-2:0.2:2, -2:0.2:2, -2:0.2:2);
%!  val = x.^2 + y.^2 + z.^2;
%!  fv1 = isosurface (x, y, z, val, 1);
%!  h_patch1 = patch (fv1, "FaceColor", "c", "EdgeColor", "none", ...
%!                         "FaceLighting", "Gouraud");
%!  isonormals (x, y, z, val, h_patch1);
%!  fv2 = isosurface (x, y+3, z, val, 1);
%!  h_patch2 = patch (fv2, "FaceColor", "r", "EdgeColor", "none", ...
%!                         "FaceLighting", "Gouraud");
%!  isonormals (x, y+3, z, val, h_patch2);
%!  view (3);
%!  axis equal;  axis tight;
%!  title ("Patch with lighting");
%!  h_light1 = light ();
%!
%! h_axes2 = subplot (2, 2, 2);
%!  patch (fv1, "FaceColor", "c", "EdgeColor", "none");
%!  patch (fv2, "FaceColor", "r", "EdgeColor", "none");
%!  view (3);
%!  axis equal;  axis tight;
%!  title ("Patch without lighting");
%!
%! ## surfaces
%! h_axes3 = subplot (2, 2, 3);
%!  h_surf1 = surf (h_axes3, peaks, "LineStyle", "none", ...
%!                                  "FaceLighting", "Gouraud");
%!  title ("Surface with lighting");
%!  view (3);
%!  h_light2 = light ();
%!
%! h_axes3 = subplot (2, 2, 4);
%!  h_surf2 = surf (h_axes3, peaks, "LineStyle", "none");
%!  view (3);
%!  title ("Surface without lighting");

%!demo
%! ## Lighting modes on patches
%! clf;
%! [x,y,z] = meshgrid (-.2:0.05:.2, -.2:0.05:.2, -.2:0.05:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes1 = axes ();
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "none");
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "flat");
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! axis tight
%! axis equal
%! view (2);
%! light ("Position", [-1 1 1]);
%! title ({"FaceLighting on patches", "none - flat - gouraud"});

%!demo
%! ## Lighting modes on surfaces
%! clf;
%! Z = peaks ();
%! [X, Y] = meshgrid (1:columns (Z), 1:rows (Z));
%!
%! h_axes1 = axes ();
%! surf (X, Y, Z, "LineStyle", "none", "FaceLighting", "none");
%! hold on;
%! surf (X + round (1.2 * columns (Z)), Y, Z, "LineStyle", "none", ...
%!       "FaceLighting", "flat");
%! surf (X + round (2.4 * columns (Z)), Y, Z, "LineStyle", "none", ...
%!       "FaceLighting", "gouraud");
%! axis tight
%! axis equal
%! view (2);
%! light ("Position", [-1 1 1]);
%! title ({"FaceLighting on surfaces", "none - flat - gouraud"});

%!demo
%! ## multiple lights
%! clf;
%! h_axes = subplot (1, 2, 1);
%!  [x,y,z] = meshgrid (-2:0.1:2, -2:0.1:2, -2:0.1:2);
%!  val = x.^2 + y.^2 + z.^2;
%!  fv = isosurface (x, y, z, val, 1);
%!  h_patch = patch (fv, "FaceColor", "w", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  isonormals (x, y, z, val, h_patch);
%!  view (3);
%!  axis equal;  axis tight;
%!  title ("Patch with one light");
%!  h_light = light ("Color", "g");
%!
%! h_axes2 = subplot (1, 2, 2);
%!  h_patch2 = patch (fv, "FaceColor", "w", "EdgeColor", "none", ...
%!                        "FaceLighting", "Gouraud");
%!  isonormals (x, y, z, val, h_patch2);
%!  view (3);
%!  axis equal;  axis tight;
%!  title ("Patch with three lights");
%!  h_light1 = light ("Color", "r");
%!  h_light2 = light ("Position", [0 1 1], "Color", "b");
%!  h_light3 = light ("Position", [-1 -1 2], "Color", "g");

%!demo
%! ## Diffuse and specular reflection
%! clf;
%! h_axes = axes ();
%! rng = linspace (-0.2, +0.2, 12);
%! [x,y,z] = meshgrid (rng);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0, "SpecularStrength", 0);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0, "SpecularStrength", .5);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0, "SpecularStrength", 1);
%! isonormals (x+1, y, z, val, h_patch);
%!
%! fv = isosurface (x, y+.5, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0.5, "SpecularStrength", 0);
%! isonormals (x, y+.5, z, val, h_patch);
%! fv = isosurface (x+.5, y+.5, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0.5, "SpecularStrength", .5);
%! isonormals (x+.5, y+.5, z, val, h_patch);
%! fv = isosurface (x+1, y+.5, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0.5, "SpecularStrength", 1);
%! isonormals (x+1, y+.5, z, val, h_patch);
%!
%! fv = isosurface (x, y+1, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 1, "SpecularStrength", 0);
%! isonormals (x, y+1, z, val, h_patch);
%! fv = isosurface (x+.5, y+1, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 1, "SpecularStrength", .5);
%! isonormals (x+.5, y+1, z, val, h_patch);
%! fv = isosurface (x+1, y+1, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 1, "SpecularStrength", 1);
%! isonormals (x+1, y+1, z, val, h_patch);
%!
%! view (2);
%! axis equal
%! h_light = light ("Position", [-1 1 1]);
%!
%! xlabel ("SpecularStrength");
%! ylabel ("DiffuseStrength");
%! title ("Effects of SpecularStrength and DiffuseStrength");

%!demo
%! ## Ambient Strength and Ambient Light Color
%! clf;
%! [x,y,z] = meshgrid (-.2:0.05:.2, -.2:0.05:.2, -.2:0.05:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes1 = subplot (3,1,3);
%!  set (h_axes1, "AmbientLightColor", "g");
%!  fv = isosurface (x, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", 0);
%!  isonormals (x, y, z, val, h_patch);
%!  fv = isosurface (x+.5, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", .7);
%!  isonormals (x+.5, y, z, val, h_patch);
%!  fv = isosurface (x+1, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", 1);
%!  isonormals (x+1, y, z, val, h_patch);
%!  h_light = light ("Position", [-1 1 1]);
%!  view (2);
%!  axis tight
%!  axis equal
%!  xlabel ("AmbientStrength");
%!  ylabel ({"AmbientLightColor", "[0 1 0]"});
%!
%! h_axes2 = subplot (3,1,2);
%!  set (h_axes2, "AmbientLightColor", [.5 0 1]);
%!  fv = isosurface (x, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", 0);
%!  isonormals (x, y, z, val, h_patch);
%!  fv = isosurface (x+.5, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", .7);
%!  isonormals (x+.5, y, z, val, h_patch);
%!  fv = isosurface (x+1, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", 1);
%!  isonormals (x+1, y, z, val, h_patch);
%!  h_light = light ("Position", [-1 1 1]);
%!  view (2);
%!  axis tight
%!  axis equal
%!  ylabel ({"AmbientLightColor", "[.5 0 1]"});
%!
%! h_axes3 = subplot (3,1,1);
%!  set (h_axes3, "AmbientLightColor", "w");
%!  fv = isosurface (x, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", 0);
%!  isonormals (x, y, z, val, h_patch);
%!  fv = isosurface (x+.5, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", .7);
%!  isonormals (x+.5, y, z, val, h_patch);
%!  fv = isosurface (x+1, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%!  set (h_patch, "AmbientStrength", 1);
%!  isonormals (x+1, y, z, val, h_patch);
%!  h_light = light ("Position", [-1 1 1]);
%!  view (2);
%!  axis tight
%!  axis equal
%!  ylabel ({"AmbientLightColor", "[1 1 1]"});
%!  title ("Effects of AmbientLightColor and AmbientStrength");

%!demo
%! ## Specular Exponent
%! clf;
%! [x,y,z] = meshgrid (-.2:0.02:.2, -.2:0.02:.2, -.2:0.02:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes = axes ();
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularExponent", 15);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularExponent", 5);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularExponent", 1);
%! isonormals (x+1, y, z, val, h_patch);
%! h_light = light ("Position", [-1 1 1]);
%! view (2);
%! axis tight
%! axis equal
%! xlabel ("SpecularExponent");
%! title ("Effects of SpecularExponent");

%!demo
%! ## SpecularColorReflectance
%! clf;
%! [x,y,z] = meshgrid (-.2:0.02:.2, -.2:0.02:.2, -.2:0.02:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes = axes ();
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularColorReflectance", 0);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularColorReflectance", 0.5);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularColorReflectance", 1);
%! isonormals (x+1, y, z, val, h_patch);
%! view (2);
%! h_light = light ("Position", [-1 1 1]);
%! axis tight
%! axis equal
%! xlabel ("SpecularColorReflectance");
%! title ("Effects of SpecularColorReflectance");

%!demo
%! ## BackFaceLighting
%! clf;
%! [x,y,z] = meshgrid (-.5:0.1:2, -2:0.1:2, -2:0.1:2);
%! val = x.^2 + y.^2 + z.^2;
%! fv = isosurface (x, y, z, val, 1);
%! vn = isonormals (x, y, z, val, fv.vertices);
%! h_axes1 = subplot (1, 3, 1);
%! h_patch = patch (fv, "FaceColor", "c", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud", "VertexNormals", vn);
%! set (h_patch, "BackFaceLighting", "reverselit");
%! h_light = light ();
%! view (h_axes1, [-50 30]);
%! title ("reverselit");
%! axis equal
%!
%! h_axes2 = subplot (1, 3, 2);
%! h_patch = patch (fv, "FaceColor", "c", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud", "VertexNormals", -vn);
%! set (h_patch, "BackFaceLighting", "lit");
%! h_light = light ();
%! view (h_axes2, [-50 30]);
%! axis equal
%! title ("lit");
%!
%! h_axes3 = subplot (1, 3, 3);
%! h_patch = patch (fv, "FaceColor", "c", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud", "VertexNormals", -vn);
%! set (h_patch, "BackFaceLighting", "unlit");
%! h_light = light ();
%! view (h_axes3, [-50 30]);
%! axis equal
%! title ("unlit");

%!demo
%! ## BackFaceLighting 2
%! ## Matlab fails for "reverselit" with negated isonormals and for "unlit"
%! clf;
%! N = 30; iso = .8;
%! lin = linspace (-1, 1, N);
%! [x, y, z] = meshgrid (1.2*lin, 1.2*lin, lin);
%! val = (x).^2 + (y).^2 - iso/2.7*cos (2*pi*z);
%! val(x>0 & (y-x)>0) = NaN;
%! fv = isosurface (x, y, z, val, iso);
%! vn = isonormals (x, y, z, val, fv.vertices);
%!
%! subplot (2, 3, 1);
%!  view (140, 20);
%!  axis equal
%!  hp = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!              "BackFaceLighting", "reverselit", "VertexNormals", -vn);
%!  hl = light ("Position", [1 0 .5]);
%!  lighting gouraud
%!  title ({"BackFaceLighting", '"reverselit"', 'isonormals(...,"negate")'});
%!
%! subplot (2, 3, 2);
%!  view (140, 20);
%!  axis equal
%!  hp = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!              "BackFaceLighting", "lit", "VertexNormals", -vn);
%!  hl = light ("Position", [1 0 .5]);
%!  lighting gouraud
%!  title ({"BackFaceLighting", '"lit"', 'isonormals(...,"negate")'});
%!
%! subplot (2, 3, 3);
%!  view (140, 20);
%!  axis equal
%!  hp = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!              "BackFaceLighting", "unlit", "VertexNormals", -vn);
%!  hl = light ("Position", [1 0 .5]);
%!  lighting gouraud
%!  title ({"BackFaceLighting", '"unlit"', 'isonormals(...,"negate")'});
%!
%! subplot (2, 3, 4);
%!  view (140, 20);
%!  axis equal
%!  hp = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!              "BackFaceLighting", "reverselit", "VertexNormals", vn);
%!  hl = light ("Position", [1 0 .5]);
%!  lighting gouraud
%!  title ({"BackFaceLighting", '"reverselit"', "isonormals(...)"});
%!
%! subplot (2, 3, 5);
%!  view (140, 20);
%!  axis equal
%!  hp = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!              "BackFaceLighting", "lit", "VertexNormals", vn);
%!  hl = light ("Position", [1 0 .5]);
%!  lighting gouraud
%!  title ({"BackFaceLighting", '"lit"', "isonormals(...)"});
%!
%! subplot (2, 3, 6);
%!  view (140, 20);
%!  axis equal
%!  hp = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!              "BackFaceLighting", "unlit", "VertexNormals", vn);
%!  hl = light ("Position", [1 0 .5]);
%!  lighting gouraud
%!  title ({"BackFaceLighting", '"unlit"', "isonormals(...)"});

%!demo
%! ## Colored patch
%! clf;
%! [x,y,z] = meshgrid (-.2:0.02:.2, -.2:0.02:.2, -.2:0.02:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes = axes ();
%! view (3);
%! fv = isosurface (x, y, z, val, .039, z);
%! h_patch = patch (fv, "FaceColor", "flat", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularExponent", 15);
%! isonormals (x, y, z, val, h_patch);
%! h_light = light ("Position", [-1 1 1]);
%! view (3);
%! axis tight
%! axis equal
%! title ("Colored patch");

%!demo
%! ## Colored mesh (patch)
%! clf;
%! [x,y,z] = meshgrid (-.2:0.05:.2, -.2:0.05:.2, -.2:0.05:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes = axes ();
%! fv = isosurface (x, y, z, val, .039, z);
%! h_patch = patch (fv, "FaceColor", "w", "EdgeColor", "interp");
%! isonormals (x, y, z, val, h_patch);
%! h_light = light ();
%! lighting gouraud
%! view (3);
%! axis tight;
%! axis equal;
%! box off;
%! title ("Colored mesh (patch)");

%!demo
%! ## Colored mesh (surface)
%! clf;
%! h_mesh = mesh (peaks);
%! title ("Colored mesh (surface)");
%! h_light = light ();
%! lighting gouraud

%!demo
%! ## Light in hggroup
%! clf;
%! h_mesh = mesh (peaks);
%! h_hg = hggroup ();
%! title ("Light in hggroup");
%! h_light = light ("Parent", h_hg);
%! lighting gouraud

%!demo
%! ## Style local/infinite
%! clf;
%! [x,y,z] = meshgrid (-.2:0.02:.2, -.2:0.02:.2, -.2:0.02:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes1 = subplot (1,2,1);
%!  fv = isosurface (x, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none");
%!  isonormals (x, y, z, val, h_patch);
%!  title ('"Style" = "infinite" (default)');
%!  h_light = light ("Position", [.3 0 .3]);
%!  lighting gouraud
%!  view (3);
%!  axis equal
%!
%! h_axes2 = subplot (1,2,2);
%!  fv = isosurface (x, y, z, val, .039);
%!  h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none");
%!  isonormals (x, y, z, val, h_patch);
%!  title ('"Style" = "local"');
%!  h_light = light ("Style", "local", "Position", [.3 0 .3]);
%!  lighting gouraud
%!  view (3);
%!  axis equal

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = light ();
%!   assert (findobj (hf, "Type", "light"), h);
%!   assert (get (h, "Position"), [1, 0, 1]);
%!   assert (get (h, "Color"), [1, 1, 1]);
%!   assert (get (h, "Style"), "infinite");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! ha = gca ();
%! unwind_protect
%!   h = light (ha, "Position", [1 2 3], "Color", "r");
%!   assert (get (h, "Position"), [1 2 3]);
%!   assert (get (h, "Color"), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
