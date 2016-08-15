## Copyright (C) 2016 Markus Muetzel
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
## @deftypefn  {} {} light ()
## @deftypefnx {} {} light (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} light (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} light (@dots{})
## Create light object in the current axes or for axes @var{hax}.
##
## When a light object is present in an axes object and the properties
## @qcode{"EdgeLighting"} or @qcode{"FaceLighting"} of a @code{patch} or
## @code{surface} object are set to a value other than @qcode{"none"}, these
## objects are drawn with light and shadow effects.  Supported values for these
## properties are @qcode{"none"} (no lighting effects), @qcode{"flat"}
## (facetted look of the objects) and @qcode{"gouraud"} (linear interpolation
## of the lighting effects between the vertices).
## For @code{patch} objects, the normals must be set manually (property
## @qcode{"VertexNormals"}).
##
## Up to eight light objects are supported per axes.
##
## Lighting is only supported for graphics toolkits supporting OpenGL (i.e.
## @qcode{"fltk"} and @qcode{"qt"}).
##
## The following properties specific to the light object can be passed with
## their respective values:
##
## @table @asis
## @item @qcode{"Color":} The color of the light object can be passed as an
## RGB-vector (e.g., @code{[1 0 0]} for red) or as a string (e.g., @qcode{"r"}
## for red).  The default color is white (@code{[1 1 1]}).
##
## @item @qcode{"Position":} The direction from which the light emanates as an
## 1x3-vector.  The default direction is @code{[1 0 1]}.
##
## @item @qcode{"Style":} This string defines whether the light emanates from a
## light source at infinite distance (@qcode{"infinite"}) or from a local point
## source (@qcode{"local"}).
## @end table
##
## If @code{light} is called with an axes handle @var{hax}, it must be passed
## as the first argument.
##
## Optionally, the handle to the light object is returned in @var{h}.
##
## @seealso{get, set, patch, surface, lighting, material}
## @end deftypefn

## Author: mmuetzel

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
%! %% Demonstrate effects of lighting
%! clf;
%! %% patches
%! h_axes1 = subplot (2, 2, 1);
%! [x,y,z] = meshgrid (-2:0.2:2, -2:0.2:2, -2:0.2:2);
%! val = x.^2 + y.^2 + z.^2;
%! fv1 = isosurface (x, y, z, val, 1);
%! h_patch1 = patch (fv1, "FaceColor", "c", "EdgeColor", "none", ...
%!                        "FaceLighting", "Gouraud");
%! isonormals (x, y, z, val, h_patch1);
%! fv2 = isosurface (x, y+3, z, val, 1);
%! h_patch2 = patch (fv2, "FaceColor", "r", "EdgeColor", "none", ...
%!                        "FaceLighting", "Gouraud");
%! isonormals (x, y+3, z, val, h_patch2);
%! axis equal; axis tight
%! title ("Patch with lighting");
%! view (3);
%! h_light1 = light ();
%!
%! h_axes2 = subplot (2, 2, 2);
%! patch (fv1, "FaceColor", "c", "EdgeColor", "none");
%! patch (fv2, "FaceColor", "r", "EdgeColor", "none");
%! axis equal; axis tight
%! title ("Patch without lighting");
%! view (3);
%!
%! %% surfaces
%! h_axes3 = subplot (2, 2, 3);
%! h_surf1 = surf (h_axes3, peaks, "LineStyle", "none", ...
%!                                 "FaceLighting", "Gouraud");
%! title ("Surface with lighting");
%! view (3);
%! h_light2 = light ();
%!
%! h_axes3 = subplot (2, 2, 4);
%! h_surf2 = surf (h_axes3, peaks, "LineStyle", "none");
%! title ("Surface without lighting");
%! view (3);

%!demo
%! %% Lighting modes
%! clf;
%! [x,y,z] = meshgrid (-.2:0.05:.2, -.2:0.05:.2, -.2:0.05:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes1 = axes ();
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "none");
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "flat");
%! isonormals (x+.5, y, z, val, h_patch)
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", ...
%!                      "FaceLighting", "Gouraud");
%! isonormals (x+1, y, z, val, h_patch);
%! axis tight
%! axis equal
%! view (2);
%! light ("Position", [-1 1 1]);
%! title ("FaceLighting: none - flat - gouraud");

%!demo
%! %% multiple lights
%! clf;
%! h_axes = subplot (1, 2, 1);
%! [x,y,z] = meshgrid (-2:0.1:2, -2:0.1:2, -2:0.1:2);
%! val = x.^2 + y.^2 + z.^2;
%! fv = isosurface (x, y, z, val, 1);
%! h_patch = patch (fv, "FaceColor", "w", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! isonormals (x, y, z, val, h_patch);
%! axis equal; axis tight
%! title ("Patch with one light");
%! view (3);
%! h_light = light ("Color", "g");
%!
%! h_axes2 = subplot (1, 2, 2);
%! h_patch2 = patch (fv, "FaceColor", "w", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! isonormals (x, y, z, val, h_patch2);
%! axis equal; axis tight
%! title ("Patch with three lights");
%! view (3);
%! h_light1 = light ("Color", "r");
%! h_light2 = light ("Position", [0 1 1], "Color", "b");
%! h_light3 = light ("Position", [-1 -1 2], "Color", "g");

%!demo
%! %% Diffuse and specular reflex
%! clf;
%! h_axes = axes ();
%! [x,y,z] = meshgrid (-.2:0.04:.2, -.2:0.04:.2, -.2:0.04:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0, "SpecularStrength", 0);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0, "SpecularStrength", .5);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0, "SpecularStrength", 1);
%! isonormals (x+1, y, z, val, h_patch);
%!
%! fv = isosurface (x, y+.5, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0.5, "SpecularStrength", 0);
%! isonormals (x, y+.5, z, val, h_patch);
%! fv = isosurface (x+.5, y+.5, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0.5, "SpecularStrength", .5);
%! isonormals (x+.5, y+.5, z, val, h_patch);
%! fv = isosurface (x+1, y+.5, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 0.5, "SpecularStrength", 1);
%! isonormals (x+1, y+.5, z, val, h_patch);
%!
%! fv = isosurface (x, y+1, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 1, "SpecularStrength", 0);
%! isonormals (x, y+1, z, val, h_patch);
%! fv = isosurface (x+.5, y+1, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 1, "SpecularStrength", .5);
%! isonormals (x+.5, y+1, z, val, h_patch);
%! fv = isosurface (x+1, y+1, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "DiffuseStrength", 1, "SpecularStrength", 1);
%! isonormals (x+1, y+1, z, val, h_patch);
%!
%! axis equal
%! h_light = light ("Position", [-1 1 1]);
%! view (2);
%!
%! xlabel ("SpecularStrength");
%! ylabel ("DiffuseStrength");


%!demo
%! %% Ambient Strength and Ambient Light Color
%! clf;
%! [x,y,z] = meshgrid (-.2:0.05:.2, -.2:0.05:.2, -.2:0.05:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes1 = subplot (3,1,3);
%! set (h_axes1, "AmbientLightColor", "g");
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", 0);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", .7);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", 1);
%! isonormals (x+1, y, z, val, h_patch);
%! h_light = light ("Position", [-1 1 1]);
%! axis tight
%! axis equal
%! view (2);
%! xlabel ("AmbientStrength");
%! ylabel ("AmbientLightColor [0 1 0]");
%!
%! h_axes2 = subplot (3,1,2);
%! set (h_axes2, "AmbientLightColor", [.5 0 1]);
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", 0);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", .7);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", 1);
%! isonormals (x+1, y, z, val, h_patch);
%! h_light = light ("Position", [-1 1 1]);
%! axis tight
%! axis equal
%! view (2);
%! ylabel ("AmbientLightColor [.5 0 1]");
%!
%! h_axes3 = subplot (3,1,1);
%! set (h_axes3, "AmbientLightColor", "w");
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", 0);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", .7);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "AmbientStrength", 1);
%! isonormals (x+1, y, z, val, h_patch);
%! h_light = light ("Position", [-1 1 1]);
%! axis tight
%! axis equal
%! view (2);
%! ylabel ("AmbientLightColor [1 1 1]");

%!demo
%! %% Specular Exponent
%! clf;
%! [x,y,z] = meshgrid (-.2:0.02:.2, -.2:0.02:.2, -.2:0.02:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes = axes ();
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularExponent", 15);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularExponent", 5);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularExponent", 1);
%! isonormals (x+1, y, z, val, h_patch);
%! h_light = light ("Position", [-1 1 1]);
%! axis tight
%! axis equal
%! view (2);
%! xlabel ("SpecularExponent");

%!demo
%! %% SpecularColorReflectance
%! clf;
%! [x,y,z] = meshgrid (-.2:0.02:.2, -.2:0.02:.2, -.2:0.02:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes = axes ();
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularColorReflectance", 0);
%! isonormals (x, y, z, val, h_patch);
%! fv = isosurface (x+.5, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularColorReflectance", 0.5);
%! isonormals (x+.5, y, z, val, h_patch);
%! fv = isosurface (x+1, y, z, val, .039);
%! h_patch = patch (fv, "FaceColor", "r", "EdgeColor", "none", "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularColorReflectance", 1);
%! isonormals (x+1, y, z, val, h_patch);
%! h_light = light ("Position", [-1 1 1]);
%! axis tight
%! axis equal
%! view (2);
%! xlabel ("SpecularColorReflectance");

%!demo
%! %% BackFaceLighting
%! [x,y,z] = meshgrid (-.5:0.1:2, -2:0.1:2, -2:0.1:2);
%! val = x.^2 + y.^2 + z.^2;
%! fv = isosurface (x, y, z, val, 1);
%! vn = isonormals (x, y, z, val, fv.vertices, "negate");
%! h_axes1 = subplot (1, 3, 1);
%! h_patch = patch (fv, "FaceColor", "c", "EdgeColor", "none", ...
%!                  "FaceLighting", "Gouraud", "VertexNormals", vn);
%! set (h_patch, "BackFaceLighting", "reverselit");
%! h_light = light ();
%! view (h_axes1, [-50 30]);
%! title ("reverselit");
%! axis equal
%!
%! h_axes2 = subplot (1, 3, 2);
%! h_patch = patch (fv, "FaceColor", "c", "EdgeColor", "none", ...
%!           "FaceLighting", "Gouraud", "VertexNormals", vn);
%! set (h_patch, "BackFaceLighting", "lit");
%! h_light = light ();
%! view (h_axes2, [-50 30]);
%! title ("lit");
%! axis equal
%!
%! h_axes3 = subplot (1, 3, 3);
%! h_patch = patch (fv, "FaceColor", "c", "EdgeColor", "none", ...
%!           "FaceLighting", "Gouraud", "VertexNormals", vn);
%! set (h_patch, "BackFaceLighting", "unlit");
%! h_light = light ();
%! view (h_axes3, [-50 30]);
%! title ("unlit");
%! axis equal

%!demo
%! %% Colored patch
%! clf;
%! [x,y,z] = meshgrid (-.2:0.02:.2, -.2:0.02:.2, -.2:0.02:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes = axes ();
%! fv = isosurface (x, y, z, val, .039, z);
%! h_patch = patch (fv, "FaceColor", "flat", "EdgeColor", "none", ...
%!             "FaceLighting", "Gouraud");
%! set (h_patch, "SpecularExponent", 15);
%! isonormals (x, y, z, val, h_patch);
%! title ('Colored patch');
%! h_light = light ("Position", [-1 1 1]);
%! axis tight
%! axis equal
%! view (3);

%!demo
%! %% Colored mesh (patch)
%! clf;
%! [x,y,z] = meshgrid (-.2:0.05:.2, -.2:0.05:.2, -.2:0.05:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes = axes ();
%! fv = isosurface (x, y, z, val, .039, z);
%! h_patch = patch (fv, 'FaceColor', 'w', 'EdgeColor', 'interp');
%! isonormals (x, y, z, val, h_patch);
%! title ('Colored mesh (patch)');
%! h_light = light;
%! lighting gouraud
%! axis tight
%! axis equal
%! view (3);

%!demo
%! %% Colored mesh (surface)
%! clf;
%! h_mesh = mesh (peaks);
%! title ('Colored mesh (surface)');
%! h_light = light;
%! lighting gouraud

%!demo
%! %% Light in hggroup
%! clf;
%! h_mesh = mesh (peaks);
%! h_hg = hggroup ();
%! title ('Light in hggroup');
%! h_light = light ('Parent', h_hg);
%! lighting gouraud

%!demo
%! %% Style local/infinite
%! clf;
%! [x,y,z] = meshgrid (-.2:0.02:.2, -.2:0.02:.2, -.2:0.02:.2);
%! val = (x.^2 + y.^2 + z.^2);
%!
%! h_axes1 = subplot (1,2,1);
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, 'FaceColor', 'r', 'EdgeColor', 'none');
%! isonormals (x, y, z, val, h_patch);
%! title ('"Style" set to "infinite" (default)');
%! h_light = light ('Position', [.3 0 .3]);
%! lighting gouraud
%! axis equal
%! view (3);
%!
%! h_axes2 = subplot (1,2,2);
%! fv = isosurface (x, y, z, val, .039);
%! h_patch = patch (fv, 'FaceColor', 'r', 'EdgeColor', 'none');
%! isonormals (x, y, z, val, h_patch);
%! title ('"Style" set to "local"');
%! h_light = light ('Style', 'local', 'Position', [.3 0 .3]);
%! lighting gouraud
%! axis equal
%! view (3);


%!test
%! hf = figure ("Visible", "off");
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
%! hf = figure ("Visible", "off");
%! ha = gca;
%! unwind_protect
%!   h = light (ha, "Position", [1 2 3], "Color", "r");
%!   assert (get (h, "Position"), [1 2 3]);
%!   assert (get (h, "Color"), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

