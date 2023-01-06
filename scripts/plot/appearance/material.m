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
## @deftypefn  {} {} material shiny
## @deftypefnx {} {} material dull
## @deftypefnx {} {} material metal
## @deftypefnx {} {} material default
## @deftypefnx {} {} material ([@var{as}, @var{ds}, @var{ss}])
## @deftypefnx {} {} material ([@var{as}, @var{ds}, @var{ss}, @var{se}])
## @deftypefnx {} {} material ([@var{as}, @var{ds}, @var{ss}, @var{se}, @var{scr}])
## @deftypefnx {} {} material (@var{hlist}, @dots{})
## @deftypefnx {} {@var{mtypes} =} material ()
## @deftypefnx {} {@var{refl_props} =} material (@var{mtype_string})
## Set reflectance properties for the lighting of surfaces and patches.
##
## This function changes the ambient, diffuse, and specular strengths, as well
## as the specular exponent and specular color reflectance, of all
## @code{patch} and @code{surface} objects in the current axes.  This can be
## used to simulate, to some extent, the reflectance properties of certain
## materials when used with @code{light}.
##
## When called with a string, the aforementioned properties are set
## according to the values in the following table:
##
## @multitable @columnfractions .0 .2 .15 .15 .15 .15 .15 .0
## @headitem @tab @var{mtype} @tab ambient- strength @tab diffuse-
## strength @tab specular- strength @tab specular- exponent @tab specular-
## color- reflectance @tab
## @item @tab @qcode{"shiny"} @tab 0.3 @tab 0.6 @tab 0.9 @tab 20 @tab 1.0 @tab
## @item @tab @qcode{"dull"} @tab 0.3 @tab 0.8 @tab 0.0 @tab 10 @tab 1.0 @tab
## @item @tab @qcode{"metal"} @tab 0.3 @tab 0.3 @tab 1.0 @tab 25 @tab 0.5 @tab
## @item @tab @qcode{"default"} @tab @qcode{"default"} @tab @qcode{"default"} @tab @qcode{"default"} @tab
## @qcode{"default"} @tab @qcode{"default"} @tab
## @end multitable
##
## When called with a vector of three elements, the ambient, diffuse, and
## specular strengths of all @code{patch} and @code{surface} objects in the
## current axes are updated.  An optional fourth vector element updates the
## specular exponent, and an optional fifth vector element updates the
## specular color reflectance.
##
## A list of graphic handles can also be passed as the first argument.  In
## this case, the properties of these handles and all child @code{patch} and
## @code{surface} objects will be updated.
##
## Additionally, @code{material} can be called with a single output argument.
## If called without input arguments, a column cell vector @var{mtypes} with
## the strings for all available materials is returned.  If the one input
## argument @var{mtype_string} is the name of a material, a 1x5 cell vector
## @var{refl_props} with the reflectance properties of that material is
## returned.  In both cases, no graphic properties are changed.
##
## @seealso{light, fill, mesh, patch, pcolor, surf, surface}
## @end deftypefn

function retval = material (varargin)

  if (! ((nargout == 0 && (nargin == 1 || nargin == 2))
         || (nargout == 1 && (nargin == 0 || nargin == 1))))
    print_usage ();
  endif

  ## resolve input
  h = [];
  if (nargout == 0)
    ## Check whether first argument is list of graphics handles.
    if (all (ishghandle (varargin{1})))
      h = varargin{1};
      varargin(1) = [];
    endif

    ## There must be one (additional) argument.
    if (numel (varargin) != 1)
      if (nargin == 2)
        error (["material: When called with two arguments, the first argument " ...
                "must be a list of handles to graphics objects."]);
      else
        print_usage ();
      endif
    endif
  elseif (nargin == 0)
    ## Return name of materials.
    retval = {"shiny"; "dull"; "metal"; "default"};
    return;
  endif

  mtype = varargin{1};

  se = [];
  scr = [];

  ## check material type
  if (ischar (mtype))
    switch (lower (mtype))
      case "shiny"
        as = 0.3;
        ds = 0.6;
        ss = 0.9;
        se = 20;
        scr = 1.0;

      case "dull"
        as = 0.3;
        ds = 0.8;
        ss = 0.0;
        se = 10;
        scr = 1.0;

      case "metal"
        as = 0.3;
        ds = 0.3;
        ss = 1.0;
        se = 25;
        scr = .5;

      case "default"
        as = "default";
        ds = "default";
        ss = "default";
        se = "default";
        scr = "default";

      otherwise
        error ("material: unknown material type '%s'", mtype);

    endswitch

    if (nargout == 1)
      ## Return 1x5 cell vector with reflectance properties.
      retval = {as, ds, ss, se, scr};
      return;
    endif

  elseif (nargout == 1)
    ## If we reach here with one output argument, the input was wrong.
    print_usage ();

  elseif (isvector (mtype))
    num_mtype = numel (mtype);
    if (num_mtype < 3 || num_mtype > 5)
      error ("material: incorrect number of elements in material vector");
    endif
    as = mtype(1);
    ds = mtype(2);
    ss = mtype(3);
    if (num_mtype >= 4)
      se = mtype(4);
      if (num_mtype == 5)
        scr = mtype(5);
      endif
    endif

  else
    error ("material: MTYPE must be a named material or a vector");
  endif

  if (isempty (h))
    h = gca ();
  endif
  ## find all patch and surface objects in current axes
  hps = findobj (h, "Type", "patch", "-or", "Type", "surface");

  ## set properties
  set (hps,
       "ambientstrength", as, "diffusestrength", ds, "specularstrength", ss);

  if (! isempty (se))
    set (hps, "specularexponent", se);
    if (! isempty (scr))
      set (hps, "specularcolorreflectance", scr);
    endif
  endif

endfunction


%!demo
%! clf;
%! ## patch
%! [x,y,z] = meshgrid (-2:0.2:2, -2:0.2:2, -2:0.2:2);
%! val = x.^2 + y.^2 + z.^2;
%! fv1 = isosurface (x, y, z, val, 1);
%! h_patch = patch (fv1, "FaceColor", "r", "EdgeColor", "none", ...
%!                       "FaceLighting", "Gouraud");
%! isonormals (x, y, z, val, h_patch);
%! axis equal;  axis tight;
%! view (3);
%! box off;
%! drawnow ();
%! light ();
%! material ([0 0.5 1 10 .5]);
%! title ("material() with numeric input");

%!demo
%! clf;
%! ## surface
%! hax = axes ();
%! surf (hax, peaks, "LineStyle", "none", "FaceLighting", "Gouraud");
%! view (3);
%! light ();
%! material metal;
%! title ("material metal");

%!test
%! hf = figure ("Visible", "off");
%! unwind_protect
%!   hp = patch ();
%!   hs = surface ();
%!   material dull
%!   assert (get (hp, "ambientstrength"), 0.3);
%!   assert (get (hs, "ambientstrength"), 0.3);
%!   assert (get (hp, "diffusestrength"), 0.8);
%!   assert (get (hs, "diffusestrength"), 0.8);
%!   assert (get (hp, "specularstrength"), 0.0);
%!   assert (get (hs, "specularstrength"), 0.0);
%!   assert (get (hp, "specularexponent"), 10);
%!   assert (get (hs, "specularexponent"), 10);
%!   assert (get (hp, "specularcolorreflectance"), 1.0);
%!   assert (get (hs, "specularcolorreflectance"), 1.0);
%!   material default
%!   assert (get (hp, "ambientstrength"),
%!           get (0, "defaultpatchambientstrength"));
%!   assert (get (hs, "ambientstrength"),
%!           get (0, "defaultsurfaceambientstrength"));
%!   assert (get (hp, "diffusestrength"),
%!           get (0, "defaultpatchdiffusestrength"));
%!   assert (get (hs, "diffusestrength"),
%!           get (0, "defaultsurfacediffusestrength"));
%!   assert (get (hp, "specularstrength"),
%!           get (0, "defaultpatchspecularstrength"));
%!   assert (get (hs, "specularstrength"),
%!           get (0, "defaultsurfacespecularstrength"));
%!   assert (get (hp, "specularexponent"),
%!           get (0, "defaultpatchspecularexponent"));
%!   assert (get (hs, "specularexponent"),
%!           get (0, "defaultsurfacespecularexponent"));
%!   assert (get (hp, "specularcolorreflectance"),
%!           get (0, "defaultpatchspecularcolorreflectance"));
%!   assert (get (hs, "specularcolorreflectance"),
%!           get (0, "defaultsurfacespecularcolorreflectance"));
%!   material ([0.5 0.6 0.7 20 0.8])
%!   assert (get (hp, "ambientstrength"), 0.5);
%!   assert (get (hs, "ambientstrength"), 0.5);
%!   assert (get (hp, "diffusestrength"), 0.6);
%!   assert (get (hs, "diffusestrength"), 0.6);
%!   assert (get (hp, "specularstrength"), 0.7);
%!   assert (get (hs, "specularstrength"), 0.7);
%!   assert (get (hp, "specularexponent"), 20);
%!   assert (get (hs, "specularexponent"), 20);
%!   assert (get (hp, "specularcolorreflectance"), 0.8);
%!   assert (get (hs, "specularcolorreflectance"), 0.8);
%!   material (hp, "shiny")
%!   assert (get (hp, "ambientstrength"), 0.3);
%!   assert (get (hs, "ambientstrength"), 0.5);
%!   assert (get (hp, "diffusestrength"), 0.6);
%!   assert (get (hs, "diffusestrength"), 0.6);
%!   assert (get (hp, "specularstrength"), 0.9);
%!   assert (get (hs, "specularstrength"), 0.7);
%!   assert (get (hp, "specularexponent"), 20);
%!   assert (get (hs, "specularexponent"), 20);
%!   assert (get (hp, "specularcolorreflectance"), 1.0);
%!   assert (get (hs, "specularcolorreflectance"), 0.8);
%!   material (hf, "metal")
%!   assert (get (hp, "ambientstrength"), 0.3);
%!   assert (get (hs, "ambientstrength"), 0.3);
%!   assert (get (hp, "diffusestrength"), 0.3);
%!   assert (get (hs, "diffusestrength"), 0.3);
%!   assert (get (hp, "specularstrength"), 1.0);
%!   assert (get (hs, "specularstrength"), 1.0);
%!   assert (get (hp, "specularexponent"), 25);
%!   assert (get (hs, "specularexponent"), 25);
%!   assert (get (hp, "specularcolorreflectance"), 0.5);
%!   assert (get (hs, "specularcolorreflectance"), 0.5);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! mtypes = material ();
%! assert (iscell (mtypes));
%! assert (columns (mtypes), 1);
%! assert (all (cellfun (@ischar, mtypes)));

%!test
%! refl_props = material ("metal");
%! assert (refl_props, {0.3, 0.3, 1, 25, 0.5});

## Test input validation
%!error <Invalid call to material> material ()
%!error <Invalid call to material> material (-1, 2, 3)
%!error <Invalid call to material> a = material (-1)
%!error <Invalid call to material> a = material (-1, 2)
%!error <Invalid call to material> a = material ({})
%!error <Invalid call to material> a = material ([.3 .4 .5])
%!error <called with too many outputs> [a, b] = material ()
%!error <first argument must be a list of handles> material (-1, "metal")
%!error <unknown material type 'foo'> material foo
%!error <incorrect number of elements in material vector> material (-1)
%!error <incorrect number of elements in material vector> material ([1 2 3 4 5 6])
%!error <MTYPE must be a named material or a vector> material ({})

%!error <Invalid call to material.>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   material (hf);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
