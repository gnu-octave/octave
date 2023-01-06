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
## @deftypefn  {} {} lighting (@var{type})
## @deftypefnx {} {} lighting (@var{hax}, @var{type})
## Set the lighting of patch or surface graphic objects.
##
## Valid arguments for @var{type} are
##
## @table @asis
## @item @qcode{"flat"}
## Draw objects with faceted lighting effects.
##
## @item @qcode{"gouraud"}
## Draw objects with linear interpolation of the lighting effects between the
## vertices.
##
## @item @qcode{"none"}
## Draw objects without light and shadow effects.
## @end table
##
## If the first argument @var{hax} is an axes handle, then change the lighting
## effects of objects in this axes, rather than the current axes returned by
## @code{gca}.
##
## The lighting effects are only visible if at least one light object is
## present and visible in the same axes.
##
## @seealso{light, fill, mesh, patch, pcolor, surf, surface, shading}
## @end deftypefn

function lighting (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("lighting", varargin{:});

  if (nargin != 1)
    print_usage ();
  endif

  mode = varargin{1};
  if (! ischar (mode))
    error ("lighting: MODE must be a string");
  elseif (! any (strcmpi (mode, {"flat", "gouraud", "none"})))
    error ('lighting: Invalid MODE "%s"', mode);
  endif

  if (isempty (hax))
    hax = gca ();
  endif

  ## Find all patch and surface objects that are descendants of hax
  ## and which are not part of a contour plot hggroup.
  hlist = [];
  kids = get (hax, "children");
  while (! isempty (kids))
    types = get (kids, "type");
    hlist = [hlist; kids(strcmp(types, "patch"))];
    hlist = [hlist; kids(strcmp(types, "surface"))];
    parents = kids(strcmp (types, "axes"));
    hglist = kids(strcmp (types, "hggroup"));

    kids = get (parents, "children");
    for i = 1 : numel (hglist)
      props = get (hglist(i));
      if (! isfield (props, "levelstep"))
        kids = [kids; get(hglist(i), "children")];
      endif
    endfor

  endwhile

  ## FIXME: This is the old, simple code.
  ##        Unfortunately, it also lights contour plots which is not desirable.
  ## hp = findobj (hax, "type", "patch");
  ## hs = findobj (hax, "type", "surface");
  ## hlist = [hp(:); hs(:)];

  if (isempty (hlist))
    return;
  endif

  ## Change "EdgeLighting" for meshes instead of "FaceLighting"
  fc = get (hlist, "facecolor");
  is_mesh = strcmp (fc, "none");
  if (! iscell (fc))
    fc = {fc};
  endif
  bc = get (hax, "color");
  if (strcmp (bc, "none"))
    if (isprop (get (hax, "parent"), "color"))
      bc = get (get (hax, "parent"), "color");
    endif
    if (isempty (bc) || strcmp (bc, "none"))
      bc = [1 1 1];
    endif
  endif
  is_mesh = is_mesh | cellfun (@(x) isequal (x, bc), fc);

  set (hlist(! is_mesh), "facelighting", mode, "edgelighting", "none");
  set (hlist(is_mesh), "edgelighting", mode, "facelighting", "none");

endfunction


%!demo
%! clf;
%! colormap ("default");
%! sombrero ();
%! shading flat
%! lighting flat
%! light ();
%! title ("shading flat - lighting flat");

%!demo
%! clf;
%! colormap ("default");
%! sombrero ();
%! shading interp
%! lighting gouraud
%! light ();
%! title ("shading interp - lighting gouraud");

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks ());
%! shading flat
%! lighting flat
%! light ();
%! view (3)
%! title ("shading flat - lighting flat");

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks ());
%! shading interp
%! lighting gouraud
%! light ();
%! view (3)
%! title ("shading interp - lighting gouraud");

%!demo
%! clf;
%! colormap ("default");
%! mesh (sombrero ());
%! shading flat
%! lighting flat
%! light ();
%! title ("shading flat - lighting flat");

%!demo
%! clf;
%! colormap ("default");
%! mesh (sombrero ());
%! shading interp
%! lighting gouraud
%! light ();
%! title ("shading interp - lighting gouraud");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ha = axes ();
%!   hm = mesh (sombrero ());
%!   hp = patch ();
%!   hs = surface ();
%!   lighting flat
%!   assert (get (hp, "facelighting"), "flat");
%!   assert (get (hs, "facelighting"), "flat");
%!   assert (get (hp, "edgelighting"), "none");
%!   assert (get (hs, "edgelighting"), "none");
%!   assert (get (hm, "edgelighting"), "flat");
%!   lighting gouraud
%!   assert (get (hp, "facelighting"), "gouraud");
%!   assert (get (hs, "facelighting"), "gouraud");
%!   assert (get (hp, "edgelighting"), "none");
%!   assert (get (hs, "edgelighting"), "none");
%!   assert (get (hm, "edgelighting"), "gouraud");
%!   lighting none
%!   assert (get (hp, "facelighting"), "none");
%!   assert (get (hs, "facelighting"), "none");
%!   assert (get (hp, "edgelighting"), "none");
%!   assert (get (hs, "edgelighting"), "none");
%!   assert (get (hm, "edgelighting"), "none");
%!   lighting (ha, "flat")
%!   assert (get (hp, "facelighting"), "flat");
%!   assert (get (hs, "facelighting"), "flat");
%!   assert (get (hp, "edgelighting"), "none");
%!   assert (get (hs, "edgelighting"), "none");
%!   assert (get (hm, "edgelighting"), "flat");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test on axes which has no patch or mesh objects
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   lighting flat
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <Invalid call> lighting ()
%!error lighting (1, 2, "flat")
%!error <MODE must be a string> lighting (-1)
%!error <MODE must be a string> lighting ({})
%!error <Invalid MODE "foo"> lighting foo
%!error <Invalid call to lighting> lighting (-1, "flat")
