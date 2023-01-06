########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {} shading (@var{type})
## @deftypefnx {} {} shading (@var{hax}, @var{type})
## Set the shading of patch or surface graphic objects.
##
## Valid arguments for @var{type} are
##
## @table @asis
## @item @qcode{"flat"}
## Single colored patches with invisible edges.
##
## @item @qcode{"faceted"}
## Single colored patches with black edges.
##
## @item @qcode{"interp"}
## Colors between patch vertices are interpolated and the patch edges are
## invisible.
## @end table
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
## @seealso{fill, mesh, patch, pcolor, surf, surface, hidden, lighting}
## @end deftypefn

function shading (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("shading", varargin{:});

  if (nargin != 1)
    print_usage ();
  endif

  mode = varargin{1};
  if (! ischar (mode))
    error ("shading: MODE must be a valid string");
  elseif (! any (strcmpi (mode, {"flat", "interp", "faceted"})))
    error ('shading: Invalid MODE "%s"', mode);
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
    for i = 1 : numel (hglist)
      props = get (hglist(i));
      if (! isfield (props, "levelstep"))
        parents(end+1) = hglist(i);
      endif
    endfor
    if (numel (parents) <= 1)
      kids = get (parents, "children");
    else
      ## See bug #55993 where multiple hggroups caused failure.
      kids = get (parents, "children");
      kids = [kids{:}](:);  # convert cell array to column vector of handles
    endif

  endwhile

  ## NOTE: This is the old, simple code.
  ##       Unfortunately, it also shades contour plots which is not desirable.
  ## hp = findobj (hax, "type", "patch");
  ## hs = findobj (hax, "type", "surface");
  ## hlist = [hp(:); hs(:)];

  if (isempty (hlist))
    return;
  endif

  ## Change "EdgeColor" for meshes instead of "FaceColor"
  fc = get (hlist, "facecolor");
  is_mesh = strcmp (fc, "none");
  if (! iscell (fc))
    fc = {fc};
  endif
  bg = get (hax, "color");
  if (strcmp (bg, "none"))
    if (isprop (get (hax, "parent"), "color"))
      bg = get (get (hax, "parent"), "color");
    endif
    if (isempty (bg) || strcmp (bg, "none"))
      bg = [1 1 1];
    endif
  endif
  is_mesh |= cellfun (@(x) isequal (x, bg), fc);

  ec = "none";
  if (strcmpi (mode, "faceted"))
    ec = [0 0 0];
    mode = "flat";
  endif

  set (hlist(! is_mesh), "facecolor", mode, "edgecolor", ec);
  set (hlist(is_mesh), "edgecolor", mode);

endfunction


%!demo
%! clf;
%! colormap ("default");
%! sombrero ();
%! shading faceted;
%! title ('shading ''faceted''');

%!demo
%! clf;
%! colormap ("default");
%! sombrero ();
%! shading faceted;
%! h = findobj (gca (), "type", "surface");
%! facecolor = get (h, "facecolor");
%! edgecolor = get (h, "edgecolor");
%! set (h, "edgecolor", facecolor, "facecolor", edgecolor);
%! title ({'shading ''faceted''', 'with "edgecolor" and "facecolor" reversed'});

%!demo
%! clf;
%! colormap ("default");
%! sombrero ();
%! shading flat;
%! title ('shading ''flat''');

%!demo
%! clf;
%! colormap ("default");
%! sombrero ();
%! shading flat;
%! h = findobj (gca (), "type", "surface");
%! facecolor = get (h, "facecolor");
%! edgecolor = get (h, "edgecolor");
%! set (h, "edgecolor", facecolor, "facecolor", edgecolor);
%! title ({'shading ''flat''', 'with "edgecolor" and "facecolor" reversed'});

%!demo
%! clf;
%! colormap ("default");
%! sombrero ();
%! shading interp;
%! title ('shading ''interp''');

%!demo
%! clf;
%! colormap ("default");
%! sombrero ();
%! shading interp;
%! h = findobj (gca (), "type", "surface");
%! facecolor = get (h, "facecolor");
%! edgecolor = get (h, "edgecolor");
%! set (h, "edgecolor", facecolor, "facecolor", edgecolor);
%! title ({'shading ''interp''', 'with "edgecolor" and "facecolor" reversed'});

%!demo
%! clf;
%! colormap ("default");
%! peaks ();
%! shading interp;
%! h = findobj (gca (), "type", "surface");
%! set (h, "edgecolor", "k");
%! title ({'shading ''interp''', 'with "edgecolor" set to black'});

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks ());
%! shading faceted;
%! title ('shading ''faceted''');

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks ());
%! shading faceted;
%! h = findobj (gca (), "type", "surface");
%! facecolor = get (h, "facecolor");
%! edgecolor = get (h, "edgecolor");
%! set (h, "edgecolor", facecolor, "facecolor", edgecolor);
%! title ({'shading ''faceted''', 'with "edgecolor" and "facecolor" reversed'});

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks ());
%! shading flat;
%! title ('shading ''flat''');

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks ());
%! shading flat;
%! h = findobj (gca (), "type", "surface");
%! facecolor = get (h, "facecolor");
%! edgecolor = get (h, "edgecolor");
%! set (h, "edgecolor", facecolor, "facecolor", edgecolor);
%! title ({'shading ''flat''', 'with "edgecolor" and "facecolor" reversed'});

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks ());
%! shading interp;
%! title ('shading ''interp''');

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks ());
%! shading interp;
%! h = findobj (gca (), "type", "surface");
%! facecolor = get (h, "facecolor");
%! edgecolor = get (h, "edgecolor");
%! set (h, "edgecolor", facecolor, "facecolor", edgecolor);
%! title ({'shading ''interp''', 'with "edgecolor" and "facecolor" reversed'});

%!demo
%! clf;
%! colormap ('default');
%! mesh (sombrero ());
%! shading interp;
%! title ('mesh with shading ''interp''');

%!demo
%! clf;
%! colormap ('default');
%! mesh (sombrero ());
%! shading flat;
%! title ('mesh with shading ''flat''');

%!test
%! hf = figure ("Visible", "off");
%! unwind_protect
%!   ha = axes ();
%!   hm = mesh (sombrero ());
%!   hp = patch ();
%!   hs = surface ();
%!   shading interp;
%!   assert (get (hp, "facecolor"), "interp");
%!   assert (get (hs, "facecolor"), "interp");
%!   assert (get (hp, "edgecolor"), "none");
%!   assert (get (hs, "edgecolor"), "none");
%!   assert (get (hm, "edgecolor"), "interp");
%!   shading faceted;
%!   assert (get (hp, "facecolor"), "flat");
%!   assert (get (hs, "facecolor"), "flat");
%!   assert (get (hp, "edgecolor"), [0 0 0]);
%!   assert (get (hs, "edgecolor"), [0 0 0]);
%!   assert (get (hm, "edgecolor"), "flat");
%!   shading (ha, "interp");
%!   assert (get (hp, "facecolor"), "interp");
%!   assert (get (hs, "facecolor"), "interp");
%!   assert (get (hp, "edgecolor"), "none");
%!   assert (get (hs, "edgecolor"), "none");
%!   assert (get (hm, "edgecolor"), "interp");
%!   set (hp, "edgecolor", "k");
%!   set (hs, "edgecolor", "k");
%!   shading flat;
%!   assert (get (hp, "facecolor"), "flat");
%!   assert (get (hs, "facecolor"), "flat");
%!   assert (get (hp, "edgecolor"), "none");
%!   assert (get (hs, "edgecolor"), "none");
%!   assert (get (hm, "edgecolor"), "flat");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test on axes which has no patch or mesh objects
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   shading flat
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <Invalid call> shading ()
%!error shading (1, 2, "flat")
%!error <MODE must be a valid string> shading (-1)
%!error <MODE must be a valid string> shading ({})
%!error <Invalid MODE "foo"> shading foo
