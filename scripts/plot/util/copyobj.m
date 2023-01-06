########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{hnew} =} copyobj (@var{horig})
## @deftypefnx {} {@var{hnew} =} copyobj (@var{horig}, @var{hparent})
## Construct a copy of the graphic objects associated with the handles
## @var{horig} and return new handles @var{hnew} to the new objects.
##
## If a parent handle @var{hparent} (root, figure, axes, or hggroup) is
## specified, the copied object will be created as a child of @var{hparent}.
##
## If @var{horig} is a vector of handles, and @var{hparent} is a scalar,
## then each handle in the vector @var{hnew} has its @qcode{"Parent"} property
## set to @var{hparent}.  Conversely, if @var{horig} is a scalar and
## @var{hparent} a vector, then each parent object will receive a copy of
## @var{horig}.  If @var{horig} and @var{hparent} are both vectors with the
## same number of elements then @code{@var{hnew}(i)} will have parent
## @code{@var{hparent}(i)}.
## @seealso{struct2hdl, hdl2struct, findobj}
## @end deftypefn

function hnew = copyobj (horig, hparent = 0)

  partypes = {"root", "figure", "axes", "hggroup"};
  othertypes = {"line", "patch", "surface", "image", "text", "uicontrol"};
  alltypes = [partypes othertypes];

  if (! ishghandle (horig))
    print_usage ();
  elseif (! ishghandle (hparent))
    hparent = figure (fix (hparent));
  else
    for hp = hparent(:)'
      if (! any (strcmpi (get (hp, "type"), partypes)))
        print_usage ();
      endif
    endfor
  endif

  if (numel (horig) != numel (hparent)
      && ! (isscalar (hparent) || isscalar (horig)))
    error ("copyobj: size of HORIG and HPARENT must match, or one must be a scalar");
  endif

  ## current figure and axes
  cf = gcf ();
  ca = get (cf, "currentaxes");

  ## compatibility of input handles
  for i = 1:numel (horig)
    kididx(i) = find (strcmp (alltypes, get (horig(i), "type")));
  endfor

  for i = 1:numel (hparent)
    paridx(i) = find (strcmp (alltypes, get (hparent(i), "type")));
  endfor

  if (kididx <= paridx)
    error ("copyobj: %s object can't be a child of %s",
           alltypes{kididx}, alltypes{paridx});
  endif

  ## loop over vector inputs
  if (nargin == 1)
    ## No parent specified
    for i = numel (horig)
      str = hdl2struct (horig(i));
      hnew(i) = struct2hdl (str);
    endfor
  elseif (isscalar (hparent))
    for i = 1:numel (horig)
      str = hdl2struct (horig(i));
      hnew(i) = struct2hdl (str, hparent);
    endfor
  elseif (isscalar (horig))
    str = hdl2struct (horig);
    for i = 1:numel (hparent)
      hnew(i) = struct2hdl (str, hparent(i));
    endfor
  else
    for i = 1:numel (horig)
      str = hdl2struct (horig(i));
      hnew(i) = struct2hdl (str, hparent(i));
    endfor
  endif

  ## reset current figure (and eventually axes) to original
  set (0, "currentfigure", cf);
  if (ancestor (hnew(1), "figure") == cf && ! isempty (ca))
    set (cf, "currentaxes", ca);
  endif

endfunction


## Absurd number of drawnow() function calls in demos is due to problem
## with FLTK backend which is not respecting the set ("position") call.

%!demo
%! hobj = clf;
%! set (hobj, "name", "Original", "numbertitle", "off");
%! hold on;
%! x = 1:10;
%! y = x.^2;
%! dy = 2 * (.2 * x);
%! y2 = (x - 3).^2;
%! hg = errorbar (x, y, dy);
%! set (hg, "marker", "^", "markerfacecolor", rand (1,3));
%! plot (x, y2, "ok-");
%! legend ("errorbar", "line");
%! drawnow ();
%! pos = get (hobj, "position");
%! scrn = get (0, "screensize");
%! set (hobj, "position", [scrn(3)/2-pos(3)-10, scrn(4)/2-pos(4)/2, pos(3:4)]);
%! drawnow ();
%! hnew = copyobj (hobj, groot);
%! drawnow ();
%! set (hnew, "name", "Copyobj");
%! drawnow ();
%! set (hnew, "position", [scrn(3)/2, scrn(4)/2-pos(4)/2, pos(3:4)]);
%! drawnow ();

%!demo
%! hobj = clf;
%! set (hobj, "name", "Original", "numbertitle", "off");
%! subplot (2,2,1);
%!  hold on;
%!  contourf (rand (10, 10));
%!  colorbar ();
%! subplot (2,2,2);
%!  quiver (rand (10, 10), rand (10, 10));
%! subplot (2,2,3);
%!  colormap (jet (64));
%!  hold on;
%!  sombrero ();
%!  colorbar ("peer", gca, "NorthOutside");
%! subplot (2,2,4);
%!  imagesc (rand (30, 30));
%!  text (15, 15, "Rotated text", ...
%!        "HorizontAlalignment", "Center", "Rotation", 30);
%! drawnow ();
%! pos = get (hobj, "position");
%! scrn = get (0, "screensize");
%! set (hobj, "position", [scrn(3)/2-pos(3)-10, scrn(4)/2-pos(4)/2, pos(3:4)]);
%! drawnow ();
%! hnew = copyobj (hobj, groot);
%! drawnow ();
%! set (hnew, "name", "Copyobj");
%! drawnow ();
%! set (hnew, "position", [scrn(3)/2, scrn(4)/2-pos(4)/2, pos(3:4)]);
%! drawnow ();

%!demo
%! hobj = clf;
%! set (hobj, "name", "Original", "numbertitle", "off");
%! x = 0:0.1:2*pi;
%! y1 = sin (x);
%! y2 = exp (x - 1);
%! ax = plotyy (x,y1, x-1,y2, @plot, @semilogy);
%! xlabel ("X");
%! ylabel (ax(1), "Axis 1");
%! ylabel (ax(2), "Axis 2");
%! colororder = get (gca, "ColorOrder");
%! lcolor = colororder(1,:);
%! rcolor = colororder(2,:);
%! text (0.5, 0.5, "Left Axis", ...
%!       "color", lcolor, "horizontalalignment", "center", "parent", ax(1));
%! text (4.5, 80, "Right Axis", ...
%!       "color", rcolor, "horizontalalignment", "center", "parent", ax(2));
%! title ({"plotyy() example"; "left axis uses @plot, right axis uses @semilogy"});
%! drawnow ();
%! pos = get (hobj, "position");
%! scrn = get (0, "screensize");
%! set (hobj, "position", [scrn(3)/2-pos(3)-10, scrn(4)/2-pos(4)/2, pos(3:4)]);
%! drawnow ();
%! hnew = copyobj (hobj, groot);
%! drawnow ();
%! set (hnew, "name", "Copyobj");
%! drawnow ();
%! set (hnew, "position", [scrn(3)/2, scrn(4)/2-pos(4)/2, pos(3:4)]);
%! drawnow ();

%!testif HAVE_MAGICK; (have_window_system () && __have_feature__ ("QT_OFFSCREEN") && any (strcmp ("qt", available_graphics_toolkits ())));
%! toolkit = graphics_toolkit ();
%! graphics_toolkit ("qt");
%! unwind_protect
%!   h1 = figure ("visible", "off", "paperposition", [0.25, 2.5, 8.0, 6.0]);
%!   x = 0:0.1:2*pi;
%!   y1 = sin (x);
%!   y2 = exp (x - 1);
%!   ax = plotyy (x,y1, x-1,y2, @plot, @semilogy);
%!   xlabel ("X");
%!   ylabel (ax(1), "Axis 1");
%!   ylabel (ax(2), "Axis 2");
%!   axes (ax(1));
%!   text (0.5, 0.5, "Left Axis", ...
%!         "color", [0 0 1], "horizontalalignment", "center");
%!   axes (ax(2));
%!   text (4.5, 80, "Right Axis", ...
%!         "color", [0 0.5 0], "horizontalalignment", "center");
%!   s1 = hdl2struct (h1);
%!   h2 = struct2hdl (s1);
%!   png1 = [tempname() ".png"];
%!   png2 = [tempname() ".png"];
%!   unwind_protect
%!     print (h1, png1);
%!     [img1, map1, alpha1] = imread (png1);
%!     print (h2, png2);
%!     [img2, map2, alpha2] = imread (png2);
%!   unwind_protect_cleanup
%!     unlink (png1);
%!     unlink (png2);
%!   end_unwind_protect
%!   assert (img1, img2);
%!   assert (map1, map2);
%!   assert (alpha1, alpha2);
%! unwind_protect_cleanup
%!   close (h1);
%!   close (h2);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

%!test
%! unwind_protect
%!   tag = "foo";
%!   hf = figure ("visible", "off");
%!   hax = axes ("tag", tag);
%!   hpa = patch ();
%!   set (hpa, "facecolor", [.5 .5 .5], "tag", tag);
%!   hax2 = copyobj (hax, hf);
%!   assert (get (hax2, "tag"), tag);
%!   hpa2 = get (hax2, "children");
%!   assert (get (hpa2, "facecolor"), [.5 .5 .5]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
