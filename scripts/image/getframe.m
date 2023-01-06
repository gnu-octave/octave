########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{frame} =} getframe ()
## @deftypefnx {} {@var{frame} =} getframe (@var{hax})
## @deftypefnx {} {@var{frame} =} getframe (@var{hfig})
## @deftypefnx {} {@var{frame} =} getframe (@dots{}, @var{rect})
##
## Capture a figure or axes as a movie frame structure.
##
## Without an argument, capture the current axes excluding ticklabels, title,
## and x/y/zlabels.  The returned structure @var{frame} has a field
## @code{cdata}, which contains the actual image data in the form of an
## @nospell{NxMx3} (RGB) uint8 matrix, and a field @code{colormap} which is
## provided for @sc{matlab} compatibility but is always empty.
##
## If the first argument @var{hax} is an axes handle, then capture this axes,
## rather than the current axes returned by @code{gca}.
##
## If the first argument @var{hfig} is a figure handle then the entire
## corresponding figure canvas is captured.
##
## Finally, if a second argument @var{rect} is provided it must be a
## four-element vector ([left bottom width height]) defining the region inside
## the figure to be captured.  Regardless of the figure @qcode{"units"}
## property, @var{rect} must be defined in @strong{pixels}.
##
## @seealso{im2frame, frame2im, movie}
## @end deftypefn

function frame = getframe (h = [], rect = [])

  hf = hax = [];
  if (isempty (h))
    hf = get (0, "currentfigure");
    if (isempty (hf))
      error ("getframe: no figure to capture");
    endif
    hax = get (hf, "currentaxes");
    if (isempty (hax))
      error ("getframe: no axes to capture");
    endif
  elseif (isfigure (h))
    hf = h;
  elseif (isaxes (h))
    hf = ancestor (h, "figure");
    hax = h;
  else
    error ("getframe: H must be a figure or axes handle");
  endif

  if (strcmp (get (hf, "__graphics_toolkit__"), "gnuplot"))
    error ("getframe: not implemented for gnuplot graphics toolkit");
  endif

  unwind_protect
    htmp = hax;
    if (h == hf)
      htmp = hf;
    endif
    units = get (htmp, "units");
    set (htmp, "units", "pixels");
    pos = get (htmp, "position");
    if (h == hf)
      pos(1:2) = 1;
    endif
  unwind_protect_cleanup
    set (htmp, "units", units)
  end_unwind_protect

  if (! isempty (rect))
    xv = [pos(1); pos(1)+pos(3); pos(1)+pos(3); pos(1)];
    yv = [pos(2); pos(2); pos(2)+pos(4); pos(2)+pos(4)];
    x = [rect(1); rect(1)+rect(3); rect(1)+rect(3); rect(1)];
    y = [rect(2); rect(2); rect(2)+rect(4); rect(2)+rect(4)];
    in = inpolygon (x, y, xv, yv);
    if (! all (in))
      error ("getframe: RECT must define a region inside the figure");
    endif
    pos = rect;
  endif

  __check_rendering_capability__ ("getframe", hf);

  ## __get_frame__ requires that the figure "units" is "pixels"
  unwind_protect
    units = get (hf, "units");
    set (hf, "units", "pixels");
    cdata = __get_frame__ (hf);
  unwind_protect_cleanup
    set (hf, "units", units)
  end_unwind_protect

  i1 = max (floor (pos(1)), 1);
  i2 = min (ceil (pos(1)+pos(3)-1), columns (cdata));
  idxx = i1:i2;
  i1 = max (floor (pos(2)), 1);
  i2 = min (ceil (pos(2)+pos(4)-1), rows (cdata));
  idxy = fliplr (rows (cdata) - (i1:i2) + 1);

  frame = struct ("cdata", cdata(idxy,idxx,:), "colormap", []);

endfunction


%!demo
%! clf;
%! contourf (rand (5));
%! drawnow ();
%! frame = getframe ();
%! imshow (frame.cdata);

%!demo
%! clf reset;
%! contourf (rand (5));
%! frame = getframe (gcf ());
%! imshow (frame.cdata);
%! set (gca, "position", [0 0 1 1]);

%!demo
%! clf;
%! hax1 = subplot (2,1,1);
%! contourf (rand (5));
%! title ("Original");
%! frame = getframe (hax1);
%! hax2 = subplot (2,1,2);
%! image (frame.cdata);
%! title ("Frame");

%!demo
%! clf;
%! hax1 = subplot (2,1,1);
%! contourf (rand (5));
%! title ("Original");
%!
%! ## Get the coordinates of the lower-left hand corner in pixels
%! set (hax1, "units", "pixels");
%! pos = get (hax1, "position");
%! set (hax1, "units", "normalized");
%! rect = [pos(1:2) pos(3:4)/2];
%!
%! frame = getframe (hax1, rect);
%! hax2 = subplot (2,1,2);
%! image (frame.cdata);
%! title ("Lower left hand corner");

%!testif HAVE_QT_OFFSCREEN; have_window_system () && strcmp ("qt", graphics_toolkit ())
%! hf = figure ("visible", "off");
%! unwind_protect
%!   pos = get (hf, "position");
%!   assert (size (getframe (hf).cdata)(1:2), pos(4:-1:3));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!testif HAVE_QT_OFFSCREEN; have_window_system () && strcmp ("qt", graphics_toolkit ())
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("visible", "off", "position", [0 0 1 1]);
%!   verts = [0 0; .5 0; 1 0; ...
%!            0 .5; .5 .5; 1 .5; ...
%!            0 1; .5 1; 1 1];
%!   faces = [1 2 5 4; 2 3 6 5; 4 5 8 7; 5 6 9 8];
%!   fvc = [1 0 0; 0 1 0; 0 0 1; 1 0 1];
%!   patch ("vertices", verts, "faces", faces, "facevertexcdata", fvc, ...
%!          "facecolor", "flat");
%!
%!   kk = 1;
%!   pos = get (hf, "position");
%!
%!   for jj = [0.05 0.55]
%!     for ii = [0.05 0.55]
%!       rect = [ii jj .4 .4].*[pos(3:4) pos(3:4)];
%!       frame = getframe (hax, rect).cdata;
%!       assert (frame(:,:,1) == fvc(kk,1)*255);
%!       assert (frame(:,:,2) == fvc(kk,2)*255);
%!       assert (frame(:,:,3) == fvc(kk,3)*255);
%!       kk++;
%!     endfor
%!   endfor
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
