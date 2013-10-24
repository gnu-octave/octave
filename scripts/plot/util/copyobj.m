## Copyright (C) 2012-2013 pdiribarne
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{hnew} =} copyobj (@var{horig})
## @deftypefnx {Function File} {@var{hnew} =} copyobj (@var{horig}, @var{hparent})
## Construct a copy of the graphic object associated with handle @var{horig}
## and return a handle @var{hnew} to the new object.
##
## If a parent handle @var{hparent} (root, figure, axes, or hggroup) is
## specified, the copied object will be created as a child of @var{hparent}.
## @seealso{struct2hdl, hdl2struct, findobj}
## @end deftypefn

## Author: pdiribarne <pdiribarne@new-host.home>
## Created: 2012-04-01

function hnew = copyobj (horig, hparent = 0)

  partypes = {"root", "figure", "axes", "hggroup"};
  othertypes = {"line", "patch", "surface", "image", "text"};
  alltypes = [partypes othertypes];

  if (! ishandle (horig) || nargin > 2)
    print_usage ();
  elseif (! ishandle (hparent))
    hparent = figure (fix (hparent));
  elseif (! any (strcmpi (get (hparent).type, partypes)))
    print_usage ();
  endif

  ## current figure and axes
  cf = gcf ();
  ca = gca ();
  
  ## compatibility of input handles
  kididx = find (strcmp (alltypes, get (horig).type));
  paridx = find (strcmp (alltypes, get (hparent).type));

  if (kididx <= paridx)
    error ("copyobj: %s object can't be children to %s.",
           alltypes{kididx}, alltypes{paridx});
  elseif (nargin == 1)
    str = hdl2struct (horig);
    hnew = struct2hdl (str);
  else
    str = hdl2struct (horig);
    hnew = struct2hdl (str, hparent);
  endif

  ## reset current figure (and eventually axes) to original
  set (0, "currentfigure", cf);
  if (get (hnew, "parent") == cf)
    set (cf, "currentaxes", ca)
  endif
  
endfunction


%!demo
%! hdl = figure (1234);
%! clf;
%! hold on;
%! x = 1:10;
%! y = x.^2;
%! dy = 2 * (.2 * x);
%! y2 = (x - 3).^2;
%! hg = errorbar (x, y, dy,'#~');
%! set (hg, 'marker', '^', 'markerfacecolor', rand (1,3));
%! plot (x, y2, 'ok-');
%! legend ('errorbar', 'line');
%! hnew = copyobj (hdl);

%!#demo
%! ## FIXME: This demo fails for an obscure reason.
%! ## It appears that there is something wrong with Octave code for patches.
%! ## This demo must remain commented out until patch() has been reworked.
%! unwind_protect
%!   hdl = figure (1234);
%!   clf;
%!   subplot (2,2,1);
%!   hold on;
%!   contourf (rand (10, 10));
%!   colorbar ();
%!   subplot (2,2,2);
%!   quiver (rand (10, 10), rand (10, 10));
%!   subplot (2,2,3);
%!   colormap (jet (64));
%!   hold on;
%!   sombrero ();
%!   colorbar ('peer', gca, 'NorthOutside');
%!   subplot (2,2,4);
%!   imagesc (rand (30, 30));
%!   text (15, 15, 'Rotated text', ...
%!         'HorizontAlalignment', 'Center', 'Rotation', 30);
%!   hnew = copyobj (hdl);
%! unwind_protect_cleanup
%!   close all;
%! end_unwind_protect

%!testif HAVE_MAGICK
%! toolkit = graphics_toolkit ();
%! graphics_toolkit ("gnuplot");
%! unwind_protect
%!   h1 = figure ("visible", "off");
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
%!   s2 = hdl2struct (h2);
%!   png1 = strcat (tmpnam (), ".png");
%!   png2 = strcat (tmpnam (), ".png");
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

