## Copyright (C) 2007-2012 Kai Habel
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
## @deftypefn {Function File} {} spinmap (@var{t}, @var{inc})
## Cycle the colormap for @var{t} seconds with an increment
## of @var{inc}.  Both parameters are optional.  The default cycle time
## is 5 seconds and the default increment is 2.
##
## A higher value of @var{inc} causes a faster cycle through the
## colormap.
## @seealso{gca, colorbar}
## @end deftypefn

## Author: Kai Habel <kai.habel at gmx.de>

function spinmap (t, inc)

  if (nargin == 0)
    inc = 2;
    t = 5;
  elseif (nargin == 1)
    inc = 2;
  endif

  cmap = get (gcf (), "colormap");
  clen = rows (cmap);

  t0 = clock;

  while (etime (clock, t0) < t)
    for n = 1:inc:clen
      newmap = shift (cmap, n, 1);
      set (gcf (), "colormap", newmap);
      drawnow ();
    endfor
  endwhile

  set (gcf (), "colormap", cmap);

endfunction

