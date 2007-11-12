## Copyright (C) 2007 Kai Habel
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## OctPlot is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with OctPlot; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File}  spinmap (@var{T}, @var{inc})
## @deftypefnx {Function File}  spinmap (@var{T})
## @deftypefnx {Function File}  spinmap ()
## Cycles the colormap for @var{T} seconds with an increment
## of @var{inc}. Both parameter are optional. In that case 5s and an increment 
## of 2 is taken.
## A higher @var{inc} causes a faster cycle through the colormap.
## @end deftypefn
## @seealso{gca, colorbar}

## Author: Kai Habel <kai.habel at gmx.de>

function spinmap(T, inc)

  if (nargin == 0)
    inc = 2;
    T = 5;
  elseif (nargin == 1)
    inc = 2;
  endif

  cmap = get(gcf,"Colormap");
  clen = rows(cmap);

  t0 = clock;

  while (etime(clock, t0) < T)
    for n = 1 : inc : clen
      newmap = shift(cmap, n, 1);
      set(gcf,"Colormap",newmap)
      drawnow
    endfor
  end
  set(gcf,"Colormap",cmap)
end
