## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} imshow (@var{x}, @var{map})
## @deftypefnx {Function File} {} imshow (@var{x}, @var{n})
## @deftypefnx {Function File} {} imshow (@var{i}, @var{n})
## @deftypefnx {Function File} {} imshow (@var{r}, @var{g}, @var{b})
## Display images.
##
## @code{imshow (@var{x})} displays an indexed image using the current
## colormap.
##
## @code{imshow (@var{x}, @var{map})} displays an indexed image using the
## specified colormap.
##
## @code{imshow (@var{i}, @var{n})} displays a gray scale intensity image.
##
## @code{imshow (@var{r}, @var{g}, @var{b})} displays an RGB image.
## @end deftypefn

## SEE ALSO: image, imagesc, colormap, gray2ind, rgb2ind.

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function imshow (a1, a2, a3)

  if (nargin < 0 || nargin > 3)
    usage ("imshow (args)");
  elseif (nargin == 2)
    if (length (a2) == 1)
      [a1, a2] = gray2ind (a1, a2);
    endif
    colormap (a2);
  elseif (nargin == 3)
    [a1, a2] = rgb2ind (a1, a2, a3);
    colormap (a2);
  endif

  image (a1);

endfunction
