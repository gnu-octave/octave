## Copyright (C) 1994-2012 John W. Eaton
## Copyright (C) 2012 Carnë Draug
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

## private function for the ind2something functions which have a lot of code
## in common

function [x, map] = ind2x (name, x, map)

  ## Check if X is an indexed image.
  if (ndims (x) != 2 || issparse (x) || (isfloat (x) && ! isindex (x)) ||
      ! ismember (class (x), {"double", "single", "uint8", "uint16"}))
    error ("%s: X must be an indexed image", name);
  endif

  ## Check the color map.
  if (! iscolormap (map))
    error ("%s: MAP must be a valid colormap", name);
  endif

  ## Do we have enough colors in the color map?
  ## there's an offset of 1 when the indexed image is an integer class so we fix
  ## it now and convert it to float only if really necessary and even then only
  ## to single precision since its enough for both uint8 and uint16
  maxidx = max (x(:));
  if (isinteger (x))
    if (maxidx == intmax (class (x)))
      x = single (x);
    endif
    x      += 1;
    maxidx += 1;
  endif

  rm = rows (map);
  if (rm < maxidx)
    ## Pad with the last color in the map.
    pad = repmat (map(end,:), maxidx-rm, 1);
    map(end+1:maxidx, :) = pad;
  endif

endfunction
