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

## private function for the ind2XXX functions which have a lot of code in common

function [x, map] = ind2x (caller, x, map)

  ## Check if X is an indexed image.
  if (ndims (x) < 2 || issparse (x) || (isfloat (x) && ! isindex (x)) ||
      ! any (strcmp (class (x), {"uint8", "uint16", "single", "double"})))
    error ("%s: X must be an indexed image", caller);
  endif

  ## Check if map is a valid colormap.
  if (! iscolormap (map))
    error ("%s: MAP must be a valid colormap", caller);
  endif

  ## Do we have enough colors in the color map?
  ## there's an offset of 1 when the indexed image is an integer class so we fix
  ## it now and convert it to float only if really necessary and even then only
  ## to single precision since that is enough for both uint8 and uint16.
  maxidx = max (x(:));
  if (isinteger (x))
    if (maxidx == intmax (class (x)))
      x = single (x);
    endif
    x      += 1;
    maxidx += 1;
  endif

  num_colors = rows (map);
  if (num_colors < maxidx)
    ## Pad with the last color in the map for matlab compatibility
    pad = repmat (map(end,:), maxidx - num_colors, 1);
    map(end+1:maxidx, :) = pad;
  endif

endfunction
