## Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2004, 2005,
##               2006, 2007 John W. Eaton
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
## @deftypefn  {Function File} {@var{rgb} =} ind2rgb (@var{x}, @var{map})
## @deftypefnx {Function File} {[@var{r}, @var{g}, @var{b}] =} ind2rgb (@var{x}, @var{map})
## Convert an indexed image to red, green, and blue color components.
## If the colormap doesn't contain enough colors, pad it with the
## last color in the map.
## If @var{map} is omitted, the current colormap is used for the conversion.
## @seealso{rgb2ind, image, imshow, ind2gray, gray2ind}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [R, G, B] = ind2rgb (X, map)

  ## Do we have the right number of inputs?
  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (nargin == 1)
    map = colormap ();
  endif

  ## Check if X is an indexed image.
  if (ndims (X) != 2 || any (X(:) != round (X(:))) || min (X(:)) < 1)
    error ("ind2rgb: first input argument must be an indexed image");
  endif
  
  ## Check the color map.
  if (ndims (map) != 2 || columns (map) != 3)
    error ("ind2rgb: second input argument must be a color map");
  endif

  ## Do we have enough colors in the color map?
  maxidx = max (X(:));
  rm = rows (map);
  if (rm < maxidx)
    ## Pad with the last color in the map.
    pad = repmat (map(end,:), maxidx-rm, 1);
    map(end+1:maxidx, :) = pad;
  endif
  
  ## Compute result
  [hi, wi] = size (X);
  R = reshape (map (X(:), 1), hi, wi);
  G = reshape (map (X(:), 2), hi, wi);
  B = reshape (map (X(:), 3), hi, wi);

  ## Use 3D array if only one output is requested.
  if (nargout <= 1)
    R(:,:,3) = B;
    R(:,:,2) = G;
  endif
endfunction
