## Copyright (C) 1999-2012 Kai Habel
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
## @deftypefn {Function File} {@var{hsv_map} =} rgb2hsv (@var{rgb})
## Transform a colormap or image from the RGB space to the HSV space.
##
## A color in the RGB space consists of the red, green and blue intensities.
##
## In the HSV space each color is represented by their hue, saturation
## and value (brightness).  Value gives the amount of light in the color.
## Hue describes the dominant wavelength.
## Saturation is the amount of hue mixed into the color.
## @seealso{hsv2rgb}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function hsv_map = rgb2hsv (rgb)

  if (nargin != 1)
    print_usage ();
  endif

  ## If we have an image convert it into a color map.
  if (ismatrix (rgb) && ndims (rgb) == 3)
    is_image = true;
    Sz = size (rgb);
    rgb = [rgb(:,:,1)(:), rgb(:,:,2)(:), rgb(:,:,3)(:)];
    ## Convert to a double image.
    if (isinteger (rgb))
      C = class (rgb);
      low = double (intmin (C));
      high = double (intmax (C));
      rgb = (double (rgb) - low) / (high - low);
    endif
  else
    is_image = false;
  endif

  if (! ismatrix (rgb) || columns (rgb) != 3)
    error ("rgb2hsv: RGB_MAP must be a matrix of size n x 3");
  endif

  ## get the max and min
  s = min (rgb')';
  v = max (rgb')';

  ## set hue to zero for undefined values (gray has no hue)
  h = zeros (size (v));
  notgray = (s != v);

  ## blue hue
  idx = (v == rgb(:,3) & notgray);
  if (any (idx))
    h(idx) = 2/3 + 1/6 * (rgb(idx,1) - rgb(idx,2)) ./ (v(idx) - s(idx));
  endif

  ## green hue
  idx = (v == rgb(:,2) & notgray);
  if (any (idx))
    h(idx) = 1/3 + 1/6 * (rgb(idx,3) - rgb(idx,1)) ./ (v(idx) - s(idx));
  endif

  ## red hue
  idx = (v == rgb(:,1) & notgray);
  if (any (idx))
    h(idx) =       1/6 * (rgb(idx,2) - rgb(idx,3)) ./ (v(idx) - s(idx));
  endif

  ## correct for negative red
  idx = (h < 0);
  h(idx) = 1+h(idx);

  ## set the saturation
  s(! notgray) = 0;
  s(notgray) = 1 - s(notgray) ./ v(notgray);

  hsv_map = [h, s, v];

  ## If input was an image, convert it back into one.
  if (is_image)
    hsv_map = reshape (hsv_map, Sz);
  endif

endfunction
