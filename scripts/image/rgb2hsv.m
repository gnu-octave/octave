## Copyright (C) 1999, 2000  Kai Habel
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
## @deftypefn {Function File} {} @var{hsv_map} = rgb2hsv (@var{rgb_map})
## Transform a colormap from the rgb space to the hsv space.
##
## A color n the RGB space consists of the red, green and blue intensities.
##
## In the HSV space each color is represented by their hue, saturation
## and value (brightness).  Value gives the amount of light in the color.
## Hue describes the dominant wavelegth. 
## Saturation is the amount of Hue mixed into the color. 
## @end deftypefn
## @seealso{hsv2rgb}

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function hsval = rgb2hsv (rgb)

  if (nargin != 1)
    usage ("hsv_map = rgb2hsv (rgb_map)");
  endif

  if (! is_matrix (rgb) || columns (rgb) != 3)
    error ("rgb2hsv: argument must be a matrix of size n x 3");
  endif

  # get saturation and value
  v = max (rgb');
  s = (v' > 0) .* (1 .- min (rgb') ./ v)';

  # if v==0 set s to 0 too
  s(isnan (s)) = 0;

  # subtract minimum and divide trough maximum
  # to get the bright and saturated colors
  sc = (rgb - kron ([1, 1, 1], min (rgb')'));
  sv = sc ./ kron([1, 1, 1], max (sc')');

  # if r=g=b (gray value) set hue to 0
  sv(isnan (sv)) = 0;

  # hue=f(color) must be splitted into 6 parts 
  # 2 for each color

  # h1(green)
  tmp = (sv(:, 1) == 1 & sv(:,3) == 0) .* (1/6 * sv(:,2) + eps);
  # avoid problems with h2(red) since hue(0)==hue(1)
  h = (tmp < 1/6) .* tmp; 
  # h2(green)
  h = h + ((h == 0) & sv(:,1) == 0 & sv(:,3) == 1) \
      .* (-1/6 * sv(:,2) + 2/3 + eps);

  # h1(red)
  h = h + ((h == 0) & sv(:,2) == 1 & sv(:,3) == 0) \
      .* (-1/6 * sv(:,1) + 1/3 + eps);

  # h2(red)
  h = h + ((h == 0) & sv(:,2) == 0 & sv(:,3) == 1) \
      .* (1/6 * sv(:,1) + 2/3 + eps);

  # h1(blue)
  h = h + ((h == 0) & sv(:,1) == 1 & sv(:,2) == 0) \
      .* (-1/6 * sv(:,3) + 1 + eps);

  # h2(blue)
  h = h + ((h == 0) & sv(:,1) == 0 & sv(:,2) == 1) \
      .* (1/6 * sv(:,3) + 1/3);

  hsval = [h, s, v'];

endfunction
