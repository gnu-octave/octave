## Copyright (C) 1999,2000  Kai Habel
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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{rgb_map} =} hsv2rgb (@var{hsv_map})
## Transform a colormap from the hsv space to the rgb space. 
## @seealso{rgb2hsv}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function rgb_map = hsv2rgb (hsv_map)

## each color value x=(r,g,b) is calculated with
## x = (1-sat)*val+sat*val*f_x(hue)
## where f_x(hue) is a piecewise defined function for
## each color with f_r(hue-2/3) = f_g(hue) = f_b(hue-1/3)

  if (nargin != 1)
    print_usage ();
  endif

  if (! ismatrix (hsv_map) || columns (hsv_map) != 3)
    error ("hsv2rgb: argument must be a matrix of size nx3");
  endif

  ## set values <0 to 0 and >1 to 1
  hsv_map = (hsv_map >= 0 & hsv_map <= 1) .* hsv_map \
      + (hsv_map < 0) .* 0 + (hsv_map > 1);

  ## fill rgb map with v*(1-s)
  rgb_map = kron ([1, 1, 1], hsv_map(:,3) .* (1 - hsv_map(:,2)));

  ## red(hue-2/3)=green(hue)=blue(hue-1/3)
  ## apply modulo 1 for red and blue 
  t = hsv_map(:,1);
  tp = t';
  hue = [(tp - 2/3 - floor (t - 2/3)');
         tp;
         (tp - 1/3 - floor (t - 1/3)')]';

  ## factor s*v -> f
  f = kron ([1, 1, 1], hsv_map(:,2)) .* kron ([1, 1, 1], hsv_map(:,3));

  ## add s*v* hue-function to rgb map
  rgb_map = rgb_map +  f .* (6 * (hue < 1/6) .* hue
			     + (hue >= 1/6 & hue < 1/2)
			     + (hue >= 1/2 & hue < 2/3) .* (4 - 6 * hue));

endfunction
