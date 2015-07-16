## Copyright (C) 1999-2015 Kai Habel
## Copyright (C) 2015 Carnë Draug
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
## @deftypefn  {Function File} {@var{rgb_map} =} hsv2rgb (@var{hsv_map})
## @deftypefnx {Function File} {@var{rgb_img} =} hsv2rgb (@var{hsv_img})
## Transform a colormap or image from HSV to RGB color space.
##
## A color in HSV space is represented by hue, saturation and value
## (brightness) levels in a cylindrical coordinate system.  Hue is the
## azimuth and describes the dominant color.  Saturation is the radial
## distance and gives the amount of hue mixed into the color.  Value is
## the height and is the amount of light in the color.
##
## The input can be both a colormap or RGB image.  In the case of floating
## point input, values are expected to be on the [0 1] range.  In the case
## of hue (azimuth), since the value corresponds to an angle,
## @code{mod (h, 1)} is used.
##
## @example
## >> hsv2rgb ([0.5 1 1])
## @result{} ans = 0 1 1
##
## >> hsv2rgb ([2.5 1 1])
## @result{} ans = 0 1 1
##
## >> hsv2rgb ([3.5 1 1])
## @result{} ans = 0 1 1
## @end example
##
## Output class and size will be the same as input.
##
## @seealso{rgb2hsv, ind2rgb, ntsc2rgb}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function rgb = hsv2rgb (hsv)

  ## Each color value x = (r,g,b) is calculated with
  ## x = (1-sat)*val+sat*val*f_x(hue)
  ## where f_x(hue) is a piecewise defined function for
  ## each color with f_r(hue-2/3) = f_g(hue) = f_b(hue-1/3).

  if (nargin != 1)
    print_usage ();
  endif

  cls = class (hsv);
  ## If we have an image convert it into a color map.
  if (! iscolormap (hsv))
    if (! any (strcmp (cls, {"uint8", "uint16", "single", "double"})))
      error ("hsv2rgb: HSV of invalid data type '%s'", cls);
    elseif (size (hsv, 3) != 3)
      error ("hsv2rgb: HSV must be a colormap or HSV image");
    elseif (! isreal (hsv) || ! isnumeric (hsv))
      error ("hsv2rgb: HSV must be numeric and real");
    endif
    is_image = true;

    ## Allow for ND images, i.e., multiple images on the 4th dimension.
    sz = size (hsv);
    nd = ndims (hsv);
    if (nd == 3)
      is_ndimage = false;
    elseif (nd == 4)
      is_ndimage = true;
      hsv = permute (hsv, [1 2 4 3]);
    elseif (nd > 4)
      error ("hsv2rgb: invalid HSV with more than 4 dimensions");
    endif
    hsv = reshape (hsv, [numel(hsv)/3 3]);
  else
    is_image = false;
    is_ndimage = false;
  endif

  ## Convert to floating point (remember to leave class single alone)
  if (isinteger (hsv))
    hsv = double (hsv) / double (intmin (cls));
    is_uint = true;
  else
    is_uint = false;
  endif

  h = hsv(:,1);
  s = hsv(:,2);
  v = hsv(:,3);

  ## Values of Saturation and Value should also be in the [0 1] range.  With
  ## the exception of hue, values outside this range don't make any sense
  ## in a cylindrical coordinate system but we must return something for
  ## Matlab compatibility.  User case is when a function returns an hsv
  ## image just slightly outside the range due to floating point rounding
  ## errors.

  ## Prefill rgb map with v*(1-s)
  rgb = repmat (v .* (1 - s), 1, 3);

  ## red = hue-2/3 : green = hue : blue = hue-1/3
  ## Apply modulo 1 to keep within range [0, 1]
  hue = mod ([h-2/3  h  h-1/3], 1);

  ## factor s*v -> f
  f = repmat (s .* v, 1, 3);

  ## add s*v*hue-function to rgb map
  rgb += f .* (6 * (hue < 1/6) .* hue
               + (hue >= 1/6 & hue < 1/2)
               + (hue >= 1/2 & hue < 2/3) .* (4 - 6 * hue));

  if (is_image)
    if (is_ndimage)
      rgb = reshape (rgb, [sz(1:2) sz(4) sz(3)]);
      rgb = permute (rgb, [1 2 4 3]);
    else
      rgb = reshape (rgb, sz);
    endif
  endif

  if (is_uint)
    rgb *= intmax (cls);
  endif

endfunction

## Test pure colors
%!assert (hsv2rgb ([0 0 1]), [1 1 1])
%!assert (hsv2rgb ([1 1 0]), [0 0 0])
%!assert (hsv2rgb ([0 1 1]), [1 0 0])
%!assert (hsv2rgb ([1 1 1]), [1 0 0])
%!assert (hsv2rgb ([1/3 1 1]), [0 1 0])
%!assert (hsv2rgb ([2/3 1 1]), [0 0 1])

%!assert (hsv2rgb ([0 0.5 0.5]), hsv2rgb ([1 0.5 0.5]))

## Not Matlab compatible (Matlab would return [1/2  1/12  1/12]) but
## it's also invalid input.  This is, however, the same output as python
## colorsys module.
%!assert (hsv2rgb ([0.5 -0.5 0.5]), [0.75 0.5 0.5])

## Not Matlab compatible.  Matlab returns NaN when hue is outside the
## [0 1] range.  But since it's an angle, we can manage it.
%!assert (hsv2rgb ([0 0.5 0.5]), hsv2rgb ([2 0.5 0.5]))
%!assert (hsv2rgb ([0.5 0.5 0.5]), hsv2rgb ([2.5 0.5 0.5]), eps)

%!test
%! hsv_map = rand (64, 3);
%! assert (rgb2hsv (hsv2rgb (hsv_map)), hsv_map, 1e-6);

%!test
%! hsv_img = rand (64, 64, 3);
%! assert (rgb2hsv (hsv2rgb (hsv_img)), hsv_img, 1e-6);

## support sparse input
%!assert (hsv2rgb (sparse ([0 0 1])), sparse ([1 1 1]))
%!assert (hsv2rgb (sparse ([0 1 1])), sparse ([1 0 0]))
%!assert (hsv2rgb (sparse ([1 1 1])), sparse ([1 0 0]))

## Test input validation
%!error hsv2rgb ()
%!error hsv2rgb (1,2)
%!error <invalid data type> hsv2rgb ({1})
%!error <HSV must be a colormap or HSV image> hsv2rgb (ones (2,2))
