########################################################################
##
## Copyright (C) 1999-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{rgb_map} =} hsv2rgb (@var{hsv_map})
## @deftypefnx {} {@var{rgb_img} =} hsv2rgb (@var{hsv_img})
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
## @group
## >> hsv2rgb ([0.5 1 1])
## @result{} ans = 0 1 1
##
## >> hsv2rgb ([2.5 1 1])
## @result{} ans = 0 1 1
##
## >> hsv2rgb ([3.5 1 1])
## @result{} ans = 0 1 1
## @end group
## @end example
##
## Output class and size will be the same as input.
##
## @seealso{rgb2hsv, ind2rgb}
## @end deftypefn

function rgb = hsv2rgb (hsv)

  ## Each color value x = (r,g,b) is calculated with
  ## x = (1-sat)*val+sat*val*f_x(hue)
  ## where f_x(hue) is a piecewise defined function for
  ## each color with f_r(hue-2/3) = f_g(hue) = f_b(hue-1/3).

  if (nargin < 1)
    print_usage ();
  endif

  [hsv, sz, is_im, is_nd] ...
    = colorspace_conversion_input_check ("hsv2rgb", "HSV", hsv);

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

  rgb = colorspace_conversion_revert (rgb, sz, is_im, is_nd);

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
## it's also invalid input.  This is, however, the same output as Python
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
%!error <Invalid call> hsv2rgb ()
%!error <invalid data type> hsv2rgb ({1})
%!error <HSV must be a colormap or HSV image> hsv2rgb (ones (2,2))

## Test ND input
%!test
%! hsv = rand (16, 16, 3, 5);
%! rgb = zeros (size (hsv));
%! for i = 1:5
%!   rgb(:,:,:,i) = hsv2rgb (hsv(:,:,:,i));
%! endfor
%! assert (hsv2rgb (hsv), rgb);

## Test output class and size for input images.
## Most of the tests only test for colormap input.

%!test
%! rgb = hsv2rgb (rand (10, 10, 3));
%! assert (class (rgb), "double");
%! assert (size (rgb), [10 10 3]);

%!test
%! rgb = hsv2rgb (rand (10, 10, 3, "single"));
%! assert (class (rgb), "single");
%! assert (size (rgb), [10 10 3]);

%!test
%! rgb = (rand (10, 10, 3) * 3 ) - 0.5; # values outside range [0 1]
%! rgb = hsv2rgb (rgb);
%! assert (class (rgb), "double");
%! assert (size (rgb), [10 10 3]);

%!test
%! rgb = (rand (10, 10, 3, "single") * 3 ) - 0.5; # values outside range [0 1]
%! rgb = hsv2rgb (rgb);
%! assert (class (rgb), "single");
%! assert (size (rgb), [10 10 3]);

%!test
%! rgb = hsv2rgb (randi ([0 255], 10, 10, 3, "uint8"));
%! assert (class (rgb), "double");
%! assert (size (rgb), [10 10 3]);

%!test
%! rgb = hsv2rgb (randi ([0 65535], 10, 10, 3, "uint16"));
%! assert (class (rgb), "double");
%! assert (size (rgb), [10 10 3]);

%!test
%! rgb = hsv2rgb (randi ([-128 127], 10, 10, 3, "int8"));
%! assert (class (rgb), "double");
%! assert (size (rgb), [10 10 3]);

%!test
%! hsv_double = reshape ([2/3 1/3 1 0 1 1 1 0, 1 1 1 1], [2 2 3]);
%! hsv_uint8  = reshape (uint8 ([170 85 255 0 255 255 255 0 255 255 255 255]),
%!                       [2 2 3]);
%! hsv_int16 = int16 (double (hsv_double * uint16 (65535)) -32768);
%! expected = reshape ([0 0 1 1 0 1 0 1 1 0 0 1], [2 2 3]);
%!
%! assert (hsv2rgb (hsv_double), expected);
%! assert (hsv2rgb (hsv_uint8), expected);
%! assert (hsv2rgb (hsv_int16), expected);
%! assert (hsv2rgb (single (hsv_double)), single (expected), eps (single (2)));
