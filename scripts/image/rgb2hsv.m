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
## @deftypefn  {} {@var{hsv_map} =} rgb2hsv (@var{rgb_map})
## @deftypefnx {} {@var{hsv_img} =} rgb2hsv (@var{rgb_img})
## Transform a colormap or image from RGB to HSV color space.
##
## A color in the RGB space consists of red, green, and blue intensities.
##
## A color in HSV space is represented by hue, saturation and value
## (brightness) levels in a cylindrical coordinate system.  Hue is the
## azimuth and describes the dominant color.  Saturation is the radial
## distance and gives the amount of hue mixed into the color.  Value is
## the height and is the amount of light in the color.
##
## Output class and size will be the same as input.
##
## @seealso{hsv2rgb, rgb2ind, rgb2gray}
## @end deftypefn

function hsv = rgb2hsv (rgb)

  if (nargin < 1)
    print_usage ();
  endif

  [rgb, sz, is_im, is_nd] ...
    = colorspace_conversion_input_check ("rgb2hsv", "RGB", rgb);

  ## get the max and min for each row
  s = min (rgb, [], 2);
  v = max (rgb, [], 2);

  ## set hue to zero for undefined values (gray has no hue)
  h = zeros (rows (rgb), 1);
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
  h(h < 0) += 1;   # correct for negative red

  ## set the saturation
  s(! notgray) = 0;
  s(notgray) = 1 - s(notgray) ./ v(notgray);

  hsv = [h, s, v];
  hsv = colorspace_conversion_revert (hsv, sz, is_im, is_nd);

endfunction


## Test pure colors and gray
%!assert (rgb2hsv ([1 0 0]), [0 1 1])
%!assert (rgb2hsv ([0 1 0]), [1/3 1 1])
%!assert (rgb2hsv ([0 0 1]), [2/3 1 1])
%!assert (rgb2hsv ([1 1 0]), [1/6 1 1])
%!assert (rgb2hsv ([0 1 1]), [1/2 1 1])
%!assert (rgb2hsv ([1 0 1]), [5/6 1 1])
%!assert (rgb2hsv ([0.5 0.5 0.5]), [0 0 0.5])

## Test tolarant input checking on floats
%!assert (rgb2hsv ([1.5 1 1]), [0 1/3 1.5], eps)

%!test
%! rgb_map = rand (64, 3);
%! assert (hsv2rgb (rgb2hsv (rgb_map)), rgb_map, 1e-6);

%!test
%! rgb_img = rand (64, 64, 3);
%! assert (hsv2rgb (rgb2hsv (rgb_img)), rgb_img, 1e-6);

## support sparse input
%!assert (rgb2hsv (sparse ([0 0 1])), sparse ([2/3 1 1]))
%!assert (rgb2hsv (sparse ([0 1 1])), sparse ([1/2 1 1]))
%!assert (rgb2hsv (sparse ([1 1 1])), sparse ([0 0 1]))

## Test input validation
%!error <Invalid call> rgb2hsv ()
%!error <invalid data type 'cell'> rgb2hsv ({1})
%!error <RGB must be a colormap or RGB image> rgb2hsv (ones (2,2))

## Test ND input
%!test
%! rgb = rand (16, 16, 3, 5);
%! hsv = zeros (size (rgb));
%! for i = 1:5
%!   hsv(:,:,:,i) = rgb2hsv (rgb(:,:,:,i));
%! endfor
%! assert (rgb2hsv (rgb), hsv);

## Test output class and size for input images.
## Most of the tests only test for colormap input.

%!test
%! hsv = rgb2hsv (rand (10, 10, 3));
%! assert (class (hsv), "double");
%! assert (size (hsv), [10 10 3]);

%!test
%! hsv = rgb2hsv (rand (10, 10, 3, "single"));
%! assert (class (hsv), "single");
%! assert (size (hsv), [10 10 3]);

%!test
%! rgb = (rand (10, 10, 3) * 3 ) - 0.5; # values outside range [0 1]
%! hsv = rgb2hsv (rgb);
%! assert (class (hsv), "double");
%! assert (size (hsv), [10 10 3]);

%!test
%! rgb = (rand (10, 10, 3, "single") * 3 ) - 0.5; # values outside range [0 1]
%! hsv = rgb2hsv (rgb);
%! assert (class (hsv), "single");
%! assert (size (hsv), [10 10 3]);

%!test
%! hsv = rgb2hsv (randi ([0 255], 10, 10, 3, "uint8"));
%! assert (class (hsv), "double");
%! assert (size (hsv), [10 10 3]);

%!test
%! hsv = rgb2hsv (randi ([0 65535], 10, 10, 3, "uint16"));
%! assert (class (hsv), "double");
%! assert (size (hsv), [10 10 3]);

%!test
%! hsv = rgb2hsv (randi ([-128 127], 10, 10, 3, "int8"));
%! assert (class (hsv), "double");
%! assert (size (hsv), [10 10 3]);

%!test
%! rgb_double = reshape ([1 0 1 .5 1 1 0 .5 0 1 1 .5], [2 2 3]);
%! rgb_uint8  = reshape (uint8 ([255 0 255 128 255 255 0 128 0 255 255 128]),
%!                       [2 2 3]);
%! rgb_int16 = int16 (double (rgb_double * uint16 (65535)) -32768);
%! expected = reshape ([1/6 1/2 5/6 0 1 1 1 0 1 1 1 .5], [2 2 3]);
%!
%! assert (rgb2hsv (rgb_double), expected);
%! assert (rgb2hsv (rgb_uint8), expected, 0.005);
%! assert (rgb2hsv (single (rgb_double)), single (expected));
