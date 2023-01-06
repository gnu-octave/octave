########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{I} =} rgb2gray (@var{rgb_img})
## @deftypefnx {} {@var{gray_map} =} rgb2gray (@var{rgb_map})
## Transform an image or colormap from red-green-blue (RGB) color space to
## a grayscale intensity image.
##
## The input may be of class uint8, int8, uint16, int16, single, or double.
## The output is of the same class as the input.
##
## Implementation Note:
## The grayscale intensity is calculated as
##
## @example
## @group
## @var{I} = 0.298936*@var{R} + 0.587043*@var{G} + 0.114021*@var{B}
## @end group
## @end example
##
## @noindent
## which corresponds to the luminance channel when RGB is translated to
## @nospell{YIQ} as documented in @url{https://en.wikipedia.org/wiki/YIQ}.
## @seealso{rgb2hsv, rgb2ind}
## @end deftypefn

function I = rgb2gray (rgb)

  if (nargin < 1)
    print_usage ();
  endif

  is_int = isinteger (rgb);
  if (is_int)
    cls = class (rgb);
  endif
  [rgb, sz, is_im, is_nd] ...
    = colorspace_conversion_input_check ("rgb2gray", "RGB", rgb);

  ## Reference matrix for transform from http://en.wikipedia.org/wiki/YIQ.
  ## Matlab uses this matrix for their conversion with oddly more precision.
  xform = [0.298936; 0.587043; 0.114021];
  I = rgb * xform;

  sz(3) = 1; # grayscale images have 3rd dimension of length 1
  I = colorspace_conversion_revert (I, sz, is_im, is_nd);

  ## Restore integer class if necessary
  if (is_int)
    if (cls(end) == "8")  # uint8 or int8
      I *= 255;
      if (cls(1) == "i")  # int8
        I -= 128;
      endif
    else                  # uint16 or int16
      I *= 65535;
      if (cls(1) == "i")  # int16
        I -= 32768;
      endif
    endif
    I = feval (cls, I);
  endif

endfunction


## Test pure RED, GREEN, BLUE colors
%!assert (rgb2gray ([1 0 0]), 0.298936)
%!assert (rgb2gray ([0 1 0]), 0.587043)
%!assert (rgb2gray ([0 0 1]), 0.114021)

## test tolerance input checking on floats
%! assert (rgb2gray ([1.5 1 1]), 1.149468, -1.6e-3);

## Test ND input
%!test
%! rgb = rand (16, 16, 3, 5);
%! I = zeros (16, 16, 1, 5);
%! for i = 1:5
%!   I(:,:,1,i) = rgb2gray (rgb(:,:,:,i));
%! endfor
%! assert (rgb2gray (rgb), I);

## Test output class and size for input images.
## Most of the tests only test for colormap input.

%!test
%! I = rgb2gray (rand (10, 10, 3));
%! assert (class (I), "double");
%! assert (size (I), [10 10]);

%!test
%! I = rgb2gray (rand (10, 10, 3, "single"));
%! assert (class (I), "single");
%! assert (size (I), [10 10]);

%!test
%! rgb = (rand (10, 10, 3) * 3 ) - 0.5; # values outside range [0 1]
%! I = rgb2gray (rgb);
%! assert (class (I), "double");
%! assert (size (I), [10 10]);

%!test
%! rgb = (rand (10, 10, 3, "single") * 3 ) - 0.5; # values outside range [0 1]
%! I = rgb2gray (rgb);
%! assert (class (I), "single");
%! assert (size (I), [10 10]);

%!test
%! I = rgb2gray (randi ([0 255], 10, 10, 3, "uint8"));
%! assert (class (I), "uint8");
%! assert (size (I), [10 10]);

%!test
%! I = rgb2gray (randi ([0 65535], 10, 10, 3, "uint16"));
%! assert (class (I), "uint16");
%! assert (size (I), [10 10]);

%!test
%! I = rgb2gray (randi ([-128 127], 10, 10, 3, "int8"));
%! assert (class (I), "int8");
%! assert (size (I), [10 10]);

%!test
%! I = rgb2gray (randi ([-32768 32767], 10, 10, 3, "int16"));
%! assert (class (I), "int16");
%! assert (size (I), [10 10]);

%!test
%! rgb_double = reshape ([1 0 0 0 0 1 0 0 0 0 1 0], [2 2 3]);
%! rgb_uint8  = reshape (uint8 ([255 0 0 0 0 255 0 0 0 0 255 0]),
%!                       [2 2 3]);
%! rgb_int16 = int16 (double (rgb_double * uint16 (65535)) - 32768);
%! expected = [0.298936, 0.114021; 0.587043, 0.0];
%!
%! assert (rgb2gray (rgb_double), expected);
%! assert (rgb2gray (rgb_uint8), uint8 (expected*255));
%! assert (rgb2gray (single (rgb_double)), single (expected));

## Test input validation
%!error <Invalid call> rgb2gray ()
%!error <invalid data type 'cell'> rgb2gray ({1})
%!error <RGB must be a colormap or RGB image> rgb2gray (ones (2,2))
