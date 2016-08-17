## Copyright (C) 1994-2016 John W. Eaton
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
## @deftypefn  {} {@var{yiq_map} =} rgb2ntsc (@var{rgb_map})
## @deftypefnx {} {@var{yiq_img} =} rgb2ntsc (@var{rgb_img})
## Transform a colormap or image from red-green-blue (RGB) color space to
## luminance-chrominance (NTSC) space.  The input may be of class uint8,
## uint16, single, or double.  The output is of class double.
##
## Implementation Note:
## The reference matrix for the transformation is
##
## @example
## @group
## /Y\     0.299  0.587  0.114  /R\
## |I|  =  0.596 -0.274 -0.322  |G|
## \Q/     0.211 -0.523  0.312  \B/
## @end group
## @end example
##
## @noindent
## as documented in @url{http://en.wikipedia.org/wiki/YIQ} and truncated to 3
## significant figures.  Note: The FCC version of NTSC uses only 2 significant
## digits and is slightly different.
## @seealso{ntsc2rgb, rgb2hsv, rgb2ind}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function yiq = rgb2ntsc (rgb)

  if (nargin != 1)
    print_usage ();
  endif

  [rgb, sz, is_im, is_nd] ...
    = colorspace_conversion_input_check ("rgb2ntsc", "RGB", rgb);

  ## Reference matrix for transformation from http://en.wikipedia.org/wiki/YIQ
  ## and truncated to 3 significant figures.  Matlab uses this matrix for their
  ## conversion.
  trans = [ 0.299,  0.596,  0.211;
            0.587, -0.274, -0.523;
            0.114, -0.322,  0.312 ];
  yiq = rgb * trans;
  ## Note that if the input is of class single, we also return an image
  ## of class single.  This is Matlab incompatible by design, since
  ## Matlab always returning class double, is a Matlab bug (see patch #8709)

  yiq = colorspace_conversion_revert (yiq, sz, is_im, is_nd);

endfunction


## Test pure RED, GREEN, BLUE colors
%!assert (rgb2ntsc ([1 0 0]), [.299  .596  .211])
%!assert (rgb2ntsc ([0 1 0]), [.587 -.274 -.523])
%!assert (rgb2ntsc ([0 0 1]), [.114 -.322  .312])

%!test
%! rgb_map = rand (64, 3);
%! assert (ntsc2rgb (rgb2ntsc (rgb_map)), rgb_map, 1e-3);

%!test
%! rgb_img = rand (64, 64, 3);
%! assert (ntsc2rgb (rgb2ntsc (rgb_img)), rgb_img, 1e-3);

## test tolerance input checking on floats
%! assert (rgb2ntsc ([1.5 1 1]), [1.149   0.298   0.105], 1e-3);

## Test input validation
%!error rgb2ntsc ()
%!error rgb2ntsc (1,2)
%!error <invalid data type 'cell'> rgb2ntsc ({1})
%!error <RGB must be a colormap or RGB image> rgb2ntsc (ones (2,2))

## Test ND input
%!test
%! rgb = rand (16, 16, 3, 5);
%! yiq = zeros (size (rgb));
%! for i = 1:5
%!   yiq(:,:,:,i) = rgb2ntsc (rgb(:,:,:,i));
%! endfor
%! assert (rgb2ntsc (rgb), yiq);

## Test output class and size for input images.
## Most of the tests only test for colormap input.

%!test
%! ntsc = rgb2ntsc (rand (10, 10, 3));
%! assert (class (ntsc), "double");
%! assert (size (ntsc), [10 10 3]);

%!test
%! ntsc = rgb2ntsc (rand (10, 10, 3, "single"));
%! assert (class (ntsc), "single");
%! assert (size (ntsc), [10 10 3]);

%!test
%! rgb = (rand (10, 10, 3) * 3 ) - 0.5; # values outside range [0 1]
%! ntsc = rgb2ntsc (rgb);
%! assert (class (ntsc), "double");
%! assert (size (ntsc), [10 10 3]);

%!test
%! rgb = (rand (10, 10, 3, "single") * 3 ) - 0.5; # values outside range [0 1]
%! ntsc = rgb2ntsc (rgb);
%! assert (class (ntsc), "single");
%! assert (size (ntsc), [10 10 3]);

%!test
%! ntsc = rgb2ntsc (randi ([0 255], 10, 10, 3, "uint8"));
%! assert (class (ntsc), "double");
%! assert (size (ntsc), [10 10 3]);

%!test
%! ntsc = rgb2ntsc (randi ([0 65535], 10, 10, 3, "uint16"));
%! assert (class (ntsc), "double");
%! assert (size (ntsc), [10 10 3]);

%!test
%! ntsc = rgb2ntsc (randi ([-128 127], 10, 10, 3, "int8"));
%! assert (class (ntsc), "double");
%! assert (size (ntsc), [10 10 3]);

%!test
%! rgb_double = reshape ([1 0 0 0 0 1 0 0 0 0 1 0], [2 2 3]);
%! rgb_uint8  = reshape (uint8 ([255 0 0 0 0 255 0 0 0 0 255 0]),
%!                       [2 2 3]);
%! rgb_int16 = int16 (double (rgb_double * uint16 (65535)) -32768);
%! expected = reshape ([.299 .587 .114 0 .596 -.274 -.322 0 .211 -.523 .312 0],
%!                     [2 2 3]);
%!
%! assert (rgb2ntsc (rgb_double), expected);
%! assert (rgb2ntsc (rgb_uint8), expected);
%! assert (rgb2ntsc (single (rgb_double)), single (expected));
