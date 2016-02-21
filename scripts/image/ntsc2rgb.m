## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {} {@var{rgb_map} =} ntsc2rgb (@var{yiq_map})
## @deftypefnx {} {@var{rgb_img} =} ntsc2rgb (@var{yiq_img})
## Transform a colormap or image from luminance-chrominance (NTSC) space to
## red-green-blue (RGB) color space.
##
## Implementation Note:
## The conversion matrix is chosen to be the inverse of the matrix used for
## rgb2ntsc such that
##
## @example
## x == ntsc2rgb (rgb2ntsc (x))
## @end example
##
## @sc{matlab} uses a slightly different matrix where rounding means the
## equality above does not hold.
## @seealso{rgb2ntsc, hsv2rgb, ind2rgb}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function rgb = ntsc2rgb (yiq)

  if (nargin != 1)
    print_usage ();
  endif

  ## Unlike other colorspace conversion functions, we do not accept
  ## integers as valid input.  We check this before
  ## colorspace_conversion_input_check() which is general and would
  ## convert integers to double assuming a [0 1] interval range.
  ## The reason for not supporting integers here is that there's no
  ## common such conversion.  If we were to support a conversion
  ## the most reasonable definition would be to convert the YIQ
  ## from their integer range into the ranges:
  ##    Y = [ 0      1.106]
  ##    I = [-0.797  0.587]
  ##    Q = [-0.322  0.426]
  ## See https://savannah.gnu.org/patch/?8709#comment11
  if (! isfloat (yiq))
    error ("ntsc2rgb: YIQ must be of floating point class");
  endif
  [yiq, sz, is_im, is_nd] ...
    = colorspace_conversion_input_check ("ntsc2rgb", "YIQ", yiq);

  ## Conversion matrix constructed from 'inv (rgb2ntsc matrix)'.
  ## See programming notes in rgb2ntsc.m.  Note: Matlab matrix for inverse
  ## is slightly different.  We prefer this matrix so that
  ## x == ntsc2rgb (rgb2ntsc (x)) rather than maintaining strict compatibility
  ## with Matlab.
  trans = [ 1.0,      1.0,      1.0;
            0.95617, -0.27269, -1.10374;
            0.62143, -0.64681,  1.70062 ];
  rgb = yiq * trans;
  ## Note that if the input is of class single, we also return an image
  ## of class single.  This is Matlab incompatible by design, since
  ## Matlab always returning class double, is a Matlab bug (see patch #8709)

  ## truncating / scaling of double rgb values for Matlab compatibility
  rgb = max (0, rgb);
  idx = any (rgb > 1, 2);
  rgb(idx,:) = rgb(idx,:) ./ max (rgb(idx,:), [], 2);

  rgb = colorspace_conversion_revert (rgb, sz, is_im, is_nd);
endfunction


%!shared trans
%! trans = [ 1.0,      1.0,      1.0;
%!          0.95617, -0.27269, -1.10374;
%!          0.62143, -0.64681,  1.70062 ];

## Test pure R, G, B colors
%!assert (ntsc2rgb ([.299  .596  .211]), [1 0 0], 1e-5)
%!assert (ntsc2rgb ([.587 -.274 -.523]), [0 1 0], 1e-5)
%!assert (ntsc2rgb ([.114 -.322  .312]), [0 0 1], 1e-5)

%!test
%! rgb_map = rand (64, 3);
%! assert (ntsc2rgb (rgb2ntsc (rgb_map)), rgb_map, 1e-3);

%!test
%! rgb_img = rand (64, 64, 3);
%! assert (ntsc2rgb (rgb2ntsc (rgb_img)), rgb_img, 1e-3);

## test cropping of rgb output
%!assert (ntsc2rgb ([1.5 0 0]), [1   1   1])

## Test scaling of output.  After conversion, cut of negative values
## and scaling of all the others relative to the maximum above 1.
%!test
%! ntsc = [0.4229  0.0336  0.7184];
%! rgb = ntsc * trans;    # [0.9014  -0.0509  1.6075]
%! rgb(1) /= rgb(3); # scaled based on the maximum
%! rgb(2) = 0; # cut to 0
%! rgb(3) = 1; # cut to 1
%! assert (ntsc2rgb (ntsc), rgb)

## test scaling when conversion has more than one value above 1
## (check that it does pick the maximum)
%!test
%! ntsc = [0.8229  0.3336  0.7184];
%! rgb = ntsc * trans;    # [1.58831   0.26726   1.67642]
%! rgb /= rgb(3);
%! assert (ntsc2rgb (ntsc), rgb)

## check scaling for more than 1 row
%!test
%! ntsc = [0.4229  0.0336  0.7184
%!         0.8229  0.3336  0.7184];
%! rgb = ntsc * trans; # [0.9014  -0.0509  1.6075;  1.58831  0.26726  1.67642]
%! rgb(1,1) /= rgb(1,3);
%! rgb(1,2) = 0;
%! rgb(1,3) = 1;
%! rgb(2,:) /= rgb(2,3);
%! assert (ntsc2rgb (ntsc), rgb)

## Test input validation
%!error ntsc2rgb ()
%!error ntsc2rgb (1,2)
%!error <YIQ must be of floating point class> ntsc2rgb (uint8 (1))
%!error <YIQ must be a colormap or YIQ image> ntsc2rgb (ones (2,2))
%!error <YIQ must be of floating point class> ntsc2rgb (ones ([10 10 3], "uint8"))
%!error <YIQ must be of floating point class> ntsc2rgb (ones ([10 10 3], "uint16"))
%!error <YIQ must be of floating point class> ntsc2rgb (ones ([10 10 3], "int16"))

## Test ND input
%!test
%! yiq = rand (16, 16, 3, 5);
%! rgb = zeros (size (yiq));
%! for i = 1:5
%!   rgb(:,:,:,i) = ntsc2rgb (yiq(:,:,:,i));
%! endfor
%! assert (ntsc2rgb (yiq), rgb)

## Test output class and size for input images.
## Most of the tests only test for colormap input.

%!test
%! rgb = ntsc2rgb (rand (10, 10, 3));
%! assert (class (rgb), "double")
%! assert (size (rgb), [10 10 3])

%!test
%! rgb = ntsc2rgb (rand (10, 10, 3, "single"));
%! assert (class (rgb), "single")
%! assert (size (rgb), [10 10 3])

%!test
%! ntsc = (rand (10, 10, 3) * 3 ) - 0.5; # values outside range [0 1]
%! rgb = ntsc2rgb (ntsc);
%! assert (class (rgb), "double")
%! assert (size (rgb), [10 10 3])

%!test
%! ntsc = (rand (10, 10, 3, "single") * 3 ) - 0.5; # values outside range [0 1]
%! rgb = ntsc2rgb (ntsc);
%! assert (class (rgb), "single")
%! assert (size (rgb), [10 10 3])

%!test
%! ntsc_double = reshape ([.299 .587 .114 0 .596 -.274 -.322 0 .211 -.523 .312 0],
%!                        [2 2 3]);
%! expected = reshape ([1 0 0 0 0 1 0 0 0 0 1 0], [2 2 3]);
%!
%! assert (ntsc2rgb (ntsc_double), expected, 1e-5)
%! assert (ntsc2rgb (single (ntsc_double)), single (expected), 1e-5)
