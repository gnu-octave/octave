## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn  {Function File} {@var{rgb} =} ind2rgb (@var{x})
## @deftypefnx {Function File} {@var{rgb} =} ind2rgb (@var{x}, @var{map})
## @deftypefnx {Function File} {[@var{R}, @var{G}, @var{B}] =} ind2rgb (@dots{})
## Convert an indexed image to red, green, and blue color components.
## If the colormap doesn't contain enough colors, pad it with the
## last color in the map.
## If @var{map} is omitted, the current colormap is used for the conversion.
## @seealso{rgb2ind, ind2gray, hsv2rgb, ntsc2rgb}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [R, G, B] = ind2rgb (x, map)

  ## Do we have the right number of inputs?
  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (nargin == 1)
    map = colormap ();
  endif

  [x, map] = ind2x ("ind2rgb", x, map);

  ## Compute result
  [hi, wi] = size (x);
  R = reshape (map (x(:), 1), hi, wi);
  G = reshape (map (x(:), 2), hi, wi);
  B = reshape (map (x(:), 3), hi, wi);

  ## Use 3D array if only one output is requested.
  if (nargout <= 1)
    R(:,:,3) = B;
    R(:,:,2) = G;
  endif
endfunction

%!shared img, map, ergb, rgb, r, g, b
%! img = [2 4 5; 3 2 5; 1 2 4];
%! map = [0.0  0.0  0.0
%!        0.2  0.4  0.6
%!        0.4  0.4  0.5
%!        0.3  0.7  1.0
%!        0.1  0.5  0.8];
%! ergb(:,:,1) = [0.2 0.3 0.1; 0.4 0.2 0.1; 0.0 0.2 0.3];
%! ergb(:,:,2) = [0.4 0.7 0.5; 0.4 0.4 0.5; 0.0 0.4 0.7];
%! ergb(:,:,3) = [0.6 1.0 0.8; 0.5 0.6 0.8; 0.0 0.6 1.0];
%! ## test basic usage with 1 and 3 outputs
%! [rgb] = ind2rgb (img, map);
%! [r, g, b] = ind2rgb (img, map);
%!assert (ergb, rgb);
%!assert (ergb, reshape ([r(:) g(:) b(:)], [size(img) 3]));
%! ## test correction for integers
%! img = uint8 (img -1);
%! [rgb] = ind2rgb (img, map);
%!assert (ergb, rgb);
%! ## check it fails when image is a float with an index value of 0
%!fail("[rgb] = ind2rgb (double(img), map)")
