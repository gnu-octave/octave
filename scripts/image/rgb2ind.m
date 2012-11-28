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
## @deftypefn  {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B})
## Convert an image in red-green-blue (RGB) space to an indexed image.
## @seealso{ind2rgb, rgb2hsv, rgb2ntsc}
## @end deftypefn

## Bugs: The color map may have duplicate entries.
## FIXME: This function has a very different syntax than the Matlab one of the same name.
##        Octave function does no support N, MAP, DITHER, or TOL arguments
## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [x, map] = rgb2ind (R, G, B)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    rgb = R;
    if (ndims (rgb) != 3 || size (rgb, 3) != 3)
      error ("rgb2ind: argument is not an RGB image");
    else
      R = rgb(:,:,1);
      G = rgb(:,:,2);
      B = rgb(:,:,3);
    endif
  elseif (! size_equal (R, G, B))
    error ("rgb2ind: R, G, and B must have the same size");
  endif

  x = reshape (1:numel (R), size (R));

  map = [R(:), G(:), B(:)];

endfunction


%% FIXME: Need some functional tests or %!demo blocks

%% Test input validation
%!error rgb2ind ()
%!error rgb2ind (1,2)
%!error rgb2ind (1,2,3,4)

