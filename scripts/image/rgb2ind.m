## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn  {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{r}, @var{g}, @var{b})
## Convert and RGB image to an Octave indexed image.
## @seealso{ind2rgb, rgb2ntsc}
## @end deftypefn

## Bugs: The color map may have duplicate entries.

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [X, map] = rgb2ind (R, G, B)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif
  
  if (nargin == 1)
    rgb = R;
    if (length (size (rgb)) == 3 && size (rgb, 3) == 3)
      R = rgb(:,:,1);
      G = rgb(:,:,2);
      B = rgb(:,:,3);
    else
      error ("rgb2ind: argument is not an RGB image");
    endif
  endif

  if (! size_equal (R, G) || ! size_equal (R, B))
    error ("rgb2ind: arguments must all have the same size");
  endif

  [hi, wi] = size (R);

  X = zeros (hi, wi);

  map = zeros (hi*wi, 3);

  map(:,1) = R(:);
  map(:,2) = G(:);
  map(:,3) = B(:);

  X(:) = 1:(hi*wi);

endfunction
