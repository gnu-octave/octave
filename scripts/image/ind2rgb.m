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
## @deftypefn {Function File} {[@var{r}, @var{g}, @var{b}] =} ind2rgb (@var{x}, @var{map})
## Convert an indexed image to red, green, and blue color components.
## If @var{map} is omitted, the current colormap is used for the conversion.
## @seealso{rgb2ind, image, imshow, ind2gray, gray2ind}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [R, G, B] = ind2rgb (X, map)

  if (nargin < 1 || nargin > 2)
    usage ("ind2rgb (X, map)");
  elseif (nargin == 1)
    map = colormap ();
  endif

  [hi, wi] = size (X);

  ## FIXME -- we should check size of X and map.

  R = reshape (map (X(:), 1), hi, wi);
  G = reshape (map (X(:), 2), hi, wi);
  B = reshape (map (X(:), 3), hi, wi);

endfunction
