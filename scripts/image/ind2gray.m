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
## @deftypefn {Function File} {} ind2gray (@var{x}, @var{map})
## Convert an Octave indexed image to a gray scale intensity image.
## If @var{map} is omitted, the current colormap is used to determine the
## intensities.
## @end deftypefn
##
## @seealso{gray2ind, rgb2ntsc, image, and colormap}

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function Y = ind2gray (X, map)

  if (nargin < 1 || nargin > 2)
    usage ("ind2gray (X, map)");
  elseif (nargin == 1)
    map = colormap ();
  endif

  [rows, cols] = size (X);

  ## Convert colormap to intensity values (the first column of the
  ## result of the call to rgb2ntsc) and then replace indices in
  ## the input matrix with indexed values in the output matrix (indexed
  ## values are the result of indexing the the intensity values by the
  ## elements of X(:)).

  Y = reshape (((rgb2ntsc (map))(:,1))(X(:)), rows, cols);

endfunction
