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
## @deftypefn {Function File} {} ind2gray (@var{x}, @var{map})
## Convert an Octave indexed image to a gray scale intensity image.
## If @var{map} is omitted, the current colormap is used to determine the
## intensities.
## @seealso{gray2ind, rgb2ntsc, image, colormap}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function y = ind2gray (x, map)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (nargin == 1)
    map = colormap ();
  endif

  [rows, cols] = size (x);

  ## Convert colormap to intensity values (the first column of the
  ## result of the call to rgb2ntsc) and then replace indices in
  ## the input matrix with indexed values in the output matrix (indexed
  ## values are the result of indexing the intensity values by the
  ## elements of x(:)).

  y = reshape (((rgb2ntsc (map))(:,1))(x(:)), rows, cols);

endfunction
