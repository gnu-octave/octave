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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} imagesc (@var{A}, @var{zoom})
## @deftypefnx {Function File} {} imagesc (@var{x}, @var{y}, @var{A}, @var{zoom})
## Display a scaled version of the matrix @var{A} as a color image.  The
## matrix is scaled so that its entries are indices into the current
## colormap.  The scaled matrix is returned.  If @var{zoom} is omitted, a
## comfortable size is chosen.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}.  At present they are ignored.
## @end deftypefn
## @seealso{image and imshow}

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function B = imagesc (x, y, A, zoom)

  if (nargin < 1 || nargin > 4)
    usage ("imagesc (matrix, zoom) or imagesc (x, y, matrix, zoom)");
  elseif (nargin == 1)
    A = x;
    zoom = [];
    x = y = [];
  elseif (nargin == 2)
    A = x;
    zoom = y;
    x = y = [];
  elseif (nargin == 3)
    zoom = [];
  endif

  [high, wide] = size (A);

  maxval = max (max (A));
  minval = min (min (A));

  ## Rescale matrix so that all values are in the range 0 to
  ## length (colormap) inclusive.

  if (maxval == minval)
    B = ones (high, wide);
  else
    ## Rescale values to between 1 and length (colormap) inclusive.
    B = round ((A - minval) / (maxval - minval) * (rows (colormap) - 1)) + 1;
  endif

  image (x, y, B, zoom);

endfunction
