### Copyright (C) 1996 John W. Eaton
###
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

## Scale and display a matrix as an image.
##
## imagesc(x) displays a scaled version of the matrix x.  The matrix is
## scaled so that its entries are indices into the current colormap.
## The scaled matrix is returned.
##
## imagesc (x, zoom) sets the magnification, the default value is 4.
##
## SEE ALSO: image, imshow

function x = imagesc (x, zoom)

  ## Written by Tony Richardson (amr@mpl.ucsd.edu) July 1994.

  if (nargin < 1 || nargin > 2)
    usage ("image (matrix, [zoom])");
  elseif (nargin == 1)
    zoom = 4;
  endif

  [ high, wide ] = size (x);

  maxval = max (max (x));
  minval = min (min (x));

  ## Rescale matrix so that all values are in the range 0 to
  ## length (colormap) inclusive.

  if (maxval == minval)
    x = ones (high, wide);
  else
    ## Rescale values to between 1 and length (colormap) inclusive.
    x = fix ((x - minval) / (maxval - minval) * (length (colormap) - 1)) + 1;
  endif

  image (x, zoom);

endfunction
