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
## @deftypefn {Function File} {} imagesc (@var{A})
## @deftypefnx {Function File} {} imagesc (@var{x}, @var{y}, @var{A})
## @deftypefnx {Function File} {} imagesc (@dots{}, @var{zoom})
## @deftypefnx {Function File} {} imagesc (@dots{}, @var{limits})
## @deftypefnx {Function File} { @var{B} = } imagesc (@dots{})
## Display a scaled version of the matrix @var{A} as a color image.  The
## matrix is scaled so that its entries are indices into the current
## colormap.  The scaled matrix is returned.  If @var{zoom} is omitted, a
## comfortable size is chosen.  If @var{limits} = [@var{lo}, @var{hi}] are
## given, then that range maps into the full range of the colormap rather 
## than the minimum and maximum values of @var{A}.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}, either as pairs giving the minimum and maximum
## values for the respective axes, or as values for each row and column
## of the matrix @var{A}.  At present they are ignored.
## @end deftypefn
##
## @seealso{image and imshow}

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function ret = imagesc (x, y, A, zoom, limits)

  if (nargin < 1 || nargin > 5)
    usage ("B = imagesc ([x, y,] matrix [,limits] [,zoom])");
  elseif (nargin == 1)
    A = x;
    zoom = x = y = limits = [];
  elseif (nargin == 2)
    A = x;
    zoom = y;
    x = y = limits = [];
  elseif (nargin == 3)
    ## Assume imagesc(x,y,A) for compatibility.  It
    ## could also be imagesc(A,limits,zoom), but if A is
    ## a 1x2 vector, this is equivalent to imagesc(x,y,A)
    ## for scalar A so we won't try to guess.
    zoom = limits = [];
  elseif (nargin == 4)
    limits = [];
  endif

  ## correct for zoom, limits parameter order
  if (length (zoom) == 2)
     swap = limits;
     limits = zoom;
     zoom = swap;
  endif

  ## use given limits or guess them from the matrix
  if (length (limits) == 2 && limits(2) >= limits(1))
     minval = limits(1);
     maxval = limits(2);
     A(A < minval) = minval;
     A(A > maxval) = maxval;
  elseif (length (limits) == 0)
     maxval = max (A(:));
     minval = min (A(:));
  else
     error ("expected data limits to be [lo, hi]");
  endif

  ## scale the limits to the range of the colormap
  if (maxval == minval)
    B = ones (size (A));
  else
    ## Rescale values to between 1 and length (colormap) inclusive.
    B = round ((A - minval) / (maxval - minval) * (rows (colormap) - 1)) + 1;
  endif

  ## display or return the image
  if (nargout == 0)
    image (x, y, B, zoom);
  else
    ret = B;
  endif

endfunction
