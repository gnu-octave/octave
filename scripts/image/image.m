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
## @deftypefn {Function File} {} image (@var{x}, @var{zoom})
## @deftypefnx {Function File} {} image (@var{x}, @var{y}, @var{A}, @var{zoom})
## Display a matrix as a color image.  The elements of @var{x} are indices
## into the current colormap and should have values between 1 and the
## length of the colormap.  If @var{zoom} is omitted, the image will be
## scaled to fit within 600x350 (to a max of 4).
##
## It first tries to use @code{gnuplot}, then @code{display} from 
## @code{ImageMagick}, then @code{xv}, and then @code{xloadimage}.
## The actual program used can be changed using the @code{image_viewer}
## function.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}. If you're not using gnuplot 4.2 or later, these
## variables are ignored.
## @seealso{imshow, imagesc, colormap, image_viewer}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function image (x, y, A, zoom)

  if (nargin == 0)
    ## Load Bobbie Jo Richardson (Born 3/16/94)
    A = loadimage ("default.img");
    zoom = 2;
    x = y = [];
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
  elseif (nargin > 4)
    usage ("image (matrix, zoom) or image (x, y, matrix, zoom)");
  endif

  if (isempty (zoom))
    ## Find an integer scale factor which sets the image to
    ## approximately the size of the screen.
    zoom = min ([350/rows(A), 600/columns(A), 4]);
    if (zoom >= 1)
      zoom = floor (zoom);
    else
      zoom = 1 / ceil (1/zoom);
    endif
  endif

  ## Get the image viewer.
  [view_cmd, view_fcn, view_zoom] = image_viewer ();

  ## Show the image.
  view_fcn (x, y, A, zoom*view_zoom, view_cmd);

endfunction
