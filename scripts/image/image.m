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
## @deftypefn {Function File} {} image (@var{img})
## @deftypefnx {Function File} {} image (@var{x}, @var{y}, @var{img})
## Display a matrix as a color image.  The elements of @var{x} are indices
## into the current colormap and should have values between 1 and the
## length of the colormap.
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

function image (x, y, img)

  newplot ();

  if (nargin == 0)
    ## Load Bobbie Jo Richardson (Born 3/16/94)
    img = loadimage ("default.img");
    x = y = [];
  elseif (nargin == 1 || nargin == 2)
    ## FIXME -- should we handle the old zoom argument?  How?  What
    ## figure property should we be setting?
    img = x;
    x = y = [];
  elseif (nargin > 3)
    print_usage ();
  endif

  tmp = __img__ (x, y, img);

  if (nargout > 0)
    h = tmp;
  endif

endfunction
