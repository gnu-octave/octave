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
## @deftypefn {Function File} {} ind2gray (@var{x}, @var{map})
## Convert an Octave indexed image to a gray scale intensity image.
## If @var{map} is omitted, the current colormap is used to determine the
## intensities.
## @end deftypefn
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

  ## Convert colormap to intensity values.

  yiq = rgb2ntsc (map);
  y = yiq(:,1);

  ## We need Fortran indexing capability, but be sure to save the user's
  ## preference.

  pref = do_fortran_indexing;

  unwind_protect

    do_fortran_indexing = 1;

    ## Replace indices in the input matrix with indexed values in the output
    ## matrix.

    [rows, cols] = size (X);
    Y = y(X(:));
    Y = reshape (Y, rows, cols);

  unwind_protect_cleanup
    do_fortran_indexing = pref;
  end_unwind_protect

endfunction
