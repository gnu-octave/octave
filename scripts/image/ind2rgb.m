## Copyright (C) 1996 John W. Eaton
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

## Convert an indexed image to red, green, and blue color components.
##
## [R G B] = ind2rgb(X) uses the current colormap for the conversion.
##
## [R G B] = ind2rgb(X,map) uses the specified colormap.
##
## SEE ALSO: rgb2ind, image, imshow, ind2gray, gray2ind.

## Author: Tony Richardson <amr@mpl.ucsd.edu>
## Created: July 1994
## Adapted-By: jwe

function [R, G, B] = ind2rgb (X, map)

  if (nargin < 1 || nargin > 2)
    usage ("ind2rgb (X, map)");
  elseif (nargin == 1)
    map = colormap ();
  endif

  [hi, wi] = size (X);

  ## XXX FIXME XXX -- we should check size of X and map.

  pref = do_fortran_indexing;

  unwind_protect

    do_fortran_indexing = 1;

    R = map (X(:), 1);
    G = map (X(:), 2);
    B = map (X(:), 3);

    R = reshape (R, hi, wi);
    G = reshape (G, hi, wi);
    B = reshape (B, hi, wi);

  unwind_protect_cleanup
    do_fortran_indexing = pref;
  end_unwind_protect

endfunction
