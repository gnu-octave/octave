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

## Convert and RGB image to an octave indexed image.
##
## [X, map] = rgb2ind (R, G, B)
##
## SEE ALSO: ind2rgb, rgb2ntsc.
##
## Bugs: The color map may have duplicate entries.

function [X, map] = rgb2ind (R, G, B)

  ## Written by Tony Richardson (amr@mpl.ucsd.edu) July 1994.

  if (nargin != 3)
    usage ("[X, map] = rgb2ind (R, G, B)");
  endif

  if (size (R) != size (G) || size (R) != size (B))
    error ("rgb2ind: arguments must all have the same size");
  endif

  [hi, wi] = size (R);

  X = zeros (hi, wi);

  map = zeros (hi*wi, 3);

  pref = do_fortran_indexing;

  unwind_protect

    do_fortran_indexing = "true";

    map(:,1) = R(:);
    map(:,2) = G(:);
    map(:,3) = B(:);

    X(:) = 1:(hi*wi);

  unwind_protect_cleanup
    do_fortran_indexing = pref;
  end_unwind_protect

endfunction
