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

## usage: vander (c)
##
## Return the Vandermonde matrix whose next to last column is c.
##
## See also: hankel, sylvester_matrix, hilb, invhilb, toeplitz

## Author: jwe

function retval = vander (c)

  if (nargin != 1)
    usage ("vander (c)");
  endif

  nr = rows (c);
  nc = columns (c);
  if (nr == 1 && nc == 1)
    retval = 1;
  elseif (nr == 1 || nc == 1)
    n = length (c);
    if (n > 0)
      retval = zeros (n, n);
      for i = 1:n
        tmp = c(i);
        for j = 1:n
          retval (i, j) = tmp ^ (n - j);
        endfor
      endfor
    endif
  else
    error ("vander: argument must be a vector");
  endif

endfunction
