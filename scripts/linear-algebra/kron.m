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

## Usage: x = kron (a, b)
##
## Form the Kronecker product of two matrices, defined block by block
## as 
##
##   x = [a(i,j) b]

function x = kron (a, b)

  ## Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin == 2)

    [m, n] = size (b);
    [ma, na] = size (a);

    ## Do 1st column.

    x = a (1, 1) * b;
    for ii = 2:ma
      tmp = a (ii, 1) * b;
      x = [x; tmp];
    endfor

    ## Do remaining columns.

    for jj = 2:na
      tmp = a (1, jj) * b;
      for ii = 2:ma
        pmt = a (ii, jj) * b;
	tmp = [tmp; pmt];
      endfor
      x = [x, tmp];
    endfor

  else
    usage ("kron (a, b)");
  endif

endfunction
