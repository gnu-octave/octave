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

## usage: toeplitz (c, r)
##
## Return the Toeplitz matrix constructed given the first column
## c, and (optionally) the first row r.
##
## If the second argument is omitted, the first row is taken to be the
## same as the first column.  If the first element of c is not the same
## as the first element of r, the first element of c is used.
##
## See also: hankel, vander, hadamard, hilb, invhib

function retval = toeplitz (c, r)

  if (nargin == 1)
    r = c;
  elseif (nargin != 2)
    usage ("toeplitz (c, r)");
  endif

  [c_nr, c_nc] = size (c);
  [r_nr, r_nc] = size (r);

  if ((c_nr != 1 && c_nc != 1) || (r_nr != 1 && r_nc != 1))
    error ("toeplitz: expecting vector arguments");
  endif

  if (c_nc != 1)
    c = c.';
  endif

  if (r_nr != 1)
    r = r.';
  endif

  if (r (1) != c (1))
    warning ("toeplitz: column wins diagonal conflict");
  endif

  ## If we have a single complex argument, we want to return a
  ## Hermitian-symmetric matrix (actually, this will really only be
  ## Hermitian-symmetric if the first element of the vector is real).

  if (nargin == 1)
    c = conj (c);
    c(1) = conj (c(1));
  endif

  ## This should probably be done with the colon operator...

  nc = length (r);
  nr = length (c);

  retval = zeros (nr, nc);

  for i = 1:min (nc, nr)
    retval (i:nr, i) = c (1:nr-i+1);
  endfor

  for i = 1:min (nr, nc-1)
    retval (i, i+1:nc) = r (2:nc-i+1);
  endfor

endfunction
