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

function retval = invhilb (n)

  ## usage: invhilb (n)
  ##
  ## Return the inverse of a Hilbert matrix of order n.  This is slow but
  ## exact.  Compare with inv (hilb (n)).
  ##
  ## See also: hankel, vander, hadamard, hilb, toeplitz

  if (nargin != 1)
    usage ("invhilb (n)");
  endif

  nmax = length (n);
  if (nmax == 1)
    retval = zeros (n);
    for l = 1:n
      for k = l:n
        tmp = 1;
        for i = 1:n
          tmp = tmp * (i + k - 1);
        endfor
        for i = 1:n
          if (i != k)
            tmp = tmp * (l + i - 1);
          endif
        endfor
        for i = 1:n
          if (i != l)
            tmp = tmp / (i - l);
          endif
        endfor
        for i = 1:n
          if (i != k)
            tmp = tmp / (i - k);
          endif
        endfor
        retval (k, l) = tmp;
        retval (l, k) = tmp;
      endfor
    endfor
  else
    error ("hilb: expecting scalar argument, found something else");
  endif

endfunction
