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

## usage: hadamard (k)
##
## Return the Hadamard matrix of order n = 2^k.
##
## See also: hankel, vander, hilb, invhilb, toeplitz

## Author: jwe

function retval = hadamard (k)

  if (nargin != 1)
    usage ("hadamard (n)");
  endif

  if (is_scalar (k))
    if (k < 1)
      retval = 1;
    else
      tmp = hadamard (k-1);
      retval = [tmp, tmp; tmp, -tmp];
    endif
  else
    error ("hadamard: expecting scalar argument");
  endif

endfunction
