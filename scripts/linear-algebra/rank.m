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

## usage: rank (a, tol)
##
## Return the rank of the matrix a.  The rank is taken to be the number
## of singular values of a that are greater than tol.
##
## If the second argument is omitted, it is taken to be
##
##   tol =  max (size (a)) * sigma (1) * eps;
##
## where eps is machine precision and sigma is the largest singular
## value of a.

function retval = rank (A, tol)

  if (nargin == 1)
    sigma = svd (A);
    tolerance = max (size (A)) * sigma (1) * eps;
  elseif (nargin == 2)
    sigma = svd (A);
    tolerance = tol;
  else
    usage ("rank (A)");
  endif
  retval = sum (sigma > tolerance);

endfunction
