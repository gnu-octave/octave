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

## usage: orth (A, tol)
##        orth (A)
##
## Returns an orthonormal basis of the range of A.
##
## The dimension of the range space is taken as the number of singular
## values of A greater than tol; the default for tol is
## max (size (A)) * sigma_max (A) * eps, where sigma_max (A) is the
## maximal singular value of A.

function retval = orth (A, tol)

  ## Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Dec 24, 1993.

  [U, S, V] = svd (A);

  [rows, cols] = size (A);

  [S_nr, S_nc] = size (S);

  if (S_nr == 1 || S_nc == 1)
    s = S(1);
  else
    s = diag (S);
  endif

  if (nargin == 1)
    tol = max (size (A)) * s (1) * eps;
  elseif (nargin != 2)
    usage ("orth (A [, tol])"); 
  endif

  rank = sum (s > tol);

  if (rank > 0)
    retval = -U (:, 1:rank);
  else
    retval = zeros (rows, 0);
  endif

endfunction
