## Copyright (C) 1995, 1998 A. Scottedward Hodel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## function [housv,beta,zer] = housh(x,j,z)
## computes householder reflection vector housv to reflect x to be
## jth column of identity, i.e., (I - beta*housv*housv')x =e(j)
## inputs
##   x: vector
##   j: index into vector
##   z: threshold for zero  (usually should be the number 0)
## outputs: (see Golub and Van Loan)
##   beta: If beta = 0, then no reflection need be applied (zer set to 0)
##   housv: householder vector
## mar 6,1987 : rev dec 17,1988
##             rev sep 19,1991 (blas)
## translated from FORTRAN Aug 1995

## Author: A. S. Hodel

function [housv, beta, zer] = housh (x, j, z)

  ## check for valid inputs
  if (!is_vector (x) && ! is_scalar (x))
    error ("housh: first input must be a vector")
  elseif (! is_scalar(j))
    error ("housh: second argment must be an integer scalar")
  else
    housv = x;
    m = max (abs (housv));
    if (m != 0.0)
      housv = housv / m;
      alpha = norm (housv);
      if (alpha > z)
        beta = 1.0 / (alpha * (alpha + abs (housv(j))));
        sg = sign (housv(j));
        if (sg == 0)
          sg = 1;
        endif
        housv(j) = housv(j) + alpha*sg;
      else
        beta = 0.0;
      endif
    else
      beta = 0.0;
    endif
    zer = (beta == 0);
  endif

endfunction
