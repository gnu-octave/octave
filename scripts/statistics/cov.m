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

## usage: cov (X [, Y])
##
## If each row of X and Y is an observation and each column is a
## variable, the (i,j)-th entry of cov(X, Y) is the covariance
## between the i-th variable in X and the j-th variable in Y.
## cov(X) is cov(X, X).

function retval = cov (X, Y)

  ## Written by Kurt Hornik (hornik@ci.tuwien.ac.at) March 1993.
  ## Dept of Probability Theory and Statistics TU Wien, Austria.

  if (nargin < 1 || nargin > 2)
    usage ("cov (X [, Y])");
  endif

  [Tx, kx] = size (X);
  if (nargin == 2)
    [Ty, ky] = size (Y);
    if (Tx != Ty)
      error ("cov: X and Y must have the same number of rows.");
    endif
    X = X - ones (Tx, 1) * sum (X) / Tx;
    Y = Y - ones (Tx, 1) * sum (Y) / Tx;
    retval = conj (X' * Y / (Tx - 1));
  elseif (nargin == 1)
    X = X - ones (Tx, 1) * sum (X) / Tx;
    retval = conj (X' * X / (Tx - 1));
  endif

endfunction
