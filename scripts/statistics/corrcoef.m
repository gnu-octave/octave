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

## usage: corrcoef (X [, Y])
##
## If each row of X and Y is an observation and each column is a variable,
## the (i,j)-th entry of corrcoef(X, Y) is the correlation between the
## i-th variable in X and the j-th variable in Y.
## corrcoef(X) is corrcoef(X, X).

## Author: Kurt Hornik <hornik@ci.tuwien.ac.at>
## Created: March 1993
## Adapted-By: jwe

function retval = corrcoef (X, Y)

  if (nargin < 1 || nargin > 2)
    usage ("corrcoef (X [, Y])");
  endif

  if (nargin == 2)
    C = cov (X, Y);
    S = std (X)' * std (Y);
    retval = C ./ S;
  elseif (nargin == 1)
    C = cov (X);
    s = diag (C);
    retval = C ./ sqrt (s*s');
  endif

endfunction
