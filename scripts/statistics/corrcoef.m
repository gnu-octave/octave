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

## usage: corrcoef (x, y)
##
## The (i,j)-th entry of corrcoef (x, y) is the correlation between the
## i-th variable in x and the j-th variable in y.
## For matrices, each row is an observation and each column a variable;
## vectors are always observations and may be row or column vectors.
## corrcoef (x) is corrcoef (x, x).

## Author: Kurt Hornik <hornik@ci.tuwien.ac.at>
## Created: March 1993
## Adapted-By: jwe

function retval = corrcoef (x, y)

  if (nargin < 1 || nargin > 2)
    usage ("corrcoef (x, y)");
  endif

  if (nargin == 2)
    c = cov (x, y);
    s = std (x)' * std (y);
    retval = c ./ s;
  elseif (nargin == 1)
    c = cov (x);
    s = reshape (sqrt (diag (c)), 1, columns (c));
    retval = c ./ sqrt (s * s');
  endif

endfunction
