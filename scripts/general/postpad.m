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

## postpad (x, l)
##
## Appends zeros to the vector x until it is of length l.
## postpad (x, l, c) appends the constant c instead of zero.
##
## If length (x) > l, elements from the end of x are removed
## until a vector of length l is obtained.

function y = postpad (x, l, c)

  ## Author:
  ##  Tony Richardson
  ##  amr@mpl.ucsd.edu
  ##  June 1994

  if (nargin == 2)
    c = 0;
  elseif (nargin < 2 || nargin > 3)
    usage ("postpad (x, l) or postpad (x, l, c)");
  endif

  if (is_matrix (x))
    error ("first argument must be a vector");
  elseif (! is_scalar (l))
    error ("second argument must be a scaler");
  endif

  if (l < 0)
    error ("second argument must be non-negative");
  endif

  lx = length (x);

  if (lx >= l)
    y = x(1:l);
  else
    if (rows (x) > 1)
      tmp = c * ones (l-lx, 1);
      y = [x; tmp];
    else
      tmp = c * ones (1, l-lx);
      y = [x, tmp];
    endif
  endif

endfunction
