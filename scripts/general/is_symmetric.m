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

## Usage: is_symmetric (x {,tol})
##
## If x is symmetric, return the dimension of x, otherwise, return 0.
##
## See also: size, rows, columns, length, is_matrix, is_scalar,
## is_square, is_vector

## Author: A. S. Hodel <scotte@eng.auburn.edu>
## Created: August 1993
## Adapted-By: jwe

function retval = is_symmetric (x,tol)

  retval = 0;

  if (nargin == 1 || nargin == 2)
    [nr, nc] = size (x);
    if (nr == nc && nr > 0)
      if (nargin == 1)
	tol = eps;
      endif
      if (isstr (x))
	x = toascii (x);
      endif
      if (norm (x - x') / norm(x) <= tol)
        retval = nr;
      endif
    endif
  else
    usage ("is_symmetric (x {,tol})");
  endif

endfunction
