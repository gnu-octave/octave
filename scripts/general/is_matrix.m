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

## usage: is_matrix (x)
##
## Return 1 if X is a matrix.  Otherwise, return 0.
##
## See also: size, rows, columns, length, is_scalar, is_vector

## Author: jwe

function retval = is_matrix (x)

  if (nargin == 1)
    [nr, nc] = size (x);
    retval = (nr > 0 && nc > 0);
  else
    usage ("is_matrix (x)");
  endif

endfunction
