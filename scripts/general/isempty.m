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

## usage: isempty (x)
##
## Return 1 if the argument is an empty matrix.  Otherwise, return 0.

## Author: jwe

function retval = isempty (var)

  retval = 0;

  if (nargin == 1)
    if (is_matrix (var))
      [nr, nc] = size (var);
      retval = (nr == 0 || nc == 0);
    endif
  else
    usage ("isempty (var)");
  endif


endfunction
