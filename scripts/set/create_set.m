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

## usage: create_set(x)
##
## Returns the unique elements of x, sorted in ascending order.
##
## See - union, intersection, complement

## Author: jwe

function y = create_set(x)

  if ( nargin != 1)
    usage ("create_set(x)");
  endif

  if(isempty(x))
    y = [];
  else
    [nrx, ncx] = size(x);
    nelx = nrx*ncx;
    x = reshape(x,1,nelx);
    y = zeros(1,nelx);

    x = sort(x);
    cur_val = y(1) = x(1);
    yindex = xindex = 2;

    while (xindex <= nelx)
      if(cur_val != x(xindex))
        cur_val = x(xindex);
        y(yindex++) = cur_val;
      endif
      xindex++;
    endwhile
    y = y(1:(yindex-1));
  endif

endfunction
