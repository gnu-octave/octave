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

## usage: median (a)
##
## For vector arguments, return the median of the values.
##
## For matrix arguments, return a row vector containing the median for
## each column.
##
## See also: std, mean

## Author: jwe

function retval = median (a)

  if (nargin != 1)
    usage ("median (a)");
  endif

  [nr, nc] = size (a);
  s = sort (a);
  if (nr == 1 && nc > 0)
    if (rem (nc, 2) == 0)
      i = nc/2;
      retval = (s (i) + s (i+1)) / 2;
    else
      i = ceil (nc/2);
      retval = s (i);
    endif
  elseif (nr > 0 && nc > 0)
    if (rem (nr, 2) == 0)
      i = nr/2;
      retval = (s (i,:) + s (i+1,:)) / 2;
    else
      i = ceil (nr/2);
      retval = s (i,:);
    endif
  else
    error ("median: invalid matrix argument");
  endif

endfunction
