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

## usage: intersection(a,b)
##
## Returns the intersection of sets a and b.
##
## See - create_set, union, complement

function y = intersection(a,b)

  if (nargin != 2)
    usage ("intersection(a,b)");
  endif

  if(isempty(a) || isempty(b))
    y = [];
    return;
  endif

  a = create_set(a);
  b = create_set(b);

  if(length(a) < length(b))
    yindex = 1;
    y = zeros(1,length(a));
    for index = 1:length(a)
      if(any(b == a(index)))
        y(yindex++) = a(index);
      endif
    endfor
  else
    yindex = 1;
    y = zeros(1,length(b));
    for index = 1:length(b)
      if(any(a == b(index)))
        y(yindex++) = b(index);
      endif
    endfor
  endif

  y = y(1:(yindex-1));

endfunction
