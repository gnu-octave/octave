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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} complement (@var{x}, @var{y})
## Return the elements of set @var{y} that are not in set @var{x}.  For
## example,
##
## @example
## @group
## complement ([ 1, 2, 3 ], [ 2, 3, 5 ])
##      @result{} 5
## @end group
## @end example
## @end deftypefn
##
## @seealso{create_set, union, and intersection}

## Author: jwe

function y = complement (a, b)

  if (nargin != 2)
    usage ("complement(a,b)");
  endif

  if (isempty (a))
    y = create_set(b);
  elseif (isempty (b))
    y = [];
  else
    a = create_set (a);
    b = create_set (b);
    yindex = 1;
    y = zeros (1, length (b));
    for index = 1:length (b)
      if (all (a != b (index)))
        y(yindex++) = b(index);
      endif
    endfor
    y = y(1:(yindex-1));
  endif

endfunction
