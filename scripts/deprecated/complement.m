## Copyright (C) 1994, 1996, 1997, 1999, 2000, 2004, 2005, 2006, 2007,
##               2008, 2009 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} complement (@var{x}, @var{y})
## Return the elements of set @var{y} that are not in set @var{x}.  For
## example:
##
## @example
## @group
## complement ([ 1, 2, 3 ], [ 2, 3, 5 ])
##      @result{} 5
## @end group
## @end example
## @seealso{union, intersect, unique}
## @end deftypefn

## Author: jwe

## Deprecated in version 3.2

function y = complement (a, b)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "complement is obsolete and will be removed from a future version of Octave, please use setdiff instead");
  endif

  if (nargin != 2)
    print_usage ();
  endif

  if (isempty (a))
    y = unique (b);
  elseif (isempty (b))
    y = [];
  else
    a = unique (a);
    b = unique (b);
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

%!assert(all (all (complement ([1, 2, 3], [3; 4; 5; 6]) == [4, 5, 6])));

%!assert(all (all (complement ([1, 2, 3], [3, 4, 5, 6]) == [4, 5, 6])));

%!assert(isempty (complement ([1, 2, 3], [3, 2, 1])));

%!error complement (1);

%!error complement (1, 2, 3);

