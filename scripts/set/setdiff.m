## Copyright (C) 2000 Paul Kienzle
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

## -*- texinfo -*-
## @deftypefn {Function File} {} setdiff(@var{a}, @var{b})
##
## Return the elements in @var{a} but not in @var{b}, sorted in ascending
## order. If @var{a} and @var{b} are both column vectors return a column
## vector, otherwise return a row vector.
##
## @end deftypefn
## @seealso{unique, union, intersect, setxor, ismember}

## Author: Paul Kienzle
## Adapted-by: jwe

function c = setdiff (a, b)

  if (nargin != 2)
    usage ("setdiff (a, b)");
  endif

  c = unique (a);
  if (! isempty (c) && ! isempty (b))
    ## form a and b into combined set
    b = unique (b);
    [dummy, idx] = sort ([c(:); b(:)]);
    ## eliminate those elements of a that are the same as in b
    n = length (dummy);
    c(idx(find (dummy(1:n-1) == dummy(2:n)))) = [];
    ## reshape if necessary
    if (size (c, 1) != 1 && size (b, 1) == 1)
      c = c.';
    endif
  endif

endfunction
  