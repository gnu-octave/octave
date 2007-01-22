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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} setdiff (@var{a}, @var{b})
## @deftypefnx {Function File} {} setdiff (@var{a}, @var{b}, "rows")
## Return the elements in @var{a} that are not in @var{b}, sorted in
## ascending order.  If @var{a} and @var{b} are both column vectors
## return a column vector, otherwise return a row vector.
##
## Given the optional third argument @samp{"rows"}, return the rows in
## @var{a} that are not in @var{b}, sorted in ascending order by rows.
## @seealso{unique, union, intersect, setxor, ismember}
## @end deftypefn

## Author: Paul Kienzle
## Adapted-by: jwe

function c = setdiff (a, b, byrows)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 3)
    if (! strcmpi (byrows, "rows"))
      error ("expecting third argument to be \"rows\"");
    else
      byrows = true;
    endif
  else
    byrows = false;
  endif

  if (byrows)
    c = unique (a, "rows");
    if (! isempty (c) && ! isempty (b))
      ## Form a and b into combined set.
      b = unique (b, "rows");
      [dummy, idx] = sortrows ([c; b]);
      ## Eliminate those elements of a that are the same as in b.
      n = length (dummy);
      c(idx(find (dummy(1:n-1) == dummy(2:n))), :) = [];
    endif
  else
    c = unique (a);
    if (! isempty (c) && ! isempty (b))
      ## Form a and b into combined set.
      b = unique (b);
      [dummy, idx] = sort ([c(:); b(:)]);
      ## Eliminate those elements of a that are the same as in b.
      n = length (dummy);
      c(idx(find (dummy(1:n-1) == dummy(2:n)))) = [];
      ## Reshape if necessary.
      if (size (c, 1) != 1 && size (b, 1) == 1)
	c = c.';
      endif
    endif
  endif
  
endfunction
  
%!assert(setdiff(["bb";"zz";"bb";"zz"],["bb";"cc";"bb"],"rows"), "zz")
%!assert(setdiff(["b";"z";"b";"z"],["b";"c";"b"],"rows"), "z")
%!assert(setdiff(["b";"z";"b";"z"],["b";"c";"b"]), "z")
%!assert(setdiff([1, 1; 2, 2; 3, 3; 4, 4], [1, 1; 2, 2; 4, 4], "rows"), [3 3])
%!assert(setdiff([1; 2; 3; 4], [1; 2; 4], "rows"), 3)
