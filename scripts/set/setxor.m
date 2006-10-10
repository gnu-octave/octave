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
## @deftypefn {Function File} {} setxor (@var{a}, @var{b})
##
## Return the elements exclusive to @var{a} or @var{b}, sorted in ascending
## order. If @var{a} and @var{b} are both column vectors return a column
## vector, otherwise return a row vector.
##
## @seealso{unique, union, intersect, setdiff, ismember}
## @end deftypefn

function c = setxor (a, b)
  if (nargin != 2)
    print_usage ();
  endif

  ## Form A and B into sets.
  a = unique (a);
  b = unique (b);

  if (isempty (a))
    c = b;
  elseif (isempty (b))
    c = a;
  else
    ## Reject duplicates.
    c = sort ([a(:); b(:)]);
    n = length (c);
    idx = find (c(1:n-1) == c(2:n));
    if (! isempty (idx))
      c([idx, idx+1]) = [];
    endif
    if (size (a, 1) == 1 || size (b, 1) == 1)
      c = c.';
    endif
  endif
endfunction

%!assert(setxor([1,2,3],[2,3,4]),[1,4])
