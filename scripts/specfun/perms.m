## Copyright (C) 2001-2012 Paul Kienzle
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn {Function File} {} perms (@var{v})
##
## Generate all permutations of @var{v}, one row per permutation.  The
## result has size @code{factorial (@var{n}) * @var{n}}, where @var{n}
## is the length of @var{v}.
##
## As an example, @code{perms([1, 2, 3])} returns the matrix
##
## @example
## @group
##   1   2   3
##   2   1   3
##   1   3   2
##   2   3   1
##   3   1   2
##   3   2   1
## @end group
## @end example
## @end deftypefn

function A = perms (v)
  if (nargin != 1)
    print_usage ();
  endif
  v = v(:);
  n = length (v);

  if (n == 0)
    A = [];
  else
    A = v(1);
    for j = 2:n
      B = A;
      A = zeros (prod (2:j), n, class (v));
      k = size (B, 1);
      idx = 1:k;
      for i = j:-1:1
        A(idx,1:i-1) = B(:,1:i-1);
        A(idx,i) = v(j);
        A(idx,i+1:j) = B(:,i:j-1);
        idx += k;
      endfor
    endfor
  endif
endfunction

%!error perms ();
%!error perms (1, 2);

%!assert (perms ([1,2,3]), [1,2,3;2,1,3;1,3,2;2,3,1;3,1,2;3,2,1]);
%!assert (perms (1:3), perms ([1,2,3]));

%!assert (perms (int8([1,2,3])), int8([1,2,3;2,1,3;1,3,2;2,3,1;3,1,2;3,2,1]));
