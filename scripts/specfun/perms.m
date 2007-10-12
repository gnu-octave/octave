## Copyright (C) 2001 Paul Kienzle
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
## Generate all permutations of @var{v}, one row per permutation. The
## result has size @code{factorial (@var{n}) * @var{n}}, where @var{n}
## is the length of @var{v}.
##
## As an example, @code{perms([1, 2, 3])} returns the matrix
## @example
##   1   2   3
##   2   1   3
##   1   3   2
##   2   3   1
##   3   1   2
##   3   2   1
## @end example
## @end deftypefn

function A = perms (v)
  if (nargin != 1)
    print_usage ();
  endif
  v = v(:);
  n = length (v);
  if (n == 1)
    A = v;
  else
    B = perms (v(1:n-1));
    Bidx = 1:size (B, 1);
    A = v(n) * ones (prod (2:n), n);
    A(Bidx,1:n-1) = B;
    k = size (B, 1);
    for i = n-1:-1:2
      A(k+Bidx,1:i-1) = B(Bidx,1:i-1);
      A(k+Bidx,i+1:n) = B(Bidx,i:n-1);
      k = k + size (B, 1);
    endfor
    A(k+Bidx,2:n) = B;
  endif
endfunction
