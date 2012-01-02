## Copyright (C) 2005-2012 William Poetra Yoga Hadisoeseno
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
## @deftypefn {Function File} {} isequal (@var{x1}, @var{x2}, @dots{})
## Return true if all of @var{x1}, @var{x2}, @dots{} are equal.
## @seealso{isequalwithequalnans}
## @end deftypefn

function retval = isequal (x1, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  retval = __isequal__ (false, x1, varargin{:});

endfunction

## test size and shape
%!assert(isequal([1,2,3,4],[1,2,3,4]), true)
%!assert(isequal([1;2;3;4],[1;2;3;4]), true)
%!assert(isequal([1,2,3,4],[1;2;3;4]), false)
%!assert(isequal([1,2,3,4],[1,2;3,4]), false)
%!assert(isequal([1,2,3,4],[1,3;2,4]), false)

%!test
%! A = 1:8;
%! B = reshape (A, 2, 2, 2);
%! assert (isequal (A, B), false);

%!test
%! A = reshape (1:8, 2, 2, 2);
%! B = A;
%! assert (isequal (A, B), true);

%!test
%! A = reshape (1:8, 2, 4);
%! B = reshape (A, 2, 2, 2);
%! assert (isequal (A, B), false);

## test for equality
%!assert(isequal([1,2,3,4],[1,2,3,4]), true)
%!assert(isequal(['a','b','c','d'],['a','b','c','d']), true)
## Test multi-line strings
%!assert(isequal(["test";"strings"],["test";"strings"],["test";"strings"]), true)
## test for inequality
%!assert(isequal([1,2,3,4],[1;2;3;4]),false)
%!assert(isequal({1,2,3,4},[1,2,3,4]),false)
%!assert(isequal([1,2,3,4],{1,2,3,4}),false)
%!assert(isequal([1,2,NaN,4],[1,2,NaN,4]),false)
%!assert(isequal(['a','b','c','d'],['a';'b';'c';'d']),false)
%!assert(isequal({'a','b','c','d'},{'a';'b';'c';'d'}),false)
## test for equality (struct)
%!assert(isequal(struct('a',1,'b',2),struct('a',1,'b',2)),true)
%!assert(isequal(struct('a',1,'b',2),struct('a',1,'b',2),struct('a',1,'b',2)),true)
%!assert(isequal(struct('a','abc','b',2),struct('a','abc','b',2)),true)
## test for inequality (struct)
%!assert(isequal(struct('a',NaN,'b',2),struct('a',NaN,'b',2),struct('a',NaN,'b',2)),false)
