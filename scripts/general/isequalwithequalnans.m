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
## @deftypefn {Function File} {} isequalwithequalnans (@var{x1}, @var{x2}, @dots{})
## Assuming NaN == NaN, return true if all of @var{x1}, @var{x2}, @dots{}
## are equal.
## @seealso{isequal}
## @end deftypefn

function retval = isequalwithequalnans (x1, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  retval = __isequal__ (true, x1, varargin{:});

endfunction

## test for equality
%!assert(isequalwithequalnans({1,2,NaN,4},{1,2,NaN,4}), true)
%!assert(isequalwithequalnans([1,2,NaN,4],[1,2,NaN,4]), true)
## test for inequality
%!assert(isequalwithequalnans([1,2,NaN,4],[1,NaN,3,4]),false)
%!assert(isequalwithequalnans([1,2,NaN,4],[1,2,3,4]),false)
## test for equality (struct)
%!assert(isequalwithequalnans(struct('a',NaN,'b',2),struct('a',NaN,'b',2),struct('a',NaN,'b',2)),true)
%!assert(isequalwithequalnans(1,2,1), false)
