## Copyright (C) 2000-2012 Paul Kienzle
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
## @deftypefn {Function File} {} factorial (@var{n})
## Return the factorial of @var{n} where @var{n} is a positive integer.  If
## @var{n} is a scalar, this is equivalent to @code{prod (1:@var{n})}.  For
## vector or matrix arguments, return the factorial of each element in the
## array.  For non-integers see the generalized factorial function
## @code{gamma}.
## @seealso{prod, gamma}
## @end deftypefn

function x = factorial (n)
  if (nargin != 1)
    print_usage ();
  elseif (any (n(:) < 0 | n(:) != fix (n(:))))
    error ("factorial: N must all be non-negative integers");
  endif
  x = round (gamma (n+1));
endfunction

%!assert (factorial(5), prod(1:5))
%!assert (factorial([1,2;3,4]), [1,2;6,24])
%!assert (factorial(70), exp(sum(log(1:70))), -128*eps)
%!fail ('factorial(5.5)', "must all be non-negative integers")
%!fail ('factorial(-3)', "must all be non-negative integers")
