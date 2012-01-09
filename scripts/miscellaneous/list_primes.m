## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn  {Function File} {} list_primes ()
## @deftypefnx {Function File} {} list_primes (@var{n})
## List the first @var{n} primes.  If @var{n} is unspecified, the first
## 25 primes are listed.
##
## The algorithm used is from page 218 of the @TeX{}book.
## @seealso{primes, isprime}
## @end deftypefn

## Author: jwe

function retval = list_primes (n)

  if (nargin > 0)
    if (! isscalar (n))
      error ("list_primes: argument must be a scalar");
    endif
  endif

  if (nargin == 0)
    n = 25;
  endif

  if (n == 1)
    retval = 2;
    return;
  endif

  if (n == 2)
    retval = [2; 3];
    return;
  endif

  retval = zeros (1, n);
  retval (1) = 2;
  retval (2) = 3;

  n = n - 2;
  i = 3;
  p = 5;
  while (n > 0)

    is_prime = 1;
    is_unknown = 1;
    d = 3;
    while (is_unknown)
      a = fix (p / d);
      if (a <= d)
        is_unknown = 0;
      endif
      if (a * d == p)
        is_prime = 0;
        is_unknown = 0;
      endif
      d = d + 2;
    endwhile

    if (is_prime)
      retval (i++) = p;
      n--;
    endif
    p = p + 2;

  endwhile

endfunction

%!test
%! assert (list_primes(), [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,\
%!                        43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]);
%! assert (list_primes(5), [2, 3, 5, 7, 11]);

