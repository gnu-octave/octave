## Copyright (C) 1994, 1995, 1996, 1997, 1999, 2000, 2004, 2005, 2006,
##               2007 John W. Eaton
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
## @deftypefn {Function File} {} roots (@var{v})
##
## For a vector @var{v} with @math{N} components, return
## the roots of the polynomial
## @iftex
## @tex
## $$
## v_1 z^{N-1} + \cdots + v_{N-1} z + v_N.
## $$
## @end tex
## @end iftex
## @ifnottex
##
## @example
## v(1) * z^(N-1) + ... + v(N-1) * z + v(N)
## @end example
## @end ifnottex
##
## As an example, the following code finds the roots of the quadratic
## polynomial
## @iftex
## @tex
## $$ p(x) = x^2 - 5. $$
## @end tex
## @end iftex
## @ifnottex
## @example
## p(x) = x^2 - 5.
## @end example
## @end ifnottex
## @example
## c = [1, 0, -5];
## roots(c)
## @result{}  2.2361
## @result{} -2.2361
## @end example
## Note that the true result is
## @iftex
## @tex
## $\pm \sqrt{5}$
## @end tex
## @end iftex
## @ifnottex
## @math{+/- sqrt(5)}
## @end ifnottex
## which is roughly
## @iftex
## @tex
## $\pm 2.2361$.
## @end tex
## @end iftex
## @ifnottex
## @math{+/- 2.2361}.
## @end ifnottex
## @seealso{compan}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 24 December 1993
## Adapted-By: jwe

function r = roots (v)

  if (nargin != 1 || min (size (v)) > 1)
    print_usage ();
  endif

  n = length (v);
  v = reshape (v, 1, n);

  ## If v = [ 0 ... 0 v(k+1) ... v(k+l) 0 ... 0 ], we can remove the
  ## leading k zeros and n - k - l roots of the polynomial are zero.

  f = find (v);
  m = max (size (f));

  if (m > 0 && n > 1)
    v = v(f(1):f(m));
    l = max (size (v));
    if (l > 1)
      A = diag (ones (1, l-2), -1);
      A(1,:) = -v(2:l) ./ v(1);
      r = eig (A);
      if (f(m) < n)
        tmp = zeros (n - f(m), 1);
        r = [r; tmp];
      endif
    else
      r = zeros (n - f(m), 1);
    endif
  else
    r = [];
  endif

endfunction

%!assert(all (all (abs (roots ([1, -6, 11, -6]) - [3; 2; 1]) < sqrt (eps))));

%!assert(isempty (roots ([])));

%!error roots ([1, 2; 3, 4]);

