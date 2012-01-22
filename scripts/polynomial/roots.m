## Copyright (C) 1994-2012 John W. Eaton
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
## @tex
## $$
## v_1 z^{N-1} + \cdots + v_{N-1} z + v_N.
## $$
## @end tex
## @ifnottex
##
## @example
## v(1) * z^(N-1) + @dots{} + v(N-1) * z + v(N)
## @end example
##
## @end ifnottex
##
## As an example, the following code finds the roots of the quadratic
## polynomial
## @tex
## $$ p(x) = x^2 - 5. $$
## @end tex
## @ifnottex
##
## @example
## p(x) = x^2 - 5.
## @end example
##
## @end ifnottex
##
## @example
## @group
## c = [1, 0, -5];
## roots (c)
## @result{}  2.2361
## @result{} -2.2361
## @end group
## @end example
##
## Note that the true result is
## @tex
## $\pm \sqrt{5}$
## @end tex
## @ifnottex
## @math{+/- sqrt(5)}
## @end ifnottex
## which is roughly
## @tex
## $\pm 2.2361$.
## @end tex
## @ifnottex
## @math{+/- 2.2361}.
## @end ifnottex
## @seealso{poly, compan, fzero}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 24 December 1993
## Adapted-By: jwe

function r = roots (v)

  if (nargin != 1 || min (size (v)) > 1)
    print_usage ();
  elseif (any (isnan(v) | isinf(v)))
    error ("roots: inputs must not contain Inf or NaN");
  endif

  n = numel (v);
  v = v(:);

  ## If v = [ 0 ... 0 v(k+1) ... v(k+l) 0 ... 0 ], we can remove the
  ## leading k zeros and n - k - l roots of the polynomial are zero.

  if (isempty (v))
    f = v;
  else
    f = find (v ./ max (abs (v)));
  endif
  m = numel (f);

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

%!test
%! p = [poly([3 3 3 3]), 0 0 0 0];
%! r = sort (roots (p));
%! assert (r, [0; 0; 0; 0; 3; 3; 3; 3], 0.001)

%!assert(all (all (abs (roots ([1, -6, 11, -6]) - [3; 2; 1]) < sqrt (eps))));

%!assert(isempty (roots ([])));

%!error roots ([1, 2; 3, 4]);

%!assert(isempty (roots (1)));

%!error roots ([1, 2; 3, 4]);

%!error roots ([1 Inf 1]);

%!error roots ([1 NaN 1]);

%!assert(roots ([1e-200, -1e200, 1]), 1e-200)
%!assert(roots ([1e-200, -1e200 * 1i, 1]), -1e-200 * 1i)
