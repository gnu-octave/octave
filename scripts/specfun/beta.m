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
## @deftypefn {Mapping Function} {} beta (@var{a}, @var{b})
## For real inputs, return the Beta function,
## @tex
## $$
##  B (a, b) = {\Gamma (a) \Gamma (b) \over \Gamma (a + b)}.
## $$
## @end tex
## @ifnottex
##
## @example
## beta (a, b) = gamma (a) * gamma (b) / gamma (a + b).
## @end example
##
## @end ifnottex
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 13 June 1993
## Adapted-By: jwe

function retval = beta (a, b)

  if (nargin != 2)
    print_usage ();
  endif

  if (any (size (a) != size (b)) && numel (a) != 1 && numel (b) != 1)
    error ("beta: inputs A and B have inconsistent sizes");
  endif

  if (! isreal (a) || ! isreal (b))
    error ("beta: inputs A and B must be real");
  endif

  retval = real (exp (gammaln (a) + gammaln (b) - gammaln (a+b)));

endfunction

%!test
%! a=[1, 1.5, 2, 3];
%! b=[4, 3, 2, 1];
%! v1=beta(a,b);
%! v2=beta(b,a);
%! v3=gamma(a).*gamma(b)./gamma(a+b);
%! assert(all(abs(v1-v2)<sqrt(eps)) && all(abs(v2-v3)<sqrt(eps)));

%!error beta();

%!error beta(1);

%!assert (1, beta (1, 1))

%!test
%! a = 2:10;
%! tol = 10 * max (a) * eps;
%! assert (-a, beta (-1./a, 1), tol)
%! assert (-a, beta (1, -1./a), tol)

%!test
%! a = 0.25 + (0:5) * 0.5;
%! tol = 10 * max (a) * eps;
%! assert (zeros (size (a)), beta (a, -a), tol)
%! assert (zeros (size (a)), beta (-a, a), tol)
