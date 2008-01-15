## Copyright (C) 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2005, 2006,
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
## @deftypefn {Mapping Function} {} beta (@var{a}, @var{b})
## Return the Beta function,
## @iftex
## @tex
## $$
##  B (a, b) = {\Gamma (a) \Gamma (b) \over \Gamma (a + b)}.
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## beta (a, b) = gamma (a) * gamma (b) / gamma (a + b).
## @end example
## @end ifinfo
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 13 June 1993
## Adapted-By: jwe

function retval = beta (a, b)

  if (nargin != 2)
    print_usage ();
  endif

  retval = exp (gammaln (a) + gammaln (b) - gammaln (a+b));

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

