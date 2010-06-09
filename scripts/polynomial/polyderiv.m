## Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2004,
##               2005, 2006, 2007, 2008 John W. Eaton
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
## @deftypefn  {Function File} {} polyderiv (@var{c})
## @deftypefnx {Function File} {[@var{q}] =} polyderiv (@var{b}, @var{a})
## @deftypefnx {Function File} {[@var{q}, @var{r}] =} polyderiv (@var{b}, @var{a})
## Return the coefficients of the derivative of the polynomial whose
## coefficients are given by vector @var{c}.  If a pair of polynomials
## is given @var{b} and @var{a}, the derivative of the product is
## returned in @var{q}, or the quotient numerator in @var{q} and the
## quotient denominator in @var{r}.
## @seealso{poly, polyint, polyreduce, roots, conv, deconv, residue,
## filter, polygcd, polyval, polyvalm}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function [q, r] = polyderiv (p, a)

  if (nargin == 1 || nargin == 2)
    if (! isvector (p))
      error ("polyderiv: argument must be a vector");
    endif
    if (nargin == 2)
      if (! isvector (a))
        error ("polyderiv: argument must be a vector");
      endif
      if (nargout == 1) 
        ## derivative of p*a returns a single polynomial
        q = polyderiv (conv (p, a));
      else
        ## derivative of p/a returns numerator and denominator
        r = conv (a, a);
        if (numel (p) == 1)
          q = -p * polyderiv (a);
        elseif (numel (a) == 1)
          q = a * polyderiv (p);
        else
          q = conv (polyderiv (p), a) - conv (p, polyderiv (a));
          q = polyreduce (q);
        endif

        ## remove common factors from numerator and denominator
        x = polygcd (q, r);
        if (length(x) != 1)
          q = deconv (q, x);
          r = deconv (r, x);
        endif

        ## move all the gain into the numerator
        q = q/r(1);
        r = r/r(1);
      endif
    else
      lp = numel (p);
      if (lp == 1)
        q = 0;
        return;
      elseif (lp == 0)
        q = [];
        return;
      endif

      ## Force P to be a row vector.
      p = p(:).';

      q = p(1:(lp-1)) .* [(lp-1):-1:1];
    endif
  else
    print_usage ();
  endif

endfunction

%!assert(all (all (polyderiv ([1, 2, 3]) == [2, 2])));

%!assert(polyderiv (13) == 0);

%!error polyderiv ([]);

%!error polyderiv ([1, 2; 3, 4]);

