## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} polyderiv (@var{c})
## @deftypefnx {Function File} {[@var{q}] =} polyder (@var{b}, @var{a})
## @deftypefnx {Function File} {[@var{q}, @var{r}] =} polyder (@var{b}, @var{a})
## Return the coefficients of the derivative of the polynomial whose
## coefficients are given by vector @var{c}.  If a pair of polynomials
## is given @var{b} and @var{a}, the derivative of the product is
## returned in @var{q}, or the quotient numerator in @var{q} and the
## quotient denominator in @var{r}.
## @end deftypefn
## @seealso{poly, polyinteg, polyreduce, roots, conv, deconv, residue,
## filter, polygcd, polyval, and polyvalm}

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
    usage ("q = polyderiv (p) or q = polyderiv (b, a) or [q, r] = polyderiv (b, a)");
  endif


endfunction
