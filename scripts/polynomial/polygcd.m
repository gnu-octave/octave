## Copyright (C) 2000 Paul Kienzle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{q}]} polygcd (@var{b}, @var{a}, @var{tol})
##
## Find greatest common divisor of two polynomials.  This is equivalent
## to the polynomial found by multiplying together all the common roots.
## Together with deconv, you can reduce a ratio of two polynomials.
## Tolerance defaults to 
## @example 
## sqrt(eps).
## @end example
##  Note that this is an unstable
## algorithm, so don't try it on large polynomials.
##
## Example
## @example
##    polygcd(poly(1:8),poly(3:12)) - poly(3:8)
##    deconv(poly(1:8),polygcd(poly(1:8),poly(3:12))) - poly(1:2)
## @end example
## @end deftypefn
##
## @seealso{poly, polyinteg, polyderiv, polyreduce, roots, conv, deconv,
## residue, filter, polyval, and polyvalm}

function x = polygcd (b, a, tol)

  if (nargin == 2 || nargin == 3)
    if (nargin == 2)
      tol = sqrt (eps);
    endif
    if (length (a) == 1 || length (b) == 1)
      if (a == 0)
	x = b;
      elseif (b == 0)
	x = a;
      else
	x = 1;
      endif
    else
      a /= a(1);
      while (1)
	[d, r] = deconv (b, a);
	nz = find (abs (r) > tol);
	if (isempty (nz))
	  x = a;
	  break;
	else
	  r = r(nz(1):length(r));
	endif
	b = a;
	a /= r(1);
      endwhile
    endif
  else
    usage ("x = polygcd (b, a [,tol])");
  endif

endfunction
