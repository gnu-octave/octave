## Copyright (C) 1996 John W. Eaton
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

## usage: betai (a, b, x)
##
## Returns the incomplete beta function
##   betai (a, b, x) = BETA(a,b)^(-1) INT_0^x t^(a-1) (1-t)^(b-1) dt.
## If x has more than one component, both a and b must be scalars.
## If x is a scalar, a and b must be of compatible dimensions.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 2 August 1994.
## Adapted-By: jwe

function y = betai (a, b, x)
  
  ## Computation is based on the series expansion
  ##   betai(a, b, x) 
  ##     = \frac{1}{B(a, b)} x^a 
  ##         \sum_{k=0}^\infty \frac{(1-b)\cdots(k-b)}{a+k} \frac{x^k}{k!}
  ## for x <= 1/2.  For x > 1/2, betai(a, b, x) = 1 - betai(b, a, 1-x).

  if (nargin <> 3)
    usage (" betai (a, b, x)");
  endif

  if (! (a > 0 && b > 0))
    error ("betai: a and b must both be positive");
  endif
  [nr, nc] = size (x);
  if (min ([nr, nc]) == 0)
    error ("betai: x must not be empty.");
  endif
  if (any (x < 0) || any (x > 1))
    error ("betai: all entries of x must be in [0,1].");
  endif

  if (nr > 1 || nc > 1)
    
    if (! (is_scalar (a) && is_scalar (b)))
      error ("betai: if x is not a scalar, a and b must be scalars");
    endif

    n = nr * nc;
    x = reshape (x, 1, n);
    y = zeros (1, n);
    c = exp (lgamma (a+b) - lgamma (a) - lgamma (b));

    y (find (x == 1)) = ones (1, sum (x == 1));

    ## Now do the series computation.  The error when truncating at term K
    ## is always less than 2^(-K), hence the following choice of K.

    K = ceil (-log (eps) / log (2));
    k = (1:K)';

    ind = find ((x > 0) & (x <= 1/2));
    len = length (ind);
    if (len > 0)
      tmp    = cumprod((1 - b./k) * x(ind)) ./ ((a + k) * ones(1, len));
      y(ind) = c * exp(a * log(x(ind))) .* (1/a + sum(tmp));
    endif
    
    ind = find ((x > 1/2) & (x < 1));
    len = length(ind);
    if (len > 0)
      tmp    = cumprod ((1 - a./k) * (1 - x(ind))) ./ ((b + k) * ones(1, len));
      y(ind) = 1 - c * exp (b * log (1-x(ind))) .* (1/b + sum (tmp));
    endif
  
    y = reshape (y, nr, nc);
    
  else
    
    [ra, ca] = size (a);
    [rb, cb] = size (b);
    if (! (ra == rb && ca == cb))
      error ("betai: a and b must have the same size");
    endif

    n = ra * ca;
    a = reshape (a, 1, n);
    b = reshape (b, 1, n);
    c = exp (lgamma (a+b) - lgamma (a) - lgamma (b));
    
    if (x == 0)
      y   = zeros (1, n);
    elseif (x == 1)
      y   = ones (1, n);
    else
      K = ceil (-log (eps) / log (2));
      k = (1:K)' * ones (1, n);
      h = ones (K, 1);
      if (x > 0 && x <= 1/2)
	tmp = cumprod ((1 - (h * b) ./ k) * x) ./ ((h * a) + k);
	y   = c .* exp (a * log(x)) .* (1 ./ a + sum (tmp));
      else
	tmp = cumprod ((1 - (h * a) ./ k) * (1-x)) ./ ((h * b) + k);
	y   = 1 - c .* exp (b * log (1-x)) .* (1 ./ b + sum (tmp));
      endif
    endif
  
    y = reshape (y, ra, ca);
    
  endif

endfunction
