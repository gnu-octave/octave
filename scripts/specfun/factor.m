## Copyright (C) 2000, 2006, 2007, 2009 Paul Kienzle
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
## @deftypefn  {Function File} {@var{p} =} factor (@var{q})
## @deftypefnx {Function File} {[@var{p}, @var{n}] =} factor (@var{q})
##
## Return prime factorization of @var{q}.  That is, @code{prod (@var{p})
## == @var{q}} and every element of @var{p} is a prime number.  If
## @code{@var{q} == 1}, returns 1. 
##
## With two output arguments, return the unique primes @var{p} and
## their multiplicities.  That is, @code{prod (@var{p} .^ @var{n}) ==
## @var{q}}.
## @seealso{gcd, lcm}
## @end deftypefn

## Author: Paul Kienzle

## 2002-01-28 Paul Kienzle
## * remove recursion; only check existing primes for multiplicity > 1
## * return multiplicity as suggested by Dirk Laurie
## * add error handling

function [x, m] = factor (n)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isscalar (n) || n != fix (n))
    error ("factor: n must be a scalar integer");
  endif

  ## Special case of no primes less than sqrt(n).
  if (n < 4)
    x = n;
    m = 1;
    return;
  endif 

  x = [];
  ## There is at most one prime greater than sqrt(n), and if it exists,
  ## it has multiplicity 1, so no need to consider any factors greater
  ## than sqrt(n) directly. [If there were two factors p1, p2 > sqrt(n),
  ## then n >= p1*p2 > sqrt(n)*sqrt(n) == n. Contradiction.]
  p = primes (sqrt (n));
  while (n > 1)
    ## Find prime factors in remaining n.
    p = p (rem (n, p) == 0);
    if (isempty (p))
      ## Can't be reduced further, so n must itself be a prime.
      p = n;
    endif
    x = [x, p];
    ## Reduce n.
    n = n / prod (p);
  endwhile
  x = sort (x);

  ## Determine muliplicity.
  if (nargout > 1)
    idx = find ([0, x] != [x, 0]);
    x = x(idx(1:length(idx)-1));
    m = diff (idx);
  endif

endfunction

## test:
##   assert(factor(1),1);
##   for i=2:20
##      p = factor(i);
##      assert(prod(p),i);
##      assert(all(isprime(p)));
##      [p,n] = factor(i);
##      assert(prod(p.^n),i);
##      assert(all([0,p]!=[p,0]));
##   end
