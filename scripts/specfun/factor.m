########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{pf} =} factor (@var{q})
## @deftypefnx {} {[@var{pf}, @var{n}] =} factor (@var{q})
## Return the prime factorization of @var{q}.
##
## The prime factorization is defined as @code{prod (@var{pf}) == @var{q}}
## where every element of @var{pf} is a prime number.  If @code{@var{q} == 1},
## return 1.  The output @var{pf} is of the same numeric class as the input.
##
## With two output arguments, return the unique prime factors @var{pf} and
## their multiplicities.  That is,
## @code{prod (@var{pf} .^ @var{n}) == @var{q}}.
##
## Implementation Note: If the input @var{q} is @code{single} or @code{double},
## then it must not exceed the corresponding @code{flintmax}.  For larger
## inputs, cast them to @code{uint64} if they're less than 2^64:
##
## @example
## @group
## factor (uint64 (18446744073709011493))
##    @result{}     571111    761213  42431951
## @end group
## @end example
##
## For even larger inputs, use @code{sym} if you have the Symbolic package
## installed and loaded:
##
## @example
## @group
## factor (sym ('9444733049654361449941'))
##    @result{} (sym)
##               1           1
##  1099511627689 ⋅8589934669
## @end group
## @end example
## @seealso{gcd, lcm, isprime, primes}
## @end deftypefn

function [pf, n] = factor (q)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isscalar (q) || ! isreal (q) || q < 0 || q != fix (q))
    error ("factor: Q must be a real non-negative integer");
  endif

  ## Special case if q is prime, because isprime() is now much faster than
  ## factor().  This also absorbs the case of q < 4, where there are no primes
  ## less than sqrt(q).
  if (q < 4 || isprime (q))
    pf = q;
    n = 1;
    return;
  endif

  ## If we are here, then q is composite.

  cls = class (q);  # store class
  if (isfloat (q) && q > flintmax (q))
    error ("factor: Q too large to factor (> flintmax)");
  endif

  ## The overall flow is this:
  ## 1. Divide by small primes smaller than q^0.2, if any.
  ## 2. Use Pollard Rho to reduce the value below 1e10 if possible.
  ## 3. Divide by primes smaller than sqrt (q), if any.
  ## 4. At all stages, stop if the remaining value is prime.

  ## First divide by primes (q ^ 0.2).
  ## For q < 1e10, we can hard-code the primes.
  if (q < 1e10)
    smallprimes = feval (cls, ...
      [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97]);
  else
    smallprimes = primes (feval (cls, q ^ 0.2));
  endif

  ## pf is the list of prime factors returned with type of input class.
  pf = feval (cls, []);
  [pf, q] = reducefactors (q, pf, smallprimes);

  ## pf now contains all prime factors of q within smallprimes, including
  ## repetitions, in ascending order.
  ##
  ## q itself has been divided by those prime factors to become smaller,
  ## unless q was prime to begin with.

  sortflag = false;
  if (isprime (q))
    pf(end+1) = q;
  elseif (q > 1)
    ## Use Pollard Rho technique to pull factors one at a time.
    while (q > 1e10 && ! isprime (q))
      pr = feval (cls, __pollardrho__ (q));  # pr is a factor of q.

      ## There is a small chance (13 in 1e5) that pr is not actually prime.
      ## To guard against that, factorize pr, which will force smaller factors
      ## to be found.  The use of isprime above guards against infinite
      ## recursion.
      if (! isprime (pr))
        pr = factor (pr);
      endif

      [pf, q] = reducefactors (q, pf, pr);
      ## q is now divided by all occurrences of factor(s) pr.
      sortflag = true;
    endwhile

    if (isprime (q))
      pf(end+1) = q;
    elseif (q > 1)
      ## If we are here, then q is composite but less than 1e10,
      ## and that is fast enough to test by division.
      largeprimes = primes (feval (cls, sqrt (q)));
      [pf, q] = reducefactors (q, pf, largeprimes);

      ## If q is still not 1, then it must be a prime of power 1.
      if (q > 1)
        pf(end+1) = q;
      endif
    endif
  endif

  ## The Pollard Rho technique can give factors in arbitrary order,
  ## so we need to sort pf if that was used.
  if (sortflag)
    pf = sort (pf);
  endif

  ## Determine multiplicity.
  if (nargout > 1)
    idx = find ([0, pf] != [pf, 0]);
    pf = pf(idx(1:length (idx)-1));
    n = diff (idx);
  endif

endfunction

function [pf, q] = reducefactors (qin, pfin, divisors)

  pf = pfin;
  q = qin;
  divisors = divisors (mod (q, divisors) == 0);

  for pp = divisors  # for each factor in turn
    ## Keep extracting all occurrences of that factor before going to larger
    ## factors.
    while (mod (q, pp) == 0)
      pf(end+1) = pp;
      q /= pp;
    endwhile
  endfor

endfunction


## Test special case input
%!assert (factor (1), 1)
%!assert (factor (2), 2)
%!assert (factor (3), 3)

%!test
%! for i = 2:20
%!   pf = factor (i);
%!   assert (prod (pf), i);
%!   assert (all (isprime (pf)));
%!   [pf, n] = factor (i);
%!   assert (prod (pf.^n), i);
%!   assert (all ([0,pf] != [pf,0]));
%! endfor

## Make sure that all factors returned are indeed prime, even when
## __pollardrho__ returns a composite factor.
%!assert (all (isprime (factor (uint64 (18446744073707633197)))))
%!assert (all (isprime (factor (uint64 (18446744073707551733)))))
%!assert (all (isprime (factor (uint64 (18446744073709427857)))))
%!assert (all (isprime (factor (uint64 (18446744073709396891)))))
%!assert (all (isprime (factor (uint64 (18446744073708666563)))))
%!assert (all (isprime (factor (uint64 (18446744073708532009)))))
%!assert (all (isprime (factor (uint64 (18446744073708054211)))))
%!assert (all (isprime (factor (uint64 (18446744073707834741)))))
%!assert (all (isprime (factor (uint64 (18446744073707298053)))))
%!assert (all (isprime (factor (uint64 (18446744073709407383)))))
%!assert (all (isprime (factor (uint64 (18446744073708730121)))))
%!assert (all (isprime (factor (uint64 (18446744073708104447)))))
%!assert (all (isprime (factor (uint64 (18446744073709011493)))))

%!assert (factor (uint8 (8)), uint8 ([2 2 2]))
%!assert (factor (single (8)), single ([2 2 2]))
%!test
%! [pf, n] = factor (int16 (8));
%! assert (pf, int16 (2));
%! assert (n, double (3));

## Test input validation
%!error <Invalid call> factor ()
%!error <Q must be a real non-negative integer> factor ([1,2])
%!error <Q must be a real non-negative integer> factor (6i)
%!error <Q must be a real non-negative integer> factor (-20)
%!error <Q must be a real non-negative integer> factor (1.5)
%!error <Q too large to factor> factor (flintmax ("single") + 2)
%!error <Q too large to factor> factor (flintmax ("double") + 2)
