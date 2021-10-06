########################################################################
##
## Copyright (C) 2000-2021 The Octave Project Developers
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
## Implementation Note: The input @var{q} must be less than @code{flintmax}
## when the input is a floating-point class (double or single).
## @seealso{gcd, lcm, isprime, primes}
## @end deftypefn

function [pf, n] = factor (q)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isscalar (q) || ! isreal (q) || q < 0 || q != fix (q))
    error ("factor: Q must be a real non-negative integer");
  endif

  ## Special case of no primes less than sqrt (q).
  if (q < 4)
    pf = q;
    n = 1;
    return;
  endif

  cls = class (q);  # store class
  if (isfloat (q) && q > flintmax (q))
    error ("factor: Q too large to factor (> flintmax)");
  endif

  ## The basic idea is to divide by the prime numbers from 1 to sqrt(q).
  ## But primes(sqrt(q)) can be very time-consuming to compute for q > 1e16,
  ## so we divide by smaller primes first.
  ##
  ## This won't make a difference for prime q, but it makes a big (100x)
  ## difference for large composite q.  Since there are many more composites
  ## than primes, this leads overall to a speedup.
  ##
  ## There is at most one prime greater than sqrt(q), and if it exists,
  ## it has multiplicity 1, so no need to consider any factors greater
  ## than sqrt(q) directly.  If there were two factors p1, p2 > sqrt(q), then
  ##
  ##   q >= p1*p2 > sqrt(q)*sqrt(q) == q,
  ##
  ## which is a contradiction.
  ##
  ## The following calculation of transition and number of divisors to use
  ## was determined empirically.  As of now (October 2021) it gives the best
  ## overall performance over the range of 1 <= q <= intmax ("uint64").
  ##
  ## For future programmers: check periodically for performance improvements
  ## and tune this transition as required.  Trials that didn't yield success
  ## in (October 2021):
  ##
  ## 1.) persistent smallprimes = primes (FOO)
  ##
  ##     For various fixed FOO in the range 10 <= FOO <= 10e6.
  ##     (FOO is independent of q.)  The thought had been that making it
  ##     persistent would cache it so it didn't need to be recomputed for
  ##     subsequent calls, but it slowed it down overall.  It seems calling
  ##     primes twice with smaller q is still faster than one persistent
  ##     call for a large q.
  ##
  ## 2.) smallprimes = primes (q ^ FOO)
  ##
  ##     For various values of FOO.  For FOO >= 0.25 or FOO <= 0.16, the
  ##     performance is very poor.  FOO needs to be in the 0.17 to 0.24 range,
  ##     somewhat.  Benchmark experiments indicate it should increase gently
  ##     from 0.18 to 0.21 as q goes from 10^11 to 10^18.
  ##
  ##     But putting in such an expression would require calculating the log
  ##     of q, which defeats any performance improvement.  Or a step-wise
  ##     approximation like:
  ##
  ##     foo = 0.18 + 0.01 * (q > 1e12) + 0.01 * (q > 1e14) ...
  ##                                    + 0.01 * (q > 1e16);
  ##     smallprimes = primes (feval (cls, q^foo));
  ##
  ##     where the RHS of foo would go from 0.18 to 0.21 over several orders
  ##     of magnitude without calling the log.  Obviously that is overly
  ##     empirical, so putting in q^0.2 seems to be the most robust overall
  ##     for 64-bit q.

  ## Lookup table for sufficiently small values for q.
  if (q < 10e9)
    ## Lookup, rather calling up to primes(100) is about 3% faster, than the
    ## previous value of primes(30).  Same for very small q < 1e6.
    ##
    ## For 1e9 < q < 10e9 the lookup approach is about 7% faster.

    smallprimes = feval (cls, ...
      [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97]);

    ## Only for really small values of q, statements like
    ##
    ##   smallprimes(smallprimes > q) = [];
    ##
    ## are relevant and slow down significantly for large values of q.
  else
    # For sufficiently large q, go up to the 5th root of q for now.
    smallprimes = primes (feval (cls, q^0.2));
  endif

  ## pf is the list of prime factors returned with type of input class.
  pf = feval (cls, []);
  [pf, q] = reducefactors (q, pf, smallprimes);

  ## pf now contains all prime factors of q within smallprimes, including
  ## repetitions, in ascending order.
  ##
  ## q itself will be divided by those prime factors to become smaller,
  ## unless q was prime to begin with.

  ## Now go all the way to sqrt(q), where q is smaller than the original q in
  ## most cases.
  ##
  ## Note: Do not try to weed out the smallprimes inside largeprimes, whether
  ## using length(smallprimes) or max(smallprimes) -- it slows it down!
  largeprimes = primes (sqrt (q));
  [pf, q] = reducefactors (q, pf, largeprimes);

  ## At this point, all prime factors <= the sqrt of the original q have been
  ## pulled out in ascending order.
  ##
  ## If q = 1, then no further primes are left.
  ## If q > 1, then q itself must be prime, and it must be the single prime
  ## factor that was larger than the sqrt of the original q.
  if (q > 1)
    pf(end+1) = q;
  endif

  ## At this point, all prime factors have been pulled out of q in ascending
  ## order.  There is no need to sort(pf).

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
  ## The following line is a few milliseconds faster than
  ## divisors (mod (q, divisors) ~= 0) = [];
  divisors = divisors (mod (q, divisors) == 0);

  for pp = divisors  # for each factor in turn
    ## Keep extracting all occurrences of that factor before going to larger
    ## factors.
    ##
    ## Note: mod() was marginally faster than rem(), when assessed over 10e6
    ##       trials of the whole factor() function.
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
