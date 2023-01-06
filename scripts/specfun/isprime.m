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
## @deftypefn {} {@var{tf} =} isprime (@var{x})
## Return a logical array which is true where the elements of @var{x} are prime
## numbers and false where they are not.
##
## A prime number is conventionally defined as a positive integer greater than
## 1 (e.g., 2, 3, @dots{}) which is divisible only by itself and 1.  Octave
## extends this definition to include both negative integers and complex
## values.  A negative integer is prime if its positive counterpart is prime.
## This is equivalent to @code{isprime (abs (x))}.
##
## If @code{class (@var{x})} is complex, then primality is tested in the domain
## of Gaussian integers (@url{https://en.wikipedia.org/wiki/Gaussian_integer}).
## Some non-complex integers are prime in the ordinary sense, but not in the
## domain of Gaussian integers.  For example, @math{5 = (1+2i)*(1-2i)} shows
## that 5 is not prime because it has a factor other than itself and 1.
## Exercise caution when testing complex and real values together in the same
## matrix.
##
## Examples:
##
## @example
## @group
## isprime (1:6)
##   @result{}  0  1  1  0  1  0
## @end group
## @end example
##
## @example
## @group
## isprime ([i, 2, 3, 5])
##   @result{}  0  0  1  0
## @end group
## @end example
##
## Programming Note: @code{isprime} is suitable for all @var{x}
## in the range abs(@var{x})
## @tex
## $ < 2^{64}$.
## @end tex
## @ifnottex
##  < 2^64.
## @end ifnottex
##
## Compatibility Note: @sc{matlab} does not extend the definition of prime
## numbers and will produce an error if given negative or complex inputs.
## @seealso{primes, factor, gcd, lcm}
## @end deftypefn

function tf = isprime (x)

  if (nargin < 1)
    print_usage ();
  elseif (any (fix (x) != x))
    error ("isprime: X contains non-integer entries");
  endif

  if (isempty (x))
    tf = x;
    return;
  endif

  if (iscomplex (x))
    tf = isgaussianprime (x);
    return;
  endif

  ## Code strategy is to quickly compare entries in x with small primes
  ## using lookup(), then do direct division on larger numbers up to
  ## a threshold, then call Miller-Rabin for numbers over the threshold.

  x = abs (x);  # handle negative entries

  ## Generate prime table of suitable length up to maxp.
  ## The value of maxp needs to be at least 37,
  ## because of the method used by __isprimelarge__ below.
  maxp = 37;
  pr = [2 3 5 7 11 13 17 19 23 29 31 37];
  tf = lookup (pr, x, "b");  # quick search for table matches.

  THRESHOLD = 195e8;
  ## FIXME: THRESHOLD is the input value at which Miller-Rabin
  ## becomes more efficient than direct division. For smaller numbers,
  ## use direct division. For larger numbers, use Miller-Rabin.
  ##
  ## From numerical experiments in Jun 2022, this was observed:
  ##    THRESHOLD       Division       Miller-Rabin
  ##         29e9       29.8196s       26.2484s       (previous value)
  ##         20e9       26.7445s       26.0161s
  ##         10e9       20.9330s       25.3247s
  ##         19e9       26.5397s       26.8987s
  ##        195e8       26.5735s       26.4749s
  ## which is close enough, so new threshold = 195e8.
  ##
  ## The test code was this:
  ##   n = THRESHOLD - (1:1e7); tic; isprime(n); toc
  ##   n = THRESHOLD + (1:1e7); tic; isprime(n); toc
  ##
  ## Two notes for future programmers:
  ##
  ## 1. Test and tune THRESHOLD periodically. Miller-Rabin is only CPU-limited,
  ##    while factorization by division is very memory-intensive. This is
  ##    plainly noticeable from the loudness of the computer fans when running
  ##    the division technique! CPU speed and RAM speed scale differently over
  ##    time, so test and tune THRESHOLD periodically.
  ##
  ## 2. If you make improvements elsewhere in the code that favor one over
  ##    the other (not symmetric), you should also retune THRESHOLD afterwards.
  ##    If the Miller-Rabin part is sped up, the optimum THRESHOLD will
  ##    decrease, and if factorization is sped up, it will increase.
  ##

  ## Process large entries that are still suitable for direct division
  m = x (x > maxp & x <= THRESHOLD);
  if ( ! isempty (m))
    ## Start by dividing through by the small primes until the remaining list
    ## of entries is small (and most likely prime themselves).
    pr2 = primes (sqrt (max (m)));
    tf |= lookup (pr2, x, "b");
    for p = pr2
      m = m(rem (m, p) != 0);
      if (numel (m) < numel (pr) / 10)
        break;
      endif
    endfor

    ## Check the remaining list of possible primes against the remaining
    ## prime factors which were not tested in the for loop.
    ## This is just an optimization to use arrayfun over for loop.
    pr2 = pr2 (pr2 > p);
    mm = arrayfun (@(x) all (rem (x, pr2)), m);
    m = m(mm);

    ## Add any remaining entries, which are truly prime, to the results.
    if ( ! isempty (m))
      tf |= lookup (sort (m), x, "b");
    endif
  endif

  ## Process remaining entries (everything above THRESHOLD) with Miller-Rabin
  ii = (x(:)' > THRESHOLD);
  tf(ii) = __isprimelarge__ (x(ii));

endfunction

function tf = isgaussianprime (z)

  ## Assume prime unless proven otherwise
  tf = true (size (z));

  x = real (z);
  y = imag (z);

  ## If purely real or purely imaginary, ordinary prime test for
  ## that complex part if that part is 3 mod 4.
  xidx = y==0 & mod (x, 4) == 3;
  yidx = x==0 & mod (y, 4) == 3;

  tf(xidx) &= isprime (x(xidx));
  tf(yidx) &= isprime (y(yidx));

  ## Otherwise, prime if x^2 + y^2 is prime
  zidx = ! (xidx | yidx);          # Skip entries that were already evaluated
  zabs = x(zidx).^2 + y(zidx).^2;
  tf(zidx) &= isprime (zabs);

endfunction


%!assert (isprime (3), true)
%!assert (isprime (4), false)
%!assert (isprime (uint64 (18446744073709551557)), true)
%!assert (isprime (5i), false)
%!assert (isprime (7i), true)
%!assert (isprime ([1+2i, (2+3i)*(-1+2i)]), [true, false])
%!assert (isprime (-2), true)
%!assert (isprime (complex (-2)), false)
%!assert (isprime (2i), false)
%!assert (isprime ([i, 2, 3, 5]), [false, false, true, false])
%!assert (isprime (0), false)
%!assert (isprime (magic (3)), logical ([0, 0, 0; 1, 1, 1; 0, 0, 1]))

## Test input validation
%!error <Invalid call> isprime ()
%!error <X contains non-integer entries> isprime (0.5i)
%!error <X contains non-integer entries> isprime (0.5)
