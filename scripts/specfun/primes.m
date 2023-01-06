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
## @deftypefn {} {@var{p} =} primes (@var{n})
## Return all primes up to @var{n}.
##
## The output data class (double, single, uint32, etc.@:) is the same as the
## input class of @var{n}.  The algorithm used is the Sieve of Eratosthenes.
##
## Note: For a specific number @var{n} of primes, call
## @code{list_primes (@var{n})}.  Alternatively, call
## @code{primes (@var{n}*log (@var{k}*@var{n}))(1:@var{n})} where @var{k} is
## about 5 or 6.  This works because the distance from one prime to the next is
## proportional to the logarithm of the prime, on average.  On integrating,
## there are about @var{n} primes less than @code{@var{n} * log (5*@var{n})}.
##
## @seealso{list_primes, isprime}
## @end deftypefn

function p = primes (n)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isscalar (n) && isreal (n)))
    error ("primes: N must be a real scalar");
  endif
  if (ischar (n))
    n = double (n);
  endif
  if (! isfinite (n) && n != -Inf)
    error ("primes: N must be finite (not +Inf or NaN)");
  endif

  cls = class (n);     # if n is not double, store its class
  n = double (n);      # and use only double for internal use.
  # This conversion is needed for both calculation speed (twice as fast as
  # integer) and also for the accuracy of the sieve calculation when given
  # integer input, to avoid unwanted rounding in the sieve lengths.

  if (n > flintmax ())
    warning ("primes: input exceeds flintmax.  Results may be inaccurate.");
  endif

  if (n < 353)
    ## Lookup table of first 70 primes
    a = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, ...
         53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, ...
         109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, ...
         173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, ...
         233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, ...
         293, 307, 311, 313, 317, 331, 337, 347, 349];
    p = a(a <= n);
  elseif (n < 100e3)
    ## Classical Sieve algorithm
    ## Fast, but memory scales as n/2.
    len = floor ((n-1)/2);        # length of the sieve
    sieve = true (1, len);        # assume every odd number is prime
    for i = 1:(sqrt (n)-1)/2      # check up to sqrt (n)
      if (sieve(i))               # if i is prime, eliminate multiples of i
        sieve(3*i+1:2*i+1:len) = false; # do it
      endif
    endfor
    p = [2, 1+2*find(sieve)];     # primes remaining after sieve
  else
    ## Sieve algorithm optimized for large n
    ## Memory scales as n/3 or 1/6th less than classical Sieve
    lenm = floor ((n+1)/6);       # length of the 6n-1 sieve
    lenp = floor ((n-1)/6);       # length of the 6n+1 sieve
    sievem = true (1, lenm);      # assume every number of form 6n-1 is prime
    sievep = true (1, lenp);      # assume every number of form 6n+1 is prime

    for i = 1:(sqrt (n)+1)/6      # check up to sqrt (n)
      if (sievem(i))              # if i is prime, eliminate multiples of i
        sievem(7*i-1:6*i-1:lenm) = false;
        sievep(5*i-1:6*i-1:lenp) = false;
      endif                       # if i is prime, eliminate multiples of i
      if (sievep(i))
        sievep(7*i+1:6*i+1:lenp) = false;
        sievem(5*i+1:6*i+1:lenm) = false;
      endif
    endfor
    p = sort ([2, 3, 6*find(sievem)-1, 6*find(sievep)+1]);
  endif

  # cast back to the type of the input
  p = cast (p, cls);

endfunction


%!assert (size (primes (350)), [1, 70])
%!assert (primes (357)(end), 353)
%!assert (primes (uint64 (358))(end), uint64 (353))
%!assert (primes (int32 (1e6))(end), int32 (999983))
%!assert (class (primes (single (10))), "single")
%!assert (class (primes (uint8 (10))), "uint8")
%!assert (primes (-Inf), zeros (1,0))

%!error <Invalid call> primes ()
%!error <N must be a real scalar> primes (ones (2,2))
%!error <N must be a real scalar> primes (5i)
%!error <N must be finite> primes (Inf)
%!error <N must be finite> primes (NaN)
