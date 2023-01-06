////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "error.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// This function implements the Schrage technique for modular multiplication.
// The returned value is equivalent to "mod (a*b, modulus)"
// but calculated without overflow.
uint64_t
safemultiply (uint64_t a, uint64_t b, uint64_t modulus)
{
  if (! a || ! b)
    return 0;
  else if (b == 1)
    return a;
  else if (a == 1)
    return b;
  else if (a > b)
    {
      uint64_t tmp = a;
      a = b;
      b = tmp;
    }

  uint64_t q = modulus / a;
  uint64_t r = modulus - q * a;
  uint64_t term1 = a * (b % q);
  uint64_t term2 = (r < q) ? r * (b / q) : safemultiply (r, b / q, modulus);
  return (term1 > term2) ? (term1 - term2) : (term1 + modulus - term2);
}

// This function returns "mod (a^b, modulus)"
// but calculated without overflow.
uint64_t
safepower (uint64_t a, uint64_t b, uint64_t modulus)
{
  uint64_t retval = 1;
  while (b > 0)
    {
      if (b & 1)
        retval = safemultiply (retval, a, modulus);
      b >>= 1;
      a = safemultiply (a, a, modulus);
    }
  return retval;
}

// This function implements a single round of Miller-Rabin primality testing.
// Returns false if composite, true if pseudoprime for this divisor.
bool
millerrabin (uint64_t div, uint64_t d, uint64_t r, uint64_t n)
{
  uint64_t x = safepower (div, d, n);
  if (x == 1 || x == n-1)
    return true;

  for (uint64_t j = 1; j < r; j++)
    {
      x = safemultiply (x, x, n);
      if (x == n-1)
        return true;
    }
  return false;
}

// This function uses the Miller-Rabin test to find out whether the input is
// prime or composite. The input is required to be a scalar 64-bit integer.
bool
isprimescalar (uint64_t n)
{
  // Fast return for even numbers.
  // n==2 is excluded by the time this function is called.
  if (! (n & 1))
    return false;

  // n is now odd. Rewrite n as d * 2^r + 1, where d is odd.
  uint64_t d = n-1;
  uint64_t r = 0;
  while (! (d & 1))
    {
      d >>= 1;
      r++;
    }

  // Miller-Rabin test with the first 12 primes.
  // If the number passes all 12 tests, then it is prime.
  // If it fails any, then it is composite.
  // The first 12 primes suffice to test all 64-bit integers.
  return millerrabin ( 2, d, r, n) && millerrabin ( 3, d, r, n)
         && millerrabin ( 5, d, r, n) && millerrabin ( 7, d, r, n)
         && millerrabin (11, d, r, n) && millerrabin (13, d, r, n)
         && millerrabin (17, d, r, n) && millerrabin (19, d, r, n)
         && millerrabin (23, d, r, n) && millerrabin (29, d, r, n)
         && millerrabin (31, d, r, n) && millerrabin (37, d, r, n);

  /*
  Mathematical references for the curious as to why we need only
  the 12 smallest primes for testing all 64-bit numbers:
  (1) https://oeis.org/A014233
      Comment: a(12) > 2^64.  Hence the primality of numbers < 2^64 can be
      determined by asserting strong pseudoprimality to all prime bases <= 37
      (=prime(12)). Testing to prime bases <=31 does not suffice,
      as a(11) < 2^64 and a(11) is a strong pseudoprime
      to all prime bases <= 31 (=prime(11)). - Joerg Arndt, Jul 04 2012
  (2) https://arxiv.org/abs/1509.00864
      Strong Pseudoprimes to Twelve Prime Bases
      Jonathan P. Sorenson, Jonathan Webster

  In addition, a source listed here: https://miller-rabin.appspot.com/
  reports that all 64-bit numbers can be covered with only 7 divisors,
  namely 2, 325, 9375, 28178, 450775, 9780504, and 1795265022.
  There was no peer-reviewed article to back it up though,
  so this code uses the 12 primes <= 37.
  */

}

DEFUN (__isprimelarge__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{x} =} __isprimelarge__ (@var{n})
Use the Miller-Rabin test to find out whether the elements of N are prime or
composite.  The input N is required to be a vector or array of 64-bit integers.
You should call isprime(N) instead of directly calling this function.

@seealso{isprime, factor}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function is intended for internal use by isprime.m,
  // so the following error handling should not be necessary. But it is
  // probably good practice for any curious users calling it directly.
  uint64NDArray vec = args(0).xuint64_array_value
                      ("__isprimelarge__: unable to convert input. Call isprime() instead.");

  boolNDArray retval (vec.dims(), false);

  for (octave_idx_type i = vec.numel() - 1; i >= 0; i--)
    retval(i) = isprimescalar (vec(i));
  // Note: If vec(i) <= 37, this function could go into an infinite loop.
  // That situation does not arise when calling this from isprime.m
  // but it could arise if the user calls this function directly with low input
  // or negative input.
  // But it turns out that adding this validation:
  //   "if (vec(i) <= 37) then raise an error else call isprimescalar (vec(i))"
  // slows this function down by over 20% for some inputs,
  // so it is better to leave all the input validation in isprime.m
  // and not add it here. The function DOCSTRING now explicitly says:
  // "You should call isprime(N) instead of directly calling this function."

  return ovl (retval);
}

/*
%!assert (__isprimelarge__ (41:50), logical ([1 0 1 0 0 0 1 0 0 0]))
%!assert (__isprimelarge__ (uint64 (12345)), false)
%!assert (__isprimelarge__ (uint64 (2147483647)), true)
%!assert (__isprimelarge__ (uint64 (2305843009213693951)), true)
%!assert (__isprimelarge__ (uint64 (18446744073709551557)), true)

%!assert (__isprimelarge__ ([uint64(12345), uint64(2147483647), ...
%!                           uint64(2305843009213693951), ...
%!                           uint64(18446744073709551557)]),
%!        logical ([0 1 1 1]))

%!error <unable to convert input> (__isprimelarge__ ({'foo'; 'bar'}))
*/


// This function implements a fast, private GCD function
// optimized for uint64_t.  No input validation by design.
inline
uint64_t
localgcd (uint64_t a, uint64_t b)
{
  return (a <= b) ? ( (b % a == 0) ? a : localgcd (a, b % a) )
         : ( (a % b == 0) ? b : localgcd (a % b, b) );
}

// This function implements a textbook version of the Pollard Rho
// factorization algorithm with Brent update.
// The code is short and simple, but the math behind it is complicated.
uint64_t
pollardrho (uint64_t n, uint64_t c = 1)
{
  uint64_t i = 1, j = 2;    // cycle index values
  uint64_t x = (c+1) % n;   // can also be rand () % n
  uint64_t y = x;           // other value in the chain
  uint64_t g = 0;           // GCD

  while (true)
    {
      i++;

      // Calculate x = mod (x^2 + c, n) without overflow.
      x = safemultiply (x, x, n) + c;
      if (x >= n)
        x -= n;

      // Calculate GCD (abs (x-y), n).
      g = (x > y) ? localgcd (x - y, n) : (x < y) ? localgcd (y - x, n) : 0;

      if (i == j)  // cycle detected ==> double j
        {
          y = x;
          j <<= 1;
        }

      if (g == n || i > 1000000)  // cut losses, restart with a different c
        return pollardrho (n, c + 2);

      if (g > 1)  // found GCD ==> exit loop properly
        {
          error_unless (n % g == 0);  // theoretical possibility of GCD error
          return g;
        }
    }
}

DEFUN (__pollardrho__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{x} =} __pollardrho__ (@var{n})
Private function.  Use the Pollard Rho test to find a factor of @var{n}.
The input @var{n} is required to be a composite 64-bit integer.
Do not pass it a prime input!  You should call factor(@var{n}) instead
of directly calling this function.

@seealso{isprime, factor}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_uint64 inp = args(0).xuint64_scalar_value
                      ("__pollardrho__: unable to convert input. Call factor() instead.");

  uint64_t n = inp;
  octave_uint64 retval = pollardrho (n);

  return ovl (retval);
}

/*
%!assert (__pollardrho__ (uint64 (78567695338254293)), uint64 (443363))
%!assert (__pollardrho__ (1084978968791), uint64 (832957))

%!error <unable to convert input> (__pollardrho__ ({'foo'; 'bar'}))
*/

OCTAVE_END_NAMESPACE(octave)
