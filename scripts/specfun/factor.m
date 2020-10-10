########################################################################
##
## Copyright (C) 2000-2020 The Octave Project Developers
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
## (9.0072e+15) in order to factor correctly.
## @seealso{gcd, lcm, isprime, primes}
## @end deftypefn

function [pf, n] = factor (q)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isscalar (q) || ! isreal (q) || q < 0 || q != fix (q))
    error ("factor: Q must be a real non-negative integer");
  endif

  ## Special case of no primes less than sqrt(q).
  if (q < 4)
    pf = q;
    n = 1;
    return;
  endif

  cls = class (q); # store class
  q = double (q);  # internal algorithm relies on numbers being doubles.
  qorig = q;
  pf = [];
  ## There is at most one prime greater than sqrt(q), and if it exists,
  ## it has multiplicity 1, so no need to consider any factors greater
  ## than sqrt(q) directly.  [If there were two factors p1, p2 > sqrt(q),
  ## then q >= p1*p2 > sqrt(q)*sqrt(q) == q.  Contradiction.]
  p = primes (sqrt (q));
  while (q > 1)
    ## Find prime factors in remaining q.
    p = p(rem (q, p) == 0);
    if (isempty (p))
      ## Can't be reduced further, so q must itself be a prime.
      p = q;
    endif
    pf = [pf, p];
    ## Reduce q.
    q /= prod (p);
  endwhile
  pf = sort (pf);

  ## Verify algorithm was successful
  q = prod (pf);
  if (q != qorig)
    error ("factor: Q too large to factor");
  elseif (q >= flintmax ())
    warning ("factor: Q too large.  Answer is unreliable");
  endif

  ## Determine multiplicity.
  if (nargout > 1)
    idx = find ([0, pf] != [pf, 0]);
    pf = pf(idx(1:length (idx)-1));
    n = diff (idx);
  endif

 ## Restore class of input
 pf = feval (cls, pf);

endfunction


%!assert (factor (1), 1)
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
%!error factor ()
%!error factor (1,2)
%!error <Q must be a real non-negative integer> factor (6i)
%!error <Q must be a real non-negative integer> factor ([1,2])
%!error <Q must be a real non-negative integer> factor (1.5)
%!error <Q must be a real non-negative integer> factor (-20)
