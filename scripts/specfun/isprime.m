## Copyright (C) 2000-2013 Paul Kienzle
## Copyright (C) 2010 VZLU Prague
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
## @deftypefn {Function File} {} isprime (@var{x})
## Return a logical array which is true where the elements of @var{x} are
## prime numbers and false where they are not.
##
## @code{isprime} is appropriate if the maximum value in @var{x} is not too
## large (< 1e15).  For larger values special purpose factorization code
## should be used.
##
## @example
## @group
## isprime (1:6)
##     @result{} [0, 1, 1, 0, 1, 0]
## @end group
## @end example
## @seealso{primes, factor, gcd, lcm}
## @end deftypefn

function t = isprime (x)

  if (nargin != 1)
    print_usage ();
  elseif (! isreal (x) || any ((x < 0 | x != fix (x))(:)))
    error ("isprime: X must be a non-negative integer");
  endif

  if (isempty (x))
    t = x;
    return;
  endif

  maxn = max (x(:));
  ## generate prime table of suitable length.
  maxp = min (maxn, max (sqrt (maxn), 1e7)); # FIXME: threshold not optimized.
  pr = primes (maxp);
  ## quick search for table matches.
  t = lookup (pr, x, "b");
  ## take the rest.
  m = x(x > maxp);
  if (! isempty (m))
    ## there are still possible primes. filter them out by division.
    if (maxn <= intmax ("uint32"))
      m = uint32 (m);
    elseif (maxn <= intmax ("uint64"))
      m = uint64 (m);
    else
      warning ("isprime: X contains integers too large to be tested");
    endif
    pr = cast (pr(pr <= sqrt (maxn)), class (m));
    for p = pr
      m = m(rem (m, p) != 0);
      if (length (m) < length (pr) / 10)
        break;
      endif
    endfor
    pr = pr(pr > p);
    mm = arrayfun (@(x) all (rem (x, pr)), m);
    m = m(mm);
    if (! isempty (m))
      m = cast (sort (m), class (x));
      t |= lookup (m, x, "b");
    endif
  endif

endfunction


%!assert (isprime (3), true)
%!assert (isprime (4), false)
%!assert (isprime (magic (3)), logical ([0, 0, 0; 1, 1, 1; 0, 0, 1]))

%% Test input validation
%!error isprime ()
%!error isprime (1, 2)
%!error <X must be a non-negative integer> isprime (i)
%!error <X must be a non-negative integer> isprime ([1 2; -3 4])
%!error <X must be a non-negative integer> isprime ([1 2; 3.1 4])

