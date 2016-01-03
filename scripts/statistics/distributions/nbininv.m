## Copyright (C) 2015 Lachlan Andrew
## Copyright (C) 2012-2015 Rik Wehbring
## Copyright (C) 1995-2012 Kurt Hornik
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
## @deftypefn {} {} nbininv (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the negative binomial distribution with parameters
## @var{n} and @var{p}.
##
## When @var{n} is integer this is the Pascal distribution.
## When @var{n} is extended to real numbers this is the Polya distribution.
##
## The number of failures in a Bernoulli experiment with success probability
## @var{p} before the @var{n}-th success follows this distribution.
## @end deftypefn

function inv = nbininv (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (n) || ! isscalar (p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("nbininv: X, N, and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n) || iscomplex (p))
    error ("nbininv: X, N, and P must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single") || isa (p, "single"))
    inv = zeros (size (x), "single");
  else
    inv = zeros (size (x));
  endif

  k = (isnan (x) | (x < 0) | (x > 1) | isnan (n) | (n < 1) | (n == Inf)
       | isnan (p) | (p < 0) | (p > 1));
  inv(k) = NaN;

  k = (x == 1) & (n > 0) & (n < Inf) & (p >= 0) & (p <= 1);
  inv(k) = Inf;

  k = find ((x >= 0) & (x < 1) & (n > 0) & (n < Inf)
            & (p > 0) & (p <= 1));
  if (! isempty (k))
    x = x(k);
    m = zeros (size (k));
    if (isscalar (n) && isscalar (p))
      [m, unfinished] = scalar_nbininv (x(:), n, p);
      m(unfinished) = bin_search_nbininv (x(unfinished), n, p);
    else
      m = bin_search_nbininv (x, n(k), p(k));
    endif
    inv(k) = m;
  endif

endfunction


## Core algorithm to calculate the inverse negative binomial, for n and p real
## scalars and y a column vector, and for which the output is not NaN or Inf.
## Compute CDF in batches of doubling size until CDF > x, or answer > 500.
## Return the locations of unfinished cases in k.
function [m, k] = scalar_nbininv (x, n, p)
  k = 1:length (x);
  m = zeros (size (x));
  prev_limit = 0;
  limit = 10;
  do
    cdf = nbincdf (prev_limit:limit, n, p);
    r = bsxfun (@le, x(k), cdf);
    [v, m(k)] = max (r, [], 2);     # find first instance of x <= cdf
    m(k) += prev_limit - 1;
    k = k(v == 0);

    prev_limit = limit;
    limit += limit;
  until (isempty (k) || limit >= 1000)

endfunction

## Vectorized binary search.
## Can handle vectors n and p, and is faster than the scalar case when the
## answer is large.
## Could be optimized to call nbincdf only for a subset of the x at each stage,
## but care must be taken to handle both scalar and vector n,p.  Bookkeeping
## may cost more than the extra computations.
function m = bin_search_nbininv (x, n, p)
  k = 1:length (x);
  lower = zeros (size (x));
  limit = 1;
  while (any (k) && limit < 1e100)
    cdf = nbincdf (limit, n, p);
    k = (x > cdf);
    lower(k) = limit;
    limit += limit;
  endwhile
  upper = max (2*lower, 1);
  k = find (lower != limit/2);    # elements for which above loop finished
  for i = 1:ceil (log2 (max (lower)))
    mid = (upper + lower)/2;
    cdf = nbincdf (floor (mid), n, p);
    r = (x <= cdf);
    upper(r)  = mid(r);
    lower(! r) = mid(! r);
  endfor
  m = ceil (lower);
  m(x > nbincdf (m, n, p)) += 1;  # fix off-by-one errors from binary search

endfunction


%!shared x
%! x = [-1 0 3/4 1 2];
%!assert (nbininv (x, ones (1,5), 0.5*ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (nbininv (x, 1, 0.5*ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (nbininv (x, ones (1,5), 0.5), [NaN 0 1 Inf NaN])
%!assert (nbininv (x, [1 0 NaN Inf 1], 0.5), [NaN NaN NaN NaN NaN])
%!assert (nbininv (x, [1 0 1.5 Inf 1], 0.5), [NaN NaN 2 NaN NaN])
%!assert (nbininv (x, 1, 0.5*[1 -Inf NaN Inf 1]), [NaN NaN NaN NaN NaN])
%!assert (nbininv ([x(1:2) NaN x(4:5)], 1, 0.5), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (nbininv ([x, NaN], 1, 0.5), [NaN 0 1 Inf NaN NaN])
%!assert (nbininv (single ([x, NaN]), 1, 0.5), single ([NaN 0 1 Inf NaN NaN]))
%!assert (nbininv ([x, NaN], single (1), 0.5), single ([NaN 0 1 Inf NaN NaN]))
%!assert (nbininv ([x, NaN], 1, single (0.5)), single ([NaN 0 1 Inf NaN NaN]))

## Test accuracy, to within +/- 1 since it is a discrete distribution
%!shared y, tol
%! y = magic (3) + 1;
%! tol = 1;
%!assert (nbininv (nbincdf (1:10, 3, 0.1), 3, 0.1), 1:10, tol)
%!assert (nbininv (nbincdf (1:10, 3./(1:10), 0.1), 3./(1:10), 0.1), 1:10, tol)
%!assert (nbininv (nbincdf (y, 3./y, 1./y), 3./y, 1./y), y, tol)

## Test input validation
%!error nbininv ()
%!error nbininv (1)
%!error nbininv (1,2)
%!error nbininv (1,2,3,4)
%!error nbininv (ones (3), ones (2), ones (2))
%!error nbininv (ones (2), ones (3), ones (2))
%!error nbininv (ones (2), ones (2), ones (3))
%!error nbininv (i, 2, 2)
%!error nbininv (2, i, 2)
%!error nbininv (2, 2, i)

