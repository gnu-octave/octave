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
## @deftypefn {Function File} {} binoinv (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the binomial distribution with parameters
## @var{n} and @var{p}, where @var{n} is the number of trials and
## @var{p} is the probability of success.
## @end deftypefn

function inv = binoinv (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (n) || ! isscalar (p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("binoinv: X, N, and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n) || iscomplex (p))
    error ("binoinv: X, N, and P must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single") || isa (p, "single"));
    inv = zeros (size (x), "single");
  else
    inv = zeros (size (x));
  endif

  k = (!(x >= 0) | !(x <= 1) | !(n >= 0) | (n != fix (n)) |
       !(p >= 0) | !(p <= 1));
  inv(k) = NaN;

  k = find ((x >= 0) & (x <= 1) & (n >= 0) & (n == fix (n)
             & (p >= 0) & (p <= 1)));
  if (any (k))
    x = x(k);
    if (isscalar (n) && isscalar (p))
      [inv(k), unfinished] = scalar_binoinv (x(:), n, p);
      k = k(unfinished);
      if (! isempty (k))
        inv(k) = bin_search_binoinv (x(k), n, p);
      endif
    else
      [inv(k), unfinished] = vector_binoinv (x(:), n(:), p(:));
      k = k(unfinished);
      if (! isempty (k))
        inv(k) = bin_search_binoinv (x(k), n(k), p(k));
      endif
    endif
  endif

endfunction

## Core algorithm to calculate the inverse binomial, for n and p real scalars
## and y a column vector, and for which the output is not NaN or Inf.
## Compute CDF in batches of doubling size until CDF > x, or answer > 500
## Return the locations of unfinished cases in k.
function [m, k] = scalar_binoinv (x, n, p)
  k = 1:length (x);
  m = zeros (size (x));
  prev_limit = 0;
  limit = 10;
  cdf = 0;
  v = 0;
  do
    cdf = binocdf (prev_limit:limit-1, n, p);
    r = bsxfun (@le, x(k), cdf);
    [v, m(k)] = max (r, [], 2);     # find first instance of x <= cdf
    m(k) += prev_limit - 1;
    k = k(v == 0);

    prev_limit = limit;
    limit += limit;
  until (isempty (k) || limit >= 1000)

endfunction

## Core algorithm to calculate the inverse binomial, for n, p, and y column
## vectors, and for which the output is not NaN or Inf.
## Compute CDF in batches of doubling size until CDF > x, or answer > 500
## Return the locations of unfinished cases in k.
## Calculates CDF by summing PDF, which is faster than calls to binocdf.
function [m, k] = vector_binoinv (x, n, p)
  k = 1:length(x);
  m = zeros (size (x));
  prev_limit = 0;
  limit = 10;
  cdf = 0;
  v = 0;
  do
    xx = repmat (prev_limit:limit-1, [length(k), 1]);
    nn = kron (ones (1, limit-prev_limit), n(k));
    pp = kron (ones (1, limit-prev_limit), p(k));
    pdf = binopdf (xx, nn, pp);
    pdf(:,1) += cdf(v==0, end);
    cdf = cumsum (pdf, 2);
    r = bsxfun (@le, x(k), cdf);
    [v, m(k)] = max (r, [], 2);     # find first instance of x <= cdf
    m(k) += prev_limit - 1;
    k = k(v == 0);

    prev_limit = limit;
    limit += min (limit, max (1e4/numel (k), 10));  # limit memory use
  until (isempty (k) || limit >= 1000)

endfunction

## Vectorized binary search.
## Can handle vectors n and p, and is faster than the scalar case when the
## answer is large.
## Could be optimized to call binocdf only for a subset of the x at each stage,
## but care must be taken to handle both scalar and vector n, p.  Bookkeeping
## may cost more than the extra computations.
function m = bin_search_binoinv (x, n, p)
  k = 1:length (x);
  lower = zeros (size (x));
  limit = 500;              # lower bound on point at which prev phase finished
  while (any (k) && limit < 1e100)
    cdf = binocdf (limit, n, p);
    k = (x > cdf);
    lower(k) = limit;
    limit += limit;
  end
  upper = max (2*lower, 1);
  k = find (lower != limit/2);       # elements for which above loop finished
  for i = 1:ceil (log2 (max (lower)))
    mid = (upper + lower)/2;
    cdf = binocdf (floor(mid(:)), n, p);
    r = (x <= cdf);
    upper(r)  = mid(r);
    lower(!r) = mid(!r);
  endfor
  m = ceil (lower);
  m(x > binocdf (m(:), n, p)) += 1;  # fix off-by-one errors from binary search
endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (binoinv (x, 2*ones (1,5), 0.5*ones (1,5)), [NaN 0 1 2 NaN])
%!assert (binoinv (x, 2, 0.5*ones (1,5)), [NaN 0 1 2 NaN])
%!assert (binoinv (x, 2*ones (1,5), 0.5), [NaN 0 1 2 NaN])
%!assert (binoinv (x, 2*[0 -1 NaN 1.1 1], 0.5), [NaN NaN NaN NaN NaN])
%!assert (binoinv (x, 2, 0.5*[0 -1 NaN 3 1]), [NaN NaN NaN NaN NaN])
%!assert (binoinv ([x(1:2) NaN x(4:5)], 2, 0.5), [NaN 0 NaN 2 NaN])

## Test class of input preserved
%!assert (binoinv ([x, NaN], 2, 0.5), [NaN 0 1 2 NaN NaN])
%!assert (binoinv (single ([x, NaN]), 2, 0.5), single ([NaN 0 1 2 NaN NaN]))
%!assert (binoinv ([x, NaN], single (2), 0.5), single ([NaN 0 1 2 NaN NaN]))
%!assert (binoinv ([x, NaN], 2, single (0.5)), single ([NaN 0 1 2 NaN NaN]))

## Test accuracy, to within +/- 1 since it is a discrete distribution
%!shared y, tol
%! y = magic (3) + 1;
%! tol = 1;
%!assert (binoinv (binocdf (1:10, 11, 0.1), 11, 0.1), 1:10, tol)
%!assert (binoinv (binocdf (1:10, 2*(1:10), 0.1), 2*(1:10), 0.1), 1:10, tol)
%!assert (binoinv (binocdf (y, 2*y, 1./y), 2*y, 1./y), y, tol)

## Test input validation
%!error binoinv ()
%!error binoinv (1)
%!error binoinv (1,2)
%!error binoinv (1,2,3,4)
%!error binoinv (ones (3), ones (2), ones (2))
%!error binoinv (ones (2), ones (3), ones (2))
%!error binoinv (ones (2), ones (2), ones (3))
%!error binoinv (i, 2, 2)
%!error binoinv (2, i, 2)
%!error binoinv (2, 2, i)

