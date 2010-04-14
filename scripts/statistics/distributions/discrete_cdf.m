## Copyright (C) 2010 David Bateman
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
## @deftypefn {Function File} {} discrete_cdf (@var{x}, @var{v}, @var{p})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of a univariate discrete distribution which
## assumes the values in @var{v} with probabilities @var{p}.
## @end deftypefn

function cdf = discrete_cdf (x, v, p)

  if (nargin != 3)
    print_usage ();
  endif

  sz = size (x);

  if (! isvector (v))
    error ("discrete_cdf: v must be a vector");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_cdf: p must be a vector with length (v) elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_cdf: p must be a nonzero, nonnegative vector");
  endif

  n = numel (x);
  m = length (v);
  x = reshape (x, n, 1);
  v = reshape (v, 1, m);
  p = reshape (p / sum (p), m, 1);

  cdf = zeros (sz);
  k = find (isnan (x));
  if (any (k))
    cdf (k) = NaN;
  endif
  k = find (!isnan (x));
  if (any (k))
    n = length (k);
    [vs, vi] = sort (v);
    cdf = [0 ; cumsum(p(vi))](lookup (vs, x(k)) + 1);
  endif

endfunction
