## Copyright (C) 1996, 1997  Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} discrete_inv (@var{x}, @var{v}, @var{p})
## For each component of @var{x}, compute the quantile (the inverse of
## the CDF) at @var{x} of the univariate distribution which assumes the
## values in @var{v} with probabilities @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of a discrete distribution

function inv = discrete_inv (x, v, p)

  if (nargin != 3)
    usage ("discrete_inv (x, v, p)");
  endif

  sz = size (x);

  if (! isvector (v))
    error ("discrete_inv: v must be a vector");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_inv: p must be a vector with length (v) elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_inv: p must be a nonzero, nonnegative vector");
  endif

  n = numel (x);
  x = reshape (x, 1, n);
  m = length (v);
  v = sort (v);
  s = reshape (cumsum (p / sum (p)), m, 1);

  ## Allow storage allocated for P to be reclaimed.
  p = [];

  inv = NaN * ones (sz);
  if (any (k = find (x == 0)))
    inv(k) = -Inf;
  endif
  if (any (k = find (x == 1)))
    inv(k) = v(m) * ones (size (k));
  endif

  if (any (k = find ((x > 0) & (x < 1))))
    n = length (k);

    ## The following loop is a space/time tradeoff in favor of space,
    ## since the dataset may be large.
    ##
    ## Vectorized code is:
    ##
    ##     inv(k) = v(sum ((ones (m, 1) * x(k)) > (s * ones (1, n))) + 1);

    for q = 1:n
      inv(k(q)) = v(sum (x(k(q)) > s) + 1);
    endfor
  endif

endfunction


