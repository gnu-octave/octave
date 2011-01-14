## Copyright (C) 1996-2011 Kurt Hornik
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
## @deftypefn {Function File} {} discrete_inv (@var{x}, @var{v}, @var{p})
## For each component of @var{x}, compute the quantile (the inverse of
## the CDF) at @var{x} of the univariate distribution which assumes the
## values in @var{v} with probabilities @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of a discrete distribution

function inv = discrete_inv (x, v, p)

  if (nargin != 3)
    print_usage ();
  endif

  sz = size (x);

  if (! isvector (v))
    error ("discrete_inv: V must be a vector");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_inv: P must be a vector with length (V) elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_inv: P must be a nonzero, nonnegative vector");
  endif

  n = numel (x);
  x = reshape (x, 1, n);
  m = length (v);
  [v, idx] = sort (v);
  p = reshape (cumsum (p (idx) / sum (p)), m, 1);

  inv = NaN (sz);
  if (any (k = find (x == 0)))
    inv(k) = -Inf;
  endif
  if (any (k = find (x == 1)))
    inv(k) = v(m) * ones (size (k));
  endif

  if (any (k = find ((x > 0) & (x < 1))))
    n = length (k);
    inv (k) = v(length (p) - lookup (sort (p,"descend"), x(k)) + 1);
  endif

endfunction


