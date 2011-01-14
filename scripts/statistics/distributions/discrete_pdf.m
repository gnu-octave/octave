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
## @deftypefn {Function File} {} discrete_pdf (@var{x}, @var{v}, @var{p})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of a univariate discrete distribution which assumes
## the values in @var{v} with probabilities @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: pDF of a discrete distribution

function pdf = discrete_pdf (x, v, p)

  if (nargin != 3)
    print_usage ();
  endif

  sz = size (x);

  if (! isvector (v))
    error ("discrete_pdf: V must be a vector");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_pdf: P must be a vector with length (V) elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_pdf: P must be a nonzero, nonnegative vector");
  endif

  n = numel (x);
  m = length (v);
  x = reshape (x, n, 1);
  v = reshape (v, 1, m);
  p = reshape (p / sum (p), m, 1);

  pdf = NaN (sz);
  k = find (!isnan (x));
  if (any (k))
    n = length (k);
    [vs, vi] = sort (v);
    pdf (k) = p (vi(lookup (vs, x(k), 'm')));
  endif

endfunction
