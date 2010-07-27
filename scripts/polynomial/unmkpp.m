## Copyright (C) 2000, 2006, 2007, 2008, 2009 Paul Kienzle
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
## @deftypefn {Function File} {[@var{x}, @var{p}, @var{n}, @var{k}, @var{d}] =} unmkpp (@var{pp})
##
## Extract the components of a piece-wise polynomial structure @var{pp}.
## These are as follows:
##
## @table @asis
## @item @var{x}
## Sample points.
##
## @item @var{p}
## Polynomial coefficients for points in sample interval.  @code{@var{p}
## (@var{i}, :)} contains the coefficients for the polynomial over
## interval @var{i} ordered from highest to lowest.  If @code{@var{d} >
## 1}, @code{@var{p} (@var{r}, @var{i}, :)} contains the coefficients for 
## the r-th polynomial defined on interval @var{i}.
##
## @item @var{n}
## Number of polynomial pieces.
##
## @item @var{k}
## Order of the polynomial plus 1.
##
## @item @var{d}
## Number of polynomials defined for each interval.
## @end table
##
## @seealso{mkpp, ppval, spline}
## @end deftypefn

function [x, P, n, k, d] = unmkpp (pp)
  if (nargin == 0)
    print_usage ();
  endif
  if (! isstruct (pp))
    error ("unmkpp: expecting piecewise polynomial structure");
  endif
  x = pp.x;
  P = pp.P;
  n = size (P, 2);
  k = size (P, 3);
  d = pp.d;
  if (d == 1)
    P = reshape (P, n, k);
  endif
endfunction
