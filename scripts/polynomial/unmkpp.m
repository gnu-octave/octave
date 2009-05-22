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
## @item @var{p}
## Polynomial coefficients for points in sample interval.  @code{@var{p}
## (@var{i}, :)} contains the coefficients for the polynomial over
## interval @var{i} ordered from highest to lowest.  If @code{@var{d} >
## 1}, @code{@var{p} (@var{r}, @var{i}, :)} contains the coefficients for 
## the r-th polynomial defined on interval @var{i}.  However, this is 
## stored as a 2-D array such that @code{@var{c} = reshape (@var{p} (:,
## @var{j}), @var{n}, @var{d})} gives @code{@var{c} (@var{i},  @var{r})}
## is the j-th coefficient of the r-th polynomial over the i-th interval.
## @item @var{n}
## Number of polynomial pieces.
## @item @var{k}
## Order of the polynomial plus 1.
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
  n = pp.n;
  k = pp.k;
  d = pp.d;
endfunction
