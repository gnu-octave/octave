## Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2004, 2005,
##               2006, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} cov (@var{x}, @var{y})
## Compute covariance.
##
## If each row of @var{x} and @var{y} is an observation and each column is
## a variable, the (@var{i}, @var{j})-th entry of
## @code{cov (@var{x}, @var{y})} is the covariance between the @var{i}-th
## variable in @var{x} and the @var{j}-th variable in @var{y}.
## @iftex
## @tex
## $$
## \sigma_{ij} = {1 \over N-1} \sum_{i=1}^N (x_i - \bar{x})(y_i - \bar{y})
## $$
## where $\bar{x}$ and $\bar{y}$ are the mean values of $x$ and $y$.
## @end tex
## @end iftex
## If called with one argument, compute @code{cov (@var{x}, @var{x})}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute covariances

function c = cov (x, y)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (rows (x) == 1)
    x = x';
  endif
  n = rows (x);

  if (nargin == 2)
    if (rows (y) == 1)
      y = y';
    endif
    if (rows (y) != n)
      error ("cov: x and y must have the same number of observations");
    endif
    x = x - ones (n, 1) * sum (x) / n;
    y = y - ones (n, 1) * sum (y) / n;
    c = conj (x' * y / (n - 1));
  elseif (nargin == 1)
    x = x - ones (n, 1) * sum (x) / n;
    c = conj (x' * x / (n - 1));
  endif

endfunction

%!test
%! x = rand (10);
%! cx1 = cov (x);
%! cx2 = cov (x, x);
%! assert(size (cx1) == [10, 10] && size (cx2) == [10, 10] && norm(cx1-cx2) < 1e1*eps);

%!error cov ();

%!error cov (1, 2, 3);

