## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} cov (@var{x}, @var{y})
## If each row of @var{x} and @var{y} is an observation and each column is
## a variable, the (@var{i}, @var{j})-th entry of
## @code{cov (@var{x}, @var{y})} is the covariance between the @var{i}-th
## variable in @var{x} and the @var{j}-th variable in @var{y}.  If called
## with one argument, compute @code{cov (@var{x}, @var{x})}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute covariances

function c = cov (x, y)

  if (nargin < 1 || nargin > 2)
    usage ("cov (x, y)");
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
