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
## @deftypefn {Function File} {} spearman (@var{x}, @var{y})
## Compute Spearman's rank correlation coefficient @var{rho} for each of
## the variables specified by the input arguments.
##
## For matrices, each row is an observation and each column a variable;
## vectors are always observations and may be row or column vectors.
##
## @code{spearman (@var{x})} is equivalent to @code{spearman (@var{x},
## @var{x})}.
##
## For two data vectors @var{x} and @var{y}, Spearman's @var{rho} is the
## correlation of the ranks of @var{x} and @var{y}.
##
## If @var{x} and @var{y} are drawn from independent distributions,
## @var{rho} has zero mean and variance @code{1 / (n - 1)}, and is
## asymptotically normally distributed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Spearman's rank correlation rho

function rho = spearman (x, y)

  if ((nargin < 1) || (nargin > 2))
    print_usage ();
  endif

  if (rows (x) == 1)
    x = x';
  endif
  n = rows (x);

  if (nargin == 1)
    rho = cor (ranks (x));
  else
    if (rows (y) == 1)
      y = y';
    endif
    if (rows (y) != n)
      error ("spearman: x and y must have the same number of observations");
    endif
    rho = cor (ranks (x), ranks (y));
  endif

endfunction
