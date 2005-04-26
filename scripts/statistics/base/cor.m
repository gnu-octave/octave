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
## @deftypefn {Function File} {} cor (@var{x}, @var{y})
## The (@var{i}, @var{j})-th entry of @code{cor (@var{x}, @var{y})} is
## the correlation between the @var{i}-th variable in @var{x} and the
## @var{j}-th variable in @var{y}.
##
## For matrices, each row is an observation and each column a variable;
## vectors are always observations and may be row or column vectors.
##
## @code{cor (@var{x})} is equivalent to @code{cor (@var{x}, @var{x})}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Compute correlations

function retval = cor (x, y)

  if (nargin < 1 || nargin > 2)
    usage ("cor (x, y)");
  endif

  if (nargin == 2)
    c = cov (x, y);
    s = std (x)' * std (y);
    retval = c ./ s;
  elseif (nargin == 1)
    c = cov (x);
    s = reshape (sqrt (diag (c)), 1, columns (c));
    retval = c ./ (s' * s);
  endif

endfunction
