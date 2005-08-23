## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} corrcoef (@var{x}, @var{y})
## If each row of @var{x} and @var{y} is an observation and each column is
## a variable, the (@var{i}, @var{j})-th entry of
## @code{corrcoef (@var{x}, @var{y})} is the correlation between the
## @var{i}-th variable in @var{x} and the @var{j}-th variable in @var{y}.
## If called with one argument, compute @code{corrcoef (@var{x}, @var{x})}.
## @end deftypefn

## Author: Kurt Hornik <hornik@wu-wien.ac.at>
## Created: March 1993
## Adapted-By: jwe

function retval = corrcoef (x, y)

  if (nargin < 1 || nargin > 2)
    usage ("corrcoef (x, y)");
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
