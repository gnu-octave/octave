## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} ranks (@var{x})
## If @var{x} is a vector, return the (column) vector of ranks of
## @var{x} adjusted for ties.
##
## If @var{x} is a matrix, do the above for each column of @var{x}.
## @end deftypefn

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Compute ranks

## This code is rather ugly, but is there an easy way to get the ranks
## adjusted for ties from sort?

function y = ranks (x)

  if (nargin != 1)
    usage ("ranks (x)");
  endif

  y = [];

  [r, c] = size (x);
  if ((r == 1) && (c > 0))
    p = x' * ones (1, c);
    y = sum (p < p') + (sum (p == p') + 1) / 2;
  elseif (r > 1)
    o = ones (1, r);
    for i = 1 : c;
      p = x (:, i) * o;
      y = [y, ( sum (p < p') + (sum (p == p') + 1) / 2 )'];
    endfor
  endif

endfunction
