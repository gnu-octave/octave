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

## usage:  [t, l_x] = table (x)
##         [t, l_x, l_y] = table (x, y)
##
## Create a contingency table t from data vectors.  The l vectors are
## the corresponding levels.
##
## Currently, only 1- and 2-dimensional tables are supported.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Cross tabulation

function [t, v, w] = table (x, y)

  if (nargin == 1)
    if !(is_vector (x))
      error ("table:  x must be a vector");
    endif
    v = values (x);
    for i = 1 : length (v)
      t(i) = sum (x == v(i) | isnan (v(i)) * isnan (x));
    endfor
  elseif (nargin == 2)
    if !(is_vector (x) && is_vector (y) && (length (x) == length (y)))
      error ("table:  x and y must be vectors of the same length");
    endif
    v = values (x);
    w = values (y);
    for i = 1 : length (v)
      for j = 1 : length (w)
        t(i,j) = sum ((x == v(i) | isnan (v(i)) * isnan (x)) &
                      (y == w(j) | isnan (w(j)) * isnan (y)));
      endfor
    endfor
  else
    usage ("[t, l_x, ...] = table (x, ...)");
  endif

endfunction