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
## @deftypefn {Function File} {} studentize (@var{x})
## If @var{x} is a vector, subtract its mean and divide by its standard
## deviation.
##
## If @var{x} is a matrix, do the above for each column.
## @end deftypefn

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Subtract mean and divide by standard deviation

function t = studentize (x)

  if (nargin != 1)
    usage ("studentize (x)");
  endif

  if is_vector (x)
    if (std (x) == 0)
      t = zeros (size (x));
    else
      t = (x - mean (x)) / std (x);
    endif
  elseif is_matrix (x)
    l = ones (rows (x), 1);
    t = x - l * mean (x);
    t = t ./ (l * max ([(std (t)); (! any (t))]));
  else
    error ("studentize:  x must be a vector or a matrix.");
  endif

endfunction