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

## usage:  S = statistics (X)
##
## If X is a matrix, return a matrix S with the min, first quartile,
## median, third quartile, max, mean, std, skewness and kurtosis of the
## columns of X as its rows.
##
## If X is a vector, treat it as a column vector.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Compute basic statistics

function S = statistics (X)

  if (nargin != 1)
    usage ("S = statistics (X)");
  endif

  if (prod (size (X)) > 1)
    if (is_vector (X))
      X = reshape (X, length (X), 1);
    endif
    for k=1:columns(X)
      S(:,k) = [(min (X(:,k)));
                (empirical_inv ([0.25;0.5;0.75], X(:,k)));
                (max (X(:,k)));
                (mean (X(:,k)));
                (std (X(:,k)));
                (skewness (X(:,k)));
                (kurtosis (X(:,k)))];
    endfor
  else
    error ("statistics:  invalid argument");
  endif

endfunction
