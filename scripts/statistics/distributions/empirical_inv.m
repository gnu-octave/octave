## Copyright (C) 1996, 1997  Kurt Hornik
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

## usage:  empirical_inv (X, DATA)
##
## For each element of X, compute the quantile (the inverse of the CDF)
## at X of the empirical distribution obtained from the univariate
## sample DATA.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Quantile function of the empirical distribution

function inv = empirical_inv (X, DATA)

  if (! is_vector (DATA))
    error ("empirical_inv:  DATA must be a vector");
  endif

  inv = discrete_inv (X, DATA, ones (size (DATA)) / length (DATA));

endfunction
