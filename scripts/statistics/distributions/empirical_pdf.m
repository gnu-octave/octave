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

## usage:  empirical_pdf (X, DATA)
##
## For each element of X, compute the probability density function (PDF)
## at X of the empirical distribution obtained from the univariate
## sample DATA.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  PDF of the empirical distribution

function pdf = empirical_pdf (X, DATA)

  if (! is_vector (DATA))
    error ("empirical_pdf:  DATA must be a vector");
  endif

  pdf = discrete_pdf (X, DATA, ones (size (DATA)) / length (DATA));

endfunction
