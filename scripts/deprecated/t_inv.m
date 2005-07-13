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
## @deftypefn {Function File} {} t_inv (@var{x}, @var{n})
## For each component of @var{x}, compute the quantile (the inverse of
## the CDF) at @var{x} of the t (Student) distribution with parameter
## @var{n}.
## @end deftypefn

## For very large n, the "correct" formula does not really work well,
## and the quantiles of the standard normal distribution are used
## directly.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the t distribution

function inv = t_inv (varargin)

 inv =  tinv (varargin{:});

endfunction
