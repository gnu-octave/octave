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
## @deftypefn {Function File} {} statistics (@var{x})
## If @var{x} is a matrix, return a matrix with the minimum, first
## quartile, median, third quartile, maximum, mean, standard deviation,
## skewness and kurtosis of the columns of @var{x} as its rows.
##
## If @var{x} is a vector, treat it as a column vector.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute basic statistics

function S = statistics (X, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  nd = ndims (X);
  sz = size (X);
  nel = numel (X);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    dim  = 1;
    while (dim < nd + 1 && sz(dim) == 1)
      dim = dim + 1;
    endwhile
    if (dim > nd)
      dim = 1;
    endif
  else
    if (! (isscalar (dim) && dim == round (dim))
	&& dim > 0
	&& dim < (nd + 1))
      error ("statistics: dim must be an integer and valid dimension");
    endif
  endif
  
  if (! ismatrix (X) || sz(dim) < 2)
    error ("statistics: invalid argument");
  endif    

  ## This code is a bit heavy, but is needed until empirical_inv 
  ## takes other than vector arguments.
  c = sz(dim);
  stride = prod (sz(1:dim-1));
  sz(dim) = 3;
  emp_inv = zeros (sz);
  for i = 1 : nel / c;
    offset = i;
    offset2 = 0;
    while (offset > stride)
      offset -= stride;
      offset2++;
    endwhile
    rng = [0 : c-1] * stride + offset + offset2 * stride * c;
    rng2 = [0 : 2] * stride + offset + offset2 * stride * 3;
    emp_inv(rng2) = empirical_inv ([0.25; 0.5; 0.75], X(rng));
  endfor

  S = cat (dim, min (X, [], dim), emp_inv, max (X, [], dim), mean (X, dim),
	   std (X, [], dim), skewness (X, dim), kurtosis (X, dim));

endfunction
