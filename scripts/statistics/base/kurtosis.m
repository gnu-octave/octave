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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} kurtosis (@var{x})
## If @var{x} is a vector of length @var{N}, return the kurtosis
## @iftex
## @tex
## $$
##  {\rm kurtosis} (x) = {1\over N \sigma(x)^4} \sum_{i=1}^N (x_i-\bar{x})^4 - 3
## $$
## @end tex
## @end iftex
## @ifinfo
## 
## @example
## kurtosis (x) = N^(-1) std(x)^(-4) sum ((x - mean(x)).^4) - 3
## @end example
## @end ifinfo
## 
## @noindent
## of @var{x}.  If @var{x} is a matrix, return the row vector containing
## the kurtosis of each column.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 29 July 1994
## Adapted-By: jwe

function retval = kurtosis (x)

  if (nargin != 1)
    usage ("kurtosis (x)");
  endif

  if (is_vector (x))
    x = x - mean (x);
    if (! any (x))
      retval = 0;
    else
      retval = sum (x .^ 4) / (length (x) * std (x) ^ 4) - 3;
    endif
  elseif (is_matrix (x))
    [nr, nc] = size (x);
    x = x - ones (nr, 1) * mean (x);
    retval = zeros (1, nc);
    s      = std (x);
    ind    = find (s > 0);
    retval (ind) = sum (x (:, ind) .^ 4) ./ (nr * s (ind) .^ 4) - 3;
  else
    error ("kurtosis: x has to be a matrix or a vector.");
  endif

endfunction
