## Copyright (C) 1995, 1996  Kurt Hornik
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
## @deftypefn {Function File} {} diff (@var{x}, @var{k})
## If @var{x} is a vector of length @var{n}, @code{diff (@var{x})} is the
## vector of first differences
## @iftex
## @tex
##  $x_2 - x_1, \ldots{}, x_n - x_{n-1}$.
## @end tex
## @end iftex
## @ifinfo
##  @var{x}(2) - @var{x}(1), @dots{}, @var{x}(n) - @var{x}(n-1).
## @end ifinfo
## 
## If @var{x} is a matrix, @code{diff (@var{x})} is the matrix of column
## differences.
## 
## The second argument is optional.  If supplied, @code{diff (@var{x},
## @var{k})}, where @var{k} is a nonnegative integer, returns the
## @var{k}-th differences.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 2 February 1995
## Adapted-By: jwe

function x = diff (x, k)
  
  if (nargin == 1)
    k = 1;
  elseif (nargin == 2)
    if (! (is_scalar (k) && k == round (k) && k >= 0))
      error ("diff: k must be a nonnegative integer");
    elseif (k == 0)
      return;
    endif
  else
    usage ("diff (x [, k]");
  endif
  
  if (isstr (x))
    error ("diff: symbolic differentiation not (yet) supported");
  elseif (is_vector (x))
    n = length (x);
    if (n <= k)
      x = [];
    else
      for i = 1 : k
	x = x (2 : (n - i + 1)) - x (1 : (n - i));
      endfor
    endif
  elseif (is_matrix (x))
    n = rows (x);
    if (n <= k)
      x = [];
    else
      for i = 1 : k
	x = x (2 : (n - i + 1), :) - x (1: (n - i), :);
      endfor
    endif
  else
    x = [];
  endif

endfunction
