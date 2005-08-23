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
## @deftypefn {Function File} {} ranks (@var{x}, @var{dim})
## If @var{x} is a vector, return the (column) vector of ranks of
## @var{x} adjusted for ties.
##
## If @var{x} is a matrix, do the above for along the first 
## non-singleton dimension. If the optional argument @var{dim} is
## given, operate along this dimension.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute ranks

## This code was rather ugly, since it didn't use sort due to the
## fact of how to deal with ties. Now it does use sort and its
## even uglier!!! At least it handles NDArrays..

function y = ranks (x, dim)

  if (nargin != 1 && nargin != 2)
    usage ("ranks (x, dim)");
  endif

  nd = ndims (x);
  sz = size (x);
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
      error ("ranks: dim must be an integer and valid dimension");
    endif
  endif

  if (sz(dim) == 1)
    y = ones(sz);
  else
    ## The algorithm works only on dim=1, so permute if necesary
    if (dim != 1)
      perm = [1 : nd];
      perm(1) = dim;
      perm(dim) = 1;
      x = permute (x, perm);
    endif
    sz = size (x);
    infvec = -Inf * ones ([1, sz(2 : end)]);
    [xs, xi] = sort (x);
    eq_el = find (diff ([xs; infvec]) == 0);
    if (isempty (eq_el))
      [eq_el, y] = sort (xi);
    else
      runs = complement (eq_el+1, eq_el);
      len = diff (find (diff ([Inf; eq_el; -Inf]) != 1)) + 1;
      [eq_el, y] = sort (xi);
      for i = 1 : length(runs)
	y (xi (runs (i) + [0:(len(i)-1)]) + floor (runs (i) ./ sz(1)) 
	   * sz(1)) = eq_el(runs(i)) + (len(i) - 1) / 2;
      endfor
    endif  
    if (dim != 1)
      y = permute (y, perm);
    endif
  endif

endfunction
