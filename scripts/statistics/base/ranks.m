## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2004, 2005, 2006,
##               2007, 2008, 2009 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} ranks (@var{x}, @var{dim})
## Return the ranks of @var{x} along the first non-singleton dimension
## adjust for ties.  If the optional argument @var{dim} is
## given, operate along this dimension.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute ranks

## This code was rather ugly, since it didn't use sort due to the
## fact of how to deal with ties. Now it does use sort and its
## even uglier!!! At least it handles NDArrays..

function y = ranks (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
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
    ## The algorithm works only on dim = 1, so permute if necesary.
    if (dim != 1)
      perm = [1 : nd];
      perm(1) = dim;
      perm(dim) = 1;
      x = permute (x, perm);
    endif
    sz = size (x);
    infvec = -Inf ([1, sz(2 : end)]);
    [xs, xi] = sort (x);
    eq_el = find (diff ([xs; infvec]) == 0);
    if (isempty (eq_el))
      [eq_el, y] = sort (xi);
    else
      runs = setdiff (eq_el, eq_el+1);
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
