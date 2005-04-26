## Copyright (C) 2004 David Bateman & Andy Adler
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {@var{s} =} spalloc (@var{r}, @var{c}, @var{nz})
## Returns an empty sparse matrix of size @var{r}-by-@var{c}. As Octave
## resizes sparse matrices at the first opportunity, so that no additional 
## space is needed, the argument @var{nz} is ignored. This function is 
## provided only for compatiability reasons.
##
## It should be noted that this means that code like
##
## @example
## k = 5;
## nz = r * k;
## s = spalloc (r, c, nz)
## for j = 1:c
##   idx = randperm (r);
##   s (:, j) = [zeros(r - k, 1); rand(k, 1)] (idx);
## endfor
## @end example
##
## will reallocate memory at each step. It is therefore vitally important
## that code like this is vectorized as much as possible.
## @end deftypefn
## @seealso{sparse, nzmax}

function s = spalloc (r, c, nz)
  s = sparse (r, c);
endfunction
