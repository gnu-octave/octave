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
## @deftypefn {Function File} {@var{p} =} colperm (@var{s})
## Returns the column permutations such that the columns of
## @code{@var{s} (:, @var{p})} are ordered in terms of increase number
## of non-zero elements. If @var{s} is symmetric, then @var{p} is chosen
## such that @code{@var{s} (@var{p}, @var{p})} orders the rows and
## columns with increasing number of non zeros elements.
## @end deftypefn

function p = colperm (s)
  [i, j] = spfind (s);
  idx = find (diff ([j; Inf]) != 0);
  [dummy, p] = sort (idx - [0; idx(1:(end-1))]);
endfunction
