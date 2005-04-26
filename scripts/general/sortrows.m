## Copyright (C) 2000 Daniel Calvelo
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
## @deftypefn {Function File} {} sortrows (@var{a}, @var{c})
## Sort the rows of the matrix @var{a} according to the order of the
## columns specified in @var{c}.  If @var{c} is omitted, a
## lexicographical sort is used.
## @end deftypefn

## Author: Daniel Calvelo, Paul Kienzle
## Adapted-by: jwe

function [s, i] = sortrows (m, c)
  
  if (nargin < 2)
    indices = [1:size(m,2)]';
  else
    indices = c(:);
  endif

  if (isstr (m))
    s = toascii (m);
  else
    s = m;
  endif

  ## Since sort is 'stable' the order of identical elements will be
  ## preserved, so by traversing the sort indices in reverse order we
  ## will make sure that identical elements in index i are subsorted by
  ## index j.
  indices = flipud (indices);
  i = [1:size(m,1)]';
  for ii = 1:length (indices);
    [trash, idx] = sort (s(:,indices(ii)));
    s = s(idx,:);
    i = i(idx);
  endfor

  if (isstr (m))
    s = setstr (s);
  endif

endfunction
