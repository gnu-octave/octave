## Copyright (C) 2000 Daniel Calvelo
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
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## B = sortrows (A)
##     returns matrix with rows sorted "lexicographically" 
##
## B = sortrows(A, c)
##     returns matrix with rows sorted according to the order of the
##     columns specified in c.
##
## Set implicit_str_to_num_ok and implicit_num_to_str_ok to 1 if you 
## use this for string sorting and octave 2.1.50 or earlier.

## Author: Daniel Calvelo
## 2001-02-24 Paul Kienzle
##    * cleanup according to Octave conventions
##    * return reverse index
##    * handle string arguments

function [s, i] = sortrows (m, c)
  
  if nargin < 2
    indices = [ 1 : size(m,2) ]';
  else
    indices = c (:);
  endif

  if (isstr (m)) 
    s = toascii (m);
  else
    s = m;
  endif

  ## since sort is 'stable' the order of identical elements will be
  ## preserved, so by traversing the sort indices in reverse order
  ## we will make sure that identical elements in index i are subsorted
  ## by index j.
  indices = flipud (indices);
  i = [1 : size(m,1)]';
  for ii = 1 : length (indices);
    [ trash, idx ] = sort ( s ( : , indices (ii) ) ); 
    s = s ( idx, : );
    i = i (idx );
  endfor
  if (isstr (m))
    s = setstr(s);
  endif
endfunction
