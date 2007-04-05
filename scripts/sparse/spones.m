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
## @deftypefn {Function File} {@var{y} =} spones (@var{x})
## Replace the non-zero entries of @var{x} with ones. This creates a
## sparse matrix with the same structure as @var{x}.
## @end deftypefn

function s = spones (s)
  if (issparse (s))
    [i, j, v, m, n] = spfind (s);
  else
    [i, j, v] = find (s);
    [m, n] = size (s);
  end
  s = sparse (i, j, 1, m, n);
endfunction

%!assert(issparse(spones([1,2;3,0])))
%!assert(spones([1,2;3,0]),sparse([1,1;1,0]))
%!assert(spones(sparse([1,2;3,0])),sparse([1,1;1,0]))
