## Copyright (C) 2004, 2005, 2007 David Bateman & Andy Adler
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
## @deftypefn {Function File} {@var{y} =} spones (@var{x})
## Replace the non-zero entries of @var{x} with ones. This creates a
## sparse matrix with the same structure as @var{x}.
## @end deftypefn

function s = spones (s)

  if (nargin != 1)
    print_usage ();
  endif

  [i, j, v] = find (s);
  [m, n] = size (s);

  s = sparse (i, j, 1, m, n);

endfunction

%!assert(issparse(spones([1,2;3,0])))
%!assert(spones([1,2;3,0]),sparse([1,1;1,0]))
%!assert(spones(sparse([1,2;3,0])),sparse([1,1;1,0]))
