## Copyright (C) 2003 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
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
## @deftypefn {Function File} {} logm (@var{a})
## Compute the matrix logarithm of the square matrix @var{a}.  Note that
## this is currently implemented in terms of an eigenvalue expansion and
## needs to be improved to be more robust.
## @end deftypefn

function B = logm (A)

  if (nargin != 1)
    usage ("B = logm (A)");
  endif

  [V, D] = eig (A);
  B = V * diag (log (diag (D))) * inv (V);

endfunction
