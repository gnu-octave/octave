## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} null (@var{a}, @var{tol})
## Return an orthonormal basis of the null space of @var{a}.
##
## The dimension of the null space is taken as the number of singular
## values of @var{a} not greater than @var{tol}.  If the argument @var{tol}
## is missing, it is computed as
##
## @example
## max (size (@var{a})) * max (svd (@var{a})) * eps
## @end example
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 24 December 1993.
## Adapted-By: jwe

function retval = null (A, tol)

  if (isempty (A))
    retval = [];
  else
    [U, S, V] = svd (A);

    [rows, cols] = size (A);

    [S_nr, S_nc] = size (S);

    if (S_nr == 1 || S_nc == 1)
      s = S(1);
    else
      s = diag (S);
    endif

    if (nargin == 1)
      tol = max (size (A)) * s (1) * eps;
    elseif (nargin != 2)
      usage ("null (A, tol)");
    endif

    rank = sum (s > tol);

    if (rank < cols)
      retval = V (:, rank+1:cols);
    else
      retval = zeros (cols, 0);
    endif
  endif

endfunction
