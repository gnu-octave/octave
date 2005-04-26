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
## @deftypefn {Function File} {} issymmetric (@var{x}, @var{tol})
## If @var{x} is symmetric within the tolerance specified by @var{tol},
## then return the dimension of @var{x}.  Otherwise, return 0.  If
## @var{tol} is omitted, use a tolerance equal to the machine precision.
## @end deftypefn
##
## @seealso{size, rows, columns, length, ismatrix, isscalar,
## issquare, and isvector}

## Author: A. S. Hodel <scotte@eng.auburn.edu>
## Created: August 1993
## Adapted-By: jwe

function retval = issymmetric (x,tol)

  if (nargin == 1 || nargin == 2)
    retval = issquare (x);
    if (retval != 0)
      if (nargin == 1)
        tol = eps;
      endif
      norm_x = norm (x);
      if (norm_x != 0 && norm (x - x') / norm_x > tol)
        retval = 0;
      endif
    endif
  else
    usage ("issymmetric (x {,tol})");
  endif

endfunction
