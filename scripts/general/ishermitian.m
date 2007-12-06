## Copyright (C) 1996, 1997, 2002, 2003, 2004, 2005, 2006, 2007
##               John W. Eaton
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
## @deftypefn {Function File} {} ishermitian (@var{x}, @var{tol})
## If @var{x} is Hermitian within the tolerance specified by @var{tol},
## then return the dimension of @var{x}.  Otherwise, return 0.  If
## @var{tol} is omitted, use a tolerance equal to the machine precision.
## Matrix @var{x} is considered symmetric if
## @code{norm (@var{x} - @var{x}', inf) / norm (@var{x}, inf) < @var{tol}}.
## @seealso{size, rows, columns, length, ismatrix, isscalar,
## issquare, issymmetric, isvector}
## @end deftypefn

## Author: A. S. Hodel <scotte@eng.auburn.edu>
## Created: August 1993
## Adapted-By: jwe

function retval = ishermitian (x, tol)

  if (nargin == 1 || nargin == 2)
    retval = issquare (x);
    if (retval != 0)
      if (nargin == 1)
        tol = eps;
      endif
      norm_x = norm (x, inf);
      if (norm_x != 0 && norm (x - x', inf) / norm_x > tol)
        retval = 0;
      endif
    endif
  else
    print_usage ();
  endif

endfunction

%!assert(ishermitian ([1, 2i; -2i, 1]) == 2);
%!assert(!ishermitian ([1, 2i; 2i, 1]));
%!assert(ishermitian ([1, 2.1i; -2i, 1.1], 0.2) == 2);
