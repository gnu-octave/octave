## Copyright (C) 1998, 1999, 2000, 2002, 2004, 2005, 2006, 2007, 2009
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
## @deftypefn {Function File} {} dot (@var{x}, @var{y}, @var{dim})
## Computes the dot product of two vectors.  If @var{x} and @var{y}
## are matrices, calculate the dot-product along the first 
## non-singleton dimension.  If the optional argument @var{dim} is
## given, calculate the dot-product along this dimension.
## @end deftypefn

## Author: jwe

function z = dot (x, y, dim)

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif

  if (nargin < 3)
    if isvector (x)
      x = x(:);
    endif
    if isvector (y)
      y = y(:);
    endif
    if (! size_equal (x, y))
      error ("dot: sizes of arguments must match");
    endif
    z = sum(x .* y);
  else
    if (! size_equal (x, y))
      error ("dot: sizes of arguments must match");
    endif
    z = sum(x .* y, dim);
  endif

endfunction
