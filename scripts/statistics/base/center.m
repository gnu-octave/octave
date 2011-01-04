## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2004, 2005, 2006,
##               2007, 2009 Kurt Hornik
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {} center (@var{x})
## @deftypefnx {Function File} {} center (@var{x}, @var{dim})
## If @var{x} is a vector, subtract its mean.
## If @var{x} is a matrix, do the above for each column.
## If the optional argument @var{dim} is given, operate along this dimension.
## @seealso{studentize}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Center by subtracting means

function retval = center (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (!isnumeric (x))
    error ("center: X must be a numeric vector or matrix");
  endif

  if (isinteger (x))
    x = double (x);
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    dim = find (sz > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("center: DIM must be an integer and a valid dimension");
    endif
  endif

  n = size (x, dim);

  if (n == 0)
    retval = x;
  else
    retval = bsxfun (@minus, x, sum (x, dim) / n);
  endif

endfunction

%!assert(center ([1,2,3]), [-1,0,1])
%!assert(center (int8 ([1,2,3])), [-1,0,1])
%!assert(center (ones (3,2,0,2)), zeros (3,2,0,2))
%!assert(center (magic (3)), [3,-4,1;-2,0,2;-1,4,-3])

%% Test input validation
%!error center ()
%!error center (1, 2, 3)
%!error center ([true true])
%!error center (1, ones(2,2))
%!error center (1, 1.5)
%!error center (1, 0)
%!error center (1, 3)
