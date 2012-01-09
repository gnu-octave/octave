## Copyright (C) 1995-2012 Kurt Hornik
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
## @deftypefn  {Function File} {} zscore (@var{x})
## @deftypefnx {Function File} {} zscore (@var{x}, @var{dim})
## If @var{x} is a vector, subtract its mean and divide by its standard
## deviation.
##
## If @var{x} is a matrix, do the above along the first non-singleton
## dimension.
## If the optional argument @var{dim} is given, operate along this dimension.
## @seealso{center}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Subtract mean and divide by standard deviation

function z = zscore (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("zscore: X must be a numeric vector or matrix");
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("zscore: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);
  if (n == 0)
    z = x;
  else
    x = center (x, dim); # center also promotes integer to double for next line
    z = zeros (sz, class (x));
    s = std (x, [], dim);
    s(s==0) = 1;
    z = bsxfun (@rdivide, x, s);
  endif

endfunction


%!assert(zscore ([1,2,3]), [-1,0,1])
%!assert(zscore (single([1,2,3])), single([-1,0,1]))
%!assert(zscore (int8([1,2,3])), [-1,0,1])
%!assert(zscore (ones (3,2,2,2)), zeros (3,2,2,2))
%!assert(zscore ([2,0,-2;0,2,0;-2,-2,2]), [1,0,-1;0,1,0;-1,-1,1])

%% Test input validation
%!error zscore ()
%!error zscore (1, 2, 3)
%!error zscore (['A'; 'B'])
%!error zscore (1, ones(2,2))
%!error zscore (1, 1.5)
%!error zscore (1, 0)
%!error zscore (1, 3)

