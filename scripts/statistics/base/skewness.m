## Copyright (C) 1996-2012 John W. Eaton
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
## @deftypefn  {Function File} {} skewness (@var{x})
## @deftypefnx {Function File} {} skewness (@var{x}, @var{dim})
## Compute the skewness of the elements of the vector @var{x}.
## @tex
## $$
## {\rm skewness} (x) = {1\over N \sigma^3} \sum_{i=1}^N (x_i-\bar{x})^3
## $$
## where $\bar{x}$ is the mean value of $x$.
## @end tex
## @ifnottex
##
## @example
## skewness (x) = 1/N std(x)^(-3) sum ((x - mean(x)).^3)
## @end example
##
## @end ifnottex
##
## @noindent
## If @var{x} is a matrix, return the skewness along the
## first non-singleton dimension of the matrix.  If the optional
## @var{dim} argument is given, operate along this dimension.
## @seealso{var, kurtosis, moment}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 29 July 1994
## Adapted-By: jwe

function retval = skewness (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("skewness: X must be a numeric vector or matrix");
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("skewness: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);
  sz(dim) = 1;
  x = center (x, dim);  # center also promotes integer to double for next line
  retval = zeros (sz, class (x));
  s = std (x, [], dim);
  idx = find (s > 0);
  x = sum (x .^ 3, dim);
  retval(idx) = x(idx) ./ (n * s(idx) .^ 3);

endfunction


%!assert(skewness ([-1,0,1]), 0);
%!assert(skewness ([-2,0,1]) < 0);
%!assert(skewness ([-1,0,2]) > 0);
%!assert(skewness ([-3,0,1]) == -1*skewness([-1,0,3]));
%!test
%! x = [0; 0; 0; 1];
%! y = [x, 2*x];
%! assert(all (abs (skewness (y) - [0.75, 0.75]) < sqrt (eps)));

%!assert (skewness (single(1)), single(0));

%% Test input validation
%!error skewness ()
%!error skewness (1, 2, 3)
%!error skewness (['A'; 'B'])
%!error skewness (1, ones(2,2))
%!error skewness (1, 1.5)
%!error skewness (1, 0)
%!error skewness (1, 3)
