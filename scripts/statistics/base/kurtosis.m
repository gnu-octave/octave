## Copyright (C) 1996-2011 John W. Eaton
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
## @deftypefn  {Function File} {} kurtosis (@var{x})
## @deftypefnx {Function File} {} kurtosis (@var{x}, @var{dim})
## Compute the kurtosis of the elements of the vector @var{x}.
## @tex
## $$
##  {\rm kurtosis} (x) = {1\over N \sigma^4} \sum_{i=1}^N (x_i-\bar{x})^4 - 3
## $$
## where $\bar{x}$ is the mean value of $x$.
## @end tex
## @ifnottex
##
## @example
## kurtosis (x) = N^(-1) std(x)^(-4) sum ((x - mean(x)).^4) - 3
## @end example
##
## @end ifnottex
## If @var{x} is a matrix, return the kurtosis over the
## first non-singleton dimension of the matrix.  If the optional
## @var{dim} argument is given, operate along this dimension.
## 
## Note: The definition of kurtosis above yields a kurtosis of zero for the
## stdnormal distribution and is sometimes referred to as "excess kurtosis".  
## To calculate kurtosis without the normalization factor of @math{-3} use
## @code{moment (@var{x}, 4, 'c') / std (@var{x})^4}.
## @seealso{var,skewness,moment}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 29 July 1994
## Adapted-By: jwe

function retval = kurtosis (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (!isnumeric (x))
    error ("kurtosis: X must be a numeric vector or matrix");
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
      error ("kurtosis: DIM must be an integer and a valid dimension");
    endif
  endif
  
  c = sz(dim);
  sz(dim) = 1;
  idx = ones (1, nd);
  idx(dim) = c;
  x = x - repmat (mean (x, dim), idx);
  retval = zeros (sz, class (x));
  s = std (x, [], dim);
  x = sum (x.^4, dim);
  ind = find (s > 0);
  retval(ind) = x(ind) ./ (c * s(ind) .^ 4) - 3;

endfunction


%!test
%! x = [-1; 0; 0; 0; 1];
%! y = [x, 2*x];
%! assert(all (abs (kurtosis (y) - [-1.4, -1.4]) < sqrt (eps)));

%% Test input validation
%!error kurtosis ()
%!error kurtosis (1, 2, 3)
%!error kurtosis (true(1,2))
%!error kurtosis (1, ones(2,2))
%!error kurtosis (1, 1.5)
%!error kurtosis (1, 0)
%!error kurtosis (1, 3)

