########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{n} =} vecnorm (@var{A})
## @deftypefnx {} {@var{n} =} vecnorm (@var{A}, @var{p})
## @deftypefnx {} {@var{n} =} vecnorm (@var{A}, @var{p}, @var{dim})
## Return the vector p-norm of the elements of array @var{A} along dimension
## @var{dim}.
##
## The p-norm of a vector is defined as
##
## @tex
## $$ {\Vert A \Vert}_p  = \left[ \sum_{i=1}^N {| A_i |}^p \right] ^ {1/p} $$
## @end tex
## @ifnottex
##
## @example
## @var{p-norm} (@var{A}, @var{p}) = sum (abs (@var{A}) .^ @var{p})) ^ (1/@var{p})
## @end example
##
## @end ifnottex
## The input @var{p} must be a positive scalar.  If omitted it defaults to 2
## (Euclidean norm or distance).  Other special values of @var{p} are 1
## (Manhattan norm, sum of absolute values) and @code{Inf} (absolute value of
## largest element).
##
## The input @var{dim} specifies the dimension of the array on which the
## function operates and must be a positive integer.  If omitted the first
## non-singleton dimension is used.
##
## @seealso{norm}
## @end deftypefn

function n = vecnorm (A, p = 2, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (A))
    error ("vecnorm: A must be numeric");
  endif

  if (! (isscalar (p) && isreal (p) && p > 0))
    error ("vecnorm: P must be positive real scalar or Inf");
  endif

  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (size (A) > 1, 1)) || (dim = 1);
  elseif (! (isscalar (dim) && isindex (dim)))
    error ("vecnorm: DIM must be a positive integer");
  endif

  ## Calculate norm using the value of p to accelerate special cases
  switch (p)
    case {1}
      n = sum (abs (A), dim);

    case {2}
      n = sqrt (sumsq (A, dim));

    case {Inf}
      n = max (abs (A), [], dim);

    otherwise
      if (rem (p,2) == 0)
        ## Even index such as 2,4,6 are specifically accelerated in math
        ## libraries.  Beyond 6, it doesn't matter which method is used.
        if (iscomplex (A))
          n = (sum ((real (A).^2 + imag (A).^2) .^ (p/2), dim)) .^ (1 / p);
        else
          n = (sum (A.^2 .^ (p/2), dim)) .^ (1 / p);
        endif
      else
        n = (sum (abs (A) .^ p, dim)) .^ (1 / p);
      endif

  endswitch

endfunction


%!test
%! A = [0 1 2; 3 4 5];
%! c = vecnorm (A);
%! r = vecnorm (A, 2, 2);
%! i = vecnorm (A, Inf);
%! assert (c, [3.0000, 4.1231, 5.3852], 1e-4);
%! assert (r, [2.2361; 7.0711], 1e-4);
%! assert (i, [3, 4, 5]);
%!test
%! A = [1, 2];
%! assert (vecnorm (A), 2.2361, 1e-4);
%!test
%! A(:, :, 1) = [1, 2];
%! A(:, :, 2) = [3, 4];
%! A(:, :, 3) = [5, 6];
%! ret(:, :, 1) = 2.2361;
%! ret(:, :, 2) = 5;
%! ret(:, :, 3) = 7.8102;
%! assert (vecnorm (A), ret, 1e-4);

## Test input validation
%!error <Invalid call> vecnorm ()
%!error <A must be numeric> vecnorm ({1})
%!error <P must be positive real scalar> vecnorm (1, [1 2])
%!error <P must be positive real scalar> vecnorm (1, i)
%!error <P must be positive real scalar> vecnorm (1, -1)
%!error <P must be positive real scalar> vecnorm (1, 0)
%!error <DIM must be a positive integer> vecnorm (1, 2, [1 2])
%!error <DIM must be a positive integer> vecnorm (1, 2, -1)
%!error <DIM must be a positive integer> vecnorm (1, 2, 0)
%!error <DIM must be a positive integer> vecnorm (1, 2, 1.5)
