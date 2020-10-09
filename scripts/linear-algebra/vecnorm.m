########################################################################
##
## Copyright (C) 2017-2020 The Octave Project Developers
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
## Return the p-norm of the elements of @var{A} along dimension @var{dim}.
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
## If @var{p} is omitted it defaults to 2 (Euclidean norm).  @var{p} can be
## @code{Inf} (absolute value of largest element).
##
## If @var{dim} is omitted the first non-singleton dimension is used.
##
## @seealso{norm}
## @end deftypefn

function n = vecnorm (A, p = 2, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isscalar (p) || ! isreal (p) || (p <= 0))
    error ("vecnorm: P must be positive real scalar or Inf");
  endif

  sz = size (A);
  if (nargin <= 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  elseif (! (isscalar (dim) && dim == fix (dim) && dim > 0))
    error ("vecnorm: DIM must be an integer and a valid dimension");
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
        n = (sum ((real (A).^2 + imag (A).^2) .^ (p/2), dim)) .^ (1 / p);
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
%!error <P must be positive real scalar> vecnorm (1, [1 2])
%!error <P must be positive real scalar> vecnorm (1, i)
%!error <P must be positive real scalar> vecnorm (1, -1)
%!error <P must be positive real scalar> vecnorm (1, 0)
%!error <DIM must be an integer and a valid dimension> vecnorm (1, 2, [1 2])
%!error <DIM must be an integer and a valid dimension> vecnorm (1, 2, [1 2])
%!error <DIM must be an integer and a valid dimension> vecnorm (1, 2, 0)
%!error <DIM must be an integer and a valid dimension> vecnorm (1, 2, -1)
