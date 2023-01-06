########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{nest} =} normest (@var{A})
## @deftypefnx {} {@var{nest} =} normest (@var{A}, @var{tol})
## @deftypefnx {} {[@var{nest}, @var{iter}] =} normest (@dots{})
## Estimate the 2-norm of the matrix @var{A} using a power series analysis.
##
## This is typically used for large matrices, where the cost of calculating
## @code{norm (@var{A})} is prohibitive and an approximation to the 2-norm is
## acceptable.
##
## @var{tol} is the tolerance to which the 2-norm is calculated.  By default
## @var{tol} is 1e-6.
##
## The optional output @var{iter} returns the number of iterations that were
## required for @code{normest} to converge.
## @seealso{normest1, norm, cond, condest}
## @end deftypefn

function [nest, iter] = normest (A, tol = 1e-6)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (A) || ndims (A) != 2)
    error ("normest: A must be a numeric 2-D matrix");
  endif

  if (! (isscalar (tol) && isreal (tol)))
    error ("normest: TOL must be a real scalar");
  endif

  if (! isfloat (A))
    A = double (A);
  endif

  tol = max (tol, eps (class (A)));
  ## Set random number generator to depend on target matrix
  v = rand ("state");
  rand ("state", full (trace (A)));
  ncols = columns (A);
  ## Randomize y to avoid bad guesses for important matrices.
  y = rand (ncols, 1);
  iter = 0;
  nest = 0;
  do
    n0 = nest;
    x = A * y;
    normx = norm (x);
    if (normx == 0)
      x = rand (ncols, 1);
    else
      x /= normx;
    endif
    y = A' * x;
    nest = norm (y);
    iter += 1;
  until (abs (nest - n0) <= tol * nest)

  rand ("state", v);    # restore state of random number generator

endfunction


%!test
%! A = toeplitz ([-2,1,0,0]);
%! assert (normest (A), norm (A), 1e-6);

%!test
%! A = rand (10);
%! assert (normest (A), norm (A), 1e-6);

## Test input validation
%!error <Invalid call> normest ()
%!error <A must be a numeric .* matrix> normest ([true true])
%!error <A must be .* 2-D matrix> normest (ones (3,3,3))
%!error <TOL must be a real scalar> normest (1, [1, 2])
%!error <TOL must be a real scalar> normest (1, 1+1i)
