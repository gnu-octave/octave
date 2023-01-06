########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn {} {@var{r} =} expm (@var{A})
## Return the exponential of a matrix.
##
## The matrix exponential is defined as the infinite Taylor series
## @tex
## $$
##  \exp (A) = I + A + {A^2 \over 2!} + {A^3 \over 3!} + \cdots
## $$
## @end tex
## @ifnottex
##
## @example
## expm (A) = I + A + A^2/2! + A^3/3! + @dots{}
## @end example
##
## @end ifnottex
## However, the Taylor series is @emph{not} the way to compute the matrix
## exponential; see @nospell{Moler and Van Loan}, @cite{Nineteen Dubious Ways
## to Compute the Exponential of a Matrix}, SIAM Review, 1978.  This routine
## uses Ward's diagonal Pad@'e approximation method with three step
## preconditioning (SIAM Journal on Numerical Analysis, 1977).  Diagonal
## Pad@'e approximations are rational polynomials of matrices
## @tex
## $D_q(A)^{-1}N_q(A)$
## @end tex
## @ifnottex
##
## @example
## @group
##      -1
## D (A)   N (A)
## @end group
## @end example
##
## @end ifnottex
## whose Taylor series matches the first
## @tex
## $2 q + 1 $
## @end tex
## @ifnottex
## @code{2q+1}
## @end ifnottex
## terms of the Taylor series above; direct evaluation of the Taylor series
## (with the same preconditioning steps) may be desirable in lieu of the
## Pad@'e approximation when
## @tex
## $D_q(A)$
## @end tex
## @ifnottex
## @code{Dq(A)}
## @end ifnottex
## is ill-conditioned.
## @seealso{logm, sqrtm}
## @end deftypefn

function r = expm (A)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (A) || ! issquare (A))
    error ("expm: A must be a square matrix");
  endif

  if (isempty (A))
    r = A;
    return;
  elseif (isscalar (A))
    r = exp (A);
    return;
  elseif (isdiag (A))
    r = diag (exp (diag (A)));
    return;
  endif

  n = rows (A);
  id = eye (n);
  ## Trace reduction.
  A(A == -Inf) = -realmax ();
  trshift = trace (A) / n;
  if (trshift > 0)
    A -= trshift * id;
  endif
  ## Balancing.
  [d, p, aa] = balance (A);
  [~, e] = log2 (norm (aa, "inf"));
  s = max (0, e);
  s = min (s, 1023);
  aa *= 2^(-s);

  ## Pade approximation for exp(A).
  c = [5.0000000000000000e-1, ...
       1.1666666666666667e-1, ...
       1.6666666666666667e-2, ...
       1.6025641025641026e-3, ...
       1.0683760683760684e-4, ...
       4.8562548562548563e-6, ...
       1.3875013875013875e-7, ...
       1.9270852604185938e-9];

  a2 = aa^2;
  x = (((c(8) * a2 + c(6) * id) * a2 + c(4) * id) * a2 + c(2) * id) * a2 + id;
  y = (((c(7) * a2 + c(5) * id) * a2 + c(3) * id) * a2 + c(1) * id) * aa;

  r = (x - y) \ (x + y);

  ## Undo scaling by repeated squaring.
  for k = 1:s
    r ^= 2;
  endfor

  ## inverse balancing.
  d = diag (d);
  r = d * r / d;
  r(p, p) = r;
  ## Inverse trace reduction.
  if (trshift > 0)
    r *= exp (trshift);
  endif

endfunction


%!assert (norm (expm ([1 -1;0 1]) - [e -e; 0 e]) < 1e-5)
%!assert (expm ([1 -1 -1;0 1 -1; 0 0 1]), [e -e -e/2; 0 e -e; 0 0 e], 1e-5)

%!assert (expm ([]), [])
%!assert (expm (10), exp (10))
%!assert (full (expm (eye (3))), expm (full (eye (3))))
%!assert (full (expm (10*eye (3))), expm (full (10*eye (3))), 8*eps)
%!assert (expm (zeros (3)), eye (3))

## Test input validation
%!error <Invalid call> expm ()
%!error <expm: A must be a square matrix> expm ({1})
%!error <expm: A must be a square matrix> expm ([1 0;0 1; 2 2])
