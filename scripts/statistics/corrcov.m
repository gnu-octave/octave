########################################################################
##
## Copyright (C) 2025-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{r} =} corrcov (@var{c})
## @deftypefnx {} {[@var{r}, @var{s}] =} corrcov (@var{c})
## Convert matrix of covariance coefficients to a matrix of correlation
## coefficients.
##
## Given a numeric, square, symmetric, and positive semidefinite matrix of
## covariance coefficients, @var{c}, calculate the corresponding linear
## correlation coefficient matrix, @var{r}, that would be produced from the
## same variables that produced @var{c}.
##
## The correlation and covariance coefficients for two vectors, @var{A} and
## @var{B}, are related by:
##
## @tex
## $$
## {\rm corr}(A,B) = {{\rm cov}(A,B) \over \sqrt{{\rm cov}(A,A) \cdot  {\rm cov}(B,B)}}
## $$
## @end tex
## @ifnottex
##
## @example
## corr (@var{A},@var{B}) = cov (@var{A},@var{B}) / sqrt (cov (@var{A},@var{A}) * cov (@var{B},@var{B}))
## @end example
##
## @end ifnottex
##
## The output @var{r} will be the same shape and type as the input, @var{c},
## where each element @var{r}(@var{i},@var{j}) contains the correlation
## coefficient calculated using @var{c}(@var{i},@var{j}) as described above.
##
## If the optional output @var{s} is requested, it will contain a column vector
## of the standard deviations of the variables that produced the covariance
## matrix @var{c}.  Noting that the variances of the variables are contained in
## the main daigonal of @var{c}, this is equivalent output to
## @w{@code{sqrt (diag ( @var{c}))}}.
##
## Implementation Note: A proper covariance matrix as produced by @code{cov} is
## square, symmetric, and positive semi-definite.  @code{corrcov} only uses an
## approximate check for the latter that may pass improper inputs with small
## negative eigenvalues.
##
## @seealso{cov, corr, corrcoef}
## @end deftypefn

function [R, s] = corrcov (c)

  if (nargin != 1)
    print_usage;
  endif

  if (! isfloat (c))
    error ("corrcov: C must be a floating point numeric matrix");
  elseif (! (issquare (c) && issymmetric (c)))
    error ("corrcov: C must be square and symmetric");
  elseif (any (eig (c) + 10 * max (eps (c(:))) < 0))
    error("corrcov: C must be positive semi-definite");
  endif

  v = diag (c);
  R = c ./ sqrt (v .* v.');

  if (nargin > 1)
    s = sqrt (v);
  endif

endfunction

%!assert (corrcov (0), NaN)
%!assert (corrcov (1), 1)
%!assert (corrcov (99.999), 1, eps)

%!assert (corrcov (zeros (2)), NaN (2))
%!assert (corrcov (ones (2)), ones (2))
%!assert (corrcov ([1, 0; 0, 1]), [1, 0; 0, 1])
%!assert (corrcov ([1, -1; -1, 1]), [1, -1; -1, 1])

%!assert (corrcov (cov ([1:8]', [8:-1:1]')), corrcoef ([1:8]', [8:-1:1]'), eps)
%!assert (corrcov (cov ([1:8]', [1:5,3:-1:1]')), corrcoef ([1:8]', [1:5,3:-1:1]'), eps)

%!assert (isa (corrcov (single (ones(2))), "single"))

%!assert (corrcov ([]), [])
%!assert (corrcov (ones (0, 0)), [])

## Verify matrices numerically close to positive semidefinite will pass.
%!assert (corrcov (cov (magic (4))), corrcoef (magic (4)), eps)

%!error <Invalid call> corrcov ()
%!error <C must be a floating> corrcov (["a, b; b, a"])
%!error <C must be a floating> corrcov (int32([1, 0; 0, 1]))
%!error <C must be a floating> corrcov ({1, 0; 0, 1})
%!error <C must be square and symmetric> corrcov (ones (0, 1))
%!error <C must be square and symmetric> corrcov (NaN)
%!error <C must be square and symmetric> corrcov (NaN (2, 2))
%!error <C must be square and symmetric> corrcov (ones (2, 3))
%!error <C must be square and symmetric> corrcov (ones (2, 2, 2))
%!error <C must be positive semi-definite> corrcov (-ones (2))

