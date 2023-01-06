########################################################################
##
## Copyright (C) 1996-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{beta}, @var{sigma}, @var{r}] =} ols (@var{y}, @var{x})
## Ordinary least squares (OLS) estimation.
##
## OLS applies to the multivariate model
## @tex
## $@var{y} = @var{x}\,@var{b} + @var{e}$
## @end tex
## @ifnottex
## @w{@math{@var{y} = @var{x}*@var{b} + @var{e}}}
## @end ifnottex
## where
## @tex
## $@var{y}$ is a $t \times p$ matrix, $@var{x}$ is a $t \times k$ matrix,
## $@var{b}$ is a $k \times p$ matrix, and $@var{e}$ is a $t \times p$ matrix.
## @end tex
## @ifnottex
## @math{@var{y}} is a @math{t}-by-@math{p} matrix, @math{@var{x}} is a
## @math{t}-by-@math{k} matrix, @var{b} is a @math{k}-by-@math{p} matrix, and
## @var{e} is a @math{t}-by-@math{p} matrix.
## @end ifnottex
##
## Each row of @var{y} is a @math{p}-variate observation in which each column
## represents a variable.  Likewise, the rows of @var{x} represent
## @math{k}-variate observations or possibly designed values.  Furthermore,
## the collection of observations @var{x} must be of adequate rank, @math{k},
## otherwise @var{b} cannot be uniquely estimated.
##
## The observation errors, @var{e}, are assumed to originate from an
## underlying @math{p}-variate distribution with zero mean and
## @math{p}-by-@math{p} covariance matrix @var{S}, both constant conditioned
## on @var{x}.  Furthermore, the matrix @var{S} is constant with respect to
## each observation such that
## @tex
## $\bar{@var{e}} = 0$ and cov(vec(@var{e})) =  kron(@var{s},@var{I}).
## @end tex
## @ifnottex
## @code{mean (@var{e}) = 0} and
## @code{cov (vec (@var{e})) = kron (@var{s}, @var{I})}.
## @end ifnottex
## (For cases
## that don't meet this criteria, such as autocorrelated errors, see
## generalized least squares, gls, for more efficient estimations.)
##
## The return values @var{beta}, @var{sigma}, and @var{r} are defined as
## follows.
##
## @table @var
## @item beta
## The OLS estimator for matrix @var{b}.
## @tex
## @var{beta} is calculated directly via $(@var{x}^T@var{x})^{-1} @var{x}^T
## @var{y}$ if the matrix $@var{x}^T@var{x}$ is of full rank.
## @end tex
## @ifnottex
## @var{beta} is calculated directly via
## @code{inv (@var{x}'*@var{x}) * @var{x}' * @var{y}} if the matrix
## @code{@var{x}'*@var{x}} is of full rank.
## @end ifnottex
## Otherwise, @code{@var{beta} = pinv (@var{x}) * @var{y}} where
## @code{pinv (@var{x})} denotes the pseudoinverse of @var{x}.
##
## @item sigma
## The OLS estimator for the matrix @var{s},
##
## @example
## @group
## @var{sigma} = (@var{y}-@var{x}*@var{beta})' * (@var{y}-@var{x}*@var{beta}) / (@math{t}-rank(@var{x}))
## @end group
## @end example
##
## @item r
## The matrix of OLS residuals, @code{@var{r} = @var{y} - @var{x}*@var{beta}}.
## @end table
## @seealso{gls, pinv}
## @end deftypefn

function [beta, sigma, r] = ols (y, x)

  if (nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (x) && isnumeric (y)))
    error ("ols: X and Y must be numeric matrices or vectors");
  endif

  if (ndims (x) != 2 || ndims (y) != 2)
    error ("ols: X and Y must be 2-D matrices or vectors");
  endif

  [nr, nc] = size (x);
  [ry, cy] = size (y);
  if (nr != ry)
    error ("ols: number of rows of X and Y must be equal");
  endif

  if (isinteger (x))
    x = double (x);
  endif
  if (isinteger (y))
    y = double (y);
  endif

  ## Start of algorithm
  z = x' * x;
  [u, p] = chol (z);

  if (p)
    beta = pinv (x) * y;
  else
    beta = u \ (u' \ (x' * y));
  endif

  if (isargout (2) || isargout (3))
    r = y - x * beta;
  endif
  if (isargout (2))

    ## z is of full rank, avoid the SVD in rnk
    if (p == 0)
      rnk = columns (z);
    else
      rnk = rank (z);
    endif

    sigma = r' * r / (nr - rnk);
  endif

endfunction


%!test
%! x = [1:5]';
%! y = 3*x + 2;
%! x = [x, ones(5,1)];
%! assert (ols (y,x), [3; 2], 50*eps);

%!test
%! x = [1, 2; 3, 4];
%! y = [1; 2];
%! [b, s, r] = ols (x, y);
%! assert (b, [1.4, 2], 2*eps);
%! assert (s, [0.2, 0; 0, 0], 2*eps);
%! assert (r, [-0.4, 0; 0.2, 0], 2*eps);

%!test
%! x = [1, 2; 3, 4];
%! y = [1; 2];
%! [b, s] = ols (x, y);
%! assert (b, [1.4, 2], 2*eps);
%! assert (s, [0.2, 0; 0, 0], 2*eps);

%!test
%! x = [1, 2; 3, 4];
%! y = [1; 2];
%! b = ols (x, y);
%! assert (b, [1.4, 2], 2*eps);

## Test input validation
%!error <Invalid call> ols ()
%!error <Invalid call> ols (1)
%!error ols ([true, true], [1, 2])
%!error ols ([1, 2], [true, true])
%!error ols (ones (2,2,2), ones (2,2))
%!error ols (ones (2,2), ones (2,2,2))
%!error ols (ones (1,2), ones (2,2))
