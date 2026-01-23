########################################################################
##
## Copyright (C) 1996-2026 The Octave Project Developers
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
    beta = x \ y;
  else
    beta = u \ (u' \ (x' * y));
  endif

  if (nargout > 1)
    r = y - x * beta;

    ## z is of full rank, avoid the SVD in rnk
    if (p == 0)
      rnk = columns (z);
    else
      rnk = rank (z);
    endif

    sigma = r' * r / (nr - rnk);
  endif

endfunction


%!demo
%! ## Filip data from NIST StRD - Polynomial Regression (degree 10)
%! ## https://www.itl.nist.gov/div898/strd/lls/data/Filip.shtml
%! xy = [0.8116   -6.860120914;  0.9072   -4.324130045
%!       0.9052   -4.358625055;  0.9039   -4.358426747
%!       0.8053   -6.955852379;  0.8377   -6.661145254
%!       0.8667   -6.355462942;  0.8809   -6.118102026
%!       0.7975   -7.115148017;  0.8162   -6.815308569
%!       0.8515   -6.519993057;  0.8766   -6.204119983
%!       0.8885   -5.853871964;  0.8859   -6.109523091
%!       0.8959   -5.79832982;   0.8913   -5.482672118
%!       0.8959   -5.171791386;  0.8971   -4.851705903
%!       0.9021   -4.517126416;  0.909    -4.143573228
%!       0.9139   -3.709075441;  0.9199   -3.499489089
%!       0.8692   -6.300769497;  0.8872   -5.953504836
%!       0.89     -5.642065153;  0.891    -5.031376979
%!       0.8977   -4.680685696;  0.9035   -4.329846955
%!       0.9078   -3.928486195;  0.7675   -8.56735134
%!       0.7705   -8.363211311;  0.7713   -8.107682739
%!       0.7736   -7.823908741;  0.7775   -7.522878745
%!       0.7841   -7.218819279;  0.7971   -6.920818754
%!       0.8329   -6.628932138;  0.8641   -6.323946875
%!       0.8804   -5.991399828;  0.7668   -8.781464495
%!       0.7633   -8.663140179;  0.7678   -8.473531488
%!       0.7697   -8.247337057;  0.77     -7.971428747
%!       0.7749   -7.676129393;  0.7796   -7.352812702
%!       0.7897   -7.072065318;  0.8131   -6.774174009
%!       0.8498   -6.478861916;  0.8741   -6.159517513
%!       0.8061   -6.835647144;  0.846    -6.53165267
%!       0.8751   -6.224098421;  0.8856   -5.910094889
%!       0.8919   -5.598599459;  0.8934   -5.290645224
%!       0.894    -4.974284616;  0.8957   -4.64454848
%!       0.9047   -4.290560426;  0.9129   -3.885055584
%!       0.9209   -3.408378962;  0.9219   -3.13200249
%!       0.7739   -8.726767166;  0.7681   -8.66695597
%!       0.7665   -8.511026475;  0.7703   -8.165388579
%!       0.7702   -7.886056648;  0.7761   -7.588043762
%!       0.7809   -7.283412422;  0.7961   -6.995678626
%!       0.8253   -6.691862621;  0.8602   -6.392544977
%!       0.8809   -6.067374056;  0.8301   -6.684029655
%!       0.8664   -6.378719832;  0.8834   -6.065855188
%!       0.8898   -5.752272167;  0.8964   -5.132414673
%!       0.8963   -4.811352704;  0.9074   -4.098269308
%!       0.9119   -3.66174277;   0.9228   -3.2644011];
%! ## Certified values (NIST)
%! b_cert = [-1467.48961422980; -2772.17959193342; -2316.37108160893;
%!           -1127.97394098372; -354.478233703349; -75.1242017393757;
%!           -10.8753180355343; -1.06221498588947; -0.670191154593408E-01;
%!           -0.246781078275479E-02; -0.402962525080404E-04];
%! y = xy(:,1);
%! x = xy(:,2);
%! ## Fit degree-10 polynomial using OLS
%! X = vander (x, 11);
%! [b, sigma, r] = ols (y, X);
%! clf;
%! subplot (3, 1, [1 2]);
%! plot (x, y, "bo", "markerfacecolor", "b", "markersize", 2);
%! hold on;
%! x_fit = linspace (min (x), max (x), 200);
%! plot (x_fit, polyval (b, x_fit), "r-", "linewidth", 1);
%! hold off;
%! grid on;
%! ylabel ("y");
%! title ("NIST Filip Dataset: Degree-10 Polynomial Fit");
%! legend ("Data", "OLS Fit", "location", "northwest");
%! subplot (3, 1, 3);
%! stem (x, r, "filled", "markersize", 2);
%! grid on;
%! xlabel ("x");
%! ylabel ("Residual");
%! title (sprintf ("Residuals (RMS = %.2e)", sqrt (mean (r.^2))));
%! assert (flip (b), b_cert, -2e-6);

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
