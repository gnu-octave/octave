## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## Performs ordinal logistic regression.
##
## Suppose Y takes values in k ordered categories, and let gamma_i (x)
## be the cumulative probability that Y falls in one of the first i
## categories given the covariate x.  Then
##   [theta, beta] =
##     logistic_regression (y, x)
## fits the model
##   logit (gamma_i (x)) = theta_i - beta' * x,   i = 1, ..., k-1.
## The number of ordinal categories, k, is taken to be the number of
## distinct values of round (y) .  If k equals 2, y is binary and the
## model is ordinary logistic regression. X is assumed to have full
## column rank.
##
##   theta = logistic_regression (y)
## fits the model with baseline logit odds only.
##
## The full form is
##   [theta, beta, dev, dl, d2l, gamma] =
##     logistic_regression (y, x, print, theta, beta)
## in which all output arguments and all input arguments except y are
## optional.
##
## print = 1 requests summary information about the fitted model to be
## displayed; print = 2 requests information about convergence at each
## iteration. Other values request no information to be displayed. The
## input arguments `theta' and `beta' give initial estimates for theta
## and beta.
##
## `dev' holds minus twice the log-likelihood.
##
## `dl' and `d2l' are the vector of first and the matrix of second
## derivatives of the log-likelihood with respect to theta and beta.
##
## `p' holds estimates for the conditional distribution of Y given x.

## Original for MATLAB written by Gordon K Smyth <gks@maths.uq.oz.au>,
## U of Queensland, Australia, on Nov 19, 1990.  Last revision Aug 3,
## 1992.

## Author:  Gordon K Smyth <gks@maths.uq.oz.au>,
## Adapted-By:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Ordinal logistic regression

## Uses the auxiliary functions logistic_regression_derivatives and
## logistic_regression_likelihood.

function [theta, beta, dev, dl, d2l, p] ...
      = logistic_regression (y, x, print, theta, beta)

  ## check input
  y = round (vec (y));
  [my, ny] = size (y);
  if (nargin < 2)
    x = zeros (my, 0);
  endif;
  [mx, nx] = size (x);
  if (mx != my)
    error ("x and y must have the same number of observations");
  endif

  ## initial calculations
  x = -x;
  tol = 1e-6; incr = 10; decr = 2;
  ymin = min (y); ymax = max (y); yrange = ymax - ymin;
  z  = (y * ones (1, yrange)) == ((y * 0 + 1) * (ymin : (ymax - 1)));
  z1 = (y * ones (1, yrange)) == ((y * 0 + 1) * ((ymin + 1) : ymax));
  z  = z(:, any (z));
  z1 = z1 (:, any(z1));
  [mz, nz] = size (z);

  ## starting values
  if (nargin < 3)
    print = 0;
  endif;
  if (nargin < 4)
    beta = zeros (nx, 1);
  endif;
  if (nargin < 5)
    g = cumsum (sum (z))' ./ my;
    theta = log (g ./ (1 - g));
  endif;
  tb = [theta; beta];

  ## likelihood and derivatives at starting values
  [g, g1, p, dev] = logistic_regression_likelihood (y, x, tb, z, z1);
  [dl, d2l] = logistic_regression_derivatives (x, z, z1, g, g1, p);
  epsilon = std (vec (d2l)) / 1000;

  ## maximize likelihood using Levenberg modified Newton's method
  iter = 0;
  while (abs (dl' * (d2l \ dl) / length (dl)) > tol)
    iter = iter + 1;
    tbold = tb;
    devold = dev;
    tb = tbold - d2l \ dl;
    [g, g1, p, dev] = logistic_regression_likelihood (y, x, tb, z, z1);
    if ((dev - devold) / (dl' * (tb - tbold)) < 0)
      epsilon = epsilon / decr;
    else
      while ((dev - devold) / (dl' * (tb - tbold)) > 0)
        epsilon = epsilon * incr;
         if (epsilon > 1e+15)
           error ("epsilon too large");
         endif
         tb = tbold - (d2l - epsilon * eye (size (d2l))) \ dl;
         [g, g1, p, dev] = logistic_regression_likelihood (y, x, tb, z, z1);
         disp ("epsilon"); disp (epsilon);
      endwhile
    endif
    [dl, d2l] = logistic_regression_derivatives (x, z, z1, g, g1, p);
    if (print == 2)
      disp ("Iteration"); disp (iter);
      disp ("Deviance"); disp (dev);
      disp ("First derivative"); disp (dl');
      disp ("Eigenvalues of second derivative"); disp (eig (d2l)');
    endif
  endwhile

  ## tidy up output

  theta = tb (1 : nz, 1);
  beta  = tb ((nz + 1) : (nz + nx), 1);

  if (print >= 1)
    printf ("\n");
    printf ("Logistic Regression Results:\n");
    printf ("\n");
    printf ("Number of Iterations:  %d\n", iter);
    printf ("Deviance:              %f\n", dev);
    printf ("Parameter Estimates:\n");
    printf ("     Theta         S.E.\n");
    se = sqrt (diag (inv (-d2l)));
    for i = 1 : nz
      printf ("   %8.4f     %8.4f\n", tb (i), se (i));
    endfor
    if (nx > 0)
      printf ("      Beta         S.E.\n");
      for i = (nz + 1) : (nz + nx)
        printf ("   %8.4f     %8.4f\n", tb (i), se (i));
      endfor
    endif
  endif

  if (nargout == 6)
    if (nx > 0)
      e = ((x * beta) * ones (1, nz)) + ((y * 0 + 1) * theta');
    else
      e = (y * 0 + 1) * theta';
    endif
    gamma = diff ([(y * 0), (exp (e) ./ (1 + exp (e))), (y * 0 + 1)]')';
  endif

endfunction
