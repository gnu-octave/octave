# Copyright (C) 1994 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function [BETA, SIGMA, R] = ols (Y, X)

# usage: [BETA, SIGMA [, R]] = ols (Y, X)
#
# Ordinary Least Squares (OLS) estimation for the multivariate model
#
#     Y = X*B + E,  mean(E) = 0,  cov(vec(E)) = kron(S,I)
#
# with Y ... T x p     As usual, each row of Y and X is an observation
#      X ... T x k     and each column a variable.
#      B ... k x p
#      E ... T x p.
#
# BETA is the OLS estimator for B, i.e.
#
#   BETA = pinv(X)*Y,
#
# where pinv(X) denotes the pseudoinverse of X.
# SIGMA is the OLS estimator for the matrix S, i.e.
#
#   SIGMA = (Y - X*BETA)'*(Y - X*BETA) / (T - rank(X)).
#
# R = Y - X*BETA is the matrix of OLS residuals.

# Written by Teresa Twaroch (twaroch@neuro.tuwien.ac.at) May 1993.
# Dept of Probability Theory and Statistics TU Wien, Austria.

  if (nargin != 2)
    error("usage : [BETA, SIGMA [, R]] = ols (Y, X)");
  endif

  [nr, nc] = size (X);
  [ry, cy] = size (Y);
  if (nr != ry)
    error ("ols: incorrect matrix dimensions");
  endif

  Z = X' * X;
  r = rank (Z);

  if (r == nc)
    BETA = inv (Z) * X' * Y;
  else
    BETA = pinv (X) * Y;
  endif

  R = Y - X * BETA;
  SIGMA = R' * R / (nr - r);

endfunction

