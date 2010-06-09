## Copyright (C) 1996, 1997, 1998, 1999, 2000, 2005, 2006, 2007, 2009
##               John W. Eaton
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
## @deftypefn {Function File} {[@var{beta}, @var{sigma}, @var{r}] =} ols (@var{y}, @var{x})
## Ordinary least squares estimation for the multivariate model
## @tex
## $y = x b + e$
## with
## $\bar{e} = 0$, and cov(vec($e$)) = kron ($s, I$)
## @end tex
## @ifnottex
## @w{@math{y = x*b + e}} with
## @math{mean (e) = 0} and @math{cov (vec (e)) = kron (s, I)}.
## @end ifnottex
##  where
## @tex
## $y$ is a $t \times p$ matrix, $x$ is a $t \times k$ matrix,
## $b$ is a $k \times p$ matrix, and $e$ is a $t \times p$ matrix.
## @end tex
## @ifnottex
## @math{y} is a @math{t} by @math{p} matrix, @math{x} is a @math{t} by
## @math{k} matrix, @math{b} is a @math{k} by @math{p} matrix, and
## @math{e} is a @math{t} by @math{p} matrix.
## @end ifnottex
##
## Each row of @var{y} and @var{x} is an observation and each column a
## variable.
##
## The return values @var{beta}, @var{sigma}, and @var{r} are defined as
## follows.
##
## @table @var
## @item beta
## The OLS estimator for @var{b}, @code{@var{beta} = pinv (@var{x}) *
## @var{y}}, where @code{pinv (@var{x})} denotes the pseudoinverse of
## @var{x}.
##
## @item sigma
## The OLS estimator for the matrix @var{s},
##
## @example
## @group
## @var{sigma} = (@var{y}-@var{x}*@var{beta})'
##   * (@var{y}-@var{x}*@var{beta})
##   / (@var{t}-rank(@var{x}))
## @end group
## @end example
##
## @item r
## The matrix of OLS residuals, @code{@var{r} = @var{y} - @var{x}*@var{beta}}.
## @end table
## @end deftypefn

## Author: Teresa Twaroch <twaroch@ci.tuwien.ac.at>
## Created: May 1993
## Adapted-By: jwe

function [BETA, SIGMA, R] = ols (Y, X)

  if (nargin != 2)
    print_usage ();
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
