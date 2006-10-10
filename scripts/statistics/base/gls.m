## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{beta}, @var{v}, @var{r}] =} gls (@var{y}, @var{x}, @var{o})
## Generalized least squares estimation for the multivariate model
## @iftex
## @tex
## $y = x b + e$
## with $\bar{e} = 0$ and cov(vec($e$)) = $(s^2)o$,
## @end tex
## @end iftex
## @ifinfo
## @math{y = x b + e} with @math{mean (e) = 0} and
## @math{cov (vec (e)) = (s^2) o},
## @end ifinfo
##  where
## @iftex
## @tex
## $y$ is a $t \times p$ matrix, $x$ is a $t \times k$ matrix, $b$ is a $k
## \times p$ matrix, $e$ is a $t \times p$ matrix, and $o$ is a $tp \times
## tp$ matrix.
## @end tex
## @end iftex
## @ifinfo
## @math{y} is a @math{t} by @math{p} matrix, @math{x} is a @math{t} by
## @math{k} matrix, @math{b} is a @math{k} by @math{p} matrix, @math{e}
## is a @math{t} by @math{p} matrix, and @math{o} is a @math{t p} by
## @math{t p} matrix.
## @end ifinfo
##
## @noindent
## Each row of @var{y} and @var{x} is an observation and each column a
## variable.  The return values @var{beta}, @var{v}, and @var{r} are
## defined as follows.
##
## @table @var
## @item beta
## The GLS estimator for @math{b}.
##
## @item v
## The GLS estimator for @math{s^2}.
##
## @item r
## The matrix of GLS residuals, @math{r = y - x beta}.
## @end table
## @end deftypefn

## Author: Teresa Twaroch <twaroch@ci.tuwien.ac.at>
## Created: May 1993
## Adapted-By: jwe

function [BETA, v, R] = gls (Y, X, O)

  if (nargin != 3)
    print_usage ();
  endif

  [rx, cx] = size (X);
  [ry, cy] = size (Y);
  if (rx != ry)
    error ("gls: incorrect matrix dimensions");
  endif

  O = O^(-1/2);
  Z = kron (eye (cy), X);
  Z = O * Z;
  Y1 = O * reshape (Y, ry*cy, 1);
  U = Z' * Z;
  r = rank (U);

  if (r == cx*cy)
    B = inv (U) * Z' * Y1;
  else
    B = pinv (Z) * Y1;
  endif

  BETA = reshape (B, cx, cy);
  R = Y - X * BETA;
  v = (reshape (R, ry*cy, 1))' * (O^2) * reshape (R, ry*cy, 1) / (rx*cy - r);

endfunction
