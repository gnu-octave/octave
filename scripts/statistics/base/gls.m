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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## @deftypefn {Function File} {[@var{beta}, @var{v}, @var{r}] =} gls (@var{y}, @var{x}, @var{o})
## Generalized least squares estimation for the multivariate model
## @iftex
## @tex
## $y = x b + e$
## with $\bar{e} = 0$ and cov(vec($e$)) = $(s^2)o$,
## @end tex
## @end iftex
## @ifinfo
## @code{@var{y} = @var{x} * @var{b} + @var{e}} with @code{mean (@var{e}) =
## 0} and @code{cov (vec (@var{e})) = (@var{s}^2)*@var{o}},
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
## @var{Y} is a @var{T} by @var{p} matrix, @var{X} is a @var{T} by @var{k}
## matrix, @var{B} is a @var{k} by @var{p} matrix, @var{E} is a @var{T} by
## @var{p} matrix, and @var{O} is a @var{T}@var{p} by @var{T}@var{p}
## matrix.
## @end ifinfo
## 
## @noindent
## Each row of Y and X is an observation and each column a variable.
## 
## The return values @var{beta}, @var{v}, and @var{r} are defined as
## follows.
## 
## @table @var
## @item beta
## The GLS estimator for @var{b}.
## 
## @item v
## The GLS estimator for @code{@var{s}^2}.
## 
## @item r
## The matrix of GLS residuals, @code{@var{r} = @var{y} - @var{x} *
## @var{beta}}.
## @end table
## @end deftypefn

## Author: Teresa Twaroch <twaroch@ci.tuwien.ac.at>
## Created: May 1993
## Adapted-By: jwe

function [BETA, v, R] = gls (Y, X, O)

  if (nargin != 3)
    usage ("[BETA, v [, R]] = gls (Y, X, O)");
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
