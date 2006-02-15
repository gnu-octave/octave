## Copyright (C) 1993, 1994, 1995 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{k}, @var{p}, @var{e}] =} lqe (@var{a}, @var{g}, @var{c}, @var{sigw}, @var{sigv}, @var{z})
## Construct the linear quadratic estimator (Kalman filter) for the
## continuous time system
## @iftex
## @tex
## $$
##  {dx\over dt} = A x + G u
## $$
## $$
##  y = C x + v
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## dx
## -- = A x + G u
## dt
##
## y = C x + v
## @end example
##
## @end ifinfo
## where @var{w} and @var{v} are zero-mean gaussian noise processes with
## respective intensities
##
## @example
## sigw = cov (w, w)
## sigv = cov (v, v)
## @end example
##
## The optional argument @var{z} is the cross-covariance
## @code{cov (@var{w}, @var{v})}.  If it is omitted,
## @code{cov (@var{w}, @var{v}) = 0} is assumed.
##
## Observer structure is @code{dz/dt = A z + B u + k (y - C z - D u)}
##
## The following values are returned:
##
## @table @var
## @item k
## The observer gain,
## @iftex
## @tex
## $(A - K C)$
## @end tex
## @end iftex
## @ifinfo
## (@var{a} - @var{k}@var{c})
## @end ifinfo
## is stable.
##
## @item p
## The solution of algebraic Riccati equation.
##
## @item e
## The vector of closed loop poles of
## @iftex
## @tex
## $(A - K C)$.
## @end tex
## @end iftex
## @ifinfo
## (@var{a} - @var{k}@var{c}).
## @end ifinfo
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1993

function [k, p, e] = lqe (a, g, c, sigw, sigv, zz)

  if ( (nargin != 5) && (nargin != 6))
    error ("lqe: invalid number of arguments");
  endif

  ## The problem is dual to the regulator design, so transform to lqr
  ## call.

  if (nargin == 5)
    [k, p, e] = lqr (a', c', g*sigw*g', sigv);
  else
    [k, p, e] = lqr (a', c', g*sigw*g', sigv, g*zz);
  endif

  k = k';

endfunction
