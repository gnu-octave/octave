## Copyright (C) 2000 Gabriele Pannocchia
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
## @deftypefn {Function File} {[@var{Lp}, @var{Lf}, @var{P}, @var{Z}] =} dkalman (@var{A}, @var{G}, @var{C}, @var{Qw}, @var{Rv}, @var{S})
## Construct the linear quadratic estimator (Kalman predictor) for the
## discrete time system
## @iftex
## @tex
## $$
##  x_{k+1} = A x_k + B u_k + G w_k
## $$
## $$
##  y_k = C x_k + D u_k + v_k
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## x[k+1] = A x[k] + B u[k] + G w[k]
##   y[k] = C x[k] + D u[k] + v[k]
## @end example
##
## @end ifinfo
## where @var{w}, @var{v} are zero-mean gaussian noise processes with
## respective intensities @code{@var{Qw} = cov (@var{w}, @var{w})} and
## @code{@var{Rv} = cov (@var{v}, @var{v})}.
##
## If specified, @var{S} is @code{cov (@var{w}, @var{v})}.  Otherwise
## @code{cov (@var{w}, @var{v}) = 0}.
##
## The observer structure is
## @iftex
## @tex
## $x_{k+1|k} = A x_{k|k-1} + B u_k + L_p (y_k - C x_{k|k-1} - D u_k)$
## $x_{k|k} = x_{k|k} + L_f (y_k - C x_{k|k-1} - D u_k)$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## x[k+1|k] = A x[k|k-1] + B u[k] + LP (y[k] - C x[k|k-1] - D u[k])
## x[k|k] = x[k|k-1] + LF (y[k] - C x[k|k-1] - D u[k])
## @end example
## @end ifinfo
##
## @noindent
## The following values are returned:
##
## @table @var
## @item Lp
## The predictor gain,
## @iftex
## @tex
## $(A - L_p C)$.
## @end tex
## @end iftex
## @ifinfo
## (@var{A} - @var{Lp} @var{C})
## @end ifinfo
## is stable.
##
## @item Lf
## The filter gain.
## 
## @item P
## The Riccati solution. 
## @iftex
## @tex
## $P = E \{(x - x_{n|n-1})(x - x_{n|n-1})'\}$
## @end tex
## @end iftex
## 
## @ifinfo
## P = E [(x - x[n|n-1])(x - x[n|n-1])']
## @end ifinfo
## 
## @item Z
## The updated error covariance matrix.
## @iftex
## @tex
## $Z = E \{(x - x_{n|n})(x - x_{n|n})'\}$
## @end tex
## @end iftex
## 
## @ifinfo
## Z = E [(x - x[n|n])(x - x[n|n])']
## @end ifinfo
## @end table
## @end deftypefn

## Author: Gabriele Pannocchia <pannocchia@ing.unipi.it>
## Created: July 2000

function [Lp, Lf, P, Z] = dkalman (A, G, C, Qw, Rv, S)

  if (nargin != 5 && nargin != 6)
    error ("dkalman: invalid number of arguments");
  endif

  ## Check A.
  if ((n = issquare (A)) == 0)
    error ("dkalman: requires 1st parameter(A) to be square");
  endif

  ## Check C.
  [p, n1] = size (C);
  if (n1 != n)
    error ("dkalman: A,C not conformal");
  endif

  ## Check G.
  [n1, nw] = size (G);
  if (n1 != n)
    error ("dkalman: A,G not conformal");
  endif

  ## Check Qw.
  if ((nw1 = issquare (Qw)) == 0)
    error ("dkalman: requires 4rd parameter(Qw) to be square");
  else
    if (nw1 != nw)
      error ("dkalman: G,Qw not conformal");
    endif
  endif

  ## Check Rv.
  if ((p1 = issquare (Rv)) == 0)
    error ("dkalman: requires 5rd parameter(Rv) to be square");
  else
    if (p1 != p)
      error ("dkalman: C,Rv not conformal");
    endif
  endif

  ## Check S if it is there
  if (nargin == 6)
    [nw1, p1] = size (S);
    if (nw1 != nw || p1 != p)
      error ("dkalman: S not conformal with Qw and Rv");
    else
      Cov_aug = [Qw, S; S', Rv];	
      if (! all (eig (Cov_aug) > 0))
	error ("dkalman: augmented noise covariance matrix must be positive definite");
      endif 
    endif
  else
    if (! all (eig (Qw) > 0) || ! all (eig (Rv) > 0))
      error ("dkalman: covariance matrices Qw,Rv must be positive definite");
    endif
    S = zeros (nw, p);
  endif

  ## Incorporate the cross term into A and Qw
  As = A - G*S/Rv*C;
  Qs = Qw - S/Rv*S';

  ## Call dare to solve the Riccati eqn.
  a = As';
  b = C';
  c = G*Qs*G';
  r = Rv;
  p = dare (a, b, c, r);

  ## Output
  Lp = (A*p*C'+G*S)/(Rv+C*p*C');
  Lf = (p*C')/(Rv+C*p*C');
  P = p;
  Z = p - Lf*C*p;

endfunction
