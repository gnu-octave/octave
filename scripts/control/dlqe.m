### Copyright (C) 1996 John W. Eaton
###
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

function [l, m, p, e] = dlqe (a, g, c, sigw, sigv, zz)

  ## Usage: [l, m, p, e] = dlqe (A, G, C, SigW, SigV {,Z})
  ##
  ## Linear quadratic estimator (Kalman filter) design for the 
  ## discrete time system
  ##
  ##  x[k+1] = A x[k] + B u[k] + G w[k]
  ##    y[k] = C x[k] + D u[k] + w[k]
  ##
  ## where w, v are zero-mean gaussian noise processes with respective
  ## intensities SigW = cov (w, w) and SigV = cov (v, v).
  ##
  ## Z (if specified) is cov(w,v); otherwise cov(w,v) = 0.
  ##
  ## Observer structure is 
  ##     z[k+1] = A z[k] + B u[k] + k(y[k] - C z[k] - D u[k]).
  ##
  ## Returns:
  ##
  ##   l = observer gain, (A - A L C) is stable
  ##   m = Ricatti equation solution
  ##   p = the estimate error covariance after the measurement update
  ##   e = closed loop poles of (A - A L C)

  ## Written by A. S. Hodel (scotte@eng.auburn.edu) August, 1993.
  ## Modified for discrete time by R. Bruce Tenison (btenison@eng.auburn.edu)
  ## October, 1993

  if (nargin != 5 && nargin != 6)
    error ("dlqe: invalid number of arguments");
  endif

  ## The problem is dual to the regulator design, so transform to lqr
  ## call.

  if (nargin == 5)
    [k, p, e] = dlqr (a', c', g*sigw*g', sigv);
    m = p';
    l = (m*c')/(c*m*c'+sigv);
  else
    [k, p, e] = dlqr (a', c', g*sigw*g', sigv, g*zz);
    m = p';
    l = (m*c'+a\g)/(c*m*c'+sigv);
    a = a-g*t/sigv*c;
    sigw = sigw-t/sigv;
  endif

  p = a\(m-g*sigw*g')/a';

endfunction
