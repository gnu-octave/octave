# Copyright (C) 1993, 1994, 1995 John W. Eaton
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
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

function [k, p, e] = lqe (a, g, c, sigw, sigv, zz)

# Usage: [k, p, e] = lqe (A, G, C, SigW, SigV {,Z})
#
# Linear quadratic estimator (Kalman filter) design for the 
# continuous time system
#
#   dx/dt = A x + B u + G w
#       y = C x + D u + v
#
# where w, v are zero-mean gaussian noise processes with respective
# intensities SigW = cov (w, w) and SigV = cov (v, v).
#
# Z (if specified) is cov(w,v); otherwise cov(w,v) = 0.
#
# Observer structure is dz/dt = A z + B u + k( y - C z - D u).
#
# Returns:
#
#   k = observer gain, (A - K C) is stable
#   p = solution of algebraic Riccati equation
#   e = closed loop poles of (A - K C)

# Written by A. S. Hodel (scotte@eng.auburn.edu) August, 1993.

  if (nargin != 5 && nargin != 6)
    error ("lqe: invalid number of arguments");
  endif

# The problem is dual to the regulator design, so transform to lqr
# call.

  if (nargin == 5)
    [k, p, e] = lqr (a', c', g*sigw*g', sigv);
  else
    [k, p, e] = lqr (a', c', g*sigw*g', sigv, g*zz);
  endif

  k = k';

endfunction
