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

## Usage: [Ad, Bd] = c2d (Ac, Bc, T)
##
## converts the continuous time system described by:
##   .
##   x = Ac x + Bc u
##
## into a discrete time equivalent model via the matrix exponential
##
##   x[n+1] = Ad x[n] + Bd u[n]
##
## assuming a zero-order hold on the input and sample time T.

## Author: R.B. Tenison <btenison@eng.auburn.edu>
## Created: October 1993
## Adapted-By: jwe

function [Ad, Bd] = c2d (Ac, Bc, T)

  ## check args
  if (nargin != 3)
    usage ("c2d (Ac, Bc, T)");
  endif

  [ma, na] = size (Ac);
  [mb, nb] = size (Bc);

  if (ma != na)
    error ("c2d: Ac must be square");
  endif

  if (ma != mb)
    error ("c2d: Ac and Bc must have the same number of rows");
  endif

  matexp = expm ([[Ac, Bc] * T; (zeros (nb, na+nb))]);

  Ad = matexp (1:na, 1:na);
  Bd = matexp (1:na, na+1:na+nb);

endfunction
