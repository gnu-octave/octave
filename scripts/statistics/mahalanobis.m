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

## usage: mahalanobis (X, Y)
##
## Returns Mahalanobis' D-square distance between the multivariate
## samples X and Y, which must have the same number of components
## (columns), but may have a different number of observations (rows).

## Author: Friedrich Leisch <leisch@ci.tuwien.ac.at>
## Created: July 1993
## Adapted-By: jwe

function retval = mahalanobis (X, Y)

  if (nargin != 2)
    usage ("mahalanobis (X, Y)");
  endif

  [xr, xc] = size (X);
  [yr, yc] = size (Y);

  if (xc != yc)
    error ("mahalanobis: X and Y must have the same number of columns.");
  endif

  Xm = sum (X) / xr;
  Ym = sum (Y) / yr;

  X = X - ones (xr, 1) * Xm;
  Y = Y - ones (yr, 1) * Ym;

  W = (X' * X + Y' * Y) / (xr + yr - 2);

  Winv = inv (W);

  retval = (Xm - Ym) * Winv * (Xm - Ym)';

endfunction
