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

function retval = mahalanobis (X, Y)

# usage: mahalanobis (X, Y)
#
# Returns Mahalanobis' D-square distance between the multivariate
# samples X and Y, which must have the same number of components
# (columns), but may have a different number of observations (rows).

# Written by Friedrich Leisch (leisch@neuro.tuwien.ac.at) July 1993.
# Dept of Probability Theory and Statistics TU Wien, Austria.

  if (nargin != 2)
    error ("usage: mahalanobis (X, Y)");
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
