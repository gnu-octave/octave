# Copyright (C) 1993 John W. Eaton
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

function retval = skewness (x)

# usage: skewness (x)
#
# If x is a vector of length N, return the skewness
#
#   skewness (x) = N^(-1) std(x)^(-3) SUM_i (x(i)-mean(x))^3
#
# of x.
#
# If x is a matrix, return the row vector containing the skewness
# of each column.

# Written by Kurt Hornik (hornik@neuro.tuwien.ac.at) June 1993.
# Dept of Probability Theory and Statistics TU Wien, Austria.

  if (nargin != 1)
    error("usage: skewness (x)");
  endif

  [nr, nc] = size (x);
  if (nr == 1 || nc == 1)
    n = max (nr, nc);
    x = x - ones (x) * sum (x) / n;
    retval = sum (x.^3) / (n * max (sumsq (x)^(3/2), ! any (x)));
  elseif (nr > 0 && nc > 0)
    x = x - ones (nr, 1) * sum (x) / nr;
    retval = sum (x.^3) ./ (nr * max (sumsq (x).^(3/2), ! any (x)));
  else
    error ("skewness: invalid matrix argument");
  endif

endfunction
