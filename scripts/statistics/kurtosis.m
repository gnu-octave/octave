# Copyright (C) 1996 John W. Eaton
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

function retval = kurtosis (x)

# usage: kurtosis (x)
#
# If x is a vector of length N, return the kurtosis
#
#   kurtosis(x) = N^(-1) std(x)^(-4) SUM_i (x(i)-mean(x))^4 - 3
#
# of x.
#
# If x is a matrix, return a row vector containing the kurtosis for each
# column.

# Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Jul 29, 1994.

  if (nargin != 1)
    usage ("kurtosis (x)");
  endif

  if (is_vector (x))
    x = x - mean (x);
    if (! any (x))
      retval = 0;
    else
      retval = sum (x .^ 4) / (length (x) * std (x) ^ 4) - 3;
    endif
  elseif (is_matrix (x))
    [nr, nc] = size (x);
    x = x - ones (nr, 1) * mean (x);
    retval = zeros (1, nc);
    s      = std (x);
    ind    = find (s > 0);
    retval (ind) = sum (x (:, ind) .^ 4) ./ (nr * s (ind) .^ 4) - 3;
  else
    error ("kurtosis: x has to be a matrix or a vector.");
  endif

endfunction
