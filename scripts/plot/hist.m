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

function [nn, xx] = hist (y, x)

# usage: [NN, XX] = hist (Y, X)  or  hist (Y, X)
#
# Produce histogram counts or plots.
#
# With one vector input argument, plot a histogram of the values with
# 10 bins.  The range of the histogram bins is determined by the range
# of the data.
#
# Given a second scalar argument, use that as the number of bins.
#
# Given a second vector argument, use that as the centers of the bins,
# with the width of the bins determened from the adjacent values in
# the vector.
#
# Extreme values are lumped in the first and last bins.
#
# With two output arguments, produce the values NN and XX such that
# bar (XX, NN) will plot the histogram.
#
# See also: bar

  if (nargin < 1 || nargin > 2)
    error ("usage: [nn, xx] = hist (y, x)");
  endif
    
  if (is_vector (y))
    max_val = max (y);
    min_val = min (y);
  else
    error ("hist: first argument must be a vector");
  endif

  if (nargin == 1)
    n = 10;
    delta = (max_val - min_val) / n / 2;
    x = linspace (min_val+delta, max_val-delta, n);
    cutoff = x + delta;
  elseif (nargin == 2)
    if (is_scalar (x))
      n = x;
      if (n <= 0)
        error ("hist: number of bins must be positive");
      endif
      delta = (max_val - min_val) / n / 2;
      x = linspace (min_val+delta, max_val-delta, n);
      cutoff = x + delta;
    elseif (is_vector (x))
      tmp = sort (x);
      if (any (tmp != x))
        fprintf (stderr, "warning: hist: bin values not sorted on input\n");
        x = tmp;
      endif
      n = length (x);
      cutoff = zeros (1, n-1);
      for i = 1:n-1
        cutoff (i) = (x (i) + x (i+1)) / 2;
      endfor
    else
      error ("hist: second argument must be a scalar or a vector");
    endif
  endif

  freq = zeros (1, n);
  freq (1) = sum (y < cutoff (1));
  for i = 2:n-1
    freq (i) = sum (y >= cutoff (i-1) & y < cutoff (i));
  endfor
  freq (n) = sum (y >= cutoff (n-1));

# With Octave 1.0, nargout is never zero, so we have to do this: 

  nn = freq;
  xx = x;
  if (nargout != 2)
    bar (xx, nn);
  endif

# Once 1.1 is released, we can do this instead:
#
#  if (nargout == 2)
#    nn = freq;
#    xx = x;
#  else
#    bar (x, freq);
#  endif

endfunction
