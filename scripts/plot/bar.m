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

function [xb, yb] = bar (x, y)

# usage: [xb, yb] = bar (x, y)
#
# Given two vectors of x-y data, bar produces a `bar' graph.
#
# If only one argument is given, it is taken as a vector of y-values
# and the x coordinates are taken to be the indices of the elements.
#
# If two output arguments are specified, the data are generated but
# not plotted.  For example,
#
#   bar (x, y);
#
# and
#
#   [xb, yb] = bar (x, y);
#   plot (xb, yb);
#
# are equivalent.
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           stairs, gplot, gsplot, replot, xlabel, ylabel, title 

  if (nargin == 1)
    if (is_vector (x))
      len = 3 * length (x) + 1;
      tmp_xb = tmp_yb = zeros (len, 1);
      tmp_xb(1) = 0.5;
      tmp_yb(1) = 0;
      k = 1;
      for i = 2:3:len
        tmp_xb(i) = k-0.5;
        tmp_xb(i+1) = k+0.5;
        tmp_xb(i+2) = k+0.5;
        tmp_yb(i) = x(k);
        tmp_yb(i+1) = x(k);
        tmp_yb(i+2) = 0.0;
        k++;
      endfor
    else
      error ("bar: argument must be a vector");
    endif
  elseif (nargin == 2)
    if (is_vector (x) && is_vector (y))
      xlen = length (x);
      ylen = length (y);
      if (xlen == ylen)
        len = 3 * xlen + 1;
        tmp_xb = tmp_yb = zeros (len, 1);
        delta = (x(2) - x(1)) / 2.0;
        tmp_xb(1) = x(1) - delta;
        tmp_yb(1) = 0.0;
	k = 1;
        for i = 2:3:len
          tmp_xb(i) = tmp_xb(i-1);
          tmp_xb(i+1) = tmp_xb(i) + 2.0 * delta;
          tmp_xb(i+2) = tmp_xb(i+1);
	  tmp_yb(i) = y(k);
	  tmp_yb(i+1) = y(k);
	  tmp_yb(i+2) = 0.0;
          if (k < xlen)
            delta = (x(k+1) - x(k)) / 2.0;
            if (x(k+1) < x(k))
              error ("bar: x vector values must be in ascending order");
            endif
          endif
          k++;
	endfor
      else
        error ("bar: arguments must be the same length");
      endif
    else
      error ("bar: arguments must be vectors");
    endif
  else
    usage ("[xb, yb] = bar (x, y)");
  endif

  if (nargout == 0)
    plot (tmp_xb, tmp_yb);
  else
    xb = tmp_xb;
    yb = tmp_yb;
  endif

endfunction
