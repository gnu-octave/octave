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

## -*- texinfo -*-
## @deftypefn {Function File} {} stairs (@var{x}, @var{y})
## Given two vectors of x-y data, bar produces a `stairstep' plot.
##
## If only one argument is given, it is taken as a vector of y-values
## and the x coordinates are taken to be the indices of the elements.
##
## If two output arguments are specified, the data are generated but
## not plotted.  For example,
##
## @example
## stairs (x, y);
## @end example
##
## @noindent
## and
##
## @example
## [xs, ys] = stairs (x, y);
## plot (xs, ys);
## @end example
##
## @noindent
## are equivalent.
## @end deftypefn
##
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, gplot, gsplot, replot, xlabel, ylabel, and title}

## Author: jwe

function [xs, ys] = stairs (x, y)


  if (nargin == 1)
    if (isvector (x))
      len = 2 * length (x);
      tmp_xs = tmp_ys = zeros (len, 1);
      k = 0;
      for i = 1:2:len
        tmp_xs(i) = k++;
        tmp_ys(i) = x(k);
        tmp_ys(i+1) = x(k);
        tmp_xs(i+1) = k;
      endfor
    else
      error ("stairs: argument must be a vector");
    endif
  elseif (nargin == 2)
    if (isvector (x) && isvector (y))
      xlen = length (x);
      ylen = length (y);
      if (xlen == ylen)
        len = 2 * xlen;
        tmp_xs = tmp_ys = zeros (len, 1);
        k = 1;
        len_m2 = len - 2;
        for i = 1:2:len_m2
          tmp_xs(i) = x(k);
          tmp_ys(i) = y(k);
          tmp_ys(i+1) = y(k);
          k++;
          tmp_xs(i+1) = x(k);
          if (x(k) < x(k-1))
            error ("stairs: x vector values must be in ascending order");
          endif
        endfor
        tmp_xs(len-1) = x(xlen);
        delta = x(xlen) - x(xlen-1);
        tmp_xs(len) = x(xlen) + delta;
        tmp_ys(len-1) = y(ylen);
        tmp_ys(len) = y(ylen);
      else
        error ("stairs: arguments must be the same length");
      endif
    else
      error ("stairs: arguments must be vectors");
    endif
  else
    usage ("[xs, ys] = stairs (x, y)");
  endif

  if (nargout == 0)
    plot (tmp_xs, tmp_ys);
  else
    xs = tmp_xs;
    ys = tmp_ys;
  endif

endfunction
