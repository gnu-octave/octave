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

function plot (...)

  ## usage: plot (x, y)
  ##        plot (x1, y1, x2, y2, ...)
  ##        plot (x, y, fmt)
  ##
  ## If the first argument is a vector and the second is a matrix, the
  ## the vector is plotted versus the columns (or rows) of the matrix.
  ## (using whichever combination matches, with columns tried first.)
  ##
  ## If the first argument is a matrix and the second is a vector, the
  ## the columns (or rows) of the matrix are plotted versus the vector.
  ## (using whichever combination matches, with columns tried first.)
  ##
  ## If both arguments are vectors, the elements of y are plotted versus
  ## the elements of x.
  ##
  ## If both arguments are matrices, the columns of y are plotted versus
  ## the columns of x.  In this case, both matrices must have the same
  ## number of rows and columns and no attempt is made to transpose the
  ## arguments to make the number of rows match.
  ##
  ## If both arguments are scalars, a single point is plotted.
  ##
  ## If only one argument is given, it is taken as the set of y
  ## coordinates and the x coordinates are taken to be the indices of the
  ## elements, starting with 1.
  ##
  ## To see possible options for FMT please see plot_opt.
  ##
  ## Examples:
  ##
  ##   plot (x, y, "@12", x, y2, x, y3, "4", x, y4, "+")
  ##
  ##     y will be plotted with points of type 2 ("+") and color 1 (red).
  ##     y2 will be plotted with lines.
  ##     y3 will be plotted with lines of color 4.
  ##     y4 will be plotted with points which are "+"s.
  ##
  ##   plot (b, "*")
  ##
  ##     b will be plotted with points of type "*".
  ##
  ## See also: semilogx, semilogy, loglog, polar, mesh, contour, plot_opt
  ##           bar, stairs, gplot, gsplot, replot, xlabel, ylabel, title 

  set nologscale;
  set nopolar;

  plot_int ("plot", all_va_args);

endfunction
