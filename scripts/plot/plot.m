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

function plot (x1, x2)

# usage: plot (x, y)
#
# If the first argument is a vector and the second is a matrix, the
# the vector is plotted versus the columns (or rows) of the matrix.
# (using whichever combination matches, with columns tried first.)
#
# If the first argument is a matrix and the second is a vector, the
# the columns (or rows) of the matrix are plotted versus the vector.
# (using whichever combination matches, with columns tried first.)
#
# If both arguments are vectors, the elements of y are plotted versus
# the elements of x.
#
# If both arguments are matrices, the columns of y are plotted versus
# the columns of x.  In this case, both matrices must have the same
# number of rows and columns and no attempt is made to transpose the
# arguments to make the number of rows match.
#
# If both arguments are scalars, a single point is plotted.
#
# If only one argument is given, it is taken as the set of y
# coordinates and the x coordinates are taken to be the indices of the
# elements, starting with 1.
#
# See also: semilogx, semilogy, loglog, polar, mesh, contour,
#           bar, stairs, gplot, gsplot, replot, xlabel, ylabel, title 

  set nologscale;
  set nopolar;

  if (nargin == 1)
    plot_int (x1);
  elseif (nargin == 2)
    plot_int (x1, x2);
  else
    usage = sprintf ("usage: plot (x)\n");
    usage = sprintf ("%s       plot (x, y)", usage);
    error (usage);
  endif

endfunction
