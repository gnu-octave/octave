# Copyright (C) 1993, 1994 John W. Eaton
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

function semilogx (x1, x2)

# usage: semilogx (x, y)
#
# Make a 2D plot of y versus x using a log scale for the x axis. 
#
# See the help message for the plot command for a description of how
# the arguments are interpreted. 
#
# See also: plot, semilogy, loglog, polar, mesh, contour, bar, stairs,
#           gplot, gsplot, replot, xlabel, ylabel, title 

  set logscale x;
  set nologscale y;
  set nopolar;

  if (nargin == 1)
    plot_int (x1);
  elseif (nargin == 2)
    plot_int (x1, x2);
  else
    usage ("semilogx (x [, y])");
  endif

endfunction
