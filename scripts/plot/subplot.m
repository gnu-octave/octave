# Copyright (C) 1995 John W. Eaton
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
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA. 

function subplot (rows, columns, index)

# usage: subplot (rows, columns, index)
#        subplot (rcn)
#
# NOTE: this will work only with gnuplot installed with
#       multiplot patch (or version 3.6 beta)
#
# Sets gnuplot in multiplot mode and plots in location
# given by index (there are columns X rows subwindows)
#
# Input:
#
#   rows   : number of rows in subplot grid
#   columns: number of columns in subplot grid
#   index  : index of subplot where to make the next plot
#
# If only one arg, then it (crn) has to be three digit value
# specifying the location in digit 1 (rows) and 2 (columns) and digit
# 3 is the plot index
#
# The plot index runs row-wise,i.e., first all the columns in a row
# are filled and then the next row is filled
#
# For example, plot with 4 X 2 grid, will have plot indices running as
# follows:
#
#   -----------------------------------
#   |        |       |       |        |
#   |    1   |    2  |    3  |    4   |
#   |        |       |       |        |
#   -----------------------------------
#   |        |       |       |        |
#   |    5   |    6  |    7  |    8   |
#   |        |       |       |        |
#   -----------------------------------
#

# Written by Vinayak Dutt, Dutt.Vinayak@mayo.EDU

# global variables to keep track of multiplot options

  global multiplot_mode 
  global multi_xsize multi_ysize 
  global multi_xn multi_yn
  global multi_xi multi_yi

# This is a real kludge.  We gnuplot should be made so that replot can
# be executed while doing multiple plots...

  global multiplot_save_auto_replot = automatic_replot

  if (nargin != 3 && nargin != 1)
    usage ("subplot (rows, columns, index) or subplot (rcn)");
  endif

  if ((isstr (automatic_replot) && strcmp (automatic_replot, "true"))
       || automatic_replot)
    warning ("turning off automatic replot for multiplot mode");
    multiplot_save_auto_replot = automatic_replot;
    automatic_replot = 0;
  endif

  if (nargin == 1)

    if (! is_scalar (rows))
      error ("subplot: input rcn has to be a scalar");
    endif

    xnp = rows;
    rows = round (xnp/100);
    columns = round ((xnp - 100*rows)/10);
    index = xnp - 10*columns - 100*rows;

  elseif (! (is_scalar (columns) && is_scalar (rows) && is_scalar (index)))
    error ("subplot: columns, rows, and index have to be scalars");
  endif

  columns = round (columns);
  rows = round (rows);
  index = round (index);

  if (index > columns*rows)
    error ("subplot: index must be less than columns*rows");
  endif

  if (columns < 1 || rows < 1 || index < 1)
    error ("subplot: columns,rows,index must be be positive");
  endif

  if (columns*rows == 1)

# switching to single plot ?

    set nomultiplot;
    set size 1,1
    set origin 0,0

    multi_xn = 1;
    multi_yn = 1;
    multiplot_mode = 0;

# Someone may have reset it betweeen calls...

    if (! isstr (automatic_replot) && ! automatic_replot)
      automatic_replot = multiplot_save_auto_replot;
    endif

    return;

  endif

# doing multiplot plots

  doagain = 0;

  if (exist ("multiplot_mode") != 1)
    doagain = 1;
  elseif (multiplot_mode != 1 || multi_xn != columns || multi_yn != rows)
    doagain = 1;
  endif

  if (doagain)

    multiplot_mode = 1;
    multi_xn = columns;
    multi_yn = rows;
    multi_xsize = 1.0 ./ columns;
    multi_ysize = 1.0 ./ rows;

    set multiplot;

    eval (sprintf ("set size %g, %g", multi_xsize, multi_ysize));

  endif

# get the sub plot location

  yp = round ((index-1)/columns);
  xp = index - yp*columns - 1;
  multi_xi = ++xp;
  multi_yi = ++yp;

# set the origin

  xo = (xp - 1.0)*multi_xsize;
  yo = (rows - yp)*multi_ysize;

  eval (sprintf ("set origin %g, %g", xo, yo));

endfunction
