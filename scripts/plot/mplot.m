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

function mplot (...)

# usage: mplot (x, y)
#        mplot (x1, y1, x2, y2, ...)
#        mplot (x, y, fmt)
#
# This is a modified version of plot() command to work with
# multiplot version of gnuplot to plot multiple plots per page.
# This plot version automatically updates the plot position to
# next plot position after making the plot in the given subplot
# position.
#
# See command plot() for the various options to this command
# as this is just mulitplot version of the same command.


# Written by Vinayak Dutt, Dutt.Vinayak@mayo.EDU

# global variables to keep track of multiplot options

  global multiplot_mode
  global multi_xsize multi_ysize
  global multi_xn multi_yn
  global multi_xi multi_yi

# This is a real kludge.  We gnuplot should be made so that replot can
# be executed while doing multiple plots...

  global multiplot_save_auto_replot = automatic_replot

  if ((isstr (automatic_replot) && strcmp (automatic_replot,"true"))
       || automatic_replot)
    warning ("turning off automatic replot for multiplot mode");
    multiplot_save_auto_replot = automatic_replot;
    automatic_replot = 0;
  endif

  set nologscale;
  set nopolar;

  plot_int ("plot", all_va_args);

# update the plot position

  if (multiplot_mode)

    if (multi_xi < multi_xn)
      multi_xi++;
    else
      multi_xi = 1;
      if (multi_yi < multi_xn)
	multi_yi++;
      else
	multi_yi = 1;
      endif
    endif

    xo = (multi_xi - 1.0)*multi_xsize;
    yo = (multi_yn - multi_yi)*multi_ysize;

    eval (sprintf ("set origin %g, %g", xo,yo));

  endif

endfunction



