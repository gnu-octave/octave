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

## usage: mplot (x, y)
##        mplot (x1, y1, x2, y2, ...)
##        mplot (x, y, fmt)
##
## This is a modified version of plot() command to work with
## multiplot version of gnuplot to plot multiple plots per page.
## This plot version automatically updates the plot position to
## next plot position after making the plot in the given subplot
## position.
##
## See command plot() for the various options to this command
## as this is just mulitplot version of the same command.

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Adapted-By: jwe

function mplot (...)

  if (! gnuplot_has_multiplot)
    error ("mplot: gnuplot does not appear to support this feature");
  endif

  ## global variables to keep track of multiplot options

  global multiplot_mode
  global multiplot_xsize multiplot_ysize
  global multiplot_xn multiplot_yn
  global multiplot_xi multiplot_yi

  ## This is a real kludge.  We gnuplot should be made so that replot can
  ## be executed while doing multiple plots...

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

  ## update the plot position

  if (multiplot_mode)

    if (multiplot_xi < multiplot_xn)
      multiplot_xi++;
    else
      multiplot_xi = 1;
      if (multiplot_yi < multiplot_xn)
	multiplot_yi++;
      else
	multiplot_yi = 1;
      endif
    endif

    xo = (multiplot_xi - 1.0)*multiplot_xsize;
    yo = (multiplot_yn - multiplot_yi)*multiplot_ysize;

    eval (sprintf ("set origin %g, %g", xo,yo));

  endif

endfunction



