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

  global __multiplot_mode__
  global __multiplot_xsize__
  global __multiplot_ysize__
  global __multiplot_xn__
  global __multiplot_yn__
  global __multiplot_xi__
  global __multiplot_yi__

  if (! exist ("__multiplot_mode__"))
    __multiplot_mode__ = 0;
  endif

  gset nologscale;
  gset nopolar;

  __plt__ ("plot", all_va_args);

  ## update the plot position

  if (__multiplot_mode__)

    if (__multiplot_xi__ < __multiplot_xn__)
      __multiplot_xi__++;
    else
      __multiplot_xi__ = 1;
      if (__multiplot_yi__ < multiplot_xn__)
	__multiplot_yi__++;
      else
	__multiplot_yi__ = 1;
      endif
    endif

    xo = (__multiplot_xi__ - 1.0) * __multiplot_xsize__;
    yo = (__multiplot_yn__ - __multiplot_yi) * __multiplot_ysize__;

    eval (sprintf ("gset origin %g, %g", xo, yo));

  endif

endfunction
