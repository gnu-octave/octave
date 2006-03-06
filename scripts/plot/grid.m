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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} grid (@var{arg})
## For two-dimensional plotting, force the display of a grid on the plot.
## The argument may be either @code{"on"} or @code{"off"}.  If it is
## omitted, @code{"on"} is assumed.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, stairs, replot, xlabel, ylabel, title}
## @end deftypefn

## Author: jwe

## PKG_ADD: mark_as_command grid

function grid (x)

  usage_msg = "grid (\"on\" | \"off\")";

  do_replot = false;

  if (nargin == 0)
    __gnuplot_raw__ ("set grid;\n");
    do_replot = true;
  elseif (nargin == 1)
    if (ischar (x))
      if (strcmp ("off", x))
        __gnuplot_raw__ ("set nogrid;\n");
	do_replot = true;
      elseif (strcmp ("on", x))
        __gnuplot_raw__ ("set grid;\n");
	do_replot = true;
      else
	usage (usage_msg);
      endif
    else
      error ("grid: argument must be a string");
    endif
  else
    usage (usage_msg);
  endif

  if (do_replot && automatic_replot)
    replot ();
  endif

endfunction
