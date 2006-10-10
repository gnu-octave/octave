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
## @deftypefnx {Function File} {} grid ("minor", @var{arg2})
## For two-dimensional plotting, force the display of a grid on the plot.
## The argument may be either @code{"on"} or @code{"off"}.  If it is
## omitted, the the current grid state is toggled.
##
## If @var{arg} is @code{"minor"} then the minor grid is toggled. When
## using a minor grid a second argument @var{arg2} is allowed, which can
## be either @code{"on"} or @code{"off"} to explicitly set the state of
## the minor grid, or alternatively a positive integer specifying the
## number of minor grid lines.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, stairs, replot, xlabel, ylabel, title}
## @end deftypefn

## Author: jwe

## PKG_ADD: mark_as_command grid

function grid (x, y)

  persistent grid_on = false;
  persistent minor_on = false;
  persistent minor_tics = 5;

  do_replot = false;

  if (nargin == 0)
    grid_on = ! grid_on;
    if (grid_on)
      __gnuplot_raw__ ("set grid;\n");
    else
      __gnuplot_raw__ ("set nogrid;\n");
    endif
    do_replot = true;
  elseif (nargin == 1)
    if (ischar (x))
      if (strcmp ("off", x))
        __gnuplot_raw__ ("set nogrid;\n");
	grid_on = false;
	do_replot = true;
      elseif (strcmp ("on", x))
        __gnuplot_raw__ ("set grid;\n");
	grid_on = true;
	do_replot = true;
      elseif (strcmp ("minor", x))
	minor_on = ! minor_on;
	if (minor_on)
	  cmd = sprintf ("set mxtics %d;\n", minor_tics);
	  __gnuplot_raw__ (cmd);
	  cmd = sprintf ("set mytics %d;\n", minor_tics);
	  __gnuplot_raw__ (cmd);
          __gnuplot_raw__ ("set grid xtics mxtics ytics mxtics;\n");
	  minor_on = true;
	else
	  if (grid_on)
            __gnuplot_raw__ ("set grid xtics nomxtics ytics nomxtics;\n");
	  else
	    __gnuplot_raw__ ("set grid noxtics nomxtics noytics nomxtics;\n");
	  endif
	  minor_on = false;
	endif
	do_replot = true;
      else
	print_usage ();
      endif
    else
      error ("grid: argument must be a string");
    endif
  elseif (nargin == 2)
    if (ischar (x))
      if (strcmp ("minor", x))
	d = str2num (y);
	if (isempty (d))
	  if (strcmp ("off", y))
	    if (grid_on)
              __gnuplot_raw__ ("set grid xtics nomxtics ytics nomxtics;\n");
	    else
	      __gnuplot_raw__ ("set grid noxtics nomxtics noytics nomxtics;\n");
	    endif
	    minor_on = false;
	  elseif (strcmp ("on", y))
	    cmd = sprintf ("set mxtics %d;\n", minor_tics);
	    __gnuplot_raw__ (cmd);
	    cmd = sprintf ("set mytics %d;\n", minor_tics);
	    __gnuplot_raw__ (cmd);
            __gnuplot_raw__ ("set grid xtics mxtics ytics mxtics;\n");
	    minor_on = true;
	  else
	    print_usage ();
	  endif
	  do_replot = true;
	else
	  if (isscalar(d) && ! isnan (d) && ! isinf (d))
	    minor_tics = max (floor (d), 0);
	    cmd = sprintf ("set mxtics %d;\n", minor_tics);
	    __gnuplot_raw__ (cmd);
	    cmd = sprintf("set mytics %d;\n", minor_tics);
	    __gnuplot_raw__ (cmd);
            __gnuplot_raw__ ("set grid xtics mxtics ytics mxtics;\n");
	    minor_on = true;
	    do_replot = true;
	  else
	    print_usage ();;
	  endif
	endif
      else
	print_usage ();;
      endif
    else
      print_usage ();;
    endif    
  else
    print_usage ();;
  endif    

  if (do_replot && automatic_replot)
    replot ();
  endif

endfunction
