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
## Force the display of a grid on the plot.
## The argument may be either @code{"on"} or @code{"off"}.  If it is
## omitted, the the current grid state is toggled.
##
## If @var{arg} is @code{"minor"} then the minor grid is toggled. When
## using a minor grid a second argument @var{arg2} is allowed, which can
## be either @code{"on"} or @code{"off"} to explicitly set the state of
## the minor grid.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, stairs, replot, xlabel, ylabel, title}
## @end deftypefn

## Author: jwe

## PKG_ADD: mark_as_command grid

function grid (x, y)

  persistent grid_on = false;
  persistent minor_on = false;

  nargs = nargin;

  if (nargs == 2)
    if (ishandle (x))
      ax = x;
      obj = get (x);
      x = y;
      nargs--;
      if (! strcmp (obj.type, "axes"))
	error ("grid: expecting first argument to be an axes object");
      endif
    else
      print_usage ();
    endif
  else
    ax = gca ();
  endif

  if (nargs == 0)
    grid_on = ! grid_on;
  elseif (nargs == 1)
    if (ischar (x))
      if (strcmp ("off", x))
	grid_on = false;
      elseif (strcmp ("on", x))
	grid_on = true;
      elseif (strcmp ("minor", x))
	minor_on = ! minor_on;
	if (minor_on)
	  grid_on = true;
	endif
      else
	print_usage ();
      endif
    else
      error ("grid: argument must be a string");
    endif
  else
    print_usage ();
  endif

  if (grid_on)
    set (ax, "xgrid", "on", "ygrid", "on", "zgrid", "on");
    if (minor_on)
      set (ax, "xminorgrid", "on", "yminorgrid", "on", "zminorgrid", "on");
    else
      set (ax, "xminorgrid", "off", "yminorgrid", "off", "zminorgrid", "off");
    endif
  else
    set (ax, "xgrid", "off", "ygrid", "off", "zgrid", "off");
    set (ax, "xminorgrid", "off", "yminorgrid", "off", "zminorgrid", "off");
  endif

endfunction
