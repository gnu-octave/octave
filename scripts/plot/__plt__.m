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

function plot_int (caller, ...)

  if (nargin == 2)

    plot_int_1 (va_arg (), "");

  elseif (nargin > 2)

    first_plot = 1;
    hold_state = ishold;

    unwind_protect

      x = va_arg ();
      nargin = nargin - 2;
      x_set = 1;
      y_set = 0;

      ## Gather arguments, decode format, and plot lines.

      while (nargin-- > 0)

	fmt = "";
	new = va_arg ();

	if (isstr (new))
	  if (! x_set)
	    error ("plot: no data to plot");
	  endif
	  fmt = plot_opt (caller, new);
	  if (! y_set)
	    plot_int_1 (x, fmt);
	  else
	    plot_int_2 (x, y, fmt);
	  endif
	  hold on;
	  x_set = 0;
	  y_set = 0;
	elseif (x_set)
	  if (y_set)
	    plot_int_2 (x, y, fmt);
	    hold on;
	    x = new;
	    y_set = 0;
	  else
	    y = new;
	    y_set = 1;          
	  endif
	else
	  x = new;
	  x_set = 1;
	endif

      endwhile

      ## Handle last plot.

      if  (x_set)
	if (y_set)
	  plot_int_2 (x, y, fmt);
	else
	  plot_int_1 (x, fmt);
	endif
      endif

    unwind_protect_cleanup

      if (! hold_state)
        hold off;
      endif

    end_unwind_protect

  else

    msg = sprintf ("%s (x)\n", caller);
    msg = sprintf ("%s       %s (x, y)\n", msg, caller);
    msg = sprintf ("%s       %s (x2, y1, x2, y2)\n", msg, caller);
    msg = sprintf ("%s       %s (x, y, fmt)", msg, caller);
    usage (msg);

  endif

endfunction
