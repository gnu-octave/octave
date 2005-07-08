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
## @deftypefn {Function File} {} __plt__ (@code{caller}, @dots{})
## @end deftypefn

## Author: jwe

function __plt__ (caller, varargin)

  __plot_globals__;

  nargs = nargin ();

  if (nargs > 1)

    k = 1;
    j = __plot_data_offset__(__current_figure__);

    x_set = false;
    y_set = false;

    ## Gather arguments, decode format, gather plot strings, and plot lines.

    while (--nargs > 0 || x_set)

      if (nargs == 0)
	## Force the last plot when input variables run out.
	next_arg = "";
      else
	next_arg = varargin{k++};
      endif

      have_data = false;

      if (isstr (next_arg))
	if (x_set)
	  fmt = __pltopt__ (caller, next_arg);
	  if (y_set)
	    [__plot_data__{__current_figure__}{j}, fmtstr] = __plt2__ (x, y, fmt);
	  else
	    [__plot_data__{__current_figure__}{j}, fmtstr] = __plt1__ (x, fmt);
	  endif
	  have_data = true;
	  x_set = false;
	  y_set = false;
	else
	  error ("plot: no data to plot");
	endif
      elseif (x_set)
	if (y_set)
	  fmt = __pltopt__ (caller, "");
	  [__plot_data__{__current_figure__}{j}, fmtstr] = __plt2__ (x, y, fmt);
	  have_data = true;
	  x = next_arg;
	  y_set = false;
	else
	  y = next_arg;
	  y_set = true;
	endif
      else
	x = next_arg;
	x_set = true;
      endif

      if (have_data)
	if (iscell (__plot_data__{__current_figure__}{j}))
	  for i = 1:length (__plot_data__{__current_figure__}{j})
	    __plot_command__{__current_figure__} \
		= sprintf ("%s%s __plot_data__{__current_figure__}{%d}{%d} %s",
			   __plot_command__{__current_figure__},
			   __plot_command_sep__, j, i, fmtstr{i});
	    __plot_command_sep__ = ",\\\n";
	  endfor
	else
	  __plot_command__{__current_figure__} \
	    = sprintf ("%s%s __plot_data__{__current_figure__}{%d} %s",
		       __plot_command__{__current_figure__},
		       __plot_command_sep__, j, fmtstr);
	  __plot_command_sep__ = ",\\\n";
	endif
	j++;
      endif

    endwhile

    __plot_data_offset__(__current_figure__) = j;

    if (! isempty (__plot_command__{__current_figure__}))
      eval (__plot_command__{__current_figure__});
    endif

  else
    msg = sprintf ("%s (y)\n", caller);
    msg = sprintf ("%s       %s (x, y, ...)\n", msg, caller);
    msg = sprintf ("%s       %s (x, y, fmt, ...)", msg, caller);
    usage (msg);
  endif

endfunction
