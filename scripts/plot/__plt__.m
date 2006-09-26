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

  __setup_plot__ ("__gnuplot_plot__");

  nargs = nargin ();

  if (nargs > 1)

    k = 1;
    j = __plot_data_offset__{__current_figure__}(__multiplot_xi__,__multiplot_yi__);

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

      if (ischar (next_arg))
	if (x_set)
	  fmt = __pltopt__ (caller, next_arg);
	  if (y_set)
	    [tdata, tfmtstr] = __plt2__ (x, y, fmt);
	  else
	    [tdata, tfmtstr] = __plt1__ (x, fmt);
	  endif
	  if (! isempty (tdata))
	    __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j} = tdata;
	    fmtstr = tfmtstr;
	    have_data = true;
	  endif
	  x_set = false;
	  y_set = false;
	else
	  error ("plot: no data to plot");
	endif
      elseif (x_set)
	if (y_set)
	  fmt = __pltopt__ (caller, "");
	  [tdata, tfmtstr] = __plt2__ (x, y, fmt);
	  if (! isempty (tdata))
	    __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j} = tdata;
	    fmtstr = tfmtstr;
	    have_data = true;
	  endif
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
	if (iscell (__plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j}))
	  for i = 1:length (__plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j})
	    usingstr = __make_using_clause__ (__plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j}{i});
	    __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__} \
		= sprintf ("%s%s __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{%d}{%d} %s %s",
			   __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__},
			   __plot_command_sep__, j, i, usingstr, fmtstr{i});
	    __plot_command_sep__ = ",\\\n";
	  endfor
	else
	  usingstr = __make_using_clause__ (__plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j});
	  __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__} \
	    = sprintf ("%s%s __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{%d} %s %s",
		       __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__},
		       __plot_command_sep__, j, usingstr, fmtstr);
	  __plot_command_sep__ = ",\\\n";
	endif
	j++;
      endif

    endwhile

    __plot_data_offset__{__current_figure__}(__multiplot_xi__,__multiplot_yi__) = j;

    if (__multiplot_mode__)
      __gnuplot_raw__ ("clear\n");
    endif
    if (! strcmp (__plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}, "__gnuplot_plot__"))
      eval (__plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__});
    endif

  else
    msg = sprintf ("%s (y)\n", caller);
    msg = sprintf ("%s       %s (x, y, ...)\n", msg, caller);
    msg = sprintf ("%s       %s (x, y, fmt, ...)", msg, caller);
    usage (msg);
  endif

endfunction
