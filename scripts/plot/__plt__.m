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

  cf = __current_figure__;
  mxi = __multiplot_xi__;
  myi = __multiplot_yi__;

  __setup_plot__ ("__gnuplot_plot__");

  nargs = nargin ();

  if (nargs > 1)

    k = 1;
    j = __plot_data_offset__{cf}(mxi,myi);
    loff = __plot_line_offset__{cf}(mxi,myi);
    loff1 = loff;

    x_set = false;
    y_set = false;

    ## Gather arguments, decode format, gather plot strings, and plot lines.

    while (--nargs > 0 || x_set)

      if (nargs == 0)
	## Force the last plot when input variables run out.
	next_arg = {""};
      else
	next_arg = varargin{k++};
      endif

      have_data = false;

      if (ischar (next_arg) || iscellstr (next_arg))
	if (x_set)
	  [fmt, keystr] = __pltopt__ (caller, next_arg);
	  if (y_set)
	    [tdata, tfmtstr, key] = __plt2__ (x, y, fmt, keystr);
	  else
	    [tdata, tfmtstr, key] = __plt1__ (x, fmt, keystr);
	  endif
	  if (! isempty (tdata))
	    __plot_data__{cf}{mxi,myi}{j} = tdata;
	    for i = 1:length (key)
	      __plot_key_labels__{cf}{mxi,myi}{loff1++} = key{i};
	    endfor
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
	  [fmt, keystr] = __pltopt__ (caller, {""});
	  [tdata, tfmtstr, key] = __plt2__ (x, y, fmt, keystr);
	  if (! isempty (tdata))
	    __plot_data__{cf}{mxi,myi}{j} = tdata;
	    for i = 1:length (key)
	      __plot_key_labels__{cf}{mxi,myi}{loff1++} = key{i};
	    endfor
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
	for i = 1:length (__plot_data__{cf}{mxi,myi}{j})
	  usingstr = __make_using_clause__ (__plot_data__{cf}{mxi,myi}{j}{i});
	  __plot_command__{cf}{mxi,myi} ...
	      = sprintf ("%s%s __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{%d}{%d} %s %s %s __plot_key_labels__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{%d}",
			 __plot_command__{cf}{mxi,myi},
			 __plot_command_sep__, j, i, usingstr,
			 fmtstr{i}, gnuplot_command_title, loff++);
	  __plot_command_sep__ = ",\\\n";
	endfor
	j++;
      endif

    endwhile

    __plot_data_offset__{cf}(mxi,myi) = j;
    __plot_line_offset__{cf}(mxi,myi) = loff;

    if (__multiplot_mode__)
      __gnuplot_raw__ ("clear\n");
    endif

    if (! strcmp (__plot_command__{cf}{mxi,myi}, "__gnuplot_plot__"))
      __do_legend__ ();
      eval (__plot_command__{cf}{mxi,myi});
    endif

  else
    msg = sprintf ("%s (y)\n", caller);
    msg = sprintf ("%s       %s (x, y, ...)\n", msg, caller);
    msg = sprintf ("%s       %s (x, y, fmt, ...)", msg, caller);
    usage (msg);
  endif

endfunction
