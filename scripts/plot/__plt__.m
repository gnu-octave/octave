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

## -*- texinfo -*-
## @deftypefn {Function File} {} __plt__ (@code{caller}, @dots{})
## @end deftypefn

## Author: jwe

function __plt__ (caller, varargin)

  nargs = nargin ();

  if (nargs > 1)

    k = 1;
    j = 1;

    x_set = false;
    y_set = false;
    have_gp_cmd = false;

    gp_cmd = "__gplot__";
    sep = "";

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
	    [data{j}, fmtstr] = __plt2__ (x, y, fmt);
	  else
	    [data{j}, fmtstr] = __plt1__ (x, fmt);
	  endif
	  have_data = true;
	  x_set = false;
	  y_set = false;
	else
	  error ("plot: no data to plot");
	endif
      elseif (x_set)
	if (y_set)
	  [data{j}, fmtstr] = __plt2__ (x, y, "");
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
	if (iscell (data{j}))
	  for i = 1:length (data{j})
	    gp_cmd = sprintf ("%s%s data{%d}{%d} %s", gp_cmd, sep,
			      j, i, fmtstr{i});
	    sep = ",\\\n";
	  endfor
	else
	  gp_cmd = sprintf ("%s%s data{%d} %s", gp_cmd, sep, j, fmtstr);
	  sep = ",\\\n";
	endif
	j++;
	have_gp_cmd = true;
      endif

    endwhile

    if (have_gp_cmd)
      eval (gp_cmd);
    endif

  else
    msg = sprintf ("%s (y)\n", caller);
    msg = sprintf ("%s       %s (x, y, ...)\n", msg, caller);
    msg = sprintf ("%s       %s (x, y, fmt, ...)", msg, caller);
    usage (msg);
  endif

endfunction
