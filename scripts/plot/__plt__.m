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

function retval = __plt__ (caller, h, varargin)

  nargs = nargin - 2;

  if (nargs > 0)

    k = 1;

    x_set = false;
    y_set = false;
    property_set = false;
    properties = {};

    ## Gather arguments, decode format, gather plot strings, and plot lines.

    retval = [];

    while (nargs > 0 || x_set)

      if (nargs == 0)
	## Force the last plot when input variables run out.
	next_cell = {};
	next_arg = {""};
      else
	next_cell = varargin(k);
	next_arg = varargin{k++};
      endif

      nargs--;

      if (ischar (next_arg) || iscellstr (next_arg))
	if (x_set)
	  [options, valid] = __pltopt__ (caller, next_arg, false);
	  if (! valid)
	    if (nargs == 0)
	      error ("%s: properties must appear followed by a value", caller);
	    endif
	    properties = [properties, [next_cell, varargin(k++)]];
	    nargs--;
	    continue;
	  else
	    while (nargs > 0 && ischar (varargin{k}))
	      if (nargs < 2)
		error ("%s: properties must appear followed by a value",
		       caller);
	      endif
	      properties = [properties, varargin(k:k+1)];
	      k += 2;
	      nargs -= 2;
	    endwhile
	  endif
	  if (y_set)
	    tmp = __plt2__ (h, x, y, options, properties);
	    properties = {};
	    retval = [retval; tmp];
	  else
	    tmp = __plt1__ (h, x, options, properties);
	    properties = {};
	    retval = [retval; tmp];
	  endif
	  x_set = false;
	  y_set = false;
	else
	  error ("plot: no data to plot");
	endif
      elseif (x_set)
	if (y_set)
	  options = __pltopt__ (caller, {""});
	  tmp = __plt2__ (h, x, y, options, properties);
	  retval = [retval; tmp];
	  x = next_arg;
	  y_set = false;
	  properties = {};
	else
	  y = next_arg;
	  y_set = true;
	endif
      else
	x = next_arg;
	x_set = true;
      endif

    endwhile

  else
    msg = sprintf ("%s (y)\n", caller);
    msg = sprintf ("%s       %s (x, y, ...)\n", msg, caller);
    msg = sprintf ("%s       %s (x, y, fmt, ...)", msg, caller);
    msg = sprintf ("%s       %s (x, y, property, value, ...)", msg, caller);
    usage (msg);
  endif

endfunction
  
