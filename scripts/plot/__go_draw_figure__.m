## Copyright (C) 2005 John W. Eaton
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
## @deftypefn {Function File} {} __go_draw_figure__ (f)
## Display the figure @var{f}.
## @end deftypefn

## Author: jwe

function __go_draw_figure__ (f, plot_stream)

  if (nargin == 2)
    if (strcmp (f.type, "figure"))

      ## Set figure properties here?

      kids = f.children;
      nkids = length (kids);

      if (nkids > 0)
	axes_count = 0;
	for i = 1:nkids
	  obj = get (kids(i));
	  switch (obj.type)
	    case "axes"
	      axes_count++;
	  endswitch
	endfor

	fputs (plot_stream, "reset;\n");

	multiplot_mode = axes_count > 1;

	if (multiplot_mode)
	  fputs (plot_stream, "set multiplot;\n");
	endif

	for i = 1:nkids
	  obj = get (kids(i));
	  switch (obj.type)
	    case "axes"
	      __go_draw_axes__ (kids(i), plot_stream);

	    otherwise
	      error ("__go_draw_figure__: unknown object class, %s",
		     obj.type);
	  endswitch
	endfor

	if (multiplot_mode)
	  fputs (plot_stream, "unset multiplot;\n");
	endif
      else
	fputs (plot_stream, "reset; clear;\n");
	fflush (plot_stream);
      endif
    else
      error ("__go_draw_figure__: expecting figure object, found `%s'",
	     f.type);
    endif
  else
    print_usage ();
  endif    

endfunction

