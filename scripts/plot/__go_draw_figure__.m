## Copyright (C) 2005, 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} __go_draw_figure__ (@var{h}, @var{plot_stream}, @var{enhanced}, @var{mono})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function __go_draw_figure__ (h, plot_stream, enhanced, mono)

  if (nargin == 4)
    htype = get (h, "type");
    if (strcmp (htype, "figure"))

      ## Set figure properties here?

      ## Get complete list of children.
      kids = allchild (h);
      nkids = length (kids);

      if (nkids > 0)
	axes_count = 0;
	for i = 1:nkids
	  obj = __get__ (kids(i));
	  switch (obj.type)
	    case "axes"
	      axes_count++;
	  endswitch
	endfor

	fputs (plot_stream, "\nreset;\n");
	fputs (plot_stream, "set autoscale fix;\n");
	fputs (plot_stream, "set multiplot;\n");
	fputs (plot_stream, "set origin 0, 0\n");
	fputs (plot_stream, "set size 1, 1\n");

	for i = 1:nkids
	  obj = get (kids(i));
	  switch (obj.type)
	    case "axes"
	      __go_draw_axes__ (kids (i), plot_stream, enhanced, mono);
	    otherwise
	      error ("__go_draw_figure__: unknown object class, %s",
		     obj.type);
	  endswitch
	endfor

	fputs (plot_stream, "unset multiplot;\n");
      else
	fputs (plot_stream, "\nreset; clear;\n");
	fflush (plot_stream);
      endif
    else
      error ("__go_draw_figure__: expecting figure object, found `%s'",
	     htype);
    endif
  else
    print_usage ();
  endif    

endfunction

