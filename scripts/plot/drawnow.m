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
## @deftypefn {Function File} {} drawnow ()
## Display the current graphics.
## @end deftypefn

## Author: jwe

function drawnow (term, file)

  ## Use this instead of calling gcf to avoid creating a figure.

  h = get (0, "currentfigure");

  if (h)

    f = get (h);

    plot_stream = f.__plot_stream__;

    if (isempty (plot_stream))
      cmd = gnuplot_binary ();
      if (gnuplot_use_title_option ())
        cmd = sprintf ("%s -title \"Figure\" %s", cmd, h);
      endif
      plot_stream = popen (cmd, "w");
      if (plot_stream < 0)
	error ("drawnow: failed to open connection to gnuplot");
      else
	set (h, "__plot_stream__", plot_stream);
	if (isempty (getenv ("DISPLAY")))
	  fprintf (plot_stream, "set terminal dumb\n;");
	endif
      endif
    endif

    if (nargin == 2)
      fprintf (plot_stream,
	       "set terminal push; set terminal %s; set output '%s'\n",
	       term, file);
    endif

    if (nargin == 2 || strcmp (f.visible, "on"))
      __uiobject_draw_figure__ (f, plot_stream);
    endif

    __request_drawnow__ (false);

    if (nargin == 2)
      fputs (plot_stream, "set terminal pop; set output;\n");
    endif

  else

    __request_drawnow__ (false);

  endif


endfunction
