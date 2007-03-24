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

  if (nargin == 2)
    h = get (0, "currentfigure");
    if (h)
      f = get (h);
      plot_stream = [];
      unwind_protect
	plot_stream = open_gnuplot_stream ([], term, file);
	__go_draw_figure__ (f, plot_stream);
      unwind_protect_cleanup
	if (! isempty (plot_stream))
	  pclose (plot_stream);
	endif
      end_unwind_protect
    else
      error ("drawnow: nothing to draw");
    endif
  elseif (nargin == 0)
    for h = __go_figure_handles__ ()
      if (! (isnan (h) || h == 0))
	f = get (h);
	if (f.__modified__)
	  plot_stream = f.__plot_stream__;
	  figure_is_visible = strcmp (f.visible, "on");
	  if (figure_is_visible)
	    if (isempty (plot_stream))
	      plot_stream = open_gnuplot_stream (h);
	    endif
	    __go_draw_figure__ (f, plot_stream);
	  elseif (! isempty (plot_stream))
	    pclose (plot_stream);
	    set (h, "__plot_stream__", []);
	  endif
	  set (h, "__modified__", false);
	endif
	__request_drawnow__ (false);
      endif
    endfor
  else
    print_usage ();
  endif

  __request_drawnow__ (false);

endfunction

function plot_stream = open_gnuplot_stream (h, term, file)

  ## If drawnow is cleared, it is possible to register __go_close_all__
  ## more than once, but that is not fatal.
  persistent __go_close_all_registered__;

  cmd = gnuplot_binary ();

  if (! isempty (h) && gnuplot_use_title_option ())
    cmd = sprintf ("%s -title \"Figure %d\"", cmd, h);
  endif

  plot_stream = popen (cmd, "w");

  if (plot_stream < 0)
    error ("drawnow: failed to open connection to gnuplot");
  else

    if (! isempty (h))
      set (h, "__plot_stream__", plot_stream);
    endif

    if (nargin == 3)
      fprintf (plot_stream, "set terminal %s\n;", term);
      fprintf (plot_stream, "set output \"%s\"\n;", file);
    elseif (isunix () && isempty (getenv ("DISPLAY")))
      fprintf (plot_stream, "set terminal dumb\n;");
    elseif (! isempty (h) && strcmp (getenv ("GNUTERM"), "wxt"))
      fprintf (plot_stream, "set terminal wxt title \"Figure %d\";\n", h);
    endif

    if (isempty (__go_close_all_registered__))
      atexit ("__go_close_all__");
      __go_close_all_registered__ = true;
    endif

  endif

endfunction
