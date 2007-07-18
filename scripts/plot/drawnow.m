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

  persistent drawnow_executing = 0;

  unwind_protect

    ## If this is a recursive call, do nothing.
    if (++drawnow_executing > 1)
      return;
    endif

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
	endif
      endfor
    else
      print_usage ();
    endif

  unwind_protect_cleanup

    drawnow_executing--;
    __request_drawnow__ (false);

  end_unwind_protect

endfunction

function plot_stream = open_gnuplot_stream (h, term, file)

  ## If drawnow is cleared, it is possible to register __go_close_all__
  ## more than once, but that is not fatal.
  persistent __go_close_all_registered__;

  cmd = gnuplot_binary ();

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
    else

      ## Guess the terminal type.
      term = getenv ("GNUTERM");
      if (isempty (term) && ! isempty (getenv ("DISPLAY")))
        term = "x11";
      elseif (! isunix ())
        term = "windows";
      else
        term = "aqua";
        ## This should really be checking for os x before setting
        ## the terminal type to aqua, but nobody will notice because
        ## every other unix will be using x11 and windows will be
        ## using windows.  Those diehards still running octave from
        ## a linux console know how to set the GNUTERM variable.
      endif

      ## If no 'h' (why not?) then open the terminal as Figure 0.
      if isempty (h)
        h = 0;
      endif

      if (strcmp (term, "x11"))
        fprintf (plot_stream, "set terminal x11 title \"Figure %d\"\n", h);
      elseif (strcmp (term, "aqua"))
        ## Aqua doesn't understand the 'title' option despite what the
        ## gnuplot 4.2 documentation says.
        fprintf (plot_stream, "set terminal aqua %d\n", h);
      elseif (strcmp (term, "wxt"))
        fprintf (plot_stream, "set terminal wxt title \"Figure %d\"\n", h);
      endif
      ## gnuplot will pick up the GNUTERM environment variable itself
      ## so no need to set the terminal type if not also setting the
      ## figure title.

    endif

    if (isempty (__go_close_all_registered__))
      atexit ("__go_close_all__");
      __go_close_all_registered__ = true;
    endif

  endif

endfunction
