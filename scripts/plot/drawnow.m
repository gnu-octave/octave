## Copyright (C) 2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} drawnow ()
## Update and display the current graphics.
##
## Octave automatically calls drawnow just before printing a prompt,
## when @code{sleep} or @code{pause} is called, or while waiting for
## command-line input.
## @end deftypefn

## Author: jwe

function drawnow (term, file, mono, debug_file)

  persistent drawnow_executing = 0;

  unwind_protect

    ## If this is a recursive call, do nothing.
    if (++drawnow_executing > 1)
      return;
    endif

    if (nargin < 3)
      mono = false;
    endif

    if (nargin >= 2 && nargin <= 4)
      [dnm, fnm, ext] = fileparts (file);
      if (! (isempty (dnm) || isdir (dnm)))
	error ("drawnow: nonexistent directory `%s'", dnm);
      endif
      h = get (0, "currentfigure");
      if (h)
	f = get (h);
	plot_stream = [];
	fid = [];
	unwind_protect
	  [plot_stream, enhanced] = open_gnuplot_stream ([], term, file);
	  __go_draw_figure__ (f, plot_stream, enhanced, mono);
	  if (nargin == 4)
	    fid = fopen (debug_file, "wb");
	    enhanced = init_plot_stream (fid, [], term, file);
	    __go_draw_figure__ (f, fid, enhanced, mono);
	  endif
	unwind_protect_cleanup
	  if (! isempty (plot_stream))
	    pclose (plot_stream);
	  endif
	  if (! isempty (fid))
	    fclose (fid);
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
		[plot_stream, enhanced] = open_gnuplot_stream (h);
		set (h, "__enhanced__", enhanced);
	      else
		enhanced = f.__enhanced__;
	      endif
	      __go_draw_figure__ (f, plot_stream, enhanced, mono);
	    elseif (! isempty (plot_stream))
	      pclose (plot_stream);
	      set (h, "__plot_stream__", []);
	      set (h, "__enhanced__", false);
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

function [plot_stream, enhanced] = open_gnuplot_stream (h, varargin)

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

    enhanced = init_plot_stream (plot_stream, h, varargin{:});

    if (isempty (__go_close_all_registered__))
      atexit ("__go_close_all__");
      __go_close_all_registered__ = true;
    endif

  endif

endfunction

function enhanced = init_plot_stream (plot_stream, h, term, file)

  if (nargin == 4)
    enhanced = enhanced_term (term);
    if (! isempty (term))
      if (enhanced)
	fprintf (plot_stream, "set terminal %s enhanced;\n", term);
      else
	fprintf (plot_stream, "set terminal %s;\n", term);
      endif
    endif
    if (! isempty (file))
      fprintf (plot_stream, "set output \"%s\";\n", file);
    endif
  else

    ## Guess the terminal type.
    term = getenv ("GNUTERM");
    if (isempty (term))
      if (! isempty (getenv ("DISPLAY")))
	term = "x11";
      elseif (! isunix ())
	term = "windows";
      else
	## This should really be checking for os x before setting
	## the terminal type to aqua, but nobody will notice because
	## every other unix will be using x11 and windows will be
	## using windows.  Those diehards still running octave from
	## a linux console know how to set the GNUTERM variable.
	term = "aqua";
      endif
    endif

    enhanced = enhanced_term (term);
    if (enhanced)
      enh_str = "enhanced";
    else
      enh_str = "";
    endif

    ## If no 'h' (why not?) then open the terminal as Figure 0.
    if (isempty (h))
      h = 0;
    endif

    if (strcmp (term, "x11"))
      fprintf (plot_stream, "set terminal x11 %s title \"Figure %d\"\n",
	       enh_str, h);
    elseif (strcmp (term, "aqua"))
      ## Aqua doesn't understand the 'title' option despite what the
      ## gnuplot 4.2 documentation says.
      fprintf (plot_stream, "set terminal aqua %d %s\n", h, enh_str);
    elseif (strcmp (term, "wxt"))
      fprintf (plot_stream, "set terminal wxt %s title \"Figure %d\"\n", 
	       enh_str, h);

    elseif (enhanced)
      fprintf (plot_stream, "set terminal %s %s\n", term, enh_str);
    endif
    ## gnuplot will pick up the GNUTERM environment variable itself
    ## so no need to set the terminal type if not also setting the
    ## figure title or enhanced mode.

  endif

endfunction

function have_enhanced = enhanced_term (term)
  persistent enhanced_terminals;

  if (isempty (enhanced_terminals))
    ## Don't include pstex, pslatex or epslatex here as the TeX commands
    ## should not be interpreted in that case.
    if (compare_versions (__gnuplot_version__ (), "4.0", ">"))
      enhanced_terminals = {"aqua", "dumb", "png", "jpeg", "gif", "pm", ...
	                    "windows", "wxt", "svg", "postscript", "x11"};
    else 
      enhanced_terminals = {"x11", "postscript"};
    endif
  endif

  term = tolower (term);

  have_enhanced = false;
  for i = 1 : length (enhanced_terminals)
    t = enhanced_terminals{i};
    if (strncmp (term, t, min (length (term), length (t))))
      have_enhanced = true;
      break;
    endif
  endfor
endfunction
