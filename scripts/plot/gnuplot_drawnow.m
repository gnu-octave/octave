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

function gnuplot_drawnow (h, term, file, mono, debug_file)

  if (nargin < 4)
    mono = false;
  endif

  if (nargin >= 3 && nargin <= 5)
    ## Produce various output formats, or redirect gnuplot stream to a
    ## debug file.
    plot_stream = [];
    fid = [];
    unwind_protect
      plot_stream = open_gnuplot_stream (1, []);
      enhanced = gnuplot_set_term (plot_stream (1), true, h, term, file);
      __go_draw_figure__ (h, plot_stream, enhanced, mono);
      if (nargin == 5)
        fid = fopen (debug_file, "wb");
        enhanced = gnuplot_set_term (fid, true, h, term, file);
        __go_draw_figure__ (h, fid, enhanced, mono);
      endif
    unwind_protect_cleanup
      if (! isempty (plot_stream))
        pclose (plot_stream);
      endif
      if (! isempty (fid))
        fclose (fid);
      endif
    end_unwind_protect
  elseif (nargin == 1)
    ##  Graphics terminal for display.
    plot_stream = get (h, "__plot_stream__");
    tag = "gnuplot_drawnow";
    if (isempty (plot_stream))
      plot_stream = open_gnuplot_stream (2, h);
      new_stream = true;
      ha = axes ("tag", tag, "visible", "off");
      set (ha, "userdata", get (h, "position"));
    else
      new_stream = false;
      unwind_protect
	set (0, "showhiddenhandles", "on");
	ha = findobj (h, "type", "axes", "tag", tag);
        position = get (h, "position");
	if (! isempty (ha))
	  prior_position = get (ha, "userdata");
	  if (! all (position == prior_position))
	    new_stream = true;
	  else
	    ## FIXME -- Obtain the x11 window id from gnuplot and
	    ## determine the current position via xwininfo.  The
	    ## "position" property may then be updated to reflect
	    ## changes in window position/size made by the mouse.
	  endif
	else
          ha = axes ("tag", tag, "visible", "off");
	endif
        set (ha, "userdata", position);
      unwind_protect_cleanup
	set (0, "showhiddenhandles", "off");
      end_unwind_protect
    endif
    enhanced = gnuplot_set_term (plot_stream (1), new_stream, h);
    __go_draw_figure__ (h, plot_stream (1), enhanced, mono);
    fflush (plot_stream (1));
  else
    print_usage ();
  endif

endfunction

function plot_stream = open_gnuplot_stream (npipes, h)
  cmd = gnuplot_binary ();
  if (npipes > 1)
    [plot_stream(1), plot_stream(2), pid] = popen2 (cmd);
    if (pid < 0)
      error ("drawnow: failed to open connection to gnuplot");
    endif
  else
    plot_stream = popen (cmd, "w");
    if (plot_stream < 0)
      error ("drawnow: failed to open connection to gnuplot");
    endif
  endif
  if (! isempty (h))
    set (h, "__plot_stream__", plot_stream);
  endif
endfunction

function enhanced = gnuplot_set_term (plot_stream, new_stream, h, term, file)
  ## Generate the gnuplot "set terminal <term> ..." command.  Include
  ## the subset of properties "position", "units", "paperposition",
  ## "paperunits", "name", and "numbertitle".  When "term" originates
  ## from print.m, it may include gnuplot terminal options.
  if (nargin == 3)
    ## This supports the gnuplot backend.
    term = gnuplot_term ();
    opts_str = "";
  else
    ## Get the one word terminal id and save the remaining as options to
    ## be passed on to gnuplot.  The terminal may respect the backend.
    [term, opts_str] = gnuplot_trim_term (term);
  endif

  enhanced = gnuplot_is_enhanced_term (term);

  ## Set the terminal.
  if (! isempty (term))

    if (enhanced)
      enh_str = "enhanced";
    else
      enh_str = "";
    endif

    if (! isempty (h) && isfigure (h))

      ## Generate gnuoplot title string for backend plot windows.
      if (isbackend (term))
        fig.numbertitle = get (h, "numbertitle");
        fig.name = get (h, "name");
        if (strcmpi (get (h, "numbertitle"), "on"))
          title_str = sprintf ("Figure %d", h);
        else
          title_str = "";
        endif
        if (! isempty (fig.name) && ! isempty (title_str))
          title_str = sprintf ("%s: %s", title_str, fig.name);
        elseif (! isempty (fig.name) && isempty (title_str))
          title_str = fig.name;
        endif
        if (! isempty (title_str))
          title_str = sprintf ("title \"%s\"", title_str);
        endif
      else
        title_str = "";
      endif

      if (isempty (strfind (opts_str, " size ")))
        ## Convert position to units used by gnuplot.
        if (isbackend (term))
          ## Get figure size in pixels.
          [gnuplot_size, gnuplot_pos] = get_figsize (h);
        else
          ## Get size of the printed plot in inches.
          gnuplot_size = get_canvassize (h);
          ## If the output is a bitmap, convert to pixels.
          if (isbitmap (term) || strcmpi (term, "svg"))
            gnuplot_size = gnuplot_size * get (0, "screenpixelsperinch");
          endif
        endif
        if (all (gnuplot_size > 0))
          ## Set terminal size.
          terminals_with_size = {"emf", "gif", "jpeg", "latex", "pbm", ...
                                 "pdf", "png", "postscript", "svg", "wxt", ...
                                 "epslatex", "pstex", "pslatex"};
          if (__gnuplot_has_feature__("x11_figure_position"))
            terminals_with_size{end+1} = "x11";
          endif
          if (any (strncmpi (term, terminals_with_size, 3)))
            size_str = sprintf ("size %d,%d", gnuplot_size(1), gnuplot_size(2));
            if (strncmpi (term, "X11", 3))
              screen_size = get (0, "screensize")(3:4);
              if (all (screen_size > 0))
                ## For X11, set the figure positon as well as the size
                ## gnuplot position is UL, Octave's is LL (same for screen/window)
                gnuplot_pos(2) = screen_size(2) - gnuplot_pos(2) - gnuplot_size(2);
                gnuplot_pos = max (gnuplot_pos, 1);
                size_str = sprintf ("%s position %d,%d", size_str, 
                                    gnuplot_pos(1), gnuplot_pos(2));
              endif
            endif
          elseif (any (strncmpi (term, {"aqua", "fig"}, 3)))
            ## Aqua and Fig also have size, but the format is different.
            size_str = sprintf ("size %d %d", gnuplot_size(1), gnuplot_size(2));
          elseif (any (strncmpi (term, {"corel", "hpgl"}, 3)))
            ## The size for corel and hpgl are goes at the end (implicit).
            size_str = sprintf ("%d %d",gnuplot_size(1), gnuplot_size(2));
          elseif (any (strncmpi (term, {"dxf"}, 3)))
            ## DXF uses autocad units.
            size_str = "";
          else
            size_str = "";
          endif
        else
          size_str = "";
	  warning ("gnuplot_set_term: size is zero")
        endif
      else
        ## A specified size take priority over the figure properies.
        size_str = "";
      endif
    else
      if isempty (h)
        disp ("gnuplot_set_term: figure handle is empty")
      elseif !isfigure(h)
        disp ("gnuplot_set_term: not a figure handle")
      endif
      title_str = "";
      size_str = "";
    endif

    ## Set the gnuplot terminal (type, enhanced?, title, & size).
    if (! isempty (term))
      term_str = sprintf ("set terminal %s", term);
      if (any (strncmpi (term, {"x11", "wxt"}, 3)) && new_stream
	  && __gnuplot_has_feature__ ("x11_figure_position"))
        ## The "close" is added to allow the figure position property
        ## to remain active.
        term_str = sprintf ("%s close", term_str);
      endif
      if (! isempty (enh_str))
        term_str = sprintf ("%s %s", term_str, enh_str);
      endif
      if (! isempty (title_str))
        term_str = sprintf ("%s %s", term_str, title_str);
      endif
      if (! isempty (size_str) && new_stream)
        ## size_str goes last to permit specification of canvas size
        ## for terminals cdr/corel.
        term_str = sprintf ("%s %s", term_str, size_str);
      endif
      if (nargin > 2 && ischar (opts_str))
        ## Options must go last.
        term_str = sprintf ("%s %s", term_str, opts_str);
      endif
      fprintf (plot_stream, sprintf ("%s\n", term_str));
    else
      ## gnuplot will pick up the GNUTERM environment variable itself
      ## so no need to set the terminal type if not also setting the
      ## figure title, enhanced mode, or position.
    endif
  endif

  if (nargin == 4)
    if (! isempty (file))
      fprintf (plot_stream, "set output \"%s\";\n", file);
    endif
  endif
endfunction

function term = gnuplot_term ()
  term = getenv ("GNUTERM");
  ## If not specified, guess the terminal type.
  if (isempty (term))
    if (ismac ())
      term = "aqua";
    elseif (! isunix ())
      term = "windows";
    else
      term = "x11";
    endif
  endif
endfunction

function [term, opts] = gnuplot_trim_term (string)
  ## Extract the terminal type and terminal options (from print.m)
  string = deblank (string);
  n = strfind (string, ' ');
  if (isempty (n))
    term = string;
    opts = "";
  else
    term = string(1:(n-1));
    opts = string((n+1):end);
  endif
endfunction

function have_enhanced = gnuplot_is_enhanced_term (term)
  persistent enhanced_terminals;
  if (isempty (enhanced_terminals))
    ## Don't include pstex, pslatex or epslatex here as the TeX commands
    ## should not be interpreted in that case.
    enhanced_terminals = {"aqua", "dumb", "png", "jpeg", "gif", "pm", ...
                          "windows", "wxt", "svg", "postscript", "x11", "pdf"};
  endif
  if (nargin < 1)
    ## Determine the default gnuplot terminal.
    term = gnuplot_term ();
  endif
  have_enhanced = false;
  for n = 1 : length (enhanced_terminals)
    t = enhanced_terminals{n};
    if (strncmp (term, t, min (length (term), length (t))))
      have_enhanced = true;
      break;
    endif
  endfor
endfunction

function ret = isbackend (term)
  if (nargin == 0)
    term = gnuplot_term ();
  endif
  ret = any (strcmpi ({"aqua", "wxt", "x11", "windows", "pm"}, term));
endfunction

function ret = isbitmap (term)
  if (nargin == 0)
    term = gnuplot_term ();
  endif
  ret = any (strcmpi ({"png", "jpeg", "gif", "pbm"}, term));
endfunction

function [fig_size, fig_pos] = get_figsize (h)
  ## Determine the size of the figure in pixels.
  position = get (h, "position");
  units = get (h, "units");
  t.inches      = 1;
  t.centimeters = 2.54;
  t.pixels      = get (0, "screenpixelsperinch");
  ## gnuplot treats pixels/points for the screen the same (?).
  t.points      = t.pixels;
  screensize    = get (0, "screensize")(3:4);
  t.normalized  = screensize / t.pixels;
  fig_size = position(3:4) * (t.pixels / t.(units));
  fig_pos  = position(1:2) * (t.pixels / t.(units));
  fig_pos(1) = max (min (fig_pos(1), screensize(1)), 10);
  fig_pos(2) = max (min (fig_pos(2), screensize(2)), 10);
  fig_size(1) = max (min (fig_size(1), screensize(1)), 10-fig_pos(1));
  fig_size(2) = max (min (fig_size(2), screensize(2)), 10-fig_pos(2));
endfunction

function plotsize = get_canvassize (h)
  ## Returns the intended size of the plot on the page in inches. 
  ## "canvas size" is a gnuplot term. Gnuplot doesn't explicity plot to
  ## an area/position on a page. Instead it plots to a "canvas" of a 
  ## explicit or implicit size.
  t.points      = get (0, "screenpixelsperinch");
  t.centimeters = 2.54;
  t.inches      = 1.00;
  papersize = get_papersize (h);
  paperunits = get (h, "paperunits");
  paperposition = get (h, "paperposition") / t.(paperunits);
  if (strcmpi (paperunits, "normalized"))
    plotsize = papersize .* paperposition(3:4);
  else
    plotsize = paperposition(3:4);
  endif
endfunction

function papersize = get_papersize (h)
  ## Returns the papersize in inches
  persistent papertypes papersizes
  if (isempty (papertypes))
    papertypes = {"usletter", "uslegal", ...
                 "a0", "a1", "a2", "a3", "a4", "a5", ...
                 "b0", "b1", "b2", "b3", "b4", "b5", ...
                 "arch-a", "arch-b", "arch-c", "arch-d", "arch-e", ...
                 "a", "b", "c", "d", "e", ...
                 "tabloid", "<custom>"};
    papersizes = [ 8.500, 11.000;
                   8.500, 14.000;
                  33.135, 46.847;
                  23.404, 33.135;
                  16.548, 23.404;
                  11.694, 16.528;
                   8.268, 11.693;
                   5.847,  8.264;
                  40.543, 57.366;
                  28.683, 40.503;
                  20.252, 28.683;
                  14.342, 20.252;
                  10.126, 14.342;
                   7.171, 10.126;
                   9.000, 12.000;
                  12.000, 18.000;
                  18.000, 24.000;
                  24.000, 36.000;
                  36.000, 48.000;
                   8.500, 11.000;
                  11.000, 17.000;
                  17.000, 22.000;
                  22.000, 34.000;
                  34.000, 44.000;
                  11.000, 17.000;
                   8.500, 11.000];
    ## <custom> has a page size since we're not doing any checking here.
    papersizes = round (1000 * papersizes);
  endif

  paperunits = get (h, "paperunits");
  if (strcmpi (paperunits, "normalized"))
    papertype = get (h, "papertype");
    n = find (strcmpi (papertypes, papertype));
    papersize = 0.001 * papersizes(n, :);
    paperunits = "inches";
  else
    t.points      = 72;
    t.centimeters = 2.54;
    t.inches      = 1.00;
    ## FIXME -- this papersize/type administration should be done at a
    ## lower level.
    if (strcmpi (get (h, "papertype"), "<custom>"))
      ## If the type is custom but the size is a standard, then set the
      ## standard type.
      papersize = get (h, "papersize");
      papersize = papersize * t.(paperunits);
      n = find (all ((ones ([size(papersizes, 1), 1])
		      * round (1000*papersize) - papersizes) == 0, 2));
      if (! isempty (n))
        set (h, "papertype", papertypes{n});
      endif
    else
      papertype = get (h, "papertype");
      n = find (strcmpi (papertypes, papertype));
      papersize = papersizes(n,:) * 0.001;
      set (h, "papersize", papersize * t.(paperunits));
    endif
  endif
endfunction
