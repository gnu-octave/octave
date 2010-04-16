## Copyright (C) 2005, 2007, 2008, 2009 John W. Eaton
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

function __go_draw_figure__ (h, plot_stream, enhanced, mono, output_to_paper, implicit_margin)

  if (nargin < 5)
    output_to_paper = false;
  elseif (nargin < 6)
    ## Gnuplot has implicit margins for some output. For example, for postscript
    ## the margin is 50pts. If not specified asssume 0.
    implicit_margin = 0;
  endif

  if (nargin >= 4 && nargin <= 6)
    htype = get (h, "type");
    if (strcmp (htype, "figure"))
      ## When printing, set paperunits to inches and rely on a listener to convert
      ## the values for papersize and paperposition.
      if (output_to_paper)
        orig_paper_units = get (h, "paperunits");
        gpval_term = __gnuplot_get_var__ (h, "GPVAL_TERM");
        gpval_termoptions = __gnuplot_get_var__ (h, "GPVAL_TERMOPTIONS");
        unwind_protect
          set (h, "paperunits", "inches");
          paper_size = get (h, "papersize");
          paper_position = get (h, "paperposition");
          paper_position = paper_position ./ paper_size([1, 2, 1, 2]);
          implicit_margin = implicit_margin ./ paper_size;
        unwind_protect_cleanup
          set (h, "paperunits", orig_paper_units);
        end_unwind_protect
        if (strcmp (gpval_term, "postscript")
            && ! isempty (strfind (gpval_termoptions, "landscape")))
          ## This needed to obtain the expected result.
          implicit_margin(2) = -implicit_margin(2);
        endif
      else
        implicit_margin = implicit_margin * [1 1];
      endif

      ## Get complete list of children.
      kids = allchild (h);
      nkids = length (kids);

      if (nkids > 0)
        fputs (plot_stream, "\nreset;\n");
        fputs (plot_stream, "set autoscale keepfix;\n");
        fputs (plot_stream, "set origin 0, 0\n");
        fputs (plot_stream, "set size 1, 1\n");
        bg = get (h, "color");
        if (isnumeric (bg))
          fprintf (plot_stream, "set obj 1 rectangle from screen 0,0 to screen 1,1 behind fc rgb \"#%02x%02x%02x\"\n", 255 * bg);
          bg_is_set = true;
        else
          bg_is_set = false;
        endif
        for i = nkids:-1:1
          if (strcmp (get (kids(i), "visible"), "on"))
            type = get (kids(i), "type");
            switch (type)
              case "axes"
                ## Rely upon listener to convert axes position to "normalized" units.
                orig_axes_units = get (kids(i), "units");
                orig_axes_position = get (kids(i), "position");
                unwind_protect
                  set (kids(i), "units", "normalized");
                  fg = get (kids(i), "color");
                  if (isnumeric (fg))
                    fprintf (plot_stream, "set obj 2 rectangle from graph 0,0 to graph 1,1 behind fc rgb \"#%02x%02x%02x\"\n", 255 * fg);
                    fg_is_set = true;
                  else
                    fg_is_set = false;
                  endif
                  if (output_to_paper)
                    axes_position_on_page = orig_axes_position .* paper_position([3, 4, 3 ,4]);
                    axes_position_on_page(1:2) = axes_position_on_page(1:2) +  paper_position(1:2);
                    set (kids(i), "position", axes_position_on_page);
                    __go_draw_axes__ (kids(i), plot_stream, enhanced, mono, implicit_margin, bg_is_set);
                  else
                    ## Return axes "units" and "position" back to their original values.
                    __go_draw_axes__ (kids(i), plot_stream, enhanced, mono, implicit_margin, bg_is_set);
                  endif
                  unwind_protect_cleanup
                  set (kids(i), "units", orig_axes_units);
                  set (kids(i), "position", orig_axes_position);
                  bg_is_set = false;
                  if (fg_is_set)
                    fputs (plot_stream, "unset obj 2\n");
                  endif
                end_unwind_protect
              otherwise
                error ("__go_draw_figure__: unknown object class, %s", type);
            endswitch
          endif
        endfor
        fputs (plot_stream, "\nunset multiplot;\n");
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

