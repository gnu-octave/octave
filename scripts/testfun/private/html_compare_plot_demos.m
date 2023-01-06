########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} html_compare (@var{toolkits})
## @deftypefnx {} {} html_compare (@var{toolkits}, @var{name}, @var{value}, @dots{})
##
## Produce an html document to compare the plot demos produced by
## @var{toolkits}.
##
## Valid property names, and their defaults, are:
##
## @table @samp
##   @item fmt
##   @code{"png"}
##
##   @item output
##   @code{"compare_plot_demos.html"}
##
##   @item template
##   @code{"html_plot_demos_template.html"}
##
##   @item column_width
##   @code{600}
## @end table
##
## The template parameter refers to a specially formatted html file
## which accompanies this m-file script.
##
## Additional toolkit description can be added to the column header
## with a parameter named equal to the toolkit.  For example:
##
## @smallexample
## @code{html_compare_plot_demos (@{"gnuplot", "fltk"@}, "gnuplot", " 4.6 patchlevel 5")}
## @end smallexample
##
## @seealso{compare_plot_demos, dump_demos, demo}
## @end deftypefn

function html_compare_plot_demos (toolkits, varargin)

  ## Set defaults
  in.fmt = "png";
  in.figfiles = {};
  in.output_fmt = @(p) sprintf ("compare_plot_demos_%02i.html", p);
  in.template = "html_plot_demos_template.html";
  in.column_width = 600;
  in.plots_per_page = 50;

  ## Parse inputs
  for n = 1:2:numel (varargin)
    in.(lower(varargin{n})) = varargin{n+1};
  endfor

  ## Compile a list of all files for all toolkits
  for t = 1:numel (toolkits)
    filter = sprintf ("%s/*.%s", toolkits{t}, in.fmt);
    in.figfiles = union (in.figfiles, {dir(filter).name});
  endfor

  fid = fopen (which (in.template), "r");
  template = char (fread (fid)) .';
  fclose (fid);

  anchor = "<!-- ##ADD TABLE HERE## -->";
  n = strfind (template, anchor);
  header = strtrim (template(1:n-1));
  trailer = strtrim (template(n+numel (anchor):end));

  page = 1;
  do
    start_fig = (page - 1) * in.plots_per_page + 1;
    stop_fig = page * in.plots_per_page;
    last_page = stop_fig >= numel (in.figfiles);
    if (last_page)
      stop_fig = numel (in.figfiles);
    endif

    fid = fopen (in.output_fmt (page), "w");
    unwind_protect
      fprintf (fid, "%s\n", header);
      fprintf (fid, "<h2>Generated on %s by %s with GNU Octave %s</h2>\n", ...
               datestr (now (), 0), mfilename, version);

      ## Create page previous/next
      if (page > 1)
        prev_page_link = sprintf (['<p><a href="%s">%s</a></p>' "\n"], ...
                                  in.output_fmt (page - 1), "previous page");
      else
        prev_page_link = "";
      endif

      if (! last_page)
        next_page_link = sprintf (['<p><a href="%s">%s</a></p>' "\n"], ...
                                  in.output_fmt (page + 1), "next page");
      else
        next_page_link = "";
      endif

      fprintf (fid, "%s%s", prev_page_link, next_page_link);

      ## Create table header
      fprintf (fid, "<table>\n<tr>\n");
      for t = 1:numel (toolkits)
        ## set default
        column_header = upper (toolkits{t});
        if (isfield (in, toolkits{t}))
          column_header = [column_header, in.(toolkits{t})];
        endif
        fprintf (fid, ['<th>%s <a href="%s/diary.log">diary</a></th>' "\n"],
                 column_header, toolkits{t});
      endfor
      fprintf (fid, "</tr>\n");
      for m = start_fig:stop_fig
        [~, file] = fileparts (in.figfiles{m});
        fn = [file "." in.fmt];
        fprintf (fid, ['<tr id="%s">' "\n"], file);
        for k = toolkits
          ffn = fullfile (k{:}, fn);
          fprintf (fid, "<td><span>%s</span><br>", ffn);
          if (exist (ffn, "file"))
            fprintf (fid, ...
                     '<img alt="%s" src="%s" style="width:%dpx">', ...
                     file, ffn, in.column_width);
          else
            err_fn = strrep (ffn, ".png", ".err");
            if (! exist (err_fn, "file"))
              warning ("File %s doesn't exist...", err_fn);
            else
              err_fid = fopen (err_fn);
              msg = char (fread (err_fid))';
              fclose (err_fid);
              fprintf (fid, "<span>%s</span>", strrep (msg, "\n", "<br>"));
            endif
          endif
          fprintf (fid, "</td>\n");
        endfor
        fprintf (fid, "</tr>\n");
      endfor

      fprintf (fid, "</table>\n");
      fprintf (fid, "%s%s", prev_page_link, next_page_link);

      fprintf (fid, "%s\n", trailer);
      page++;
    unwind_protect_cleanup
      fclose (fid);
    end_unwind_protect
  until (stop_fig == numel (in.figfiles))

endfunction
