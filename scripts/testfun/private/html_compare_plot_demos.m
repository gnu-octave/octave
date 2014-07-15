## Copyright (C) 2010 Ben Abbott
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
## @deftypefn  {Function File} {} html_compare (@var{toolkits})
## @deftypefnx {Function File} {} html_compare (@var{toolkits}, @var{name}, @var{value}, @dots{})
##
## Produces an html document to compare the plot demos produced by
## @var{toolkits}.
##
## Valid property names, and their defaults, are:
##
## @table @samp
##   @item fmt
##   @code{"png"}
##   @item output
##   @code{"compare_plot_demos.html"}
##   @item template
##   @code{"html_plot_demos_template.html"}
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
## @example
## @group
##   @code{html_compare_plot_demos ({"gnuplot", "fltk"}, "gnuplot", " 4.6 patchlevel 5")}
## @end group
## @end example
##
## @seealso{compare_plot_demos, dump_demos, demo}
## @end deftypefn

## Author: Ben Abbott  <bpabbott@mac.com>

function html_compare_plot_demos (toolkits, varargin)

  ## Set defaults
  in.fmt = "png";
  in.figfiles = {};
  in.output= "compare_plot_demos.html";
  in.template = "html_plot_demos_template.html";
  in.column_width = 600;

  ## Parse inputs
  for n = 1:2:numel(varargin)
    in.(lower(varargin{n})) = varargin{n+1};
  endfor

  ## Compile a list of all files for all toolkits
  for t = 1:numel(toolkits)
    filter = sprintf ("%s/*.%s", toolkits{t}, in.fmt);
    in.figfiles = union (in.figfiles, {dir(filter).name});
  endfor

  fid = fopen (which (in.template), "r");
  template = char (fread (fid)) .';
  fclose (fid);

  anchor = "<!-- ##ADD TABLE HERE## -->";
  n = findstr (template, anchor);
  header = template(1:n-1);
  trailer = template(n+numel(anchor):end);

  fid = fopen (in.output, "w");
  unwind_protect
    fputs (fid, header);
    fprintf (fid, "<p><b>\nGenerated on %s by %s with GNU Octave %s</p>",
             datestr (now (), 0), mfilename, version);

    ## Create table header
    fprintf (fid, "<table border='1'><tr>\n");
    for t = 1:numel(toolkits)
      ## set default
      column_header = upper (toolkits{t});
      if (isfield (in, toolkits{t}))
        column_header = strcat (column_header, in.(toolkits{t}));
      endif
      fprintf (fid, '<th>%s <a href="%s/diary.log">diary</a></th>\n', column_header, toolkits{t});
    endfor
    fprintf (fid, "</tr>\n");

    for m = 1:numel(in.figfiles)
      [~, file] = fileparts (in.figfiles{m});
      fn = strcat (file, ".", in.fmt);
      fprintf (fid, "<tr>\n");
      for k = toolkits
        ffn = fullfile (k{:}, fn);
        fprintf (fid, "  <td>%s<br>", ffn);
        if (exist (ffn, "file"))
          fprintf (fid, "<img src='%s' style='width: %dpx;'>", ffn, in.column_width);
        else
          err_fn = regexprep(ffn, ".png", ".err");
          if (! exist (err_fn, "file"))
            warning("File %s doesn't exist...", err_fn);
          else
            err_fid = fopen (err_fn);
            msg = char (fread (err_fid))';
            fclose (err_fid);
            fprintf (fid, "%s", strrep (msg, "\n", "<br>"));
          endif
        endif
        fprintf (fid, "</td>\n");
      endfor
      fprintf (fid, "</tr>\n");
    endfor
    fputs (fid, trailer);
  unwind_protect_cleanup
    fclose (fid);
  end_unwind_protect

endfunction
