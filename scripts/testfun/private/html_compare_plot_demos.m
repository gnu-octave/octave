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
## @deftypefn  {Function File} {} html_compare ()
## @deftypefnx {Function File} {} html_compare (@var{name}, @var{value}, @dots{})
##
## Produces an html document to compare the plot demos produced by Octave's
## gnuplot and FLTK toolkits with those produced by Matalb.
##
## Valid property names, and their defaults, are;
##
## @table @samp
##   @item date
##   @code{datestr (now (), 0)}
##   @item fmt
##   @code{"png"}
##   @item mfiles
##   @code{sort (cellstr (ls ("gnuplot")))}
##   @item gnuplot
##   @code{sprintf ("Gnuplot %s", __gnuplot_version__ ())}
##   @item matlab
##   @code{"Matlab Version 7.11.0.584 (R2010b)"}
##   @item octave
##   @code{sprintf ("Octave %s", version)}
##   @item output
##   @code{"compare_plots.html"}
##   @item template
##   @code{"html_template.html"}
## @end table
##
## The template parameter refers to a specially formatted html file
## which accompanies this m-file script.
##
## This script may be used to generate the index.html page loaded by
## the link below.
##
## @example
## @group
##   http://octave.sourceforge.net/compare_plots
## @end group
## @end example
##
## This is done by following the instructions below.
##
## @enumerate 
## @item Begin by downloading a local copy of the web page.
##
## @example
## @group
## $ wget -vcr --level 1 http://octave.sourceforge.net/compare_plots/
## @end group
## @end example
##
## @item Use @code{dump_demos} to produce the script used to produce the
## demo plots. In this example the script is @code{dump.m}.
##
## @example
## @group
## octave:1> dump_demos plot dump.m png
## @end group
## @end example
## 
## @item Produce the gnuplot and fltk plots, and place them in their
## designated directories.
## 
## @example
## @group
## octave:2> dump
## octave:3> movefile *.png octave.sourceforge.net/compare_plots/gnuplot/.
## octave:4> graphics_toolkit fltk
## octave:5> close all
## octave:6> dump
## octave:7> movefile *.png octave.sourceforge.net/compare_plots/fltk/.
## @end group
## @end example
## 
## @item Start Matlab and edit the script @code{dump.m} to correct the 
## remaining Octave specific syntax so that it will run under Matlab.
##
## @item Run @code{dump.m} under Matlab, and place the plots in the
## designated matlab directory.
##
## @example
## @group
## >> dump
## >> movefile *.png octave.sourceforge.net/compare_plots/matlab/.
## @end group
## @end example
## 
## @item The web page comparing all plot demos is created by
##
## @example
## @group
## octave:8> cd octave.sourceforge.net/compare_plots
## octave:9> html_compare output index.html
## @end group
## @end example
##
## @item Finally, the new page may be loaded into your browser.
##
## @seealso{dump_demos, demo}
## @end deftypefn

## Author: Ben Abbott  <bpabbott@mac.com>

function html_compare_plot_demos (varargin)

  ## TODO - Names of the toolkits should be input
  ## Set defaults  
  in.date = datestr (now (), 0);
  in.fmt = "png";
  in.figfiles = {};
  in.octave = sprintf ("Octave %s", version);
  in.output= "compare_plots.html";
  in.template = "html_plot_demos_template.html";
  in.toolkits = {"gnuplot", "matlab", "fltk"};

  ## Parse inputs
  for n = 1:2:numel(varargin)
    in.(lower(varargin{n})) = varargin{n+1};
  endfor

  for t = 1:numel(in.toolkits)
    if (! isfield (in, in.toolkits{t}))
      ## Column headers for the html page
      in.(in.toolkits{t}) = upper (in.toolkits{t});
    endif
    ## Compile a list of all files for all toolkits
    if (t == 1)
      in.figfiles = {dir(sprintf ("gnuplot/*.%s", in.fmt)).name};
    else
      filter = sprintf ("%s/*.%s", in.toolkits{t}, in.fmt);
      in.figfiles = union (in.figfiles, {dir(filter).name});
    endif
  endfor

  fid = fopen (which (in.template), "r");
  template = char (fread (fid)) .';
  fclose (fid);

  template = strrep (template, "##OCTAVE##", in.octave);
  template = strrep (template, "##GNUPLOT##", in.gnuplot);
  template = strrep (template, "##MATLAB##", in.matlab);

  n1 = findstr (template, "<!-- ##BEGIN## -->");
  n2 = findstr (template, "<!-- ##END## -->");
  header = template(1:n1-1);
  middle = template(n1+18:n2-1);
  trailer = template(n2+15:end);

  fid = fopen (in.output, "w");
  unwind_protect
    fputs (fid, header);
    for m = 1:numel(in.figfiles)
      [~, file] = fileparts (in.figfiles{m});
      fputs (fid, strrep (middle, "##PLOT##", strcat (file, ".", in.fmt)));
    endfor
    fputs (fid, trailer);
  unwind_protect_cleanup
    fclose (fid);
  end_unwind_protect

endfunction
