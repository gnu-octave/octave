## Copyright (C) 2009 Ben Abbott
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{fontname}, @var{fontsize}] =} __gnuplot_default_font__ (@var{h}, @var{term})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@bens-macbook.local>
## Created: 2009-03-16

function varargout = __gnuplot_default_font__ (h)

  if (nargin == 0)
    h = gcf ();
  endif

  plot_stream = get (h, "__plot_stream__");

  term = __gnuplot_get_var__ (h, "GPVAL_TERM");
  term_options = __gnuplot_get_var__ (h, "GPVAL_TERMOPTIONS");
  words = regexp (term_options, '(\b[^\s]+\b|"[^"]+"|''[^'']+'')', "matches");

  n = find (strcmp (words, "fname") | strcmp (words, "font"), 1);
  if (! isempty (n))
    font_name = words{n+1};
    if (font_name(1)=="\"" || font_name(1)=="'")
      font_name = font_name(2:(end-1));
    endif
     m = find (strcmp (words, "fontsize") | strcmp (words, "fsize"), 1);
    if (! isempty (m))
      font_size = sscanf (words{m+1}, "%d");
    elseif (! isempty (strfind (font_name, ",")))
      k = strfind (font_name, ",")(1);
      font_size = sscanf (font_name((k+1):end), "%d");
      font_name = font_name(1:(k-1));
    else
      font_size = 12;
    endif
  elseif (numel (words) > 1 && (words{end-1}(1)=="\"" || words{end-1}(1)=="'"))
    font_name = words{end-1}(2:(end-1));
    font_size = sscanf (words{end}, "%d");
  elseif (strcmp (term, {"epslatex"}))
    font_name = "*";
    font_size = sscanf (words{end}, "%d");
  else
    font_name = "*";
    font_size = 12;
  endif
  varargout = {font_name, font_size, words};

endfunction

%!demo
%! figure(1)
%! drawnow
%! terms = {"aqua", "corel", "dxf", "emf", "epslatex", "fig", "gif", ...
%!          "hpgl", "jpeg", "mf", "pbm", "pdf", "png", "postscript", ...
%!          "pslatex", "pstex", "svg", "tex", "wxt", "x11"};
%! plot_stream = get (gcf, "__plot_stream__");
%! orig_term = __gnuplot_get_var__ (gcf, "GPVAL_TERM");
%! unwind_protect
%!   for n = 1:numel(terms)
%!     status = fprintf (plot_stream(1), "\nset term %s;\n", terms{n});
%!     fflush (plot_stream(1));
%!     [fname, fsize, term_opts] = __gnuplot_default_font__(gcf);
%!     fprintf ("%10s: font name = '%s', font size = %d\n", terms{n}, fname, fsize)
%!   endfor
%! unwind_protect_cleanup
%!   status = fprintf (plot_stream(1), "\nset term %s;\n", orig_term);
%!   fflush (plot_stream(1));
%! end_unwind_protect

