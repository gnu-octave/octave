## Copyright (C) 1999, 2005, 2006, 2007, 2008, 2009 Daniel Heiserer
## Copyright (C) 2001 Laurent Mazet
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
## @deftypefn {Function File} {} __gnuplot_print__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

## Author: Daniel Heiserer <Daniel.heiserer@physik.tu-muenchen.de>
## Adapted-By: jwe

function __gnuplot_print__ (opts)

  file2unlink = "";

  if (isempty (opts.fontsize))
    ## If no fontsize, determine the nominal axes fontsize.
    defaultfontsize = get (0, "defaultaxesfontsize");
    axesfontsize = get (findobj (opts.figure, "type", "axes"), "fontsize");
    if (iscell (axesfontsize))
      axesfontsize = round (median (cell2mat (axesfontsize)));
    endif
    if (isempty (axesfontsize))
      opts.fontsize = defaultfontsize;
    else
      opts.fontsize = axesfontsize;
    endif
  end
  ## The axes-label and tick-label spacing is determined by
  ## the font spec given in "set terminal ..."
  gp_opts = font_spec (opts);

  unwind_protect
    switch lower (opts.devopt)
    case {"eps", "eps2", "epsc", "epsc2"}
      if (any (strcmp (opts.devopt, {"eps", "epsc"})))
        gp_opts = sprintf ("%s level1", gp_opts);
      endif
      eps_drawnow (opts, opts.name, gp_opts);
    case {"epslatex", "pslatex", "pstex", "epslatexstandalone"}
      n = find (opts.devopt == "l", 1);
      suffix = opts.devopt(1:n-1);
      dot = find (opts.name == ".", 1, "last");
      if ((! isempty (dot))
          && any (strcmpi (opts.name(dot:end),
                  {".eps", ".ps", ".pdf", ".tex", "."})))
        name = opts.name(1:dot-1);
        if (dot < numel (opts.name)
            && any (strcmpi (opts.name(dot+1:end), {"eps", "ps"})))
          ## If user provides eps/ps suffix, use it.
          suffix = opts.name(dot+1:end);
        endif
      elseif (dot == numel (opts.name))
        name = opts.name;
      endif
      if (strfind (opts.devopt, "standalone"))
        term = sprintf ("%s ",
                        strrep (opts.devopt, "standalone", " standalone"));
      else
        term = sprintf ("%s ", opts.devopt);
      endif
      local_drawnow (sprintf ("%s %s", term, gp_opts),
               strcat (name, ".", suffix, ".tex"), opts)
      movefile (strcat (name, ".", suffix, ".tex"), strcat (name, ".tex"));
      if (opts.tight_flag && strncmpi (opts.devopt, "eps", 3))
        __tight_eps_bbox__ (opts, strcat (opts.name, ".eps"));
      endif
    case {"tikz"}
      local_drawnow (sprintf ("lua tikz %s", gp_opts), opts.name, opts);
    case {"ps", "ps2", "psc", "psc2", "pdf"}
      if (any (strcmp (opts.devopt, {"ps", "psc"})))
        gp_opts = sprintf ("%s level1", gp_opts);
      endif
      ## Gnuplot's BBox LLHC is located at [50,50]
      opts.ghostscript.pageoffset = opts.ghostscript.pageoffset - 50;
      opts.ghostscript.source = strcat (tmpnam (), ".eps");
      file2unlink = opts.ghostscript.source;
      if (strcmp (opts.devopt, "pdf"))
        opts.ghostscript.device = "pdfwrite";
      elseif (any (opts.devopt == '2'))
        opts.ghostscript.device = "ps2write";
      else
        opts.ghostscript.device = "pswrite";
        opts.ghostscript.level = 1;
      endif
      opts.ghostscript.output = opts.name;
      eps_drawnow (opts, opts.ghostscript.source, gp_opts);
    case {"svg"}
      local_drawnow (sprintf ("svg dynamic %s", gp_opts), opts.name, opts);
    case {"aifm", "corel", "eepic", "emf", "fig", "pdfcairo", "pngcairo"}
      local_drawnow (sprintf ("%s %s", opts.devopt, gp_opts), opts.name, opts);
    case gs_based_devices ()
      opts.ghostscript.antialiasing = true;
      switch opts.devopt
      case "bmp"
        opts.ghostscript.device = "bmp32b";
      case "png"
        opts.ghostscript.device = "png16m";
      case {"tiff", "tiffn"}
        opts.ghostscript.device = "tiff24nc";
      otherwise
        opts.ghostscript.device = opts.devopt;
      endswitch
      opts.ghostscript.output = opts.name;
      opts.ghostscript.source = strcat (tmpnam (), ".eps");
      opts.ghostscript.epscrop = true;
      file2unlink = opts.ghostscript.source;
      eps_drawnow (opts, opts.ghostscript.source, gp_opts);
    case {"canvas", "dxf", "hpgl", "mf", "gif", "pstricks", "texdraw"}
      local_drawnow (sprintf ("%s %s", opts.devopt, gp_opts), opts.name, opts)
    case {"pdflatex", "pslatexstandalone", "pdflatexstandalone"}
      error (sprintf ("print:no%soutput", opts.devopt),
             "print.m: %s output is not available for the GNUPLOT backend.",
             upper (opts.devopt))
    otherwise
      ## various ghostscript devices for printers
      opts.ghostscript.device = opts.devopt;
      opts.ghostscript.output = opts.name;
      opts.ghostscript.epscrop = false;
      opts.ghostscript.source = strcat (tmpnam (), ".eps");
      file2unlink = opts.ghostscript.source;
      ## Gnuplot's BBox LLHC is located at [50,50]
      opts.ghostscript.pageoffset = opts.ghostscript.pageoffset - 50;
      ## Empirical observation: "-dpxlcolor" requires a sign change.
      opts.ghostscript.pageoffset = opts.ghostscript.pageoffset .* [1, -1];
      ## Printers are not included in gs_devices()
      gp_opts = font_spec (opts, "devopt", "eps");
      eps_drawnow (opts, opts.ghostscript.source, gp_opts);
    endswitch
  
    if (! isempty (opts.ghostscript.device))
      status = __ghostscript__ (opts.ghostscript);
    endif
  
  unwind_protect_cleanup
    if (! isempty (file2unlink))
      [status, output] = unlink (file2unlink);
      if (status != 0)
        warning ("print.m: %s, '%s'.", output, file2unlink)
      endif
    endif
  end_unwind_protect

endfunction

function eps_drawnow (opts, epsfile, gp_opts)
  [h, fontsize] = get_figure_text_objs (opts);
  unwind_protect
    for n = 1:numel(h)
      set (h, "fontsize", 2 * fontsize{n});
    endfor
    local_drawnow (sprintf ("postscript eps %s", gp_opts), epsfile, opts);
    if (opts.tight_flag)
      __tight_eps_bbox__ (opts, epsfile);
    endif
  unwind_protect_cleanup
    for n = 1:numel(h)
      set (h, "fontsize", fontsize{n});
    endfor
  end_unwind_protect
endfunction

function local_drawnow (term, file, opts)
  if (opts.use_color < 0)
    mono = true;
  else
    mono = false;
  endif
  figure (opts.figure)
  if (isempty (opts.debug_file) || ! opts.debug)
    drawnow (term, file, mono);
  else
    drawnow (term, file, mono, opts.debug_file);
  endif
endfunction

function device_list = gs_based_devices ();
  ## Aliases for other devices: "bmp", "png", "tiff", "tiffn", "pdf",
  ##                            "ps", "ps2", "psc", "psc2"
  device_list = {"bmp16", "bmp16m", "bmp256", "bmp32b", "bmpgray", ...
                 "jpeg", "jpegcymk", "jpeggray", "pbm", "pbmraw", ...
                 "pcx16", "pcx24b", "pcx256", "pcx2up", "pcxcmyk", ...
                 "pcxgray", "pcxmono", "pdfwrite", "pgm", "pgmraw", ...
                 "pgnm", "pgnmraw", "png16", "png16m", "png256", ...
                 "png48", "pngalpha", "pnggray", "pngmono", "pnm", ...
                 "pnmraw", "ppm", "ppmraw", "ps2write", "pswrite", ...
                 "tiff12nc", "tiff24nc", "tiff32nc", "tiffcrle", ...
                 "tiffg3", "tiffg32d", "tiffg4", "tiffgray", "tifflzw", ...
                 "tiffpack", "tiffsep", "bmp", "png", "tiff", "tiffn", ...
                 "pdf", "ps", "ps2", "psc", "psc2"};
endfunction

function f = font_spec (opts, varargin)
  for n = 1:2:numel(varargin)
    opts.(varargin{n}) = varargin{n+1};
  endfor
  f = "";
  switch opts.devopt
  case {"cgm"}
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      f = sprintf ("font ""%s,%d""", opts.font, opts.fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("font ""%s""", opts.font);
    elseif (! isempty (opts.fontsize))
      f = sprintf ("%d", opts.fontsize);
    endif
  case {"eps", "eps2", "epsc", "epsc2", gs_based_devices(){:}}
    ## Gnuplot renders fonts as half their specification, which 
    ## results in a tight spacing for the axes-labels and tick-labels.
    ## Compensate for the half scale. This will produce the proper
    ## spacing for the requested fontsize.
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      f = sprintf ("font ""%s,%d""", opts.font, 2 * opts.fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("font ""%s""", opts.font);
    elseif (! isempty (opts.fontsize))
      f = sprintf ("%d", 2 * opts.fontsize);
    endif
  case {"svg"}
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      fontsize = round (opts.fontsize * 0.75);
      f = sprintf ("fname ""%s"" fsize %d", opts.font, fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("fname ""%s""", opts.font);
    elseif (! isempty (opts.fontsize))
      fontsize = round (opts.fontsize * 0.75);
      f = sprintf ("%s fsize %d", f, fontsize);
    endif
  case {"pdf"}
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      f = sprintf ("font ""%s,%d""", opts.font, opts.fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("font ""%s""", opts.font);
    elseif (! isempty (opts.fontsize))
      f = sprintf ("fsize %d", f, opts.fontsize);
    endif
  case {"pdfcairo", "pngcairo"}
    if (! isempty (opts.font))
      f = sprintf ("font ""%s""", opts.font);
    endif
  case {"epslatex", "epslatexstandalone"}
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      f = sprintf ("font ""%s,%d""", opts.font, opts.fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("font ""%s""", opts.font);
    elseif (! isempty (opts.fontsize))
      f = sprintf ("%d", opts.fontsize);
    endif
  case {"pslatex"}
    if (! isempty (opts.fontsize))
      f = sprintf ("%d", opts.fontsize);
    endif
  case {"gif", "jpeg", "png"}
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      f = sprintf ("font ""%s ,%d""", opts.font, opts.fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("font ""%s""", opts.font);
    elseif (! isempty (opts.fontsize))
      f = sprintf ("font ""%d""", opts.fontsize);
    endif
  case {"emf"}
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      f = sprintf ("""%s"" %d", opts.font, opts.fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("""%s""", opts.font);
    elseif (! isempty (opts.fontsize))
      f = sprintf ("%d", opts.fontsize);
    endif
  case {"canvas"}
    if (! isempty (opts.fontsize))
      f = sprintf ("fsize %d", opts.fontsize);
    endif
  case {"aifm", "corel"}
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      f = sprintf ("%s %d", opts.font, opts.fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("%s", opts.font);
    elseif (! isempty (opts.fontsize))
      f = sprintf ("%d", opts.fontsize);
    endif
  case {"fig"}
    if (! isempty (opts.font) && ! isempty (opts.fontsize))
      f = sprintf ("font %s fontsize %d", opts.font, opts.fontsize);
    elseif (! isempty (opts.font))
      f = sprintf ("font %s", opts.font);
    elseif (! isempty (opts.fontsize))
      f = sprintf ("fontsize %d", opts.fontsize);
    endif
  endswitch
endfunction

function [h, fontsize] = get_figure_text_objs (opts)
  h = findall (opts.figure, "-property", "fontsize");
  fontsize = get (h, "fontsize");
  switch numel (fontsize)
  case 0
    fontsize = {};
  case 1
    fontsize = {fontsize};
  endswitch
endfunction
