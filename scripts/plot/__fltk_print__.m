## Copyright (C) 2010 Shai Ayal
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
## @deftypefn {Function File} {} __fltk_print__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

function __fltk_print__ (opts)

  file2unlink = "";

  if (! isempty (opts.fig2dev_binary))
    fig2dev_devices = {"pstex", "mf", "emf"};
  else
    ## If no fig2dev is present, support emf using pstoedit.
    fig2dev_devices = {"pstex", "mf"};
  endif

  switch lower (opts.devopt)
  case {"eps", "eps2", "epsc", "epsc2"}
    drawnow ("eps", opts.name);
    if (opts.tight_flag)
      __tight_eps_bbox__ (opts, opts.name);
    endif
  case {"epslatex", "pslatex", "pdflatex", "epslatexstandalone", ...
        "pslatexstandalone", "pdflatexstandalone"}
    ## format GL2PS_TEX
    ## FIXME - rotated text do not align properly.
    n = find (opts.devopt == "l", 1);
    suffix = opts.devopt(1:n-1);
    dot = find (opts.name == ".", 1, "last");
    if ((! isempty (dot))
        && any (strcmpi (opts.name(dot:end), {".eps", ".ps", ".pdf", ".tex", "."})))
      name = opts.name(1:dot-1);
      if (dot < numel (opts.name)
          && any (strcmpi (opts.name(dot+1:end), {"eps", "ps", "pdf"})))
        ## If user provides eps/ps/pdf suffix, use it.
        suffix = opts.name(dot+1:end);
      endif
    elseif (dot == numel (opts.name))
      name = opts.name;
    endif
    drawnow (strcat (lower (suffix), "notxt"), strcat (name, ".", suffix));
    drawnow ("tex", strcat (name, ".", suffix, ".tex"));
    movefile (strcat (name, ".", suffix, ".tex"), strcat (name, ".tex"));
    if (opts.tight_flag && strncmpi (opts.devopt, "eps", 3))
      __tight_eps_bbox__ (opts, strcat (opts.name, ".eps"));
    endif
    if (! isempty (strfind (opts.devopt, "standalone")))
      __standalone__ (strcat (name, ".tex"));
    endif
  case {"tikz"}
    ## format GL2PS_PGF
    drawnow ("pgf", opts.name);
  case {"ps", "ps2", "psc", "psc2", "pdf"}
    opts.ghostscript.source = strcat (tmpnam (), ".eps");
    file2unlink = opts.ghostscript.source;
    if (strcmp (opts.devopt, "pdf"))
      opts.ghostscript.device = "pdfwrite";
    else
      opts.ghostscript.device = "pswrite";
    endif
    opts.ghostscript.output = opts.name;
    drawnow ("eps", opts.ghostscript.source);
    if (opts.tight_flag)
      __tight_eps_bbox__ (opts, opts.ghostscript.source);
    endif
  case {"svg"}
    ## format GL2PS_SVG
    drawnow ("svg", opts.name);
  case {"jpeg", "pbm", "pbmraw", "pcx24b", "pcx256", "pcx16", ...
        "pgm", "pgmraw", "png", "ppm", "ppmraw", "pdfwrite", ...
        "tiff", "tiffn"}
    switch opts.devopt
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
    drawnow ("eps", opts.ghostscript.source)
    if (opts.tight_flag)
      __tight_eps_bbox__ (opts, opts.ghostscript.source);
    endif
  case fig2dev_devices
    tmp_figfile = strcat (tmpnam (), ".fig");
    file2unlink = tmp_figfile;
    status = __pstoedit__ (opts, "fig", tmp_figfile);
    if (status == 0)
      status = __fig2dev__ (opts, tmp_figfile);
    endif
  case {"aifm", "dxf", "emf", "fig", "hpgl"};
    status = __pstoedit__ (opts);
  otherwise
    ## various ghostscript devices for printers
    opts.ghostscript.device = opts.devopt;
    opts.ghostscript.output = opts.name;
    opts.ghostscript.epscrop = false;
    opts.ghostscript.source = strcat (tmpnam (), ".eps");
    file2unlink = opts.ghostscript.source;
    drawnow ("eps", opts.ghostscript.source)
    if (opts.tight_flag)
      __tight_eps_bbox__ (opts, opts.ghostscript.source);
    endif
  endswitch

  ## FIXME - warning: unrecognized escape sequence `\P' -- converting to `P'
  if (! isempty (opts.ghostscript.device))
    status = __ghostscript__ (opts.ghostscript);
  endif

  if (! isempty (file2unlink))
    [status, output] = unlink (file2unlink);
    if (status != 0)
      disp (output)
      warning ("print.m: failed to delete temporay file, '%s'.", file2unlink)
    endif
  endif

endfunction

function status = __fig2dev__ (opts, figfile, devopt, devfile)
  persistent warn_on_absence = true
  if (nargin < 4)
    devfile = opts.name;
  endif
  if (nargin < 3)
    devopt =  opts.devopt;
  endif
  if (! isempty (opts.fig2dev_binary))
    cmd = sprintf ("%s -L %s %s %s 2>&1", opts.fig2dev_binary, devopt, figfile, devfile);
    [status, output] = system (cmd);
    if (opts.debug)
      fprintf ("fig2dev command: %s", cmd)
    endif
    if (status)
      warning ("print:fig2devfailed", "print.m: error running fig2dev.")
      disp (cmd)
      disp (output)
    endif
  elseif (isempty (opts.fig2dev_binary) && warn_on_absence)
    warning ("print:nofig2dev", "print.m: 'fig2dev' not found in EXEC_PATH.")
    warn_on_absence = false;
  endif
endfunction

function status = __pstoedit__ (opts, devopt, name)
  persistent warn_on_absence = true
  if (nargin < 3)
    name = opts.name;
  endif
  if (nargin < 2)
    devopt =  opts.devopt;
  endif
    tmp_epsfile = strcat (tmpnam (), ".eps");
    if (opts.tight_flag)
      __tight_eps_bbox__ (opts, tmp_epsfile);
    endif
  if (! isempty (opts.pstoedit_binary))
    drawnow ("eps", tmp_epsfile)
    cmd = sprintf ("%s -f %s %s %s 2>&1", opts.pstoedit_binary, devopt, tmp_epsfile, name);
    [status, output] = system (cmd);
    if (opts.debug)
      fprintf ("pstoedit command: %s", cmd)
    endif
    if (status)
      warning ("print:pstoeditfailed", "print.m: error running pstoedit.")
      disp (cmd)
      disp (output)
    endif
    [status, output] = unlink (tmp_epsfile);
    if (status != 0)
      disp (output)
      warning ("print.m: failed to delete temporay file, '%s'.", tmp_epsfile)
    endif
  elseif (isempty (opts.pstoedit_binary) && warn_on_absence)
    warning ("print:nopstoedit", "print.m: 'pstoedit' not found in EXEC_PATH.")
    warn_on_absence = false;
  endif
endfunction

function __standalone__ (latexfile)
  prepend = {"\\documentclass{minimal}";
             "\\usepackage{epsfig,color}";
             "\\begin{document}";
             "\\centering"};
  postpend = {"\\end{document}"};
  fid = fopen (latexfile, "r");
  if (fid >= 0)
    latex = fscanf (fid, "%c", Inf);
    status = fclose (fid);
    if (status != 0)
      error ("print:errorclosingfile",
             "print.m: error closing file '%s'", latexfile)
    endif
  else
    error ("print:erroropeningfile",
           "print.m: error opening file '%s'", latexfile)
  endif
  fid = fopen (latexfile, "w");
  if (fid >= 0)
    fprintf (fid, "%s\n", prepend{:});
    fprintf (fid, "%s", latex);
    fprintf (fid, "%s\n", postpend{:});
    status = fclose (fid);
    if (status != 0)
      error ("print:errorclosingfile",
             "print.m: error closing file '%s'", latexfile)
    endif
  else
    error ("print:erroropeningfile",
           "print.m: error opening file '%s'", latexfile)
  endif
endfunction


