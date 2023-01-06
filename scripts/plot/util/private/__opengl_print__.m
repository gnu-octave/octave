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
## @deftypefn {} {@var{opts} =} __opengl_print__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

function opts = __opengl_print__ (opts)

  dos_shell = (ispc () && ! isunix ());

  if (! isempty (opts.fig2dev_binary))
    ## fig2dev is preferred for conversion to emf
    fig2dev_devices = {"pstex", "mf", "emf"};
  else
    fig2dev_devices = {"pstex", "mf"};
  endif

  gl2ps_device = {};
  pipeline = {};
  switch (lower (opts.devopt))
    case {"eps", "eps2", "epsc", "epsc2"}
      ## format GL2PS_EPS
      gl2ps_device = {"eps"};
      ## FIXME: use epstool to tighten bbox and provide preview.
      pipeline = {opts.epstool_cmd(opts, "-", opts.name)};
    case {"epslatex", "pslatex", "pdflatex", "epslatexstandalone", ...
          "pslatexstandalone", "pdflatexstandalone"}
      ## format GL2PS_TEX
      n = find (opts.devopt == "l", 1);
      suffix = opts.devopt(1:n-1);
      [ndir, name, ext] = fileparts (opts.name);
      if (isempty (ext))
        ext = "tex";
      else
        ext = ext(2:end);  # remove leading '.'
      endif
      if (any (strcmpi (ext, {suffix, "tex"})))
        name = fullfile (ndir, name);
        if (any (strcmpi (ext, {"eps", "ps", "pdf"})))
          suffix = ext;  # If user provides eps/ps/pdf suffix, use it.
        endif
      else
        error ("Octave:print:invalid-suffix",
               "invalid suffix '%s' for device '%s'.",
               ext, lower (opts.devopt));
      endif
      gl2ps_device = {sprintf("%snotxt", lower (suffix))};
      gl2ps_device{2} = "tex";
      if (dos_shell)
        pipeline = {sprintf('findstr . > "%s-inc.%s"', name, suffix)};
        pipeline{2} = sprintf ('findstr . > "%s.tex"', name);
      else
        pipeline = {sprintf('cat > "%s-inc.%s"', name, suffix)};
        pipeline{2} = sprintf ('cat > "%s.tex"', name);
      endif
    case "tikz"
      ## format GL2PS_PGF
      gl2ps_device = {"pgf"};
      pipeline = {sprintf('cat > "%s"', opts.name)};
    case "svg"
      ## format GL2PS_SVG
      gl2ps_device = {"svg"};
      svgcmd = "";
      if (opts.svgconvert)
        svgcmd = opts.svgconvert_cmd (opts, opts.ghostscript.device);
      endif
      if (! isempty (svgcmd))
        pipeline = {sprintf(svgcmd, "svg", opts.name)};
      else
        pipeline = {sprintf('cat > "%s"', opts.name)};
      endif
    case fig2dev_devices
      cmd_fig2dev = opts.fig2dev_cmd (opts, opts.devopt);
      if (strcmp (opts.devopt, "pstex"))
        cmd_pstoedit = opts.pstoedit_cmd (opts, "fig", false);
        [~, ~, ext] = fileparts (opts.name);
        if (any (strcmpi (ext, {".ps", ".tex", "."})))
          opts.name = opts.name(1:end-numel (ext));
        endif
        opts.name = [opts.name ".ps"];
        cmd = sprintf ('%s | %s > "%s"', cmd_pstoedit, cmd_fig2dev, opts.name);
        gl2ps_device = {"eps"};
        pipeline = {cmd};
        cmd_fig2dev = opts.fig2dev_cmd (opts, "pstex_t");
        gl2ps_device{2} = "eps";
        pipeline{2} = sprintf ('%s | %s > "%s"', cmd_pstoedit,
                               cmd_fig2dev, strrep (opts.name, ".ps", ".tex"));
      else
        ## Using svgconvert
        tmp = tempname ();
        opts.unlink = [opts.unlink tmp];
        cmd_pstoedit = sprintf (opts.pstoedit_cmd (opts, "fig"), ...
                                "pdf", tmp, tmp);
        tmp = [tempname(), ".fig"];
        opts.unlink = [opts.unlink tmp];
        if (ispc () && ! isunix ())
          cmd = sprintf ('%s "%s" & %s "%s" "%s"', cmd_pstoedit, tmp, cmd_fig2dev, tmp, opts.name);
        else
          cmd = sprintf ('%s "%s" ; %s "%s" "%s"', cmd_pstoedit, tmp, cmd_fig2dev, tmp, opts.name);
        endif
        gl2ps_device = {"svg"};
        pipeline = {cmd};
      endif
    case "aifm"
      tmp = tempname ();
      opts.unlink = [opts.unlink tmp];
      cmd = sprintf (opts.pstoedit_cmd (opts, "ps2ai"), "pdf", tmp, tmp);
      gl2ps_device = {"svg"};
      pipeline = {sprintf("%s > %s", cmd, opts.name)};
    case {"dxf", "emf", "fig", "hpgl"}
      tmp = tempname ();
      opts.unlink = [opts.unlink tmp];
      cmd = sprintf (opts.pstoedit_cmd (opts), "pdf", tmp, tmp);
      gl2ps_device = {"svg"};
      pipeline = {sprintf('%s "%s"', cmd, opts.name)};
    case opts.ghostscript.device
      svgcmd = "";
      if (opts.svgconvert)
        svgcmd = opts.svgconvert_cmd (opts, opts.ghostscript.device);
      endif
      dosvg = ! isempty (svgcmd);
      if (! dosvg)
        opts.ghostscript.source = "-";
      else
        tmp = tempname ();
        opts.ghostscript.source = tmp;
        opts.unlink = [opts.unlink tmp];
        svgcmd = sprintf (svgcmd, "pdf", tmp);
      endif

      opts.ghostscript.output = opts.name;
      if (opts.send_to_printer)
        opts.unlink(strcmp (opts.unlink, opts.ghostscript.output)) = [];
        opts.ghostscript.output = "-";
      endif

      [cmd_gs, cmd_cleanup] = __ghostscript__ (opts.ghostscript);
      if (opts.send_to_printer || isempty (opts.name))
        cmd_lpr = opts.lpr_cmd (opts);
        cmd = sprintf ("%s | %s", cmd_gs, cmd_lpr);
      elseif (dosvg)
        if (dos_shell)
          cmd = sprintf ("%s & %s", svgcmd, cmd_gs);
        else
          cmd = sprintf ("%s ; %s", svgcmd, cmd_gs);
        endif
      else
        cmd = sprintf ("%s", cmd_gs);
      endif

      if (dosvg)
        gl2ps_device = {"svg"};
      else
        gl2ps_device = {"eps"};
      endif

      if (! isempty (cmd_cleanup))
        if (dos_shell)
          pipeline = {sprintf("%s & %s", cmd, cmd_cleanup)};
        else
          pipeline = {sprintf("%s ; %s", cmd, cmd_cleanup)};
        endif
      else
        pipeline = {cmd};
      endif
    otherwise
      error (sprintf ("print:no%soutput", opts.devopt),
             "print: %s output is not available for OpenGL toolkits",
             upper (opts.devopt));
  endswitch

  opts.pipeline = pipeline;

  for n = 1:numel (pipeline)
    if (opts.debug)
      fprintf ("opengl-pipeline: '%s'\n", pipeline{n});
    endif

    __check_rendering_capability__ ("print", opts.figure);

    ## Use toolkits "print_figure" method
    if (ispc () && ! isunix ())
      drawnow (gl2ps_device{n}, ['| "' pipeline{n} '"']);
    else
      drawnow (gl2ps_device{n}, ["| " pipeline{n}]);
    endif
  endfor

  if (! isempty (strfind (opts.devopt, "standalone")))
    opts.latex_standalone (opts);
  endif

endfunction
