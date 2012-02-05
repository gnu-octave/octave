## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn  {Function File} {} print ()
## @deftypefnx {Function File} {} print (@var{options})
## @deftypefnx {Function File} {} print (@var{filename}, @var{options})
## @deftypefnx {Function File} {} print (@var{h}, @var{filename}, @var{options})
## Print a graph, or save it to a file
##
## @var{filename} defines the file name of the output file.  If the
## file name has no suffix, one is inferred from the specified
## device and appended to the file name.  If no
## filename is specified, the output is sent to the printer.
##
## @var{h} specifies the figure handle.  If no handle is specified
## the handle for the current figure is used.
##
## @var{options}:
##
## @table @code
## @item -f@var{h}
##   Specify the handle, @var{h}, of the figure to be printed.  The
##   default is the current figure.
##
## @item -P@var{printer}
##   Set the @var{printer} name to which the graph is sent if no
##   @var{filename} is specified.
##
## @item -G@var{ghostscript_command}
##   Specify the command for calling Ghostscript.  For Unix and Windows,
## the defaults are 'gs' and 'gswin32c', respectively.
##
## @item -color
## @itemx -mono
##   Monochrome or color output.
##
## @item -solid
## @itemx -dashed
##   Forces all lines to be solid or dashed, respectively.
##
## @item -portrait
## @itemx -landscape
##   Specify the orientation of the plot for printed output.  For
## non-printed output the aspect ratio of the output corresponds to
## the plot area defined by the "paperposition" property in the
## orientation specified.  This options is equivalent to changing
## the figure's "paperorientation" property.
##
## @item -d@var{device}
##   Output device, where @var{device} is one of:
##   @table @code
##   @item ps
##   @itemx ps2
##   @itemx psc
##   @itemx psc2
##     Postscript (level 1 and 2, mono and color).  The FLTK graphics
##     toolkit generates Postscript level 3.0.
##
##   @item eps
##   @itemx eps2
##   @itemx epsc
##   @itemx epsc2
##     Encapsulated postscript (level 1 and 2, mono and color).  The FLTK
## graphic toolkit generates Postscript level 3.0.
##
##   @item tex
##   @itemx epslatex
##   @itemx epslatexstandalone
##   @itemx pstex
##   @itemx pslatex
##   @itemx pdflatex
##     Generate a @LaTeX{} (or @TeX{}) file for labels, and eps/ps/pdf
## for graphics.  The file produced by @code{epslatexstandalone} can be
## processed directly by @LaTeX{}.  The other formats are intended to
## be included in a @LaTeX{} (or @TeX{}) document.  The @code{tex} device
## is the same as the @code{epslatex} device.  The @code{pdflatex} device
## is only available for the FLTK graphics toolkit.
##
##   @item tikz
##     Generate a @LaTeX{} file using PGF/TikZ@.  For the FLTK the result is
##   PGF.
##
##   @item ill
##   @itemx aifm
##     Adobe Illustrator (Obsolete for Gnuplot versions > 4.2)
##
##   @item cdr
##   @itemx @nospell{corel}
##     CorelDraw
##
##   @item dxf
##     AutoCAD
##
##   @item emf
##   @itemx meta
##     Microsoft Enhanced Metafile
##
##   @item fig
##     XFig.  For the Gnuplot graphics toolkit, the additional options
##     @option{-textspecial} or @option{-textnormal} can be used to control
##     whether the special flag should be set for the text in
##     the figure (default is @option{-textnormal}).
##
##   @item hpgl
##     HP plotter language
##
##   @item mf
##     Metafont
##
##   @item png
##     Portable network graphics
##
##   @item jpg
##   @itemx jpeg
##     JPEG image
##
##   @item gif
##     GIF image (only available for the Gnuplot graphics toolkit)
##
##   @item pbm
##     PBMplus
##
##   @item svg
##     Scalable vector graphics
##
##   @item pdf
##     Portable document format
##   @end table
##
##   If the device is omitted, it is inferred from the file extension,
## or if there is no filename it is sent to the printer as postscript.
##
## @item -d@var{ghostscript_device}
##   Additional devices are supported by Ghostscript.
## Some examples are;
##
##   @table @code
##   @item ljet2p
##     HP LaserJet IIP
##
##   @item ljet3
##     HP LaserJet III
##
##   @item deskjet
##     HP DeskJet and DeskJet Plus
##
##   @item cdj550
##     HP DeskJet 550C
##
##   @item paintjet
##     HP PointJet
##
##   @item pcx24b
##     24-bit color PCX file format
##
##   @item ppm
##     Portable Pixel Map file format
##
##   @item pdfwrite
##     Produces pdf output from eps
##   @end table
##
##   For a complete list, type `system ("gs -h")' to see what formats
## and devices are available.
##
##   When Ghostscript output is sent to a printer the size is determined
## by the figure's "papersize" property.  When the output
## is sent to a file the size is determined by the plot box defined by
## the figure's "paperposition" property.
##
## @itemx -append
##   Appends the PS, or PDF output to a pre-existing file of the
## same type.
##
## @itemx -r@var{NUM}
##   Resolution of bitmaps in pixels per inch.  For both metafiles and
## SVG the default is the screen resolution, for other it is 150 dpi.
## To specify screen resolution, use "-r0".
##
## @item -tight
##   Forces a tight bounding box for eps-files.
##
## @item -@var{preview}
##   Adds a preview to eps-files.  Supported formats are;
##
##   @table @code
##   @item -interchange
##     Provides an interchange preview.
##
##   @item -metalfile
##     Provides a metafile preview.
##
##   @item -pict
##     Provides pict preview.
##
##   @item -tiff
##     Provides a tiff preview.
##   @end table
##
## @item -S@var{xsize},@var{ysize}
##   Plot size in pixels for EMF, GIF, JPEG, PBM, PNG and SVG@.  For
## PS, EPS, PDF, and other vector formats the plot size is in points.
## This option is equivalent to changing the size of the plot box
## associated with "paperposition" property.  Using the command form of
## the print function, you must quote the @var{xsize},@var{ysize}
## option.  For example, by writing @w{@code{"-S640,480"}}.
##
## @item -F@var{fontname}
## @itemx -F@var{fontname}:@var{size}
## @itemx -F:@var{size}
##   Associates all text with the @var{fontname} and/or @var{fontsize}.
## @var{fontname} is ignored for some devices; dxf, fig, hpgl, etc.
## @end table
##
## The filename and options can be given in any order.
##
## Example: Print to a file, using the svg device.
##
## @example
## @group
## figure (1);
## clf ();
## surf (peaks);
## print -dsvg figure1.svg
## @end group
## @end example
##
## Example: Print to an HP Deskjet 550C.
##
## @example
## @group
## figure (1);
## clf ();
## surf (peaks);
## print -dcdj550
## @end group
## @end example
##
## @seealso{figure, orient, saveas}
## @end deftypefn

function print (varargin)

  opts = __print_parse_opts__ (varargin{:});

  opts.pstoedit_cmd = @pstoedit;
  opts.fig2dev_cmd = @fig2dev;
  opts.latex_standalone = @latex_standalone;
  opts.lpr_cmd = @lpr;
  opts.epstool_cmd = @epstool;

  if (! isfigure (opts.figure))
    error ("print: no figure to print");
  endif

  orig_figure = get (0, "currentfigure");
  figure (opts.figure);

  if (opts.append_to_file)
    [~, ~, ext] = fileparts (opts.ghostscript.output);
    opts.ghostscript.prepend = strcat (tmpnam (), ext);
    copyfile (opts.ghostscript.output, opts.ghostscript.prepend);
  endif

  unwind_protect

    ## Modify properties as specified by options
    props = [];

    ## graphics toolkit tranlates figure position to eps bbox in points
    fpos = get (opts.figure, "position");
    props(1).h = opts.figure;
    props(1).name = "position";
    props(1).value = {fpos};
    fpos(3:4) = opts.canvas_size;
    set (opts.figure, "position", fpos);

    ## Set figure background to none. This is done both for
    ## consistency with Matlab and to elliminate the visible
    ## box along the figure's perimeter.
    props(2).h = opts.figure;
    props(2).name = "color";
    props(2).value{1} = get (props(2).h, props(2).name);
    set (props(2).h, props(2).name, "none");

    if (opts.force_solid != 0)
      h = findall (opts.figure, "-property", "linestyle");
      m = numel (props);
      for n = 1:numel(h)
        props(m+n).h = h(n);
        props(m+n).name = "linestyle";
        props(m+n).value = {get(h(n), "linestyle")};
      endfor
      if (opts.force_solid > 0)
        linestyle = "-";
      else
        linestyle = "--";
      endif
      set (h, "linestyle", linestyle);
    endif

    if (opts.use_color < 0
        && ! strcmp (get (opts.figure, "__graphics_toolkit__"), "gnuplot"))
      color_props = {"color", "facecolor", "edgecolor", "colormap"};
      for c = 1:numel(color_props)
        h = findall (opts.figure, "-property", color_props{c});
        hnone = findall (opts.figure, color_props{c}, "none");
        h = setdiff (h, hnone);
        m = numel (props);
        for n = 1:numel(h)
          if (ishandle (h(n)))
            ## Need to verify objects exist since callbacks may delete objects
            ## as the colors for others are modified.
            rgb = get (h(n), color_props{c});
            props(end+1).h = h(n);
            props(end).name = color_props{c};
            props(end).value = {get(h(n), color_props{c})};
            if (isnumeric (rgb))
              ## convert RGB color to RGB gray scale
              xfer = repmat ([0.30, 0.59, 0.11], size (rgb, 1), 1);
              ggg = repmat (sum (xfer .* rgb, 2), 1, 3);
              set (h(n), color_props{c}, ggg);
            endif
          endif
        endfor
      endfor
    endif

    if (! isempty (opts.font) || ! isempty (opts.fontsize))
      h = findall (opts.figure, "-property", "fontname");
      m = numel (props);
      for n = 1:numel(h)
        if (ishandle (h(n)))
          if (! isempty (opts.font))
            props(end+1).h = h(n);
            props(end).name = "fontname";
            props(end).value = {get(h(n), "fontname")};
          endif
        endif
        if (ishandle (h(n)))
          if (! isempty (opts.fontsize))
            props(end+1).h = h(n);
            props(end).name = "fontsize";
            props(end).value = {get(h(n), "fontsize")};
          endif
        endif
      endfor
      if (! isempty (opts.font))
        set (h(ishandle(h)), "fontname", opts.font);
      endif
      if (! isempty (opts.fontsize))
        if (ischar (opts.fontsize))
          fontsize = str2double (opts.fontsize);
        else
          fontsize = opts.fontsize;
        endif
        set (h(ishandle(h)), "fontsize", fontsize);
      endif
    endif

    ## call the graphcis toolkit print script
    switch (get (opts.figure, "__graphics_toolkit__"))
    case "gnuplot"
      opts = __gnuplot_print__ (opts);
    otherwise
      opts = __fltk_print__ (opts);
    endswitch

  unwind_protect_cleanup
    ## restore modified properties
    if (isstruct (props))
      for n = 1:numel(props)
        if (ishandle (props(n).h))
          set (props(n).h, props(n).name, props(n).value{1});
        endif
      endfor
    endif

    ## Unlink temporary files
    for n = 1:numel(opts.unlink)
      [status, output] = unlink (opts.unlink{n});
      if (status != 0)
        warning ("print.m: %s, '%s'", output, opts.unlink{n});
      endif
    endfor
  end_unwind_protect

  if (isfigure (orig_figure))
    figure (orig_figure);
  endif

endfunction

function cmd = epstool (opts, filein, fileout)
  ## As epstool does not work with pipes, a subshell is used to
  ## permit piping. Since this solution does not work with the DOS
  ## command shell, the -tight and -preview options are disabled if
  ## output must be piped.

  ## DOS Shell:
  ##   gs.exe [...] -sOutputFile=<filein> - & epstool -bbox -preview-tiff <filein> <fileout> & del <filein>
  ## Unix Shell;
  ##   cat > <filein> ; epstool -bbox -preview-tiff <filein> <fileout> ; rm <filein>

  dos_shell = (ispc () && ! isunix ());

  cleanup = "";
  if (nargin < 3)
    fileout = opts.name;
  elseif (isempty (fileout))
    fileout = "-";
  endif

  if (nargin < 2 || strcmp (filein, "-") || isempty (filein))
    pipein = true;
    filein = strcat (tmpnam (), ".eps");
    if (dos_shell)
      cleanup = sprintf ("& del %s ", strrep (filein, '/', '\'));
    else
      cleanup = sprintf ("; rm %s ", filein);
    endif
  else
    pipein = false;
    filein = strcat ("'", strtrim (filein), "'");
  endif
  if (strcmp (fileout, "-"))
    pipeout = true;
    fileout = strcat (tmpnam (), ".eps");
    if (dos_shell)
      cleanup = horzcat (cleanup, sprintf ("& del %s ", strrep (fileout, '/', '\')));
    else
      cleanup = horzcat (cleanup, sprintf ("; rm %s ", fileout));
    endif
  else
    pipeout = false;
    fileout = strcat ("'", strtrim (fileout), "'");
  endif

  if (! isempty (opts.preview) && opts.tight_flag)
    warning ("print:previewandtight",
             "print.m: eps preview may not be combined with -tight");
  endif
  if (! isempty (opts.preview) || opts.tight_flag)
    if (! isempty (opts.epstool_binary))
      if (opts.tight_flag)
        cmd = "--copy --bbox";
      elseif (! isempty (opts.preview))
        switch opts.preview
        case "tiff"
          cmd = sprintf ("--add-%s-preview --device tiffg3", opts.preview);
        case {"tiff6u", "tiff6p", "metafile"}
          cmd = sprintf ("--add-%s-preview --device bmpgray", opts.preview);
        case {"tiff4", "interchange"}
          cmd = sprintf ("--add-%s-preview", opts.preview);
        case "pict"
          cmd = sprintf ("--add-%s-preview --mac-single", opts.preview);
        otherwise
          error ("print:invalidpreview",
                 "print.m: epstool cannot include preview for format '%s'",
                 opts.preview);
        endswitch
        if (! isempty (opts.ghostscript.resolution))
          cmd = sprintf ("%s --dpi %d", cmd, opts.ghostscript.resolution);
        endif
      else
        cmd = "";
      endif
      if (! isempty (cmd))
        cmd = sprintf ("%s --quiet %s %s %s ", opts.epstool_binary,
                       cmd, filein, fileout);
      endif
      if (pipein)
        if (dos_shell)
          filein(filein=="'") = "\"";
          gs_cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                                    "device", "epswrite",
                                    "source", "-",
                                    "output", filein);
          cmd = sprintf ("%s %s & %s", gs_cmd, filein, cmd);
        else
          cmd = sprintf ("cat > %s ; %s", filein, cmd);
        endif
      endif
      if (pipeout)
        if (dos_shell)
          cmd = sprintf ("%s & type %s", cmd, fileout);
        else
          cmd = sprintf ("%s ; cat %s", cmd, fileout);
        endif
      endif
      if (! isempty (cleanup))
        if (pipeout && dos_shell)
          error ("print:epstoolpipe",
                 "print.m: cannot pipe output of 'epstool' for DOS shell");
        elseif (pipeout)
          cmd = sprintf ("( %s %s )", cmd, cleanup);
        else
          cmd = sprintf ("%s %s", cmd, cleanup);
        endif
      endif
    elseif (isempty (opts.epstool_binary))
      error ("print:noepstool", "print.m: 'epstool' not found in PATH");
    endif
  else
    if (pipein && pipeout)
      if (dos_shell)
        cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                               "device", "epswrite",
                               "source", "-",
                               "output", "-");
      else
        cmd = " cat ";
      endif
    elseif (pipein && ! pipeout)
      if (dos_shell)
        ## ghostscript expects double, not single, quotes
        fileout(fileout=="'") = "\"";
        cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                               "device", "epswrite",
                               "source", "-",
                               "output", fileout);
      else
        cmd = sprintf (" cat > %s ", fileout);
      endif
    elseif (! pipein && pipeout)
      if (dos_shell)
        cmd = sprintf (" type %s ", filein);
      else
        cmd = sprintf (" cat %s ", filein);
      endif
    else
      if (dos_shell)
        cmd = sprintf (" copy %s %s ", filein, fileout);
      else
        cmd = sprintf (" cp %s %s ", filein, fileout);
      endif
    endif
  endif
  if (opts.debug)
    fprintf ("epstool command: '%s'\n", cmd);
  endif
endfunction

function cmd = fig2dev (opts, devopt)
  if (nargin < 2)
    devopt =  opts.devopt;
  endif
  dos_shell = (ispc () && ! isunix ());
  if (! isempty (opts.fig2dev_binary))
    if (dos_shell)
      ## FIXME - is this the right thing to do for DOS?
      cmd = sprintf ("%s -L %s 2> NUL", opts.fig2dev_binary, devopt);
    else
      cmd = sprintf ("%s -L %s 2> /dev/null", opts.fig2dev_binary, devopt);
    endif
  elseif (isempty (opts.fig2dev_binary))
    error ("print:nofig2dev", "print.m: 'fig2dev' not found in PATH");
  endif
  if (opts.debug)
    fprintf ("fig2dev command: '%s'\n", cmd);
  endif
endfunction

function latex_standalone (opts)
  n = find (opts.name == ".", 1, "last");
  if (! isempty (n))
    opts.name = opts.name(1:n-1);
  endif
  latexfile = strcat (opts.name, ".tex");
  switch opts.devopt
  case {"pdflatexstandalone"}
    packages = "\\usepackage{graphicx,color}";
    graphicsfile = strcat (opts.name, "-inc.pdf");
  case {"pslatexstandalone"}
    packages = "\\usepackage{epsfig,color}";
    graphicsfile = strcat (opts.name, "-inc.ps");
  otherwise
    packages = "\\usepackage{epsfig,color}";
    graphicsfile = strcat (opts.name, "-inc.eps");
  endswitch
  papersize = sprintf ("\\usepackage[papersize={%.2fbp,%.2fbp},text={%.2fbp,%.2fbp}]{geometry}",
                       opts.canvas_size, opts.canvas_size);
  prepend = {"\\documentclass{minimal}";
             packages;
             papersize;
             "\\begin{document}";
             "\\centering"};
  postpend = {"\\end{document}"};
  fid = fopen (latexfile, "r");
  if (fid >= 0)
    latex = fscanf (fid, "%c", Inf);
    status = fclose (fid);
    if (status != 0)
      error ("print:errorclosingfile",
             "print.m: error closing file '%s'", latexfile);
    endif
    ## TODO - should this be fixed in GL2PS?
    latex = strrep (latex, "\\includegraphics{}",
                    sprintf ("\\includegraphics{%s}", graphicsfile));
  else
    error ("print:erroropeningfile",
           "print.m: error opening file '%s'", latexfile);
  endif
  fid = fopen (latexfile, "w");
  if (fid >= 0)
    fprintf (fid, "%s\n", prepend{:});
    fprintf (fid, "%s", latex);
    fprintf (fid, "%s\n", postpend{:});
    status = fclose (fid);
    if (status != 0)
      error ("print:errorclosingfile",
             "print.m: error closing file '%s'", latexfile);
    endif
  else
    error ("print:erroropeningfile",
           "print.m: error opening file '%s'", latexfile);
  endif
endfunction

function cmd = lpr (opts)
  if (nargin < 2)
    devopt =  opts.devopt;
  endif
  if (! isempty (opts.lpr_binary))
    cmd = opts.lpr_binary;
    if (! isempty (opts.lpr_options))
      cmd = sprintf ("%s %s", cmd, opts.lpr_options);
    endif
    if (! isempty (opts.printer))
      cmd = sprintf ("%s -P %s", cmd, opts.printer);
    endif
  elseif (isempty (opts.lpr_binary))
    error ("print:nolpr", "print.m: 'lpr' not found in PATH");
  endif
  if (opts.debug)
    fprintf ("lpr command: '%s'\n", cmd);
  endif
endfunction

function cmd = pstoedit (opts, devopt)
  if (nargin < 2)
    devopt =  opts.devopt;
  endif
  dos_shell = (ispc () && ! isunix ());
  if (! isempty (opts.pstoedit_binary))
    if (dos_shell)
      cmd = sprintf ("%s -f %s 2> NUL", opts.pstoedit_binary, devopt);
    else
      ## FIXME - is this the right thing to do for DOS?
      cmd = sprintf ("%s -f %s 2> /dev/null", opts.pstoedit_binary, devopt);
    endif
  elseif (isempty (opts.pstoedit_binary))
    error ("print:nopstoedit", "print.m: 'pstoedit' not found in PATH");
  endif
  if (opts.debug)
    fprintf ("pstoedit command: '%s'\n", cmd);
  endif
endfunction


