## Copyright (C) 2008, 2009 David Bateman
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
##   Specify the handle, @var{h}, of the figure to be printed. The
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
##   Specify the orientation of the plot for printed output. For
## non-printed output the aspect ratio of the output corresponds to
## the plot area defined by the "paperposition" property in the
## orientation specified. This options is equivalent to changing
## the figure's "paperorientation" property.
##
## @item -d@var{device}
##   Output device, where @var{device} is one of:
##   @table @code
##   @item ps
##   @itemx ps2
##   @itemx psc
##   @itemx psc2
##     Postscript (level 1 and 2, mono and color). The FLTK backend
## generates Postscript level 3.0.
##
##   @item eps
##   @itemx eps2
##   @itemx epsc
##   @itemx epsc2
##     Encapsulated postscript (level 1 and 2, mono and color). The FLTK backend
## generates Postscript level 3.0.
##
##   @item tex
##   @itemx epslatex
##   @itemx epslatexstandalone
##   @itemx pstex
##   @itemx pslatex
##     Generate a @LaTeX{} (or @TeX{}) file for labels, and eps/ps for
## graphics.  The file produced by @code{epslatexstandalone} can be
## processed directly by @LaTeX{}.  The other formats are intended to
## be included in a @LaTeX{} (or @TeX{}) document.  The @code{tex} device
## is the same as the @code{epslatex} device.
##
##   @item tikz
##     Generate a @LaTeX{} file using gnuplot's Lua/TikZ terminal.
##
##   @item ill
##   @itemx aifm
##     Adobe Illustrator
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
##     XFig.  For the Gnuplot backend, the additional options
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
##     GIF image (only available for the Gnuplot backend)
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
##   Appends the PS, PDF, or EPS output to a pre-existing file of the 
## same type.
##
## @itemx -r@var{NUM}
##   Resolution of bitmaps in pixels per inch.  For both metafiles and 
## SVG the default is the screen resolution, for other it is 150 dpi.
## To specify screen resolution, use "-r0".
##
## @item -tight
##   Forces a tight bounding box for eps-files.  Since Ghostscript
## is used to produce other devices, this option works for those
## devices as well.
##
## @item -S@var{xsize},@var{ysize}
##   Plot size in pixels for EMF, GIF, JPEG, PBM, PNG and SVG. For
## PS, EPS, PDF, and other vector formats the plot size is in points.
## This option is equivalent to changing the size of the plot box
## associated with "paperposition" property. Using the command form of
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
## @end deftypefn

function print (varargin)

  opts = __print_parse_opts__ (varargin{:});

  if (! isfigure (opts.figure))
    error ("print: no figure to print.")
  endif

  orig_figure = get (0, "currentfigure");
  figure (opts.figure)
  drawnow ();
  backend = (get (opts.figure, "__backend__"));

  if (strcmp (backend, "gnuplot"))
    ## FIXME - this can be removed when __gnuplot_print__ has been modified
    ##         to work consistently with __fltk_print__
    opts.ghostscript_binary = opts.ghostscript.binary;
    opts.resolution = opts.ghostscript.resolution;
    opts.canvas_size = opts.canvas_size * opts.resolution / 72;
    opts.resolution = sprintf ("%d", opts.resolution);
    opts.fontsize = sprintf ("%d", opts.fontsize);
    if (strcmp (opts.devopt, "tiff"))
      error ("print:notiffoutput",
             "print.m: TIFF output is not available for the Gnuplot backend.")
    endif
    __gnuplot_print__ (opts);
    return
  else
    if (strcmp (opts.devopt, "gif"))
      error ("print:notiffoutput",
             "print.m: GIF output is not available for the FLTK backend.")
    endif
  endif

  if (opts.append_to_file && ! (strncmp (opts.devopt, "pdf", 3)
         || strncmp (opts.devopt(1:2), "ps", 2)))
    warning ("print:cannotappendfile", 
             "print.m: Cannot append files of type '%s'.", opts.devopt)
    opts.append_to_file = false;
  endif

  if (opts.append_to_file)
    saved_original_file = strcat (tmpnam (), ".", opts.devopt);
    opts.unlink(end+1) = {saved_original_file};
    movefile (opts.name, saved_original_file);
  endif

  ## Modify properties as specified by options
  ## FIXME - need an unwind_protect block
  props = [];

  ## backend tranlates figure position to eps bbox in points
  fpos = get (opts.figure, "position");
  props(1).h = opts.figure;
  props(1).name = "position";
  props(1).value = {fpos};
  fpos(3:4) = opts.canvas_size;
  set (opts.figure, "position", fpos)

  if (opts.force_solid != 0)
    h = findobj (opts.figure, "-property", "linestyle");
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
    set (h, "linestyle", linestyle)
  endif

  if (opts.use_color < 0)
    color_props = {"color", "facecolor", "edgecolor"};
    for c = 1:numel(color_props)
      h = findobj (opts.figure, "-property", color_props{c});
      hnone = findobj (opts.figure, color_props{c}, "none");
      h = setdiff (h, hnone);
      m = numel (props);
      for n = 1:numel(h)
        rgb = get (h(n), color_props{c});
        props(m+n).h = h(n);
        props(m+n).name = color_props{c};
        props(m+n).value = {get(h(n), color_props{c})};
        xfer = repmat ([0.30, 0.59, 0.11], size (rgb, 1), 1);
        ## convert RGB color to RGB gray scale
        ggg = repmat (sum (xfer .* rgb, 2), 1, 3);
        set (h(n), color_props{c}, ggg)
      endfor
    endfor
  endif

  if (! isempty (opts.font) || ! isempty (opts.fontsize))
    h = findobj (opts.figure, "-property", "fontname");
    m = numel (props);
    for n = 1:numel(h)
      if (! isempty (opts.font))
        props(end+1).h = h(n);
        props(end).name = "fontname";
        props(end).value = {get(h(n), "fontname")};
      endif
      if (! isempty (opts.fontsize))
        props(end+1).h = h(n);
        props(end).name = "fontsize";
        props(end).value = {get(h(n), "fontsize")};
      endif
    endfor
    if (! isempty (opts.font))
      set (h, "fontname", opts.font)
    endif
    if (! isempty (opts.fontsize))
      if (ischar (opts.fontsize))
        fontsize = str2double (opts.fontsize);
      else
        fontsize = opts.fontsize;
      endif
      set (h, "fontsize", fontsize)
    endif
  endif

  ## call the backend print script
  drawnow ("expose")
  feval (strcat ("__", backend, "_print__"), opts);

  ## restore modified properties
  if (isstruct (props))
    for n = 1:numel(props)
      set (props(n).h, props(n).name, props(n).value{1})
    endfor
  endif

  ## Send to the printer
  if (opts.send_to_printer)
    if (isempty (opts.ghostscript.output))
      prn_datafile = opts.name;
    else
      prn_datafile = opts.ghostscript.output;
    endif
    if (isempty (opts.printer))
      prn_cmd = sprintf ("lpr %s '%s' 2>&1", opts.lpr_options, prn_datafile);
    else
      prn_cmd = sprintf ("lpr %s -P %s '%s' 2>&1", opts.lpr_options,
                         opts.printer, prn_datafile);
    endif
    if (opts.debug)
      fprintf ("lpr command: %s\n", prn_cmd)
      [status, output] = system ("lpq");
      disp (output)
    endif
    [status, output] = system (prn_cmd);
    if (status != 0)
      disp (output)
      warning ("print.m: printing failed.")
    endif
  endif

  ## Append to file using GS
  if (opts.append_to_file)
    if (strncmp (opts.devopt, "pdf", 3))
      suffix = "pdf";
      device = suffix;
    elseif (strncmp (opts.devopt(1:2), "ps", 2))
      ## FIXME - For FLTK the fonts get mangled
      ##         See the seciton "How to concatenate several PS files" at the link,
      ##         http://en.wikibooks.org/wiki/PostScript_FAQ
      suffix = "ps";
      device = suffix;
    endif
    tmp_combined_file = strcat (tmpnam (), ".", suffix);
    opts.unlink{end+1} = tmp_combined_file;
    gs_opts = "-dQUIET -dNOPAUSE -dBATCH -dSAFER -dFIXEDMEDIA";
    gs_cmd = sprintf ("%s %s -sDEVICE=%swrite -sOutputFile=%s %s %s", 
             opts.ghostscript.binary, gs_opts, device, tmp_combined_file,
             saved_original_file, opts.name);
    [status, output] = system (gs_cmd);
    if (opts.debug)
      fprintf ("Append files: %s\n", gs_cmd);
    endif
    if (status != 0)
      warning ("print:failedtoappendfile", 
               "print.m: failed to append output to file '%s'.", opts.name)
      copyfile (saved_original_file, opts.name);
    else
      copyfile (tmp_combined_file, opts.name);
    endif
  endif

  ## Unlink temporary files
  for n = 1:numel(opts.unlink)
    [status, output] = unlink (opts.unlink{n});
    if (status != 0)
      disp (output)
      warning ("print.m: failed to delete temporay file, '%s'.", opts.unlink{n})
    endif
  endfor

  if (isfigure (orig_figure))
    figure (orig_figure);
  endif

endfunction
