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
## Print a plot, or save it to a file.
##
## @var{filename} specifies the name of the output file.  If the
## file name has no suffix, then one is inferred from the specified
## device and appended to the file name.  If no
## filename is specified the output is sent to the printer.
##
## @var{h} specifies the figure handle to print.  If no handle is specified
## the handle for the current figure is used.
##
## @var{options}:
##
## @table @code
## @item -f@var{h}
##   Specify the handle, @var{h}, of the figure to be printed.  The
## default is the current figure.
##
## @item -P@var{printer}
##   Set the @var{printer} name to which the plot is sent when no
## @var{filename} is specified.
##
## @item -G@var{ghostscript_command}
##   Specify the command for invoking Ghostscript.  The defaults for Unix and
## Windows are 'gs' and 'gswin32c' respectively.
##
## @item -color
## @itemx -mono
##   Print monochrome or color lines.
##
## @item -solid
## @itemx -dashed
##   Print solid or dashed lines.
##
## @item -portrait
## @itemx -landscape
##   Specify the orientation of the plot for printed output.
##
## @item -d@var{device}
##   Output device, where @var{device} is one of:
##   @table @code
##   @item ps
##   @itemx ps2
##   @itemx psc
##   @itemx psc2
##     Postscript (level 1 and 2, mono and color)
##
##   @item eps
##   @itemx eps2
##   @itemx epsc
##   @itemx epsc2
##     Encapsulated postscript (level 1 and 2, mono and color)
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
##     XFig.  If this format is selected the additional options
## @option{-textspecial} or @option{-textnormal} can be used to control
## whether the special flag should be set for the text in
## the figure (default is @option{-textnormal}). 
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
##     GIF image
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
##   Specify an output device supported by Ghostscript.
## Some examples are:
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
##     Converts ps or eps to pdf
##   @end table
##
##   For a complete list of available formats and devices type
##   @samp{system ("gs -h")}.
##
##   When Ghostscript output is sent to a printer the size is determined
## by the figure's "papersize" property.  When the output 
## is sent to a file the size is determined by the figure's
## "paperposition" property.
##
## @item -append
##   Append the output to a pre-existing file.  Only PDF
## and Postscript files are currently supported.
##
## @item -r@var{NUM}
##   Resolution of bitmaps in pixels per inch.  For both metafiles and 
## SVG the default is the screen resolution, for others it is 150 dpi.
## To specify screen resolution, use "-r0".
##
## @item -tight
##   Force a tight bounding box for eps-files.  Because the Ghostscript
## devices are conversions of an eps-file, this option works for those
## devices as well.
##
## @item -S@var{xsize},@var{ysize}
##   Specify plot size in pixels for EMF, GIF, JPEG, PBM, PNG and SVG@.  If
## using the command form of the print function, the  @var{xsize},@var{ysize}
## option must be quoted.  For example, by writing
## @w{@code{"-S640,480"}}.  The size defaults to that specified by the
## figure's paperposition property.
##
## @item  -F@var{fontname}
## @itemx -F@var{fontname}:@var{size}
## @itemx -F:@var{size}
##   Set the postscript font to @var{fontname} (for use with postscript,
## aifm, @nospell{corel} and fig).  By default, 'Helvetica' is set for PS/aifm,
## and 'SwitzerlandLight' for Corel.  It can also be 'Times-Roman'.
## @var{size} is given in points.  @var{fontname} is ignored for the
## fig device.
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
    __gnuplot_print__ (varargin{:});
    return
  endif

  ## FIXME - this can be moved to __print_parse_opts__ when __gnuplot_print__
  ##         has been modified to work consistently with __fltk_print__
  if (! isempty (opts.canvas_size) && ischar (opts.resolution))
    opts.canvas_size = str2num (strrep (strrep (opts.canvas_size, "X", ""), "Y", ""));
  endif
  if (! isempty (opts.resolution) && ischar (opts.resolution))
    opts.resolution = str2num (opts.resolution);
  endif
  if (! isempty (opts.fontsize) && ischar (opts.fontsize))
    opts.fontsize = str2num (opts.fontsize);
  endif

  if (opts.append_to_file)
    saved_original_file = strcat (tmpnam (), ".", opts.devopt);
    opts.unlink(end+1) = {save_original_file};
    movefile (opts.name, saved_original_file);
  endif

  ## Modify properties as specified by options
  ## FIXME - need an unwind_protect block
  props = [];

  if ((! isempty (opts.canvas_size))
      || (! strcmpi (get (opts.figure, "paperorientation"), opts.orientation)))
    m = numel (props);
    props(m+1).h = opts.figure;
    props(m+1).name = "paperposition";
    props(m+1).value = {get(opts.figure, "paperposition")};
    props(m+2).h = opts.figure;
    props(m+2).name = "paperunits";
    props(m+2).value = {get(opts.figure, "paperunits")};
    props(m+3).h = opts.figure;
    props(m+3).name = "papersize";
    props(m+3).value = {get(opts.figure, "papersize")};
    props(m+4).h = opts.figure;
    props(m+4).name = "paperorientation";
    props(m+4).value = {get(opts.figure, "paperorientation")};
    props(m+5).h = opts.figure;
    props(m+5).name = "papertype";
    props(m+5).value = {get(opts.figure, "papertype")};
    if (! isempty (opts.canvas_size))
      ## canvas_size is in pixels/points
      set (opts.figure, "paperorientation", "portrait");
      set (opts.figure, "paperposition", [0, 0, opts.canvas_size]);
      set (opts.figure, "paperunits", "points");
      set (opts.figure, "papersize", opts.canvas_size);
      fpos = get (opts.figure, "position");
      props(m+6).h = opts.figure;
      props(m+6).name = "position";
      props(m+6).value = {fpos};
      fpos(3:4) = opts.canvas_size;
      set (opts.figure, "position", fpos);
    elseif (opts.paperoutput)
      ## FIXME - should the backend handle this?
      orient (opts.orientation)
    endif
  endif

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
    if (isempty (opts.ghostscript_output))
      prn_datafile = opts.name;
    else
      prn_datafile = opts.ghostscript_output;
    endif
    if (isempty (opts.printer))
      prn_cmd = sprintf ("lpr %s '%s' 2>&1", opts.lpr_options, prn_datafile);
    else
      prn_cmd = sprintf ("lpr %s -P %s '%s' 2>&1", opts.lpr_options,
                         opts.printer, prn_datafile);
    endif
    if (opts.debug)
      fprintf ("lpr command: %s\n", prn_cmd)
    endif
    [status, output] = system (prn_cmd);
    if (status != 0)
      disp (output)
      warning ("print.m: printing failed.")
    endif
  endif

  ## Append to file using GS
  if (opts.append_to_file)
    if (strcmp (opts.devopt, "pdf"))
      suffix = "pdf";
    elseif (strcmp (opts.devopt(1:2), "ps"))
      suffix = "ps";
    endif
    tmp_combined_file = strcat (tmpnam (), ".", suffix);
    opts.unlink{end+1} = tmp_combined_file;
    gs_opts = "-q -dNOPAUSE -dBATCH";
    gs_cmd = sprintf ("%s %s -sDEVICE=%swrite -sOutputFile=%s %s %s", 
             opts.ghostscript_binary, gs_opts, suffix, tmp_combined_file,
             saved_original_file, opts.name);
    [status, output] = system (gs_cmd);
    if (opts.debug)
      fprintf ("Append files: %s\n", gs_cmd);
    endif
    if (status != 0)
      warning ("print:failedtoappendfile", 
               "print.m: failed to append output to file '%s'.", opts.name)
      movefile (saved_original_file, opts.name);
    else
      movefile (tmp_combined_file, opts.name);
    endif
  endif

  ## Unlink temporary files
  for n = 1:numel(opts.unlink)
    [status, output] = unlink (opts.unlink{n});
    if (status != 0)
      disp (output)
      warning ("print.m: failed to delete temporay file, '%s'.", opts.name)
    endif
  endfor

  if (isfigure (orig_figure))
    figure (orig_figure);
  endif

endfunction
