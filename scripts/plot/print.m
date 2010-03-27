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
## @deftypefn {Function File} {} print ()
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
## @table @code
## @item -P@var{printer}
##   Set the @var{printer} name to which the graph is sent if no
##   @var{filename} is specified.
## @item -G@var{ghostscript_command}
##   Specify the command for calling Ghostscript.  For Unix and Windows,
## the defaults are 'gs' and 'gswin32c', respectively.
## @item -color
## @itemx -mono
##   Monochrome or color lines.
## @item -solid
## @itemx -dashed
##   Solid or dashed lines.
## @item -portrait
## @itemx -landscape
##   Specify the orientation of the plot for printed output.
## @item -d@var{device}
##   Output device, where @var{device} is one of:
##   @table @code
##   @item ps
##   @itemx ps2
##   @itemx psc
##   @itemx psc2
##     Postscript (level 1 and 2, mono and color)
##   @item eps
##   @itemx eps2
##   @itemx epsc
##   @itemx epsc2
##     Encapsulated postscript (level 1 and 2, mono and color)
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
##   @item tikz
##     Generate a @LaTeX{} file using gnuplot's Lua/TikZ terminal.
##   @item ill
##   @itemx aifm
##     Adobe Illustrator
##   @item cdr
##   @itemx corel
##     CorelDraw
##   @item dxf
##     AutoCAD
##   @item emf
##   @itemx meta
##     Microsoft Enhanced Metafile
##   @item fig
##     XFig.  If this format is selected the additional options
##     @code{-textspecial} or @code{-textnormal} can be used to control
##     whether the special flag should be set for the text in
##     the figure (default is @code{-textnormal}). 
##   @item hpgl
##     HP plotter language
##   @item mf
##     Metafont
##   @item png
##     Portable network graphics
##   @item jpg
##   @itemx jpeg
##     JPEG image
##   @item gif
##     GIF image
##   @item pbm
##     PBMplus
##   @item svg
##     Scalable vector graphics
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
##   @item ljet3 
##     HP LaserJet III
##   @item deskjet
##     HP DeskJet and DeskJet Plus
##   @item cdj550
##     HP DeskJet 550C
##   @item paintjet
##     HP PointJet
##   @item pcx24b
##     24-bit color PCX file format
##   @item ppm
##     Portable Pixel Map file format
##   @end table
##
##   For a complete list, type `system ("gs -h")' to see what formats
## and devices are available.
##
##   When the ghostscript is sent to a printer the size is determined
## by the figure's "papersize" property.  When the ghostscript output 
## is sent to a file the size is determined by the figure's
## "paperposition" property.
##
## @itemx -append
##   Appends the output to a pre-existing file. Presently only PDF,
## and Postscript files are supported.
##
## @itemx -r@var{NUM}
##   Resolution of bitmaps in pixels per inch.  For both metafiles and 
## SVG the default is the screen resolution, for other it is 150 dpi.
## To specify screen resolution, use "-r0".
##
## @item -tight
##   Forces a tight bounding box for eps-files.  Since the ghostscript
## devices are conversion of an eps-file, this option works the those
## devices as well.
##
## @itemx -S@var{xsize},@var{ysize}
##   Plot size in pixels for EMF, GIF, JPEG, PBM, PNG and SVG.  If
## using the command form of the print function, you must quote the
## @var{xsize},@var{ysize} option.  For example, by writing
## @w{@code{"-S640,480"}}.  The size defaults to that specified by the
## figure's paperposition property.
##
## @item -F@var{fontname}
## @itemx -F@var{fontname}:@var{size}
## @itemx -F:@var{size}
##   @var{fontname} set the postscript font (for use with postscript,
## aifm, corel and fig).  By default, 'Helvetica' is set for PS/Aifm,
## and 'SwitzerlandLight' for Corel.  It can also be 'Times-Roman'.
## @var{size} is given in points.  @var{fontname} is ignored for the
## fig device.
## @end table
##
## The filename and options can be given in any order.
## @end deftypefn

## Author: Daniel Heiserer <Daniel.heiserer@physik.tu-muenchen.de>
## Adapted-By: jwe

function print (varargin)

  persistent warn_on_inconsistent_orientation = true
  orientation = "";
  use_color = 0; # 0=default, -1=mono, +1=color
  append_to_file = 0;
  force_solid = 0; # 0=default, -1=dashed, +1=solid
  fontsize = "";
  font = "";
  canvas_size = "";
  name = "";
  devopt = "";
  printer = "";
  debug = false;
  debug_file = "octave-print-commands.log";
  special_flag = "textnormal";
  tight_flag = false;
  resolution = "";

  persistent ghostscript_binary = "";
  if (isempty (ghostscript_binary))
    ghostscript_binary = getenv ("GSC");
    ng = 0;
    if (isunix ())
      ## Unix - Includes Mac OSX and Cygwin.
      gs_binaries = {"gs", "gs.exe"};
    else
      ## pc - Includes Win32 and mingw.
      gs_binaries = {"gs.exe", "gswin32c.exe"};
    endif
    while (ng < numel (gs_binaries) && isempty (ghostscript_binary))
      ng = ng + 1;
      ghostscript_binary = file_in_path (EXEC_PATH, gs_binaries{ng});
    endwhile
  endif

  old_fig = get (0, "currentfigure");
  unwind_protect
    ## Ensure the last figure is on the screen for single line commands like
    ##   plot(...); print(...);
    drawnow ();

    for i = 1:nargin
      arg = varargin{i};
      if (ischar (arg))
        if (strcmp (arg, "-color"))
          use_color = 1;
        elseif (strcmp (arg, "-append"))
          append_to_file = 1;
        elseif (strcmp (arg, "-mono"))
          use_color = -1;
        elseif (strcmp (arg, "-solid"))
          force_solid = 1;
        elseif (strcmp (arg, "-dashed"))
          force_solid = -1;
        elseif (strcmp (arg, "-portrait"))
          orientation = "portrait";
        elseif (strcmp (arg, "-landscape"))
          orientation = "landscape";
        elseif (strcmp (arg, "-tight"))
          tight_flag = true;
        elseif (strcmp (arg, "-textspecial"))
          special_flag = "textspecial";
        elseif (strncmp (arg, "-debug", 6))
          debug = true;
          if (length (arg) > 7)
            debug_file = arg(8:end);
          endif
        elseif (length (arg) > 2 && arg(1:2) == "-d")
          devopt = tolower(arg(3:end));
        elseif (length (arg) > 2 && arg(1:2) == "-P")
          printer = arg;
        elseif ((length (arg) > 2) && arg(1:2) == "-G")
          ghostscript_binary = arg(3:end);
          if (exist (ghostscript_binary, "file") != 2)
            ghostscript_binary = file_in_path (EXEC_PATH, ghostscript_binary);
          endif
          if (isempty (ghostscript_binary))
            error ("print.m: Ghostscript binary ""%s"" could not be located", arg(3:end))
          endif
        elseif (length (arg) > 2 && arg(1:2) == "-F")
          idx = rindex (arg, ":");
          if (idx)
            font = arg(3:idx-1);
            fontsize = arg(idx+1:length(arg));
          else
            font = arg(3:length(arg));
          endif
        elseif (length (arg) > 2 && arg(1:2) == "-S")
          canvas_size = arg(3:length(arg));
        elseif (length (arg) > 2 && arg(1:2) == "-r")
          resolution = arg(3:length(arg));
        elseif (length (arg) >= 1 && arg(1) == "-")
          error ("print: unknown option `%s'", arg);
        elseif (length (arg) > 0)
          name = arg;
        endif
      elseif (isfigure (arg))
        figure (arg);
      else
        error ("print: expecting inputs to be character string options or a figure handle");
      endif
    endfor

    have_ghostscript = (exist (ghostscript_binary, "file") == 2);

    doprint = isempty (name);
    if (doprint)
      if (isempty (devopt))
        if (use_color < 0)
          devopt = "ps";
          printname = cstrcat (tmpnam, ".ps");
        else
          devopt = "psc";
          printname = cstrcat (tmpnam, ".psc");
        endif
      else
        printname = cstrcat (tmpnam, ".", devopt);
      endif
      name = printname;
    endif

    dot = rindex (name, ".");
    if (isempty (devopt))
      if (dot == 0)
        error ("print: no format specified");
      else
        dev = tolower (name(dot+1:end));
      endif
    else
      dev = devopt;
    endif

    dev_list = {"aifm", "corel", "fig", "png", "jpeg", ...
                "gif", "pbm", "dxf", "mf", "svg", "hpgl", ...
                "ps", "ps2", "psc", "psc2", "eps", "eps2", ...
                "epsc", "epsc2", "emf", "pdf", "pslatex", ...
                "epslatex", "epslatexstandalone", "pstex", "tikz"};
    suffixes = {"ai", "cdr", "fig", "png", "jpeg", ...
                "gif", "pbm", "dxf", "mf", "svg", "hpgl", ...
                "ps", "ps", "ps", "ps", "eps", "eps", ...
                "eps", "eps", "emf", "pdf", "tex", ...
                "tex", "tex", "tex", "tikz"};
    if (dot == 0)
      name = strcat (name, ".", suffixes {strcmpi (dev_list, dev)});
      dot = rindex (name, ".");
    endif

    if (append_to_file)
      if (any (strcmpi (dev, {"ps", "ps2", "psc", "psc2", "pdf"})))
        if (have_ghostscript)
          file_exists = ((numel (dir (name)) == 1) && (! isdir (name)));
          if (! file_exists)
            append_to_file = 0;
          end
        end
      else
        warning ("print.m: appended output is not supported for device '%s'", dev)
        append_to_file = 0;
      endif
    endif

    if (strcmp (dev, "tex"))
      dev = "epslatex";
    elseif (strcmp (dev, "ill"))
      dev = "aifm";
    elseif (strcmp (dev, "cdr"))
      dev = "corel";
    elseif (strcmp (dev, "meta"))
      dev = "emf";
    elseif (strcmp (dev, "jpg"))
      dev = "jpeg";
    endif

    if (strcmp (dev, "epslatex"))
      ## gnuplot 4.0 wants ".eps" in the output name    
      if (! __gnuplot_has_feature__ ("epslatex_implies_eps_filesuffix"))
        name = cstrcat (name(1:dot), "eps");
      endif
    endif

    ## Check if the specified device is one that is supported by gnuplot.
    ## If not, assume it is a device/format supported by Ghostscript.
    if (! any (strcmp (dev, dev_list)) && have_ghostscript)
      ghostscript_output = name;
      ghostscript_device = dev;
      if (doprint)
        ## If printing, use color postscript.
        dev = "psc";
        name = cstrcat (tmpnam, ".ps");
      else
        ## If saving to a file, use color encapsulated postscript.
        dev = "epsc";
        name = cstrcat (tmpnam, ".eps");
      endif
    elseif (doprint && all (! strcmpi (suffixes {strcmpi (dev_list, dev)}, 
                                       {"ps", "eps", "hpgl"})))
      ## When not using Ghostscript, verify the format is compatible with
      ## hard copy output.
      error ("print: missing file name, or invalid print format.");
    else
      ghostscript_output = "";
    endif

    termn = dev;

    ## SVG isn't actually a bitmap, but gnuplot treats its size option as it
    ## does the bitmap terminals.
    bitmap_devices = {"emf", "gif", "jpeg", "pbm", "png", "svg"};

    if (any (strcmp (dev, {"ps", "ps2", "psc", "psc2", "epsc", "epsc2", ...
                           "eps", "eps2", "pstex", "pslatex", "epslatex", ...
                           "epslatexstandalone"})))

      ## Various postscript options
      if (any (strcmp (dev, {"pstex", "pslatex", "epslatex"})))
        options = "";
      elseif (strcmp (dev, "epslatexstandalone"))
        if (__gnuplot_has_feature__ ("epslatexstandalone_terminal"))
          termn = "epslatex";
          options = "standalone ";
        else
          error ("print: epslatexstandalone needs gnuplot 4.2 or higher");
        endif
      else
        if (dev(1) == "e")
          options = "eps ";
        else
          options = "";
        endif
        termn = "postscript";
      endif

      if (any (dev == "c") || use_color > 0
          || (! isempty (strfind (dev, "tex")) && use_color == 0))
        use_color = 1;
      else
        use_color = -1;
      endif
      
      if (use_color > 0)
        if (force_solid < 0)
          options = cstrcat (options, "color dashed ");
        else
          options = cstrcat (options, "color solid ");
        endif
      else
        if (force_solid > 0)
          options = cstrcat (options, "mono solid ");
        else
          options = cstrcat (options, "mono dashed ");
        endif
      endif

      if (! isempty (font))
        options = cstrcat (options, "\"", font, "\" ");
      endif
      if (! isempty (fontsize))
        options = cstrcat (options, " ", fontsize);
      endif

    elseif (strcmp (dev, "tikz"))
      if (! isempty (font) && ! isempty (fontsize))
        options = cstrcat (options, "\"", font, ",", fontsize, "\" ");
      elseif (! isempty (font))
        options = cstrcat (options, "\"", font, "\" ");
      else
        options = "";
      endif

    elseif (strcmp (dev, "aifm") || strcmp (dev, "corel"))
      ## Adobe Illustrator, CorelDraw
      if (use_color >= 0)
        options = " color";
      else
        options = " mono";
      endif
      if (! isempty (font))
        options = cstrcat (options, " \"", font, "\"");
      endif
      if (! isempty (fontsize))
        options = cstrcat (options, " ", fontsize);
      endif

    elseif (strcmp (dev, "fig"))
      ## XFig
      options = orientation;
      if (use_color >= 0)
        options = " color";
      else
        options = " mono";
      endif
      options = cstrcat (options, " ", special_flag);
      if (! isempty (fontsize))
        options = cstrcat (options, " fontsize ", fontsize);
      endif

    elseif (strcmp (dev, "emf"))
      ## Enhanced Metafile format
      options = " ";
      if (use_color >= 0)
        options = " color";
      else
        options = " mono";
      endif
      if (force_solid >= 0)
        options = cstrcat (options, " solid");
      endif
      if (! isempty (font))
        options = cstrcat (options, " \"", font, "\"");
      endif
      if (! isempty (fontsize))
        options = cstrcat (options, " ", fontsize);
      endif

    elseif (any (strcmp (dev, bitmap_devices)))

      if (isempty (canvas_size) && isempty (resolution) 
          && any (strcmp (dev, {"pbm", "gif", "jpeg", "png"})))
        options = "";
      elseif (strcmp (dev, "svg"))
        ## Referring to size, either "dynamic" or "fixed"
        options = "fixed";
      else
        options = "";
      end
      if (! isempty (canvas_size))
        options = cstrcat (options, " size ", canvas_size);
      endif

    elseif (any (strcmp (dev, {"dxf", "mf", "hpgl"})))
      ## AutoCad DXF, METAFONT, HPGL
      options = "";

    elseif (strcmp (dev, "pdf"))
      ## Portable Document format
      options = " ";
      if (use_color >= 0)
        options = "color";
      else
        options = "mono";
      endif
      if (force_solid >= 0)
        options = cstrcat (options, " solid");
      elseif (force_solid < 0)
        options = cstrcat (options, " dashed");
      endif
      if (! isempty (font))
        options = cstrcat (options, "\"", font, "\" ");
      endif
      if (! isempty (fontsize))
        options = cstrcat (options, " ", fontsize);
      endif

    endif
 
    if (__gnuplot_has_feature__ ("variable_GPVAL_TERMINALS"))
      available_terminals = __gnuplot_get_var__ (gcf, "GPVAL_TERMINALS");
      available_terminals = regexp (available_terminals, "\\b\\w+\\b", "match");
      ## Favor the cairo terminals.
      if (strcmp (termn, "pdf") 
          && any (strcmp (available_terminals, "pdfcairo")))
        termn = "pdfcairo";
        gnuplot_supports_term = true;
      elseif (strcmp (termn, "png")
              && any (strcmp (available_terminals, "pngcairo")))
        termn = "pngcairo";
        gnuplot_supports_term = true;
      else
        gnuplot_supports_term = any (strcmp (available_terminals, termn));
      endif
    elseif (strcmp (termn, "pdf"))
      ## Some Linux variants do not include a "pdf" capable gnuplot.
      ## To be safe, use Ghostscript.
      if (have_ghostscript)
        gnuplot_supports_term = false;
        ghostscript_device = "pdfwrite";
      else
        gnuplot_supports_term = true;
      endif
    else
      gnuplot_supports_term = true;
    endif

    if (! gnuplot_supports_term)
      if (strcmp (termn, "pdf"))
        ## If there the installed gnuplot does not support pdf, use Ghostscript.
        ghostscript_device = "pdfwrite";
        if (strfind (name, ".pdf") == numel (name) - 3)
          ghostscript_output = name;
        else
          ghostscript_output = strcat (name, ".pdf");
        endif
        name = cstrcat (tmpnam, ".ps");
        termn = "postscript";
        ## All "options" for pdf work for postscript as well.
      else
        error ("print: the device, \"%s\", is not available.", dev)
      endif
    endif

    is_eps_file = strncmp (dev, "eps", 3);
    p.units = get (gcf, "units");
    p.paperunits = get (gcf, "paperunits");
    p.papersize = get (gcf, "papersize");
    p.paperposition = get (gcf, "paperposition");
    p.paperpositionmode = get (gcf, "paperpositionmode");
    p.paperorientation = get (gcf, "paperorientation");
    if (p.papersize(1) > p.papersize(2))
      paperorientation = "landscape";
    else
      paperorientation = "portrait";
    endif
    if (! strcmp (paperorientation, get (gcf, "paperorientation"))
        && warn_on_inconsistent_orientation)
       msg = {"print.m - inconsistent papersize and paperorientation properties.\n",
               sprintf("         papersize = %.2f, %.2f\n", p.papersize),
               sprintf("         paperorientation = \"%s\"\n", p.paperorientation),
                       "         the paperorientation property has been ignored"};
      warning ("%s",msg{:})
      warn_on_inconsistent_orientation = false;
    endif

    if (strcmp (termn, "postscript") && ! strncmp (dev, "eps", 3))
      if (isempty (orientation))
        orientation = paperorientation;
      endif
      ## This is done here to accommodate ghostscript conversion.
      options = cstrcat (orientation, " ", options);
    end

    new_terminal = cstrcat (termn, " ", options);

    mono = (use_color < 0);

    terminals_for_prn = {"postscript", "pdf", "pdfcairo"};
    output_for_printer = any (strncmp (termn, terminals_for_prn, numel(termn)));

    if (isempty (resolution))
      if (any (strcmp (dev, {"emf", "svg"})) || output_for_printer)
        resolution = get (0, "screenpixelsperinch");
      else
        resolution = 150;
      endif
    else
      resolution = str2num (resolution);
      if (resolution == 0)
        resolution = get (0, "screenpixelsperinch");
      endif
    endif
    figure_properties = get (gcf);
    if (! isfield (figure_properties, "__pixels_per_inch__"))
      addproperty ("__pixels_per_inch__", gcf, "double", resolution);
    endif
    set (gcf, "__pixels_per_inch__", resolution)

    unwind_protect
      set (gcf, "paperunits", "inches");
      set (gcf, "units", "pixels");
      restore_properties = true;
      if ((! output_for_printer || is_eps_file) && ! doprint)
        ## If not PDF or PostScript, and the result is not being sent to a printer,
        ## render an image the size of the paperposition box.
        ## Trigger the listener to convert all paper props to inches.
        if (! isempty (canvas_size))
          size_in_pixels = sscanf (canvas_size ,"%d, %d");
          size_in_pixels = reshape (size_in_pixels, [1, numel(size_in_pixels)]);
          papersize_in_inches = size_in_pixels ./ resolution;
          paperposition_in_inches = [0, 0, papersize_in_inches];
        else
          paperposition_in_inches = get (gcf, "paperposition");
          paperposition_in_inches(1:2) = 0;
          papersize_in_inches = paperposition_in_inches(3:4);
        endif
        set (gcf, "papersize", papersize_in_inches);
        set (gcf, "paperposition", paperposition_in_inches);
        set (gcf, "paperpositionmode", "manual");
      else
        if (strcmp (p.paperpositionmode, "auto"))
          size_in_pixels = get (gcf, "position")(3:4);
          paperposition_in_inches(3:4) = size_in_pixels ./ resolution;
          paperposition_in_inches(1:2) = (p.papersize - paperposition_in_inches(3:4))/2;
        else
          paperposition_in_inches = p.paperposition;
        endif
        if (! isempty (orientation) && ! strcmp (orientation, paperorientation))
          ## When -landscape/portrait changes the orientation, flip both the
          ## papersize and paperposition.
          restore_properties = true;
          set (gcf, "papersize", p.papersize([2, 1]));
          set (gcf, "paperposition", paperposition_in_inches([2, 1, 4, 3]));
        else
          set (gcf, "paperposition", paperposition_in_inches);
        endif
      endif
      if (use_color < 0)
        [objs_with_color, color_of_objs] = convert_color2mono (gcf);
      endif
    if (append_to_file)
         appended_file_name = name;
         if (index(termn, "pdf"))
           name = cstrcat (tmpnam, ".pdf");
           temp_name = cstrcat (tmpnam, ".pdf");
           ghostscript_device = "pdfwrite";
         else
           name = cstrcat (tmpnam, ".ps");
           temp_name = cstrcat (tmpnam, ".ps");
           ghostscript_device = "pswrite";
         endif
    endif
      if (debug)
        drawnow (new_terminal, name, mono, debug_file);
      else
        drawnow (new_terminal, name, mono);
      endif
      if (append_to_file)
        ghostscript_options = "-q -dBATCH -dSAFER -dNOPAUSE";
        command = sprintf ("%s %s -sDEVICE=%s -sOutputFile=%s %s %s -q", ...
                    ghostscript_binary, ghostscript_options, ghostscript_device,  ...
                    temp_name, appended_file_name, name);
        status1 = system (command);
        status2 = system (sprintf ("mv %s %s", temp_name, appended_file_name));
        if (status1 != 0 || status2 != 0)
          error ("print.m: output failed to append to '%s'.", appended_file_name);
        endif
      endif
    unwind_protect_cleanup
      ## FIXME - it would be nice to delete "__pixels_per_inch__" property here.
      if (restore_properties)
        props = fieldnames (p);
        for n = 1:numel(props)
          set (gcf, props{n}, p.(props{n}))
        endfor
      endif
      if (use_color < 0)
        convert_mono_to_or_from_color (objs_with_color, color_of_objs, false);
      endif
    end_unwind_protect

    if (! isempty (ghostscript_output))
      if (is_eps_file && tight_flag)
        ## If gnuplot's output is an eps-file then crop at the bounding box.
        fix_eps_bbox (name, ghostscript_binary);
      endif
      ghostscript_options = "-q -dBATCH -dSAFER -dNOPAUSE -dTextAlphaBits=4";
      if (is_eps_file)
        ghostscript_options = sprintf ("%s -dEPSCrop", ghostscript_options);
      endif
      if (isempty (strfind (lower (ghostscript_device), "write")))
        ## If output is a bitmap then include the resolution
        ghostscript_options = sprintf ("%s -r%d", ghostscript_options, resolution);
      endif
      ghostscript_options = sprintf ("%s -sDEVICE=%s", ghostscript_options,
                                     ghostscript_device);
      command = sprintf ("\"%s\" %s -sOutputFile=\"%s\" \"%s\" 2>&1", ghostscript_binary,
                          ghostscript_options, ghostscript_output, name);
      [errcode, output] = system (command);
      unlink (name);
      if (errcode)
        error ("print: Conversion failed, %s -> %s.\nError was:\n%s\n",
               name, ghostscript_output, output);
      endif
    elseif (is_eps_file && tight_flag && ! doprint)
      ## If the saved output file is an eps file, use ghostscript to set a tight bbox.
      ## This may result in a smaller or larger bbox geometry.
      if (have_ghostscript)
        fix_eps_bbox (name, ghostscript_binary);
      endif
    endif

    if (doprint)
      if (isunix ())
        prn_opt = "-l";
      elseif (ispc ())
        prn_opt = "-o l";
      else
        ## FIXME - besides Unix and Windows, what other OS's might be considered.
        prn_opt = "";
      endif
      if (isempty (printer))
        prn_cmd = sprintf ("lpr %s '%s' 2>&1", prn_opt, printname);
      else
        prn_cmd = sprintf ("lpr %s -P %s '%s' 2>&1", prn_opt, printer, printname);
      endif
      [status, output] = system (prn_cmd);
      if (status != 0)
        disp (output)
        warning ("print.m: printing failed.")
      endif
      [status, output] = unlink (printname);
      if (status != 0)
        disp (output)
        warning ("print.m: failed to delete temporay file, '%s'.", printname)
      endif
    endif

  unwind_protect_cleanup
    if (isfigure (old_fig))
      figure (old_fig)
    endif
  end_unwind_protect

endfunction

function bb = fix_eps_bbox (eps_file_name, ghostscript_binary)

  persistent warn_on_no_ghostscript = true

  box_string = "%%BoundingBox:";

  ghostscript_options = "-q -dBATCH -dSAFER -dNOPAUSE -dTextAlphaBits=4 -sDEVICE=bbox";
  cmd = sprintf ("\"%s\" %s \"%s\" 2>&1", ghostscript_binary,
                 ghostscript_options, eps_file_name);
  [status, output] = system (cmd);

  if (status == 0)

    pattern = strcat (box_string, "[^%]*");
    pattern = pattern(1:find(double(pattern)>32, 1, "last"));
    bbox_line = regexp (output, pattern, "match");
    if (iscell (bbox_line))
      bbox_line = bbox_line{1};
    endif
    ## Remore the EOL characters.
    bbox_line(double(bbox_line)<32) = "";

    fid = fopen (eps_file_name, "r+");
    unwind_protect
      bbox_replaced = false;
      while (! bbox_replaced)
        current_line = fgetl (fid);
        if (strncmpi (current_line, box_string, numel(box_string)))
          line_length = numel (current_line);
          num_spaces = line_length - numel (bbox_line);
          if (numel (current_line) < numel (bbox_line))
            ## If there new line is longer, continue with the current line.
            new_line = current_line;
          else
            new_line = bbox_line;
            new_line(end+1:numel(current_line)) = " ";
          endif
          ## Back up to the beginning of the line (include EOL characters).
          if (ispc ())
            fseek (fid, -line_length-2, "cof");
          else
            fseek (fid, -line_length-1, "cof");
          endif
          count = fprintf (fid, "%s", new_line);
          bbox_replaced = true;
        elseif (! ischar (current_line))
          bbox_replaced = true;
          warning ("print.m: no bounding box found in '%s'.", eps_file_name)
        endif
      endwhile
    unwind_protect_cleanup
      fclose (fid);
    end_unwind_protect
  elseif (warn_on_no_ghostscript)
    warn_on_no_ghostscript = false;
    warning ("print.m: Ghostscript failed to determine the bounding box.\nError was:\n%s\n", output)
  endif

endfunction

function [h, c] = convert_color2mono (hfig)
  unwind_protect
    showhiddenhandles = get (0, "showhiddenhandles");
    set (0, "showhiddenhandles", "on");
    h.color = findobj (hfig, "-property", "color");
    h.facecolor = findobj (hfig, "-property", "facecolor");
    h.edgecolor = findobj (hfig, "-property", "edgecolor");
    h.backgroundcolor = findobj (hfig, "-property", "backgroundcolor");
    h.colormap = findobj (hfig, "-property", "colormap");
  unwind_protect_cleanup
    set (0, "showhiddenhandles", showhiddenhandles);
  end_unwind_protect
  f = fieldnames (h);
  for nf = 1:numel(f)
    if (! isempty (h.(f{nf})))
      v = get (h.(f{nf}), f{nf});
      if (! iscell (v))
        v = {v};
      endif
      c.(f{nf}) = v;
    endif
  endfor
  convert_mono_to_or_from_color (h, c, true)
endfunction

function convert_mono_to_or_from_color (h, c, mono)
  f = fieldnames (h);
  for nf = 1:numel(f)
    for nh = 1:numel (h.(f{nf}))
      color = c.(f{nf}){nh};
      ## Ignore color == {"none", "flat", ...}
      if (isfloat (color))
        if (mono)
          ## Same method as used by rgb2gray in the image pkg.
          color = rgb2ntsc (color)(:,1) * ones (1, 3);
        endif
        set (h.(f{nf})(nh), f{nf}, color);
      endif
    endfor
  endfor
endfunction

