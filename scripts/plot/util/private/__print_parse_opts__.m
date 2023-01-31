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
## @deftypefn  {} {@var{args} =} __print_parse_opts__ (@var{propname}, @var{propvalue})
## @deftypefnx {} {@var{args} =} __print_parse_opts__ (@var{struct})
## Undocumented internal function.
## @end deftypefn

function arg_st = __print_parse_opts__ (varargin)

  persistent warn_on_missing_ghostscript = true;

  arg_st.append_to_file = false;
  arg_st.canvas_size = [];
  arg_st.debug = false;
  arg_st.debug_file = "octave-print-commands.log";
  arg_st.devopt = "";
  arg_st.epstool_binary = __quote_path__ (__find_binary__ ("epstool"));
  arg_st.figure = get (0, "currentfigure");
  arg_st.fig2dev_binary = __quote_path__ (__find_binary__ ("fig2dev"));
  arg_st.fontsize = [];
  arg_st.font = "";
  arg_st.scalefontsize = 1;
  arg_st.force_solid = 0; # 0=default, -1=dashed, +1=solid
  arg_st.formatted_for_printing = false;
  arg_st.ghostscript.binary = __quote_path__ (__ghostscript_binary__ ());
  arg_st.ghostscript.debug = false;
  arg_st.ghostscript.device = "";
  arg_st.ghostscript.epscrop = true;
  arg_st.ghostscript.level = 2;
  arg_st.ghostscript.output = "";
  arg_st.ghostscript.papersize = "letter";
  arg_st.ghostscript.pageoffset = [];
  arg_st.ghostscript.resolution = 150;
  arg_st.ghostscript.antialiasing = false;
  arg_st.ghostscript.antialiasing_textalphabits = 4;
  arg_st.ghostscript.antialiasing_graphicsalphabits = 1;
  arg_st.lpr_binary = __quote_path__ (__find_binary__ ("lpr"));
  arg_st.polymerge = 1;
  arg_st.name = "";
  arg_st.orientation = "";
  arg_st.pstoedit_binary = __quote_path__ (__find_binary__ ("pstoedit"));
  arg_st.preview = "";
  arg_st.printer = "";
  arg_st.renderer = "auto";
  arg_st.resize_flag = "";
  arg_st.rgb_output = false;
  arg_st.send_to_printer = false;
  arg_st.special_flag = "textnormal";
  arg_st.svgconvert = true;
  arg_st.svgconvert_binary = __quote_path__ (__svgconv_binary__ ());
  arg_st.tight = true;
  arg_st.use_color = 0; # 0=default, -1=mono, +1=color

  if (isunix ())
    arg_st.lpr_options = "-l";
  elseif (ispc ())
    arg_st.lpr_options = "-o l";
  else
    arg_st.lpr_options = "";
  endif
  arg_st.unlink = {};

  if (nargin > 0 && isfigure (varargin{1}))
    arg_st.figure = varargin{1};
    varargin(1) = [];
  endif

  if (! isempty (findall (arg_st.figure, "type", "patch", ...
                          "-or", "type", "surface")))
    arg_st.polymerge = 2;
  endif

  for i = 1:numel (varargin)
    if (! ischar (varargin{i}) && ! iscellstr (varargin{i}))
      error ("print: input arguments must be a graphics handle or strings.");
    endif
    arg = strtrim (varargin{i});
    if (ischar (arg))
      if (isempty (arg))
        continue;
      elseif (strcmp (arg, "-color"))
        arg_st.use_color = 1;
      elseif (strcmp (arg, "-append"))
        arg_st.append_to_file = true;
      elseif (strcmp (arg, "-mono"))
        arg_st.use_color = -1;
      elseif (strcmp (arg, "-solid"))
        arg_st.force_solid = 1;
      elseif (strcmp (arg, "-dashed"))
        arg_st.force_solid = -1;
      elseif (any (strcmp (arg, {"-opengl", "-painters"})))
        arg_st.renderer = arg(2:end);
      elseif (strcmp (arg, "-image"))
        arg_st.renderer = "opengl";
      elseif (strcmp (arg, "-vector"))
        arg_st.renderer = "painters";
      elseif (strcmp (arg, "-RGBImage"))
        arg_st.rgb_output = true;
        arg_st.renderer = "opengl";
      elseif (strncmp (arg, "-portrait", length (arg)))
        arg_st.orientation = "portrait";
      elseif (strncmp (arg, "-landscape", length (arg)))
        arg_st.orientation = "landscape";
      elseif (strcmp (arg, "-loose"))
        arg_st.tight = false;
      elseif (strcmp (arg, "-tight"))
        arg_st.tight = true;
      elseif (strcmp (arg, "-svgconvert"))
        arg_st.svgconvert = true;
      elseif (strcmp (arg, "-nosvgconvert"))
        arg_st.svgconvert = false;
      elseif (strcmp (arg, "-polymerge"))
        arg_st.polymerge = 1;
      elseif (strcmp (arg, "-nopolymerge"))
        arg_st.polymerge = 0;
      elseif (strcmp (arg, "-polymerge-all"))
        arg_st.polymerge = 2;
      elseif (strcmp (arg, "-textspecial"))
        arg_st.special_flag = "textspecial";
      elseif (strcmp (arg, "-fillpage"))
        arg_st.resize_flag = "fillpage";
      elseif (strcmp (arg, "-bestfit"))
        arg_st.resize_flag = "bestfit";
      elseif (any (strcmp (arg,
                           {"-interchange", "-metafile", "-pict", "-tiff"})))
        arg_st.preview = arg(2:end);
      elseif (strncmp (arg, "-debug", 6))
        arg_st.debug = true;
        arg_st.ghostscript.debug = true;
        if (length (arg) > 7)
          arg_st.debug_file = arg(8:end);
        endif
      elseif (length (arg) > 2 && arg(1:2) == "-d")
        arg_st.devopt = tolower (arg(3:end));
      elseif (length (arg) > 2 && arg(1:2) == "-P")
        arg_st.printer = arg;
      elseif (strncmp (arg, "-EPSTOOL:", 9))
        arg_st.epstool_binary = arg(10:end);
      elseif (strncmp (arg, "-FIG2DEV:", 9))
        arg_st.fig2dev_binary = arg(10:end);
      elseif (strncmp (arg, "-PSTOEDIT:", 9))
        arg_st.pstoedit_binary = arg(10:end);
      elseif (strncmpi (arg, "-textalphabits=", 15))
        if (length (arg) == 16 && any (arg(end) == "124"))
          arg_st.ghostscript.antialiasing_textalphabits = str2double (arg(end));
        else
          error ("print: improper syntax, or value, for TextAlphaBits");
        endif
      elseif (strncmpi (arg, "-graphicsalphabits=", 19))
        if (numel (arg) == 20 && any (arg(end) == "124"))
          arg_st.ghostscript.antialiasing_graphicsalphabits = str2double (arg(end));
        else
          error ("print: improper syntax, or value, for GraphicsAlphaBits");
        endif
      elseif (length (arg) > 2 && arg(1:2) == "-G")
        arg_st.ghostscript.binary = file_in_path (getenv ("PATH"), arg(3:end));
        if (isempty (arg_st.ghostscript.binary))
          error ('print: Ghostscript binary "%s" not found in PATH',
                 arg(3:end));
        endif
        arg_st.ghostscript.binary = __quote_path__ (arg_st.ghostscript.binary);
      elseif (length (arg) > 2 && arg(1:2) == "-F")
        idx = rindex (arg, ":");
        if (idx)
          arg_st.font = arg(3:idx-1);
          arg_st.fontsize = str2double (arg(idx+1:end));
        else
          arg_st.font = arg(3:end);
        endif
      elseif (length (arg) > 2 && arg(1:2) == "-S")
        arg_st.canvas_size = str2double (strsplit (arg(3:end), ","));
      elseif (length (arg) > 2 && arg(1:2) == "-r")
        arg_st.ghostscript.resolution = str2double (arg(3:end));
      elseif (length (arg) > 2 && arg(1:2) == "-f")
        arg_st.figure = str2double (arg(3:end));
      elseif (strcmp (arg, "-noui"))
        ## Accepted, but nothing needs to be done since Octave already
        ## excludes uicontrol objects when printing.
      elseif (length (arg) >= 1 && arg(1) == "-")
        error ("print: unknown option '%s'", arg);
      elseif (length (arg) > 0)
        arg_st.name = tilde_expand (arg);
      endif
    elseif (isfigure (arg))
      arg_st.figure = arg;
    else
      error ("print: first argument must be string or figure handle");
    endif
  endfor

  ## Resolution
  if (arg_st.ghostscript.resolution == 0)
    ## Do as Matlab does.
    arg_st.ghostscript.resolution = get (0, "screenpixelsperinch");
  endif

  ## Orientation
  if (isempty (arg_st.orientation))
    if (isfigure (arg_st.figure))
      arg_st.orientation = get (arg_st.figure, "paperorientation");
    else
      ## Allows tests to be run without error.
      arg_st.orientation = "portrait";
    endif
  endif

  ## The device is infered from extension if not provided
  dot = rindex (arg_st.name, ".");
  if (isempty (arg_st.devopt))
    if (arg_st.rgb_output)
      arg_st.devopt = "png";
    elseif (dot == 0)
      arg_st.devopt = "psc";
    else
      arg_st.devopt = tolower (arg_st.name(dot+1:end));
    endif
  endif

  ## Warn about deprecated output formats
  persistent unsupported = {"aifm", "ill","cdr", "corel", ...
                            "hpgl", "mf", "cgm", "dxf"}

  if (any (strcmp (unsupported, arg_st.devopt)))
    warning ('Octave:print:deprecated-format',
             'print: "%s" format is no more officially supported', ...
             arg_st.devopt);
  endif

  ## By default, use the "opengl" renderer for all raster outputs
  ## supported by "imwrite".
  fmts = imformats ();
  persistent gl_devlist = [fmts(! cellfun (@isempty, {fmts.write})).ext, ...
                           "tiffn"];
  opengl_ok = any (strcmp (gl_devlist, arg_st.devopt));

  if (strcmp (arg_st.renderer, "auto")
      && strcmp (get (arg_st.figure, "renderermode"), "manual"))
    arg_st.renderer = get (arg_st.figure, "renderer");
  endif

  if (strcmp (arg_st.renderer, "auto"))
    if (opengl_ok && strcmp (graphics_toolkit (arg_st.figure), "qt"))
      arg_st.renderer = "opengl";
    else
      arg_st.renderer = "painters";
    endif
  elseif (strcmp (arg_st.renderer, "opengl") && ! opengl_ok)
    arg_st.renderer = "painters";
    warning (['print: unsupported output format "%s" for renderer ', ...
              '"opengl".'], arg_st.devopt);
  elseif (! strcmp (graphics_toolkit (arg_st.figure), "qt")
          && strcmp (arg_st.renderer, "opengl"))
    ## The opengl renderer only works with the "qt" toolkit
    arg_st.renderer = "painters";
    warning ('Octave:print:unsupported-renderer',
             'print: "opengl" renderer unsupported for "%s" toolkit',
             graphics_toolkit (arg_st.figure));
  endif


  if (arg_st.use_color == 0)
    if (any (strcmp ({"ps", "ps2", "eps", "eps2"}, arg_st.devopt)))
      arg_st.use_color = -1;
    else
      arg_st.use_color = 1;
    endif
  endif

  if (strcmp (arg_st.devopt, "tex"))
    arg_st.devopt = "epslatex";
  elseif (strcmp (arg_st.devopt, "ill"))
    arg_st.devopt = "aifm";
  elseif (strcmp (arg_st.devopt, "cdr"))
    arg_st.devopt = "corel";
  elseif (strcmp (arg_st.devopt, "meta"))
    arg_st.devopt = "emf";
  elseif (strcmp (arg_st.devopt, "jpg"))
    arg_st.devopt = "jpeg";
  elseif (strcmp (arg_st.devopt, "tif"))
    arg_st.devopt = "tiff";
  elseif (strcmp (arg_st.devopt, "pdfcrop"))
    arg_st.devopt = "pdfwrite";
  endif

  persistent dev_list = [{"aifm", "corel", "dumb", "fig", "png", "jpeg", ...
              "gif", "pbm", "pbmraw", "dxf", "mf", ...
              "svg", "hpgl", "ps", "ps2", "psc", ...
              "psc2", "eps", "eps2", "epsc", "epsc2", ...
              "emf", "pdf", "pslatex", "epslatex", "epslatexstandalone", ...
              "pslatexstandalone", "pdflatexstandalone", ...
              "pstex", "tiff", "tiffn", "tikz", "tikzstandalone", "pcxmono", ...
              "pcx24b", "pcx256", "pcx16", "pgm", "pgmraw", ...
              "ppm", "ppmraw", "pdflatex", "texdraw", ...
              "epscairo", "pdfcairo", "pngcairo", "cairolatex", ...
              "pdfcairolatex", "pdfcairolatexstandalone", ...
              "epscairolatex", "epscairolatexstandalone", "pstricks", ...
              "epswrite", "eps2write", "pswrite", "ps2write", "pdfwrite", ...
              "canvas", "cgm", "latex", "eepic"}, gl_devlist];

  persistent suffixes = [{"ai", "cdr", "txt", "fig", "png", "jpg", ...
              "gif", "pbm", "pbm", "dxf", "mf", ...
              "svg", "hpgl", "ps", "ps", "ps", ...
              "ps", "eps", "eps", "eps", "eps", ...
              "emf", "pdf", "tex", "tex", "tex", ...
              "tex", "tex", ...
              "ps", "tiff", "tiff", "tikz", "tikz", "pcx", ...
              "pcx", "pcx", "pcx", "pgm", "pgm", ...
              "ppm", "ppm", "tex", "tex", ...
              "eps", "pdf", "png", "tex", ...
              "tex", "tex", ...
              "tex", "tex", "tex", ...
              "eps", "eps", "ps", "ps", "pdf", ...
              "js", "cgm", "tex", "tex"}, gl_devlist];

  if (isfigure (arg_st.figure))
    __graphics_toolkit__ = get (arg_st.figure, "__graphics_toolkit__");
  else
    ## Allow tests when no figures are present.
    __graphics_toolkit__ = get (0, "defaultfigure__graphics_toolkit__");
  endif

  if (strcmp (__graphics_toolkit__, "gnuplot"))
    suffixes(strncmp (dev_list, "epslatex", 8)) = {"eps"};
  endif

  unknown_device = true;
  default_suffix = "";
  match = strcmpi (dev_list, arg_st.devopt);
  if (any (match))
    unknown_device = false;
    default_suffix = suffixes{match};
  endif

  if (dot == 0 && ! isempty (arg_st.name) && ! isempty (default_suffix))
    arg_st.name = [arg_st.name "." default_suffix];
  endif

  if (arg_st.append_to_file)
    if (isempty (arg_st.name))
      arg_st.append_to_file = false;
    elseif (any (strcmpi (arg_st.devopt, {"eps", "eps2", "epsc", "epsc2", ...
                                          "ps", "ps2", "psc", "psc2", "pdf"})))
      have_ghostscript = ! isempty (__ghostscript_binary__ ());
      if (have_ghostscript)
        file_exists = (numel (dir (arg_st.name)) == 1
                       && ! isfolder (arg_st.name));
        if (! file_exists)
          arg_st.append_to_file = false;
        endif
      else
        arg_st.append_to_file = false;
        warning ("print: appended output requires Ghostscript to be installed");
      endif
    else
      warning ("print: appended output is not supported for device '%s'",
               arg_st.devopt);
      arg_st.append_to_file = false;
    endif
  endif

  if (arg_st.rgb_output)
    if (! isempty (arg_st.printer) || ! isempty (arg_st.name))
      warning ("Octave:print:ignored_argument",
               "print: ignoring file name and printer argument when using -RGBImage option");
    endif
  elseif (! isempty (arg_st.printer) || isempty (arg_st.name))
    arg_st.send_to_printer = true;
  endif

  if (any (strcmp (arg_st.devopt, {"ps", "ps2", "psc", "psc2", "pdf"})))
    arg_st.formatted_for_printing = true;
  endif

  aliases = gs_aliases (arg_st.svgconvert);
  if (any (strcmp (arg_st.devopt, fieldnames (aliases)))
      && ! strcmp (arg_st.renderer, "opengl"))
    arg_st.devopt = aliases.(arg_st.devopt);
    unknown_device = false;
  endif

  if ((any (strcmp (arg_st.devopt, gs_device_list))
       && ! arg_st.formatted_for_printing)
      || any (strcmp (arg_st.devopt, {"pswrite", "ps2write", "pdfwrite"})))
    unknown_device = false;
    ## Use ghostscript for graphic formats
    arg_st.ghostscript.device = arg_st.devopt;
    arg_st.ghostscript.output = arg_st.name;
    arg_st.ghostscript.antialiasing = true;
    if (arg_st.formatted_for_printing)
      arg_st.ghostscript.epscrop = arg_st.tight;
    else
      ## pstoedit throws errors if the EPS file isn't cropped
      arg_st.ghostscript.epscrop = true;
    endif
  elseif (all (! strcmp (arg_st.devopt, dev_list)))
    ## Assume we are formatting output for a printer
    arg_st.formatted_for_printing = true;
    arg_st.ghostscript.device = arg_st.devopt;
    arg_st.ghostscript.output = arg_st.name;
    arg_st.ghostscript.antialiasing = false;
    arg_st.ghostscript.epscrop = arg_st.tight;
  endif

  if (unknown_device)
    error ("print: unknown device %s", arg_st.devopt);
  endif

  if (arg_st.resize_flag)
    if (! (arg_st.send_to_printer || arg_st.formatted_for_printing
           || strncmp (arg_st.devopt, "pdf", 3)
           || strncmp (arg_st.devopt, "ps", 2)))
      error ("print: the '%s' option is only valid for page formats and printers", arg_st.resize_flag);
    endif
  endif

  if (arg_st.send_to_printer)
    if (isempty (arg_st.name))
      ## Pipe the ghostscript output
      arg_st.name = "-";
    else
      error ("print: a filename may not specified when spooling to a printer");
    endif
    if (! any (strcmp (arg_st.devopt, gs_device_list)))
      ## Only supported ghostscript devices
      error ("print: format must be a valid Ghostscript format for spooling to a printer");
    endif
  elseif (isempty (arg_st.name) && ! arg_st.rgb_output)
    error ("print: an output filename must be specified");
  endif

  if (isempty (arg_st.canvas_size))
    if (isfigure (arg_st.figure))
      [arg_st.ghostscript.papersize, papersize_points, paperposition] = ...
                           gs_papersize (arg_st.figure, arg_st.orientation);
    else
      ## allows BIST tests to be run
      arg_st.ghostscript.papersize = "letter";
      paperposition = [0.25, 2.50, 8.00, 6.00] * 72;
      papersize_points = [8.5, 11.0] * 72;
    endif

    ## resize paper
    if (arg_st.resize_flag)
      if (strcmp (arg_st.resize_flag, "fillpage"))
        ## leave a 0.25 inch margin on all sides of the page.
        paperposition = [0.25 * 72, 0.25 * 72, ...
                         papersize_points(1) - 0.5*72, ...
                         papersize_points(2) - 0.5*72];
      elseif (strcmp (arg_st.resize_flag, "bestfit"))
        ## leaves a minimum page margin of 0.25 inches
        if (paperposition(3) > paperposition(4))
          fit_scale = papersize_points(1) / paperposition(3);
        else
          fit_scale = papersize_points(2) / paperposition(4);
        endif
        paperposition = [(papersize_points(1) - fit_scale*paperposition(3)) * 0.5, ...
                        (papersize_points(2) - fit_scale*paperposition(4)) * 0.5, ...
                        fit_scale * paperposition(3), ...
                        fit_scale * paperposition(4)];
      endif
    endif

    arg_st.canvas_size = paperposition(3:4);
    if (strcmp (__graphics_toolkit__, "gnuplot")
        && ! arg_st.ghostscript.epscrop)
      arg_st.ghostscript.pageoffset = paperposition(1:2) - 50;
    else
      arg_st.ghostscript.pageoffset = paperposition(1:2);
    endif
  else
    ## Size specified with -S option
    if (arg_st.ghostscript.resolution != 150)
      warning ("print: '-Sxsize,ysize' overrides resolution option -r\n");
    endif
    arg_st.scalefontsize = arg_st.canvas_size(1) / ...
                           6 / get (0, "screenpixelsperinch");
    arg_st.ghostscript.resolution = 72;
    arg_st.ghostscript.papersize = arg_st.canvas_size;
    papersize_points = arg_st.canvas_size * 72;
    arg_st.ghostscript.epscrop = true;
    arg_st.ghostscript.pageoffset = [0, 0];
    paperposition = [0, 0];
  endif

  if (arg_st.formatted_for_printing)
    arg_st.ghostscript.resolution = [];

    ## Warn if the figure is too large for the selected paper size
    if (any (papersize_points < (arg_st.canvas_size + paperposition(1:2)))
        || any (paperposition(1:2) < 0))
      warning ("Octave:print:figure-too-large", ...
               ['print: given the current "paperposition" and ', ...
                '"papersize" properties, the figure is too large ', ...
                "and will be cropped to fit on the output page."]);
    endif
  else
    arg_st.ghostscript.papersize = "";
    arg_st.ghostscript.pageoffset = [0, 0];
  endif

  if (warn_on_missing_ghostscript)
    if (isempty (arg_st.ghostscript.binary))
      warning ("print:nogs", ...
               "print: 'gs' (Ghostscript) binary is not available.  Many formats may not be available\n");
    endif
    warn_on_missing_ghostscript = false;
  endif

endfunction


## Test blocks are not allowed (and not needed) for private functions
%!#test
%! opts = __print_parse_opts__ ();
%! assert (opts.devopt, "pswrite");
%! assert (opts.use_color, 1);
%! assert (opts.send_to_printer, true);
%! assert (opts.canvas_size, [576, 432]);
%! assert (opts.ghostscript.device, "pswrite");

%!#test
%! opts = __print_parse_opts__ ("test.pdf", "-S640,480");
%! assert (opts.canvas_size, [307.2, 230.4], 0.1);

%!#test
%! opts = __print_parse_opts__ ("-dpsc", "-append", "-loose");
%! assert (opts.devopt, "pswrite");
%! assert (opts.send_to_printer, true);
%! assert (opts.use_color, 1);
%! assert (opts.append_to_file, false);
%! assert (opts.ghostscript.device, "pswrite");
%! assert (opts.ghostscript.epscrop, false);

%!#test
%! opts = __print_parse_opts__ ("-deps", "-tight");
%! assert (opts.tight, true);
%! assert (opts.send_to_printer, true);
%! assert (opts.use_color, -1);
%! assert (opts.ghostscript.device, "");

%!#test
%! opts = __print_parse_opts__ ("-djpg", "foobar", "-mono", "-loose");
%! assert (opts.devopt, "jpeg");
%! assert (opts.name, "foobar.jpg");
%! assert (opts.ghostscript.device, "jpeg");
%! assert (opts.ghostscript.epscrop, true);
%! assert (opts.ghostscript.papersize, "");
%! assert (opts.ghostscript.pageoffset, [0, 0]);
%! assert (opts.send_to_printer, false);
%! assert (opts.printer, "");
%! assert (opts.use_color, -1);

%!#test
%! opts = __print_parse_opts__ ("-ddeskjet", "foobar", "-mono", "-Pmyprinter");
%! assert (opts.ghostscript.output, "foobar.deskjet");
%! assert (opts.ghostscript.device, "deskjet");
%! assert (opts.devopt, "deskjet");
%! assert (opts.send_to_printer, true);
%! assert (opts.printer, "-Pmyprinter");
%! assert (opts.use_color, -1);

%!#test
%! opts = __print_parse_opts__ ("-f5", "-dljet3");
%! assert (opts.ghostscript.device, "ljet3");
%! assert (strfind (opts.ghostscript.output, ".ljet3"));
%! assert (opts.devopt, "ljet3");
%! assert (opts.send_to_printer, true);
%! assert (opts.figure, 5);

function cmd = __quote_path__ (cmd)

  if (! isempty (cmd))
    is_quoted = all (cmd([1, end]) == "'");
    if (! is_quoted)
      dos_shell = ! isunix () && ispc ();
      if (dos_shell && any (cmd == "/"))
        cmd = strrep (cmd, '/', '\');
      endif
      if (any (cmd == " "))
        cmd = ['"' strrep(cmd, '"', '""') '"'];
      endif
    endif
  endif

endfunction

function gs = __ghostscript_binary__ ()

  persistent ghostscript_binary = "";
  persistent warn_on_bad_gsc = true;

  if (isempty (ghostscript_binary))
    GSC = getenv ("GSC");
    if (exist (GSC, "file")
        || (! isempty (GSC) && file_in_path (getenv ("PATH"), GSC)))
      gs_binaries = {GSC};
    elseif (! isempty (GSC) && warn_on_bad_gsc)
      warning ("Octave:print:badgscenv",
               "print: GSC environment variable not set properly");
      warn_on_bad_gsc = false;
      gs_binaries = {};
    else
      gs_binaries = {};
    endif

    if (isunix ())
      ## Unix - Includes Mac OSX and Cygwin.
      gs_binaries = [gs_binaries, {"gs", "gs.exe"}];
    else
      ## pc - Includes Win32 and mingw.
      gs_binaries = [gs_binaries, ...
                     {"gs.exe", "gswin32c.exe", "gswin64c.exe", "mgs.exe"}];
    endif
    n = 0;
    while (n < numel (gs_binaries) && isempty (ghostscript_binary))
      ghostscript_binary = file_in_path (getenv ("PATH"), gs_binaries{++n});
    endwhile
  endif

  gs = ghostscript_binary;

endfunction

function bin = __svgconv_binary__ ()

  persistent binary = "";

  if (isempty (binary))
    ## default installation location is the archlib directory
    bindir = getenv ("OCTAVE_ARCHLIBDIR");
    if (isempty (bindir))
      bindir = __octave_config_info__ ("archlibdir");
    endif

    binary = fullfile (bindir, ...
                       ["octave-svgconvert", ...
                        __octave_config_info__("EXEEXT")]);

    if (! exist (binary, "file"))
      binary = "";
    endif
  endif

  bin = binary;

endfunction

function bin = __find_binary__ (binary)

  persistent data = struct ();

  if (! isfield (data, binary))
    data.(binary).bin = "";
  endif

  if (isempty (data.(binary).bin))
    if (isunix ())
      ## Unix - Includes Mac OSX and Cygwin.
      binaries = strcat (binary, {"", ".exe"});
    else
      ## pc - Includes Win32 and mingw.
      binaries = strcat (binary, {".exe"});
    endif
    n = 0;
    while (n < numel (binaries) && isempty (data.(binary).bin))
      data.(binary).bin = file_in_path (getenv ("PATH"), binaries{++n});
    endwhile
  endif

  bin = data.(binary).bin;

endfunction

function [papersize, papersize_points, paperposition] = gs_papersize (hfig, paperorientation)
  persistent papertypes papersizes;

  if (isempty (papertypes))
    papertypes = {"usletter", "uslegal",     "a0",     "a1", ...
                        "a2",      "a3",     "a4",     "a5", ...
                        "b0",      "b1",     "b2",     "b3", ...
                        "b4",      "b5", "arch-a", "arch-b", ...
                    "arch-c",  "arch-d", "arch-e",      "a", ...
                         "b",       "c",      "d",      "e", ...
                   "tabloid"};
    papersizes = [ 8.5, 11.0;  8.5, 14.0; 33.1, 46.8; 23.4, 33.1;
                  16.5, 23.4; 11.7, 16.5;  8.3, 11.7;  5.8,  8.3;
                  39.4, 55.7; 27.8, 39.4; 19.7, 27.8; 13.9, 19.7;
                   9.8, 13.9;  6.9,  9.8;  9.0, 12.0; 12.0, 18.0;
                  18.0, 24.0; 24.0, 36.0; 36.0, 48.0;  8.5, 11.0;
                  11.0, 17.0; 18.0, 24.0; 24.0, 36.0; 36.0, 48.0;
                  11.0, 17.0] * 72;
  endif

  papertype = get (hfig, "papertype");
  paperunits = get (hfig, "paperunits");
  paperposition = get (hfig, "paperposition");
  if (strcmp (papertype, "<custom>"))
    papersize = get (hfig, "papersize");
    papersize = convert2points (papersize, paperunits);
  else
    papersize = papersizes(strcmp (papertypes, papertype), :);
  endif

  if (strcmp (paperunits, "normalized"))
    paperposition .*= papersize([1,2,1,2]);
  else
    paperposition = convert2points (paperposition, paperunits);
  endif

  ## FIXME: This will be obsoleted by listeners for paper properties.
  ##        papersize is tall when portrait, and wide when landscape.
  if ((papersize(1) > papersize(2) && strcmpi (paperorientation, "portrait"))
      || (papersize(1) < papersize(2) && strcmpi (paperorientation, "landscape")))
    papersize = papersize([2,1]);
  endif

  ## papersize is now [h,w] and measured in points.
  ## Return it for possible resize outside of this function.
  papersize_points = papersize;

  if (! strcmp (papertype, "<custom>")
      && (strcmp (paperorientation, "portrait")))
    ## For portrait use the ghostscript name
    papersize = papertype;
    papersize(papersize=="-") = "";
    papersize = strrep (papersize, "us", "");
    switch (papersize)
      case "a"
        papersize = "letter";
      case {"b", "tabloid"}
        papersize = "11x17";
      case {"c", "d", "e"}
        papersize = ["arch" papersize];
    endswitch
    if (strncmp (papersize, "arch", 4))
      papersize(end) = upper (papersize(end));
    endif
  endif

endfunction

function value = convert2points (value, units)

  switch (units)
    case "inches"
      value *= 72;
    case "centimeters"
      value *= (72 / 2.54);
    case "normalized"
      error ("Octave:print:customnormalized",
             "print: papersize=='<custom>' and paperunits='normalized' may not be combined");
  endswitch

endfunction

function device_list = gs_device_list ()

  ## Graphics formats/languages, not printers.
  device_list = {"bmp16"; "bmp16m"; "bmp256"; "bmp32b"; "bmpgray"; ...
                 "epswrite"; "eps2write"; "jpeg"; "jpegcymk"; "jpeggray";
                 "pbm"; "pbmraw"; "pcx16"; "pcx24b"; "pcx256"; "pcx2up"; ...
                 "pcxcmyk"; "pcxgray"; "pcxmono"; "pdfwrite"; "pgm"; ...
                 "pgmraw"; "pgnm"; "pgnmraw"; "png16"; "png16m"; ...
                 "png256"; "png48"; "pngalpha"; "pnggray"; "pngmono"; ...
                 "pnm"; "pnmraw"; "ppm"; "ppmraw"; "pswrite"; "ps2write"; ...
                 "tiff12nc"; "tiff24nc"; "tiff32nc"; "tiff48nc"; ...
                 "tiff64nc"; "tiffcrle"; "tiffg3"; "tiffg32d"; "tiffg4"; ...
                 "tiffgray"; "tifflzw"; "tiffpack"; "tiffscaled"; ...
                 "tiffscaled24"; "tiffscaled32"; "tiffscaled4"; ...
                 "tiffscaled8"; "tiffsep"; "tiffsep1" };

endfunction

function aliases = gs_aliases (do_eps)

  ## Ghostscript device names
  aliases.bmp   = "bmp32b";
  aliases.pdf   = "pdfwrite";
  aliases.png   = "png16m";
  aliases.ps    = "ps2write";
  aliases.ps2   = "ps2write";
  aliases.psc   = "ps2write";
  aliases.psc2  = "ps2write";
  aliases.tiff  = "tiffscaled24";
  aliases.tiffn = "tiff24nc";

  if (do_eps)
    aliases.eps   = "ps2write";
    aliases.eps2  = "ps2write";
    aliases.epsc  = "ps2write";
    aliases.epsc2 = "ps2write";
  endif

endfunction
