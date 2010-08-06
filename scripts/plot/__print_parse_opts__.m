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
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{buttons}] =} ginput (@var{n})
## Return which mouse buttons were pressed and keys were hit on the current
## figure.  If @var{n} is defined, then wait for @var{n} mouse clicks
## before returning.  If @var{n} is not defined, then @code{ginput} will
## loop until the return key is pressed.
## @end deftypefn

function arg_st = __print_parse_opts__ (varargin)

  arg_st.append_to_file = false;
  arg_st.canvas_size = [];
  arg_st.debug = false;
  arg_st.debug_file = "octave-print-commands.log";
  arg_st.devopt = "";
  arg_st.figure = get (0, "currentfigure");
  arg_st.fig2dev_binary = __find_binary__ ("fig2dev");
  arg_st.fontsize = "";
  arg_st.font = "";
  arg_st.force_solid = 0; # 0=default, -1=dashed, +1=solid
  arg_st.ghostscript.binary = __ghostscript_binary__ ();
  arg_st.ghostscript.device = "";
  arg_st.ghostscript.output = "";
  arg_st.ghostscript.papersize = "";
  arg_st.ghostscript.pageoffset = [];
  arg_st.ghostscript.debug = false;
  arg_st.ghostscript.epscrop = false;
  arg_st.ghostscript.resolution = 150;
  arg_st.orientation = "";
  arg_st.pstoedit_binary = __find_binary__ ("pstoedit");
  arg_st.name = "";
  arg_st.printer = "";
  arg_st.special_flag = "textnormal";
  arg_st.tight_flag = false;
  arg_st.use_color = 0; # 0=default, -1=mono, +1=color
  arg_st.send_to_printer = false;
  
  if (isunix ())
    arg_st.lpr_options = "-l";
  elseif (ispc ())
    arg_st.lpr_options = "-o l";
  else
    ## FIXME - What other OS's might be considered.
    arg_st.lpr_options = "";
  endif
  arg_st.unlink = {};
  
  for i = 1:nargin
    arg = varargin{i};
    if (ischar (arg))
      if (strcmp (arg, "-color"))
        arg_st.use_color = 1;
      elseif (strcmp (arg, "-append"))
        arg_st.append_to_file = true;
      elseif (strcmp (arg, "-mono"))
        arg_st.use_color = -1;
      elseif (strcmp (arg, "-solid"))
        arg_st.force_solid = 1;
      elseif (strcmp (arg, "-dashed"))
        arg_st.force_solid = -1;
      elseif (strncmp (arg, "-portrait", numel (arg)))
        arg_st.orientation = "portrait";
      elseif (strncmp (arg, "-landscape", numel (arg)))
        arg_st.orientation = "landscape";
      elseif (strcmp (arg, "-tight"))
        arg_st.tight_flag = true;
      elseif (strcmp (arg, "-textspecial"))
        arg_st.special_flag = "textspecial";
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
      elseif ((length (arg) > 2) && arg(1:2) == "-G")
        arg_st.ghostscript.binary = arg(3:end);
        if (exist (arg_st.ghostscript.binary, "file") != 2)
          arg_st.ghostscript.binary = file_in_path (EXEC_PATH, arg_st.ghostscript.binary);
        endif
        if (isempty (arg_st.ghostscript.binary))
          error ("print: Ghostscript binary ""%s"" could not be located", arg(3:end))
        endif
      elseif (length (arg) > 2 && arg(1:2) == "-F")
        idx = rindex (arg, ":");
        if (idx)
          arg_st.font = arg(3:idx-1);
          arg_st.fontsize = str2num (arg(idx+1:end));
        else
          arg_st.font = arg(3:end);
        endif
      elseif (length (arg) > 2 && arg(1:2) == "-S")
        arg_st.canvas_size = str2num (arg(3:end));
      elseif (length (arg) > 2 && arg(1:2) == "-r")
        arg_st.ghostscript.resolution = arg(3:end);
      elseif (length (arg) > 2 && arg(1:2) == "-f")
        arg_st.figure = str2num (arg(3:end));
      elseif (length (arg) >= 1 && arg(1) == "-")
        error ("print: unknown option `%s'", arg);
      elseif (length (arg) > 0)
        arg_st.name = arg;
      endif
    elseif (isfigure (arg))
      arg_st.figure = arg;
    else
      error ("print: expecting inputs to be character string options or a figure handle");
    endif
  endfor

  if (arg_st.ghostscript.resolution == 0)
    ## Do as Matlab does.
    arg_st.ghostscript.resolution = num2str (get (0, "screenpixelsperinch"));
  endif

  if (isempty (arg_st.orientation))
    if (isfigure (arg_st.figure))
      arg_st.orientation = get (arg_st.figure, "paperorientation");
    else
      ## Allows tests to be run without error.
      arg_st.orientation = "portrait";
    endif
  endif

  if (isempty (arg_st.ghostscript.binary))
    arg_st.ghostscript.binary = __ghostscript_binary__ ();
  endif

  dot = rindex (arg_st.name, ".");
  if (isempty (arg_st.devopt))
    if (dot == 0)
      arg_st.devopt = "psc";
    else
      arg_st.devopt = tolower (arg_st.name(dot+1:end));
    endif
  endif

  if ((any (strcmp ({"ps", "ps2", "eps", "eps2"}, arg_st.devopt))
      || (! isempty (strfind (arg_st.devopt, "tex")))) && arg_st.use_color == 0)
    ## Mono is the default for ps, eps, and the tex/latex, devices
    arg_st.use_color = -1;
  elseif (arg_st.use_color == 0)
    arg_st.use_color = 1;
  endif

  if (arg_st.append_to_file)
    if (isempty (arg_st.name))
      arg_st.append_to_file = false;
    elseif (any (strcmpi (arg_st.devopt, {"eps", "eps2", "epsc", "epsc2", ...
                                          "ps", "ps2", "psc", "psc2", "pdf"})))
      have_ghostscript = ! isempty (__ghostscript_binary__ ());
      if (have_ghostscript)
        file_exists = ((numel (dir (arg_st.name)) == 1) && (! isdir (arg_st.name)));
        if (! file_exists)
          arg_st.append_to_file = false;
        end
      else
        arg_st.append_to_file = false;
        warning ("print.m: appended output requires ghostscript to be installed.")
      endif
    else
      warning ("print.m: appended output is not supported for device '%s'", arg_st.devopt)
      arg_st.append_to_file = false;
    endif
  endif

  if (arg_st.tight_flag)
    if (any (strcmpi (arg_st.devopt, {"ps", "ps2", "psc", "psc2", "pdf"})))
      arg_st.tight_flag = false;
      warning ("print.m: '-tight' is not supported for device '%s'", arg_st.devopt)
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
  endif

  dev_list = {"aifm", "corel", "fig", "png", "jpeg", ...
              "gif", "pbm", "pbmraw", "dxf", "mf", ...
              "svg", "hpgl", "ps", "ps2", "psc", ...
              "psc2", "eps", "eps2", "epsc", "epsc2", ...
              "emf", "pdf", "pslatex", "epslatex", "epslatexstandalone", ...
              "pslatexstandalone", "pdflatexstandalone", ...
              "pstex", "tiff", "tiffn" "tikz", "pcxmono", ...
              "pcx24b", "pcx256", "pcx16", "pgm", "pgmraw", ...
              "ppm", "ppmraw", "pdflatex"};

  suffixes = {"ai", "cdr", "fig", "png", "jpg", ...
              "gif", "pbm", "pbm", "dxf", "mf", ...
              "svg", "hpgl", "ps", "ps", "ps", ...
              "ps", "eps", "eps", "eps", "eps", ...
              "emf", "pdf", "tex", "tex", "tex", ...
              "tex", "tex", ...
              "tex", "tiff", "tiff", "tikz", "pcx", ...
              "pcx", "pcx", "pcx", "pgm", "pgm", ...
              "ppm", "ppm", "tex"};

  match = strcmpi (dev_list, arg_st.devopt);
  if (any (match))
    default_suffix = suffixes {match};
  else
    default_suffix = arg_st.devopt;
  endif

  if (dot == 0 && ! isempty (arg_st.name))
    arg_st.name = strcat (arg_st.name, ".", default_suffix);
  endif

  if (! isempty (arg_st.printer) || isempty (arg_st.name))
    arg_st.send_to_printer = true;
    if (isempty (arg_st.name))
      arg_st.name = strcat (tmpnam (), ".", default_suffix);
      arg_st.unlink{end+1} = arg_st.name;
    endif
  endif

  if (all (! strcmp (arg_st.devopt, dev_list)))
    arg_st.ghostscript.device = arg_st.devopt;
    arg_st.ghostscript.output = arg_st.name;
  endif

  if (isempty (arg_st.canvas_size))
    if (isfigure (arg_st.figure))
      [arg_st.ghostscript.papersize, paperposition] = gs_papersize (arg_st.figure,
                                                               arg_st.orientation);
    else
      ## allows tests to be run
      arg_st.ghostscript.papersize = "letter";
      paperposition = [0.25, 2.50, 8.00, 6.00] * 72;
    endif
    arg_st.canvas_size = paperposition(3:4);
    arg_st.ghostscript.pageoffset = paperposition(1:2);
  else
    ## Canvas size in points.
    arg_st.canvas_size = arg_st.canvas_size * 72 / arg_st.ghostscript.resolution;
    arg_st.ghostscript.papersize = arg_st.canvas_size;
    arg_st.ghostscript.epscrop = true;
    arg_st.ghostscript.pageoffset = [0, 0];
  endif

endfunction

%!test
%! opts = __print_parse_opts__ ();
%! assert (opts.devopt, "psc");
%! assert (opts.use_color, 1);
%! assert (opts.send_to_printer, true);
%! assert (opts.name, opts.unlink{1})
%! assert (opts.canvas_size, [576, 432]);
%! assert (opts.ghostscript.device, "")
%! for n = 1:numel(opts.unlink)
%!   unlink (opts.unlink{n});
%! endfor

%!test
%! opts = __print_parse_opts__ ("test.pdf", "-S640,480");
%! assert (opts.canvas_size, [307.2, 230.4], 0.1);

%!test
%! opts = __print_parse_opts__ ("-dpsc", "-append");
%! assert (opts.devopt, "psc");
%! assert (opts.name(end+(-2:0)), ".ps");
%! assert (opts.send_to_printer, true);
%! assert (opts.use_color, 1);
%! assert (opts.append_to_file, false);
%! assert (opts.ghostscript.device, "")
%! for n = 1:numel(opts.unlink)
%!   unlink (opts.unlink{n});
%! endfor

%!test
%! opts = __print_parse_opts__ ("-deps", "-tight");
%! assert (opts.name, opts.unlink{1})
%! assert (opts.tight_flag, true);
%! assert (opts.send_to_printer, true);
%! assert (opts.use_color, -1);
%! assert (opts.ghostscript.device, "")
%! for n = 1:numel(opts.unlink)
%!   unlink (opts.unlink{n});
%! endfor

%!test
%! opts = __print_parse_opts__ ("-djpg", "foobar", "-mono");
%! assert (opts.devopt, "jpeg")
%! assert (opts.name, "foobar.jpg")
%! assert (opts.ghostscript.device, "")
%! assert (opts.send_to_printer, false);
%! assert (opts.printer, "");
%! assert (opts.use_color, -1);
%! assert (opts.ghostscript.device, "")

%!test
%! opts = __print_parse_opts__ ("-ddeskjet", "foobar", "-mono", "-Pmyprinter");
%! assert (opts.ghostscript.output, "foobar.deskjet")
%! assert (opts.ghostscript.device, "deskjet")
%! assert (opts.devopt, "deskjet")
%! assert (opts.send_to_printer, true);
%! assert (opts.printer, "-Pmyprinter");
%! assert (opts.use_color, -1);

%!test
%! opts = __print_parse_opts__ ("-f5", "-dljet3");
%! assert (opts.name, opts.unlink{1})
%! assert (opts.ghostscript.output, opts.unlink{1})
%! assert (opts.ghostscript.device, "ljet3")
%! assert (strfind (opts.ghostscript.output, ".ljet3"))
%! assert (opts.devopt, "ljet3")
%! assert (opts.send_to_printer, true);
%! assert (opts.figure, 5)
%! for n = 1:numel(opts.unlink)
%!   unlink (opts.unlink{n});
%! endfor

function gs = __ghostscript_binary__ ()

  persistent ghostscript_binary = ""
  persistent warn_on_no_ghostscript = true
  persistent warn_on_bad_gsc = true

  if (isempty (ghostscript_binary))
    GSC = getenv ("GSC");
    if (exist (GSC, "file") || (! isempty (GSC) && file_in_path (EXEC_PATH, GSC)))
      gs_binaries = {GSC};
    elseif (! isempty (GSC) && warn_on_bad_gsc)
      warning ("print:badgscenv",
               "print.m: GSC environment variable not set properly.")
      warn_on_bad_gsc = false;
      gs_binaries = {};
    else
      gs_binaries = {};
    endif
    if (isunix ())
      ## Unix - Includes Mac OSX and Cygwin.
      gs_binaries = horzcat (gs_binaries, {"gs", "gs.exe"});
    else
      ## pc - Includes Win32 and mingw.
      gs_binaries = horzcat (gs_binaries, {"gs.exe", "gswin32c.exe"});
    endif
    n = 0;
    while (n < numel (gs_binaries) && isempty (ghostscript_binary))
      n = n + 1;
      ghostscript_binary = file_in_path (EXEC_PATH, gs_binaries{n});
    endwhile
    if (warn_on_no_ghostscript && isempty (ghostscript_binary))
      warning ("print:noghostscript",
               "print.m: ghostscript not found in EXEC_PATH.")
      warn_on_no_ghostscript = false;
    endif
  endif

  gs = ghostscript_binary;

endfunction

function bin = __find_binary__ (binary)

  persistent data = struct ()

  if (! isfield (data, binary))
    ## Reinitialize when `user_binaries' is present.
    data.(binary).bin = "";
    data.(binary).warn_on_absence = false;
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
      n = n + 1;
      data.(binary).bin = file_in_path (EXEC_PATH, binaries{n});
    endwhile
    if (isempty (data.(binary).bin) && data.(binary).warn_on_absence)
      warning (sprintf ("print:no%s", binary),
               "print.m: '%s' not found in EXEC_PATH", binary)
      data.(binary).warn_on_absence = false;
    endif
  endif

  bin = data.(binary).bin;

endfunction

function [papersize, paperposition] = gs_papersize (hfig, paperorientation)
  persistent papertypes papersizes

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
    papersize = convert2points (papersize , paperunits);
  else
    papersize = papersizes (strcmp (papertypes, papertype), :);
  endif

  if (strcmp (paperunits, "normalized"))
    paperposition = paperposition .* papersize([1,2,1,2]);
  else
    paperposition = convert2points (paperposition, paperunits);
  endif

  ## FIXME - This will be obsoleted by listeners for paper properties.
  ## Papersize is tall when portrait,and wide when landscape.
  if ((papersize(1) > papersize(2) && strcmpi (paperorientation, "portrait"))
      || (papersize(1) < papersize(2) && strcmpi (paperorientation, "landscape")))
    papersize = papersize ([2,1]);
    paperposition = paperposition([2,1,4,3]);
  endif

  if ((! strcmp (papertype, "<custom>")) && (strcmp (paperorientation, "portrait")))
    ## For portrait use the ghostscript name
    papersize = papertype;
    papersize(papersize=="-") = "";
    papersize = strrep (papersize, "us", "");
    switch papersize
    case "a"
      papersize = "letter";
    case {"b", "tabloid"}
      papersize = "11x17";
    case {"c", "d", "e"}
      papersize = strcat ("arch", papersize);
    endswitch
    if (strncmp (papersize, "arch", 4))
      papersize(end) = upper (papersize(end));
    endif
  endif

endfunction

function value = convert2points (value, units)
    switch units
    case {"inches"}
      value = value * 72;
    case {"centimeters"}
      value = value * 72 / 25.4;
    case {"normalized"}
      error ("print:customnormalized",
             "print.m: papersize=='<custom>' and paperunits='normalized' may not be combined.")
    endswitch
endfunction

