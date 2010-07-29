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

  ## FIXME - change to numeric values: `canvas_size', `resolution', `fontsize'
  arg_st.append_to_file = false;
  arg_st.canvas_size = "";
  arg_st.debug = false;
  arg_st.debug_file = "octave-print-commands.log";
  arg_st.devopt = "";
  arg_st.figure = get (0, "currentfigure");
  arg_st.fig2dev_binary = __find_binary__ ("fig2dev");
  arg_st.fontsize = "";
  arg_st.font = "";
  arg_st.force_solid = 0; # 0=default, -1=dashed, +1=solid
  arg_st.ghostscript_binary = __ghostscript_binary__ ();
  arg_st.ghostscript_device = ""; # gs converts eps/ps to this format/printer-language
  arg_st.ghostscript_output = ""; # gs converts arg_st.name to arg_st.ghostscript_output
  arg_st.pstoedit_binary = __find_binary__ ("pstoedit");
  arg_st.name = ""; # This is the file produced by the backend
  arg_st.orientation = "";
  arg_st.printer = "";
  arg_st.resolution = num2str (get (0, "screenpixelsperinch"));
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
      elseif (strcmp (arg, "-portrait"))
        arg_st.orientation = "portrait";
      elseif (strcmp (arg, "-landscape"))
        arg_st.orientation = "landscape";
      elseif (strcmp (arg, "-tight"))
        arg_st.tight_flag = true;
      elseif (strcmp (arg, "-textspecial"))
        arg_st.special_flag = "textspecial";
      elseif (strncmp (arg, "-debug", 6))
        arg_st.debug = true;
        if (length (arg) > 7)
          arg_st.debug_file = arg(8:end);
        endif
      elseif (length (arg) > 2 && arg(1:2) == "-d")
        arg_st.devopt = tolower (arg(3:end));
      elseif (length (arg) > 2 && arg(1:2) == "-P")
        arg_st.printer = arg;
      elseif ((length (arg) > 2) && arg(1:2) == "-G")
        arg_st.ghostscript_binary = arg(3:end);
        if (exist (arg_st.ghostscript_binary, "file") != 2)
          arg_st.ghostscript_binary = file_in_path (EXEC_PATH, arg_st.ghostscript_binary);
        endif
        if (isempty (arg_st.ghostscript_binary))
          error ("print: Ghostscript binary ""%s"" could not be located", arg(3:end))
        endif
      elseif (length (arg) > 2 && arg(1:2) == "-F")
        idx = rindex (arg, ":");
        if (idx)
          arg_st.font = arg(3:idx-1);
          arg_st.fontsize = arg(idx+1:length(arg));
        else
          arg_st.font = arg(3:length(arg));
        endif
      elseif (length (arg) > 2 && arg(1:2) == "-S")
        arg_st.canvas_size = arg(3:length(arg));
      elseif (length (arg) > 2 && arg(1:2) == "-r")
        arg_st.resolution = arg(3:length(arg));
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

  if (isempty (arg_st.orientation))
    if (isfigure (arg_st.figure))
      arg_st.orientation = get (arg_st.figure, "paperorientation");
    else
      ## Allows tests to be run without error.
      arg_st.orientation = get (0, "defaultfigurepaperorientation");
    endif
  endif

  if (isempty (arg_st.ghostscript_binary))
    arg_st.ghostscript_binary = __ghostscript_binary__ ();
  endif

  dot = rindex (arg_st.name, ".");
  if (isempty (arg_st.devopt))
    if (dot == 0)
      arg_st.devopt = "psc";
    else
      arg_st.devopt = tolower (arg_st.name(dot+1:end));
    endif
  endif

  if (any (strcmp ({"ps", "ps2", "eps", "eps2"}, arg_st.devopt))
      || (! isempty (strfind (arg_st.devopt, "tex")) && arg_st.use_color == 0))
    ## Mono is the default for ps, eps, and the tex/latex, devices
    arg_st.use_color = -1;
  elseif (arg_st.use_color == 0)
    arg_st.use_color = 1;
  endif

  if (arg_st.append_to_file)
    if (any (strcmpi (arg_st.devopt, {"ps", "ps2", "psc", "psc2", "pdf"})))
      have_ghostscript = ! isempty (__ghostscript_binary__ ());
      if (have_ghostscript)
        file_exists = ((numel (dir (arg_st.name)) == 1) && (! isdir (arg_st.name)));
        if (! file_exists)
          arg_st.append_to_file = false;
        end
      end
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
              "gif", "pbm", "dxf", "mf", "svg", "hpgl", ...
              "ps", "ps2", "psc", "psc2", "eps", "eps2", ...
              "epsc", "epsc2", "emf", "pdf", "pslatex", ...
              "epslatex", "epslatexstandalone", "pstex", "tikz"};

  suffixes = {"ai", "cdr", "fig", "png", "jpg", ...
              "gif", "pbm", "dxf", "mf", "svg", "hpgl", ...
              "ps", "ps", "ps", "ps", "eps", "eps", ...
              "eps", "eps", "emf", "pdf", "tex", ...
              "tex", "tex", "tex", "tikz"};

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
    arg_st.ghostscript_device = arg_st.devopt;
    arg_st.ghostscript_output = arg_st.name;
    ## FIXME - This will not work correctly if GS is used to produce a print
    ##         stream that is saved to a file and not sent to the printer.
    if (arg_st.send_to_printer)
      arg_st.devopt = "psc";
      arg_st.name = strcat (tmpnam (), ".ps");
      arg_st.unlink{end+1} = arg_st.name;
    else
      ## Assume the user desires only the figuure. This is useful for producing
      ## pdf figures for pdflatex
      ## octave:#> print -f1 -dpdfwrite figure1.pdf
      arg_st.devopt = "epsc";
      arg_st.name = strcat (tmpnam (), ".eps");
      arg_st.unlink{end+1} = arg_st.name;
    endif
  endif

  if (any (strncmp (arg_st.devopt(1:2), {"ps", "pdf"}, 2)))
    arg_st.paperoutput = true;
  else
    arg_st.paperoutput = false;
  endif

  if (arg_st.debug)
    disp ("Printing options");
    disp (arg_st)
  endif
endfunction

%!test
%! opts = __print_parse_opts__ ();
%! assert (opts.devopt, "psc");
%! assert (opts.use_color, 1);
%! assert (opts.send_to_printer, true);
%! assert (opts.paperoutput, true);
%! assert (opts.name, opts.unlink{1})
%! for n = 1:numel(opts.unlink)
%!   unlink (opts.unlink{n});
%! endfor

%!test
%! opts = __print_parse_opts__ ("-dpsc", "-append");
%! assert (opts.devopt, "psc");
%! assert (opts.name(end+(-2:0)), ".ps");
%! assert (opts.send_to_printer, true);
%! assert (opts.use_color, 1);
%! assert (opts.append_to_file, false);
%! assert (opts.paperoutput, true);
%! for n = 1:numel(opts.unlink)
%!   unlink (opts.unlink{n});
%! endfor

%!test
%! opts = __print_parse_opts__ ("-deps", "-tight");
%! assert (opts.name, opts.unlink{1})
%! assert (opts.tight_flag, true);
%! assert (opts.paperoutput, false)
%! assert (opts.send_to_printer, true);
%! assert (opts.use_color, -1);
%! for n = 1:numel(opts.unlink)
%!   unlink (opts.unlink{n});
%! endfor

%!test
%! opts = __print_parse_opts__ ("-djpg", "foobar", "-mono");
%! assert (opts.devopt, "jpeg")
%! assert (opts.name, "foobar.jpg")
%! assert (opts.ghostscript_device, "")
%! assert (opts.send_to_printer, false);
%! assert (opts.printer, "");
%! assert (opts.paperoutput, false)
%! assert (opts.use_color, -1);

%!test
%! opts = __print_parse_opts__ ("-ddeskjet", "foobar", "-mono", "-Pmyprinter");
%! assert (opts.ghostscript_output, "foobar.deskjet")
%! assert (opts.ghostscript_device, "deskjet")
%! assert (opts.devopt, "psc")
%! assert (opts.send_to_printer, true);
%! assert (opts.printer, "-Pmyprinter");
%! assert (opts.paperoutput, true)
%! assert (opts.use_color, -1);

%!test
%! opts = __print_parse_opts__ ("-f5", "-dljet3");
%! assert (opts.name, opts.unlink{2})
%! assert (opts.ghostscript_output, opts.unlink{1})
%! assert (strfind (opts.ghostscript_output, ".ljet3"))
%! assert (strfind (opts.name, ".ps"))
%! assert (opts.devopt, "psc")
%! assert (opts.send_to_printer, true);
%! assert (opts.paperoutput, true)
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
    data.(binary).warn_on_absence = true;
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


