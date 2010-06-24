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

  arg_st.orientation = "";
  arg_st.use_color = 0; # 0=default, -1=mono, +1=color
  arg_st.append_to_file = 0;
  arg_st.force_solid = 0; # 0=default, -1=dashed, +1=solid
  arg_st.fontsize = "";
  arg_st.font = "";
  arg_st.canvas_size = "";
  arg_st.name = "";
  arg_st.devopt = "";
  arg_st.printer = "";
  arg_st.debug = false;
  arg_st.debug_file = "octave-print-commands.log";
  arg_st.special_flag = "textnormal";
  arg_st.tight_flag = false;
  arg_st.resolution = "";
  
  old_fig = get (0, "currentfigure");

  unwind_protect
    for i = 1:nargin
      arg = varargin{i};
      if (ischar (arg))
        if (strcmp (arg, "-color"))
          arg_st.use_color = 1;
        elseif (strcmp (arg, "-append"))
          arg_st.append_to_file = 1;
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
          arg_st.devopt = tolower(arg(3:end));
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
        elseif (length (arg) >= 1 && arg(1) == "-")
          error ("print: unknown option `%s'", arg);
        elseif (length (arg) > 0)
          arg_st.name = arg;
        endif
      elseif (isfigure (arg))
        arg_st.figure (arg);
      else
        error ("print: expecting inputs to be character string options or a figure handle");
      endif
    endfor
  unwind_protect_cleanup
    if (isfigure (old_fig))
      figure (old_fig)
    endif
  end_unwind_protect

endfunction

