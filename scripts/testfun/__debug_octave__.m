########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn  {} {} __debug_octave__ ()
## @deftypefnx {} {} __debug_octave__ (@var{command_string})
## Try to open a terminal window with gdb connected to the current
## Octave process.
##
## If @var{command_string} is not supplied, it should have a single
## @code{%d} format option that will be replaced by the process ID
## for the current Octave process, as returned by @code{getpid}.  For
## example, on Windows systems the following two commands are
## equivalent:
##
## @example
## @group
## __debug_octave__ ()
## __debug_octave__ ("cmd /c start gdb -p %d")
## @end group
## @end example
## @end deftypefn

function __debug_octave__ (command_string)

  if (nargin == 0)
    if (ismac ())
      status = system ("lldb --version");
      if (status != 0)
        error ("unable to execute lldb");
      endif
      command_string = "osascript -e 'tell application \"Terminal\" to do script \"lldb -p %d\"'";

    elseif (isunix ())
      status = system ("gdb --version");
      if (status != 0)
        error ("unable to execute gdb");
      endif
      command_string = "x-terminal-emulator -e gdb -p %d";

    elseif (ispc ())
      status = system ("gdb --version");
      if (status != 0)
        error ("unable to execute gdb");
      endif
      command_string = "cmd /c start gdb -p %d";

    else
      error ("unknown system -- unable to determine how to start debugger");
    endif
  endif

  system (sprintf (command_string, getpid ()), false, "async");

endfunction

