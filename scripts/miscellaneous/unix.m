########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
## @deftypefn  {} {} unix ("@var{command}")
## @deftypefnx {} {@var{status} =} unix ("@var{command}")
## @deftypefnx {} {[@var{status}, @var{text}] =} unix ("@var{command}")
## @deftypefnx {} {[@dots{}] =} unix ("@var{command}", "-echo")
## Execute a system command if running under a Unix-like operating system,
## otherwise do nothing.
##
## Octave waits for the external command to finish before returning the exit
## status of the program in @var{status} and any output in @var{text}.
##
## When called with no output argument, or the @qcode{"-echo"} argument is
## given, then @var{text} is also sent to standard output.
## @seealso{dos, system, isunix, ismac, ispc}
## @end deftypefn

function [status, text] = unix (command, echo_arg)

  if (nargin < 1)
    print_usage ();
  endif

  status = 1;
  text = "";

  if (isunix ())
    [status, text] = system (command);
    if (nargin > 1 || nargout == 0)
      printf ("%s\n", text);
    endif
  endif

endfunction


%!test
%! cmd = ls_command ();
%! [status, output] = unix (cmd);
%!
%! if (isunix ())
%!   assert (status, 0);
%!   assert (ischar (output));
%!   assert (! isempty (output));
%! else
%!   assert (status, 1);
%!   assert (output, "");
%! endif

## Test input validation
%!error <Invalid call> unix ()
