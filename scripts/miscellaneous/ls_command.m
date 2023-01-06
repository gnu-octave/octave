########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{val} =} ls_command ()
## @deftypefnx {} {@var{old_val} =} ls_command (@var{new_val})
## Query or set the shell command used by Octave's @code{ls} command.
## @seealso{ls}
## @end deftypefn

function old_val = ls_command (new_val)

  persistent __ls_command__;

  if (isempty (__ls_command__))
    ## MinGW uses different ls_command
    if (ispc () && ! isunix ()
        && system ("where ls", true))
      __ls_command__ = "dir /D";
    else
      __ls_command__ = "ls -C";
    endif
  endif

  if (nargin == 0 || nargin == 1)

    old_val = __ls_command__;

    if (nargin == 1)
      if (! ischar (new_val))
        error ("ls_command: argument must be a character string");
      endif

      __ls_command__ = new_val;
    endif

  endif

endfunction


%!test
%! cmd = ls_command ();
%! assert (ischar (cmd));
%! if (ispc () && ! isunix () && system ("where ls", true))
%!   assert (cmd(1:3), "dir");
%! else
%!   assert (cmd(1:2), "ls");
%! endif

%!error <argument must be a character string> ls_command (123)
