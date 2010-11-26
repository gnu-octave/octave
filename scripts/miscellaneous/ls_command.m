## Copyright (C) 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {[@var{old_cmd} =} ls_command (@var{cmd})
## Set or return the shell command used by Octave's @code{ls} command.
## The value of @var{cmd} must be a character string.
## With no arguments, simply return the previous value.
## @seealso{ls}
## @end deftypefn

## Author: jwe

function old_cmd = ls_command (cmd)

  global __ls_command__;

  if (isempty (__ls_command__))
    ## FIXME -- ispc and isunix both return true for Cygwin.  Should they?
    if (ispc () && ! isunix ()
        && isempty (file_in_path (getenv ("PATH"), "ls")))
      __ls_command__ = "cmd /C dir /D";
    else
      __ls_command__ = "ls -C";
    endif
  endif

  if (nargin == 0 || nargin == 1)

    old_cmd = __ls_command__;

    if (nargin == 1)
      if (ischar (cmd))
        __ls_command__ = cmd;
      else
        error ("ls_command: expecting argument to be a character string");
      endif
    endif

  endif

endfunction
