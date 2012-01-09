## Copyright (C) 2006-2012 John W. Eaton
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
## @deftypefn {Command} {} ls options
## List directory contents.  For example:
##
## @example
## @group
## ls -l
##      @print{} total 12
##      @print{} -rw-r--r--   1 jwe  users  4488 Aug 19 04:02 foo.m
##      @print{} -rw-r--r--   1 jwe  users  1315 Aug 17 23:14 bar.m
## @end group
## @end example
##
## The @code{dir} and @code{ls} commands are implemented by calling your
## system's directory listing command, so the available options may vary
## from system to system.
## @seealso{dir, stat, readdir, glob, filesep, ls_command}
## @end deftypefn

## Author: jwe

function retval = ls (varargin)

  global __ls_command__;

  if (isempty (__ls_command__) || ! ischar (__ls_command__))
    ## Initialize value for __ls_command__.
    ls_command ();
  endif

  if (! iscellstr (varargin))
    error ("ls: all arguments must be character strings");
  endif

  if (nargin > 0)
    args = tilde_expand (varargin);
    if (ispc () && ! isunix ())
      ## shell (cmd.exe) on MinGW uses '^' as escape character
      args = regexprep (args, '([^\w.*? -])', '^$1');
    else
      args = regexprep (args, '([^\w.*? -])', '\$1');
    endif
    args = sprintf ("%s ", args{:});
  else
    args = "";
  endif

  cmd = sprintf ("%s %s", __ls_command__, args);

  if (page_screen_output () || nargout > 0)
    [status, output] = system (cmd);

    if (status != 0)
      error ("ls: command exited abnormally with status %d\n", status);
    elseif (nargout == 0)
      puts (output);
    else
      retval = strvcat (regexp (output, '\S+', 'match'){:});
    endif
  else
    ## Just let the output flow if the pager is off.  That way the
    ## output from things like "ls -R /" will show up immediately and
    ## we won't have to buffer all the output.
    system (cmd);
  endif

endfunction


%!test
%! list = ls ();
%! assert (ischar (list));
%! assert (! isempty (list));

%!error ls (1);

