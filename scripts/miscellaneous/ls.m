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
## @deftypefn  {} {} ls
## @deftypefnx {} {} ls @var{filenames}
## @deftypefnx {} {} ls @var{options}
## @deftypefnx {} {} ls @var{options} @var{filenames}
## @deftypefnx {} {@var{list} =} ls (@dots{})
##
## List directory contents.
##
## The @code{ls} function forwards to the @code{ls} command if it is available.
## It falls back to calling the native operating system's directory listing
## command.  Available @var{options} may vary from system to system.
##
## Filenames are subject to shell expansion if they contain any wildcard
## characters @samp{*}, @samp{?}, @samp{[]}.  If these wildcard characters are
## escaped with a backslash @samp{\} (e.g., @samp{\*}) then they are not
## treated as wildcards, but instead as the corresponding literal character.
##
## If the optional output @var{list} is requested then @code{ls} returns a
## character array with one row for each file/directory name.
##
## Example usage on a UNIX-like system:
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
## @seealso{dir, readdir, glob, what, stat, filesep, ls_command}
## @end deftypefn

function list = ls (varargin)

  if (! iscellstr (varargin))
    error ("ls: all arguments must be character strings");
  endif

  ls_cmd = ls_command ();

  if (nargin > 0)
    args = tilde_expand (varargin);

    if (ispc () && ! isunix ())
      if (strncmp (ls_cmd, "ls", 2))
        ## Replace backslashes with forward slashes (unless they escape a
        ## wildcard character)
        args = regexprep (args, '\\(?![*?\[\]])', '/');
        ## Enclose paths, potentially having spaces, in double quotes:
        args = strcat ('"', args, '"');
        ## Exclude glob patterns and escaped characters from quoted part of
        ## FILENAMES string
        args = regexprep (args, '(?<!\\)([*?])', '"$1"');
        args = regexprep (args, '(?<!\\)\[', '"[');
        args = regexprep (args, '(?<!\\)\]', ']"');
        args = regexprep (args, '(\\.)', '"$1"');
      else
        idx = ! strncmp (args, '/', 1);
        ## Enclose paths, potentially having spaces, in double quotes:
        args(idx) = strcat ('"', args(idx), '"');
        ## shell (cmd.exe) on MinGW uses '^' as escape character
        args = regexprep (args, '([^\w.*?])', '^$1');
      endif
    else
      ## Escape any special characters in filename
      args = regexprep (args, '(?<!\\)([^][\w.*?-\\])', '\\$1');
      ## Undo escaped spaces following command args
      ## Only used for command form where single str contains many args.
      ## Example: list = ls ("-l /usr/bin")
      args = regexprep (args, '(-\w+)(?:\\ )+', '$1 ');
    endif

    args = sprintf ("%s ", args{:});
  else
    args = "";
  endif

  if (nargout > 0 && strncmp (ls_cmd, "ls", 2))
    args = ["-1 ", args];
  endif

  cmd = [ls_cmd, " ", args];

  if (page_screen_output () || nargout > 0)
    [status, output] = system (cmd);

    if (status != 0)
      error ("ls: command exited abnormally with status %d\n", status);
    elseif (nargout == 0)
      puts (output);
    else
      list = strvcat (regexp (output, "[\r\n]+", "split"){:});
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

%!test
%! if (isunix ())
%!   list = ls ("/");
%!   list = (list')(:)';   # transform to a single row vector
%!   assert (! isempty (strfind (list, "sbin")));
%!   list2 = ls ("-l /");
%!   list2 = (list2')(:)';   # transform to a single row vector
%!   assert (! isempty (strfind (list2, "sbin")));
%!   assert (rows (list) == rows (list2));
%! endif

%!error <all arguments must be character strings> ls (1)
## Test below is valid, but produces confusing output on screen
%!#error <command exited abnormally> ls ("-!")
