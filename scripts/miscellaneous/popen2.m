## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{in}, @var{out}, @var{pid}] =} popen2 (@var{command}, @var{args})
## Start a subprocess with two-way communication.  The name of the process
## is given by @var{command}, and @var{args} is an array of strings
## containing options for the command.  The file identifiers for the input
## and output streams of the subprocess are returned in @var{in} and
## @var{out}.  If execution of the command is successful, @var{pid}
## contains the process ID of the subprocess.  Otherwise, @var{pid} is
## @minus{}1.
##
## For example,
##
## @example
## @group
## [in, out, pid] = popen2 ("sort", "-nr");
## fputs (in, "these\nare\nsome\nstrings\n");
## fclose (in);
## while (isstr (s = fgets (out)))
##   fputs (stdout, s);
## endwhile
## fclose (out);
##      @print{} are
##      @print{} some
##      @print{} strings
##      @print{} these
## @end group
## @end example
## @end deftypefn

## Author: jwe

function [in, out, pid] = popen2 (command, args)

  in = -1;
  out = -1;
  pid = -1;

  if (nargin == 1 || nargin == 2)

    if (nargin == 1)
      args = "";
    endif

    if (isstr (command))

      [stdin_pipe, stdin_status] = pipe ();
      [stdout_pipe, stdout_status] = pipe ();

      if (stdin_status == 0 && stdout_status == 0)

        pid = fork ();

        if (pid == 0)

	  ## In the child.

          fclose (nth (stdin_pipe, 2));
          fclose (nth (stdout_pipe, 1));

          dup2 (nth (stdin_pipe, 1), stdin);
          fclose (nth (stdin_pipe, 1));

          dup2 (nth (stdout_pipe, 2), stdout);
          fclose (nth (stdout_pipe, 2));

          if (exec (command, args) < 0)
            error ("popen2: unable to start process `%s'", command);
            exit (0);
          endif

        elseif (pid)

	  ## In the parent.

          fclose (nth (stdin_pipe, 1));
          fclose (nth (stdout_pipe, 2));

          if (fcntl (nth (stdout_pipe, 1), F_SETFL, O_NONBLOCK) < 0)
            error ("popen2: error setting file mode");
          else
            in = nth (stdin_pipe, 2);
            out = nth (stdout_pipe, 1);
          endif

        elseif (pid < 0)
          error ("popen2: fork failed -- unable to create child process");
        endif
      else
        error ("popen2: pipe creation failed");
      endif
    else
      error ("popen2: file name must be a string");
    endif
  else
    usage ("[in, out, pid] = popen2 (command, args)");
  endif

endfunction
