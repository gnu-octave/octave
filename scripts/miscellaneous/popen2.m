## Copyright (C) 1996 John W. Eaton
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

## usage: [IN, OUT, PID] = popen2 (COMMAND, ARGS)
##
## Start a subprocess with two-way communication.  COMMAND specifies
## the name of the command to start.  ARGS is an array of strings
## containing options for COMMAND.  IN and out are the file ids of the
## input and streams for the subprocess, and PID is the process id of
## the subprocess, or -1 if COMMAND could not be executed.
##
## Example:
##
##  [in, out, pid] = popen2 ("sort", "-nr");
##  fputs (in, "these\n");
##  fputs (in, "are\n");
##  fputs (in, "some\n");
##  fputs (in, "strings\n");
##  fclose (in);
##  while (isstr (s = fgets (out)))
##    fputs (stdout, s);
##  endwhile
##  fclose (out);

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

	  fclose (stdin_pipe (2));
	  fclose (stdout_pipe (1));

	  dup2 (stdin_pipe (1), stdin);
	  fclose (stdin_pipe (1));

	  dup2 (stdout_pipe (2), stdout);
	  fclose (stdout_pipe (2));

	  if (exec (command, args) < 0)
	    error ("popen2: unable to start process `%s'", command);
	    exit (0);
	  endif

	elseif (pid)

	  fclose (stdin_pipe (1));
	  fclose (stdout_pipe (2));

	  if (fcntl (stdout_pipe (1), __F_SETFL__, __O_NONBLOCK__) < 0)
	    error ("popen2: error setting file mode");
	  else
	    in = stdin_pipe (2);
	    out = stdout_pipe (1);
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
