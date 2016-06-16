/*

Copyright (C) 2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// It makes more sense to define octave_popen2 with the wrapper
// functions than it does to try to provide wrappers for all the
// individual functions and macros that it uses and that may be provided
// by gnulib.  We don't include gnulib headers directly in Octave's C++
// source files to avoid problems that may be caused by the way that
// gnulib overrides standard library functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <fcntl.h>

#include <sys/types.h>
#include <unistd.h>

#include "octave-popen2.h"

pid_t
octave_popen2 (const char *cmd, char *const *args, bool sync_mode, int *fildes)
{
  pid_t pid;

  int child_stdin[2], child_stdout[2];

  if (pipe (child_stdin) < 0)
    return -1;

  if (pipe (child_stdout) < 0)
    {
      close (child_stdin[0]);
      close (child_stdin[1]);

      return -1;
    }

  pid = fork ();

  if (pid == 0)
    {
      // Child process

      close (child_stdin[1]);
      close (child_stdout[0]);

      if (dup2 (child_stdin[0], STDIN_FILENO) >= 0)
        {
          close (child_stdin[0]);

          if (dup2 (child_stdout[1], STDOUT_FILENO) >= 0)
            {
              close (child_stdout[1]);

              if (execvp (cmd, args) < 0)
                perror ("popen2 (child)");
            }
          else
            perror ("popen2 (child)");
        }
      else
        perror ("popen2 (child)");

      exit (0);
    }
  else if (pid > 0)
    {
      // Parent process

      close (child_stdin[0]);
      close (child_stdout[1]);

#if defined (F_SETFL) && defined (O_NONBLOCK)
      if (! sync_mode && fcntl (child_stdout[0], F_SETFL, O_NONBLOCK) < 0)
        return -1;
      else
#endif
        {
          fildes[0] = child_stdin[1];
          fildes[1] = child_stdout[0];

          return pid;
        }
    }

  return pid;
}
