////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

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
#include <string.h>

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define WIN32_LEAN_AND_MEAN 1
#  include <windows.h>

#  include "uniconv-wrappers.h"
#else
#  include <errno.h>
#  include <fcntl.h>
#  include <sys/types.h>
#  include <unistd.h>
#endif

#include "octave-popen2.h"

#if defined (__WIN32__) && ! defined (__CYGWIN__)

static char *
make_command_string (const char *cmd, char *const *args)
{
  char *const *argp;
  size_t cmd_len;
  char *command;

  // Count Command length, quotes, and terminating NUL character.
  cmd_len = strlen (cmd) + 3;

  // Count argument length, space, and quotes.
  // Ignore first arg as it is the command.
  argp = args;
  while (*++argp)
    cmd_len += strlen (*argp) + 3;

  command = (char *) malloc (cmd_len);

  sprintf (command, "\"%s\"", cmd);

  argp = args;
  while (*++argp)
    sprintf (command, "%s \"%s\"", command, *argp);

  return command;
}

pid_t
octave_popen2 (const char *cmd, char *const *args, bool sync_mode,
               int *fildes, const char **errmsg)
{
  pid_t pid;

  char *command;
  bool status;

  PROCESS_INFORMATION pi;
  STARTUPINFO si;

  HANDLE hProcess = GetCurrentProcess ();
  HANDLE childRead, childWrite, parentRead, parentWrite;
  DWORD pipeMode;

  ZeroMemory (&pi, sizeof (pi));
  ZeroMemory (&si, sizeof (si));
  si.cb = sizeof (si);

  if (! CreatePipe (&childRead, &parentWrite, 0, 0)
      || ! DuplicateHandle (hProcess, childRead, hProcess, &childRead,
                            0, TRUE,
                            DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE))
    {
      *errmsg = "popen2: pipe creation failed";
      return -1;
    }

  if (! CreatePipe (&parentRead, &childWrite, 0, 0)
      || ! DuplicateHandle (hProcess, childWrite, hProcess, &childWrite,
                            0, TRUE,
                            DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE))
    {
      *errmsg = "popen2: pipe creation failed";
      return -1;
    }

  if (! sync_mode)
    {
      pipeMode = PIPE_NOWAIT;
      SetNamedPipeHandleState (parentRead, &pipeMode, 0, 0);
    }

  fildes[1] = _open_osfhandle ((intptr_t) parentRead, _O_RDONLY | _O_BINARY);
  fildes[0] = _open_osfhandle ((intptr_t) parentWrite, _O_WRONLY | _O_BINARY);

  si.dwFlags |= STARTF_USESTDHANDLES;

  si.hStdInput = childRead;
  si.hStdOutput = childWrite;
  si.hStdError = GetStdHandle (STD_ERROR_HANDLE);

  command = make_command_string (cmd, args);

  wchar_t *wcmd = u8_to_wchar (command);

  free (command);

  status = CreateProcessW (NULL, wcmd, NULL, NULL, TRUE, CREATE_NO_WINDOW,
                           NULL, NULL, &si, &pi);

  free (wcmd);

  if (! status)
    {
      *errmsg = "popen2: process creation failed";
      return -1;
    }

  pid = pi.dwProcessId;

  CloseHandle (childRead);
  CloseHandle (childWrite);

  CloseHandle (pi.hProcess);
  CloseHandle (pi.hThread);

  return pid;
}

#else

pid_t
octave_popen2 (const char *cmd, char *const *args, bool sync_mode,
               int *fildes, const char **errmsg)
{
  pid_t pid;

  int child_stdin[2], child_stdout[2];

  if (pipe (child_stdin) < 0)
    {
      *errmsg = strerror (errno);
      return -1;
    }

  if (pipe (child_stdout) < 0)
    {
      close (child_stdin[0]);
      close (child_stdin[1]);

      *errmsg = strerror (errno);
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

      _exit (127);
    }
  else if (pid > 0)
    {
      // Parent process

      close (child_stdin[0]);
      close (child_stdout[1]);

#if defined (F_SETFL) && defined (O_NONBLOCK)
      if (! sync_mode && fcntl (child_stdout[0], F_SETFL, O_NONBLOCK) < 0)
        {
          *errmsg = strerror (errno);
          return -1;
        }
      else
#endif
        {
          fildes[0] = child_stdin[1];
          fildes[1] = child_stdout[0];

          return pid;
        }
    }

  *errmsg = "foobar!";
  *errmsg = strerror (errno);
  return pid;
}

#endif
