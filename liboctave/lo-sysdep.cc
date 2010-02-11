/*

Copyright (C) 1996, 1997, 2000, 2001, 2005, 2006, 2007, 2008 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <string>
#include <vector>

#include <sys/types.h>
#include <unistd.h>

#include <fcntl.h>

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#include <windows.h>
#ifdef max
# undef min
# undef max
#endif
#endif

#include "file-ops.h"
#include "lo-error.h"
#include "pathlen.h"
#include "lo-sysdep.h"
#include "str-vec.h"
#include "oct-locbuf.h"

std::string
octave_getcwd (void)
{
  std::string retval;

  // Using the gnulib getcwd module ensures that we have a getcwd that
  // will allocate a buffer as large as necessary if buf and size are
  // both 0.

  char *tmp = getcwd (0, 0);

  if (tmp)
    {
      retval = tmp;
      free (tmp);
    }
  else
    (*current_liboctave_error_handler) ("unable to find current directory");

  return retval;
}

int
octave_chdir (const std::string& path_arg)
{
  std::string path = file_ops::tilde_expand (path_arg);

#if defined (__EMX__)
  int retval = -1;

  char *tmp_path = strsave (path.c_str ());

  if (path.length () == 2 && path[1] == ':')
    {
      char *upper_case_dir_name = strupr (tmp_path);
      _chdrive (upper_case_dir_name[0]);
      if (_getdrive () == upper_case_dir_name[0])
        retval = _chdir2 ("/");
    }
  else
    retval = _chdir2 (tmp_path);

  delete [] tmp_path;

  return retval;
#else

#if defined (__WIN32__) && ! defined (__CYGWIN__)
  if (path.length() == 2 && path[1] == ':')
    path += "\\";
#endif

  return chdir (path.c_str ());
#endif
}

#if defined (__WIN32__) && ! defined (__CYGWIN__)

pid_t
octave_popen2 (const std::string& cmd, const string_vector& args, bool sync_mode,
    int *fildes, std::string& msg)
{
  pid_t pid;
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  std::string command = "\"" + cmd + "\"";
  HANDLE hProcess = GetCurrentProcess(), childRead, childWrite, parentRead, parentWrite;
  DWORD pipeMode;

  ZeroMemory (&pi, sizeof (pi));
  ZeroMemory (&si, sizeof (si));
  si.cb = sizeof (si);

  if (! CreatePipe (&childRead, &parentWrite, 0, 0) ||
      ! DuplicateHandle (hProcess, childRead, hProcess, &childRead, 0, TRUE, DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE))
    {
      msg = "popen2: pipe creation failed";
      return -1;
    }
  if (! CreatePipe (&parentRead, &childWrite, 0, 0) ||
      ! DuplicateHandle (hProcess, childWrite, hProcess, &childWrite, 0, TRUE, DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE))
    {
      msg = "popen2: pipe creation failed";
      return -1;
    }
  if (! sync_mode)
    {
      pipeMode = PIPE_NOWAIT;
      SetNamedPipeHandleState (parentRead, &pipeMode, 0, 0);
    }
  fildes[1] = _open_osfhandle (reinterpret_cast<long> (parentRead), _O_RDONLY | _O_BINARY);
  fildes[0] = _open_osfhandle (reinterpret_cast<long> (parentWrite), _O_WRONLY | _O_BINARY);
  si.dwFlags |= STARTF_USESTDHANDLES;
  si.hStdInput = childRead;
  si.hStdOutput = childWrite;

  // Ignore first arg as it is the command
  for (int k=1; k<args.length(); k++)
    command += " \"" + args[k] + "\"";
  OCTAVE_LOCAL_BUFFER (char, c_command, command.length () + 1);
  strcpy (c_command, command.c_str ());
  if (! CreateProcess (0, c_command, 0, 0, TRUE, 0, 0, 0, &si, &pi))
    {
      msg = "popen2: process creation failed";
      return -1;
    }
  pid = pi.dwProcessId;

  CloseHandle (childRead);
  CloseHandle (childWrite);
  CloseHandle (pi.hProcess);
  CloseHandle (pi.hThread);

  return pid;
}

#endif

#if defined (_MSC_VER) && ! defined (HAVE_DIRENT_H)

// FIXME -- it would probably be better to adapt the versions of
// opendir, readdir, and closedir from Emacs as they appear to be more
// complete implementations (do the functions below work for network
// paths, for example)?  We can probably get along without rewinddir.

struct __DIR
{
  HANDLE hnd;
  WIN32_FIND_DATA fd;
  int dirty;
  struct direct d;
  const char *current;
};

DIR *
opendir (const char *name)
{
  DIR *d = static_cast<DIR *> (malloc (sizeof (DIR)));
  static char buffer[MAX_PATH];

  strncpy (buffer, name, MAX_PATH);
  if (buffer[strnlen(buffer, MAX_PATH)-1] != '\\')
    strncat (buffer, "\\*", MAX_PATH);
  else
    strncat (buffer, "*", MAX_PATH);
  d->current = buffer;
  d->hnd = FindFirstFile (buffer, &(d->fd));
  if (d->hnd == INVALID_HANDLE_VALUE)
    return 0;
  d->dirty = 1;
  return d;
}

void
rewinddir (DIR *d)
{
  if (d->hnd != INVALID_HANDLE_VALUE)
    FindClose (d->hnd);
  d->hnd = FindFirstFile (d->current, &(d->fd));
  d->dirty = 1;
}

void
closedir (DIR *d)
{
  if (d->hnd != INVALID_HANDLE_VALUE)
    FindClose (d->hnd);
  free (d);
}

struct direct *
readdir (DIR *d)
{
  if (! d->dirty)
    {
      if (! FindNextFile(d->hnd, &(d->fd)))
        return 0;
    }
  d->d.d_name = d->fd.cFileName;
  d->dirty = 0;
  return &(d->d);
}

#endif
