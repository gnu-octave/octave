////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2026 The Octave Project Developers
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

#if ! defined (octave_oct_syscalls_h)
#define octave_oct_syscalls_h 1

#include "octave-config.h"

#include <cstdio>
#include <functional>
#include <string>

#include "unwind-prot.h"

class string_vector;

#include <sys/types.h>

OCTAVE_BEGIN_NAMESPACE(octave)
OCTAVE_BEGIN_NAMESPACE(sys)

extern OCTAVE_API int dup (int old_fd);

extern OCTAVE_API int dup2 (int, int);
extern OCTAVE_API int dup2 (int, int, std::string&);

extern OCTAVE_API int execvp (const std::string&, const string_vector&);
extern OCTAVE_API int execvp (const std::string&, const string_vector&,
                              std::string&);

extern OCTAVE_API pid_t fork (std::string&);

extern OCTAVE_API pid_t vfork (std::string&);

extern OCTAVE_API pid_t getpgrp (std::string&);

extern OCTAVE_API pid_t getpid ();

extern OCTAVE_API pid_t getppid ();

extern OCTAVE_API gid_t getgid ();

extern OCTAVE_API gid_t getegid ();

extern OCTAVE_API uid_t getuid ();

extern OCTAVE_API uid_t geteuid ();

extern OCTAVE_API int pipe (int *);
extern OCTAVE_API int pipe (int *, std::string&);

extern OCTAVE_API int read (int fd, void *buf, size_t count);

extern OCTAVE_API int close (int fd);

extern OCTAVE_API pid_t waitpid (pid_t, int *status, int);
extern OCTAVE_API pid_t waitpid (pid_t, int *status, int, std::string&);

extern OCTAVE_API int wcontinue ();

extern OCTAVE_API int wcoredump (int status);

extern OCTAVE_API bool wifcontinued (int status);

extern OCTAVE_API bool wifexited (int status);

extern OCTAVE_API bool wifsignaled (int status);

extern OCTAVE_API bool wifstopped (int status);

extern OCTAVE_API int wexitstatus (int status);

extern OCTAVE_API int wnohang ();

extern OCTAVE_API int wstopsig (int status);

extern OCTAVE_API int wtermsig (int status);

extern OCTAVE_API int wuntraced ();

extern OCTAVE_API int kill (pid_t, int);
extern OCTAVE_API int kill (pid_t, int, std::string&);

extern OCTAVE_API pid_t
popen2 (const std::string&, const string_vector&, bool, int *);

extern OCTAVE_API pid_t
popen2 (const std::string&, const string_vector&, bool, int *,
        std::string&);

extern OCTAVE_API int fcntl (int, int, long);
extern OCTAVE_API int fcntl (int, int, long, std::string&);

template <typename F>
auto capture_stderr (F fn, std::string& err_str)
{
  // create pipe
  int fds[2];
  if (pipe (fds) != 0)
    return fn ();

  octave::unwind_action close_read_pipe ([fds] () { close (fds[0]); });

  int old_stderr;
  std::invoke_result_t<F> result {};

  {
    // close the write end of pipe before reading from read end
    octave::unwind_action close_write_pipe ([fds] () { close (fds[1]); });

    // save old stderr
    old_stderr = dup (fileno (stderr));
    if (old_stderr < 0)
      return fn ();

    // redirect stderr to pipe
    if (dup2 (fds[1], fileno (stderr)) < 0)
      return fn ();

    // restore stderr
    octave::unwind_action restore_stderr (
      [old_stderr] ()
      { 
        fflush (stderr);
        dup2 (old_stderr, fileno (stderr));
        close (old_stderr);
      });

    // call function
    if constexpr (std::is_void_v<std::invoke_result_t<F>>)
      fn ();
    else
      result = fn ();
  }

  // read from pipe
  fflush (stderr);
  char buffer[4096];
  for (;;)
    {
      int n = read (fds[0], buffer, sizeof (buffer));
      if (n <= 0)
        break;
      err_str.append (buffer, n);
    }

  if constexpr (std::is_void_v<std::invoke_result_t<F>>)
    return;
  else
    return result;
}

template <typename F>
auto capture_stderr (F fn)
{
  std::string err_str;

  return capture_stderr (fn, err_str);
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
