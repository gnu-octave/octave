/*

Copyright (C) 1996-2015 John W. Eaton

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

#if ! defined (octave_oct_syscalls_h)
#define octave_oct_syscalls_h 1

#include "octave-config.h"

#include <string>

class string_vector;

#include <sys/types.h>

namespace
octave
{
  namespace
  syscalls
  {
    extern OCTAVE_API int dup2 (int, int);
    extern OCTAVE_API int dup2 (int, int, std::string&);

    extern OCTAVE_API int execvp (const std::string&, const string_vector&);
    extern OCTAVE_API int execvp (const std::string&, const string_vector&,
                                  std::string&);

    extern OCTAVE_API pid_t fork (std::string&);

    extern OCTAVE_API pid_t vfork (std::string&);

    extern OCTAVE_API pid_t getpgrp (std::string&);

    extern OCTAVE_API pid_t getpid (void);

    extern OCTAVE_API pid_t getppid (void);

    extern OCTAVE_API gid_t getgid (void);

    extern OCTAVE_API gid_t getegid (void);

    extern OCTAVE_API uid_t getuid (void);

    extern OCTAVE_API uid_t geteuid (void);

    extern OCTAVE_API int pipe (int *);
    extern OCTAVE_API int pipe (int *, std::string&);

    extern OCTAVE_API pid_t waitpid (pid_t, int *status, int);
    extern OCTAVE_API pid_t waitpid (pid_t, int *status, int, std::string&);

    extern OCTAVE_API int kill (pid_t, int);
    extern OCTAVE_API int kill (pid_t, int, std::string&);

    extern OCTAVE_API pid_t
    popen2 (const std::string&, const string_vector&, bool, int *);

    extern OCTAVE_API pid_t
    popen2 (const std::string&, const string_vector&, bool, int *,
            std::string&);

    extern OCTAVE_API pid_t
    popen2 (const std::string&, const string_vector&, bool, int *,
            std::string&, bool &interactive);

    extern OCTAVE_API int fcntl (int, int, long);
    extern OCTAVE_API int fcntl (int, int, long, std::string&);
  }
}

#endif

