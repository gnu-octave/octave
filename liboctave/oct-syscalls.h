/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_syscalls_h)
#define octave_syscalls_h 1

#include <string>

class string_vector;

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

struct
octave_syscalls
{
  static int dup2 (int, int);
  static int dup2 (int, int, string&);

  static int execvp (const string&, const string_vector&);
  static int execvp (const string&, const string_vector&, string&);

  static int fcntl (int, int, long);
  static int fcntl (int, int, long, string&);

  static pid_t fork (string&);
  static pid_t vfork (string&);

  static pid_t getpgrp (string&);

  static pid_t getpid (void);
  static pid_t getppid (void);

  static gid_t getgid (void);
  static gid_t getegid (void);

  static uid_t getuid (void);
  static uid_t geteuid (void);

  static int pipe (int *);
  static int pipe (int *, string&);

  static pid_t waitpid (pid_t, int);
  static pid_t waitpid (pid_t, int, string&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
