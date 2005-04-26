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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_syswait_h)
#define octave_syswait_h 1

// This mess suggested by the autoconf manual.

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if defined (NeXT) && ! defined (_POSIX_SOURCE)
#define HAVE_SYS_WAIT_H
#endif

#if defined HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if defined (NeXT)
#define HAVE_WAITPID 1
#define waitpid(a, b, c) \
  wait4 ((a) == -1 ? 0 : (a), (union wait *)(b), c, 0)

// Use the defaults below
#undef WIFEXITED
#undef WEXITSTATUS
#undef WIFSIGNALLED
#endif

// NeXT has sys/wait.h, but it is not compatible with POSIX.1, so we
// try to define waitpid in terms of wait4.

#if defined (NeXT)
#include <sys/wait.h>
#define waitpid(a, b, c) \
  wait4 ((a) == -1 ? 0 : (a), (union wait *)(b), c, 0)
#endif

#ifndef WIFEXITED
#define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifndef WEXITSTATUS
#define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif

#ifndef WIFSIGNALLED
#define WIFSIGNALLED(stat_val) \
  (((stat_val) & 0177) != 0177 && ((stat_val) & 0177) != 0)
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
