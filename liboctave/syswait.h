/*

Copyright (C) 1996, 1997, 2005, 2006, 2007 John W. Eaton

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

#if !defined (octave_syswait_h)
#define octave_syswait_h 1

#ifdef __cplusplus
extern "C" {
#endif

/* This mess suggested by the autoconf manual.  */

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
#define WAITPID(a, b, c) \
  wait4 ((a) == -1 ? 0 : (a), (union wait *)(b), c, 0)

/* Use the defaults below.  */
#undef WIFEXITED
#undef WEXITSTATUS
#undef WIFSIGNALLED
#endif

/* NeXT has sys/wait.h, but it is not compatible with POSIX.1, so we
   try to define waitpid in terms of wait4.  */

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

#if defined (__MINGW32__) || defined (_MSC_VER)
#define HAVE_WAITPID 1
#include <process.h>
#define WAITPID(a, b, c) _cwait (b, a, c)
/* Action argument is ignored for _cwait, so arbitrary definition.  */
#define WNOHANG 0
#else
#define WAITPID(a, b, c) waitpid (a, b, c)
#endif

#ifdef __cplusplus
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
