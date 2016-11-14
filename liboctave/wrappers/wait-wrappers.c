/*

Copyright (C) 2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// These functions may be provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sys/types.h>
#include <sys/wait.h>

#include "wait-wrappers.h"

#if ! defined (WCONTINUE)
#  define WCONTINUE 0
#endif

#if ! defined (WNOHANG)
#  define WNOHANG 0
#endif

#if ! defined (WUNTRACED)
#  define WUNTRACED 0
#endif

pid_t
octave_waitpid_wrapper (pid_t pid, int *statusp, int options)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
  // gnulib's waitpid replacement currently uses _cwait, which
  // apparently only works with console applications.
  *statusp = 0;
  return -1;
#else
  return waitpid (pid, statusp, options);
#endif
}

int
octave_wcontinue_wrapper (void)
{
  return WCONTINUE;
}

int
octave_wcoredump_wrapper (int status)
{
  return WCOREDUMP (status);
}

#if ! defined (WIFCONTINUED)
#  define WIFCONTINUED(x) false
#endif

bool
octave_wifcontinued_wrapper (int status)
{
  return WIFCONTINUED (status);
}

bool
octave_wifexited_wrapper (int status)
{
  return WIFEXITED (status);
}

bool
octave_wifsignaled_wrapper (int status)
{
  return WIFSIGNALED (status);
}

bool
octave_wifstopped_wrapper (int status)
{
  return WIFSTOPPED (status);
}

int
octave_wexitstatus_wrapper (int status)
{
  return WEXITSTATUS (status);
}

int
octave_wnohang_wrapper (void)
{
  return WNOHANG;
}

int
octave_wstopsig_wrapper (int status)
{
  return WSTOPSIG (status);
}

int
octave_wtermsig_wrapper (int status)
{
  return WTERMSIG (status);
}

int
octave_wuntraced_wrapper (void)
{
  return WUNTRACED;
}

