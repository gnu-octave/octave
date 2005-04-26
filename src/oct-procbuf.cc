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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>

#include <iostream>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-procbuf.h"
#include "syswait.h"
#include "variables.h"

#include "defun.h"
#include "gripes.h"
#include "utils.h"

// Number of microseconds to delay in the parent after forking.
static int Vkluge_procbuf_delay = 0;

// This class is based on the procbuf class from libg++, written by
// Per Bothner, Copyright (C) 1993 Free Software Foundation.

static octave_procbuf *octave_procbuf_list = 0;

octave_procbuf *
octave_procbuf::open (const char *command, int mode)
{
#if defined (__CYGWIN32__)

  if (is_open ()) 
    return 0;

  f = popen (command, (mode & std::ios::in) ? "r" : "w");

  if (! f)
    return 0;

  // Oops... popen doesn't return the associated pid, so fake it for now

  proc_pid = 1;

  open_p = true;

  if (mode & std::ios::out)
    ::setvbuf (f, 0, _IOLBF, 0);

  return this;
  
#elif defined (HAVE_SYS_WAIT_H)

  int pipe_fds[2];

  volatile int child_std_end = (mode & std::ios::in) ? 1 : 0;

  volatile int parent_end, child_end;

  if (is_open ())
    return 0;

  if (pipe (pipe_fds) < 0)
    return 0;

  if (mode & std::ios::in)
    {
      parent_end = pipe_fds[0];
      child_end = pipe_fds[1];
    }
  else
    {
      parent_end = pipe_fds[1];
      child_end = pipe_fds[0];
    }

  proc_pid = ::fork ();

  if (proc_pid == 0)
    {
      ::close (parent_end);

      if (child_end != child_std_end)
	{
	  ::dup2 (child_end, child_std_end);
	  ::close (child_end);
	}

      while (octave_procbuf_list)
	{
	  FILE *fp = octave_procbuf_list->f;

	  if (fp)
	    {
	      ::fclose (fp);
	      fp = 0;
	    }

	  octave_procbuf_list = octave_procbuf_list->next;
	}

      execl ("/bin/sh", "sh", "-c", command, 0);

      exit (127);
    }

  if (Vkluge_procbuf_delay > 0)
    octave_usleep (Vkluge_procbuf_delay);

  ::close (child_end);

  if (proc_pid < 0)
    {
      ::close (parent_end);
      return 0;
    }

  f = ::fdopen (parent_end, (mode & std::ios::in) ? "r" : "w");

  if (mode & std::ios::out)
    ::setvbuf (f, 0, _IOLBF, 0);

  open_p = true;

  next = octave_procbuf_list;
  octave_procbuf_list = this;

  return this;

#else

  return 0;

#endif
}

octave_procbuf *
octave_procbuf::close (void)
{
#if defined (__CYGWIN32__)

  if (f)
    {
      wstatus = ::pclose (f);
      f = 0;
    }

  open_p = false;

  return this;
  
#elif defined (HAVE_SYS_WAIT_H)

  if (f)
    {
      pid_t wait_pid;

      int status = -1;

      for (octave_procbuf **ptr = &octave_procbuf_list;
	   *ptr != 0;
	   ptr = &(*ptr)->next)
	{
	  if (*ptr == this)
	    {
	      *ptr = (*ptr)->next;
	      status = 0;
	      break;
	    }
	}

      if (status == 0 && ::fclose (f) == 0)
	{
	  using namespace std;

	  do
	    {
	      wait_pid = ::waitpid (proc_pid, &wstatus, 0);
	    }
	  while (wait_pid == -1 && errno == EINTR);
	}

      f = 0;
    }

  open_p = false;

  return this;

#else

  return 0;

#endif
}

static int
kluge_procbuf_delay (void)
{
  double val;
  if (builtin_real_scalar_variable ("__kluge_procbuf_delay__", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && (double) ival == val)
	{
	  Vkluge_procbuf_delay = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("__kluge_procbuf_delay__");
  return -1;
}

void
symbols_of_oct_procbuf (void)
{
  DEFVAR (__kluge_procbuf_delay__, Vkluge_procbuf_delay, kluge_procbuf_delay,
    "-*- texinfo -*-\n\
@defvr __kluge_procbuf_delay__\n\
Number of microseconds to delay in the parent after forking.\n\
@end defvr");

}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
