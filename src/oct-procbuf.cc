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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>

#include <iostream.h>

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
#if defined (__CYGWIN32__)
static int Vkluge_procbuf_delay = 500000;
#else
static int Vkluge_procbuf_delay = 0;
#endif

// This class is based on the procbuf class from libg++, written by
// Per Bothner, Copyright (C) 1993 Free Software Foundation.
//
// It should work with the filebuf class from libg++, but it might not
// work with others since it depends on being able to get at the
// underlying file descriptor with filebuf::fd(), which is not
// standard.

static octave_procbuf *octave_procbuf_list = 0;

octave_procbuf *
octave_procbuf::open (const char *command, int mode)
{
#if defined (HAVE_SYS_WAIT_H)

  int pipe_fds[2];

  volatile int child_std_end = (mode & ios::in) ? 1 : 0;

  volatile int parent_end, child_end;

  if (is_open ())
    return 0;

  if (pipe (pipe_fds) < 0)
    return 0;

  if (mode & ios::in)
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
	  ::close (octave_procbuf_list->fd ());
	  octave_procbuf_list = octave_procbuf_list->next;
	}

      execl ("/bin/sh", "sh", "-c", command, NULL);

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

  attach (parent_end);

  next = octave_procbuf_list;
  octave_procbuf_list = this;

  return this;

#else

  return 0;

#endif
}

int
octave_procbuf::sys_close (void)
{
#if defined (HAVE_SYS_WAIT_H)

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

  if (status < 0 || ::close (fd ()) < 0)
    return -1;

  do
    {
      wait_pid = ::waitpid (proc_pid, &wstatus, 0);
    }
  while (wait_pid == -1 && errno == EINTR);

  if (wait_pid == -1)
    return -1;

  return wstatus;

#else

  return -1;

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
  DEFVAR (__kluge_procbuf_delay__, static_cast<double> (Vkluge_procbuf_delay),
	  kluge_procbuf_delay,
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
