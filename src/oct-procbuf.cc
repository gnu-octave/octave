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
#include "oct-syscalls.h"
#include "variables.h"

#include "defun.h"
#include "gripes.h"
#include "utils.h"

// This class is based on the procbuf class from libg++, written by
// Per Bothner, Copyright (C) 1993 Free Software Foundation.

static octave_procbuf *octave_procbuf_list = 0;

#if defined (__CYGWIN__)
#define W32POPEN popen
#define W32PCLOSE pclose
#elif defined (__MINGW32__)
#define W32POPEN _popen
#define W32PCLOSE _pclose
#endif

octave_procbuf *
octave_procbuf::open (const char *command, int mode)
{
#if defined (__CYGWIN__) || defined (__MINGW32__)

  if (is_open ()) 
    return 0;

  f = ::W32POPEN (command, (mode & std::ios::in) ? "r" : "w");

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

      execl ("/bin/sh", "sh", "-c", command, static_cast<void *> (0));

      exit (127);
    }

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


#if defined (__CYGWIN__) || defined (__MINGW32__)

  if (f)
    {
      wstatus = ::W32PCLOSE (f);
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
	      wait_pid = octave_syscalls::waitpid (proc_pid, &wstatus, 0);
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
