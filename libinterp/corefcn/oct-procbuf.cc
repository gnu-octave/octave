////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>

#include <iomanip>

// FIXME: we would prefer to avoid including these directly in Octave
// sources, but eliminating them is complicated by the mingling of
// octave_procbuf_list and the calls to system library functions like
// execl.

#if defined (HAVE_UNISTD_H)
#  if defined (HAVE_SYS_TYPES_H)
#    include <sys/types.h>
#  endif
#  include <unistd.h>
#endif

#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-procbuf.h"
#include "oct-syscalls.h"
#include "sysdep.h"
#include "unistd-wrappers.h"
#include "variables.h"

#include "defun.h"
#include "errwarn.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

#if ! defined (SHELL_PATH)
#  define SHELL_PATH "/bin/sh"
#endif

// This class is based on the procbuf class from libg++, written by
// Per Bothner, Copyright (C) 1993 Free Software Foundation.

#if (! (defined (__CYGWIN__) || defined (__MINGW32__) || defined (_MSC_VER)) \
     && defined (HAVE_UNISTD_H))

static procbuf *procbuf_list = nullptr;

#endif

#if ! defined (BUFSIZ)
#  define BUFSIZ 1024
#endif

procbuf *
procbuf::open (const char *command, int mode)
{
#if defined (__CYGWIN__) || defined (__MINGW32__) || defined (_MSC_VER)

  if (is_open ())
    return 0;

  m_f = (octave::popen (command, (mode & std::ios::in) ? "r" : "w"));

  if (! m_f)
    return 0;

  // Oops... popen doesn't return the associated pid, so fake it for now

  m_proc_pid = 1;

  m_open_p = true;

  if (mode & std::ios::out)
    ::setvbuf (m_f, nullptr, _IOLBF, BUFSIZ);

  return this;

#elif defined (HAVE_UNISTD_H)

  int pipe_fds[2];

  volatile int child_std_end = (mode & std::ios::in) ? 1 : 0;

  volatile int parent_end, child_end;

  if (is_open ())
    return nullptr;

  if (octave::sys::pipe (pipe_fds) < 0)
    return nullptr;

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

  m_proc_pid = ::fork ();

  if (m_proc_pid == 0)
    {
      octave_close_wrapper (parent_end);

      if (child_end != child_std_end)
        {
          octave_dup2_wrapper (child_end, child_std_end);
          octave_close_wrapper (child_end);
        }

      while (procbuf_list)
        {
          FILE *fp = procbuf_list->m_f;

          if (fp)
            {
              std::fclose (fp);
              fp = nullptr;
            }

          procbuf_list = procbuf_list->m_next;
        }

      execl (SHELL_PATH, "sh", "-c", command, static_cast<void *> (nullptr));

      exit (127);
    }

  octave_close_wrapper (child_end);

  if (m_proc_pid < 0)
    {
      octave_close_wrapper (parent_end);
      return nullptr;
    }

  m_f = (::fdopen (parent_end, (mode & std::ios::in) ? "r" : "w"));

  if (mode & std::ios::out)
    ::setvbuf (m_f, nullptr, _IOLBF, BUFSIZ);

  m_open_p = true;

  m_next = procbuf_list;
  procbuf_list = this;

  return this;

#else

  return 0;

#endif
}

procbuf *
procbuf::close (void)
{
#if defined (__CYGWIN__) || defined (__MINGW32__) || defined (_MSC_VER)

  if (m_f)
    {
      m_wstatus = octave::pclose (m_f);
      m_f = 0;
    }

  m_open_p = false;

  return this;

#elif defined (HAVE_UNISTD_H)

  if (m_f)
    {
      pid_t wait_pid;

      int status = -1;

      for (procbuf **ptr = &procbuf_list;
           *ptr != nullptr;
           ptr = &(*ptr)->m_next)
        {
          if (*ptr == this)
            {
              *ptr = (*ptr)->m_next;
              status = 0;
              break;
            }
        }

      if (status == 0 && std::fclose (m_f) == 0)
        {
          using namespace std;

          do
            {
              wait_pid = octave::sys::waitpid (m_proc_pid, &m_wstatus, 0);
            }
          while (wait_pid == -1 && errno == EINTR);
        }

      m_f = nullptr;
    }

  m_open_p = false;

  return this;

#else

  return 0;

#endif
}

OCTAVE_END_NAMESPACE(octave)
