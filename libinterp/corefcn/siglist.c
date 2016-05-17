/*

Copyright (C) 2000-2015 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#include "config.h"
#endif

#include <signal.h>

#include "siglist.h"

/* The following is all borrowed from Emacs.  */

#if ! (defined HAVE_STRSIGNAL || HAVE_DECL_SYS_SIGLIST)

static char *my_sys_siglist[NSIG];

#if defined (sys_siglist)
#undef sys_siglist
#endif
#define sys_siglist my_sys_siglist

#endif

void
init_signals (void)
{
#if ! (defined HAVE_STRSIGNAL || HAVE_DECL_SYS_SIGLIST)

  static int initialized = 0;

  if (! initialized)
    {
      initialized = 1;

# if defined (SIGABRT)
      sys_siglist[SIGABRT] = "Aborted";
# endif
# if defined (SIGAIO)
      sys_siglist[SIGAIO] = "LAN I/O interrupt";
# endif
# if defined (SIGALRM)
      sys_siglist[SIGALRM] = "Alarm clock";
# endif
# if defined (SIGBUS)
      sys_siglist[SIGBUS] = "Bus error";
# endif
# if defined (SIGCLD)
      sys_siglist[SIGCLD] = "Child status changed";
# endif
# if defined (SIGCHLD)
      sys_siglist[SIGCHLD] = "Child status changed";
# endif
# if defined (SIGCONT)
      sys_siglist[SIGCONT] = "Continued";
# endif
# if defined (SIGDANGER)
      sys_siglist[SIGDANGER] = "Swap space dangerously low";
# endif
# if defined (SIGDGNOTIFY)
      sys_siglist[SIGDGNOTIFY] = "Notification message in queue";
# endif
# if defined (SIGEMT)
      sys_siglist[SIGEMT] = "Emulation trap";
# endif
# if defined (SIGFPE)
      sys_siglist[SIGFPE] = "Arithmetic exception";
# endif
# if defined (SIGFREEZE)
      sys_siglist[SIGFREEZE] = "SIGFREEZE";
# endif
# if defined (SIGGRANT)
      sys_siglist[SIGGRANT] = "Monitor mode granted";
# endif
# if defined (SIGHUP)
      sys_siglist[SIGHUP] = "Hangup";
# endif
# if defined (SIGILL)
      sys_siglist[SIGILL] = "Illegal instruction";
# endif
# if defined (SIGINT)
      sys_siglist[SIGINT] = "Interrupt";
# endif
# if defined (SIGIO)
      sys_siglist[SIGIO] = "I/O possible";
# endif
# if defined (SIGIOINT)
      sys_siglist[SIGIOINT] = "I/O intervention required";
# endif
# if defined (SIGIOT)
      sys_siglist[SIGIOT] = "IOT trap";
# endif
# if defined (SIGKILL)
      sys_siglist[SIGKILL] = "Killed";
# endif
# if defined (SIGLOST)
      sys_siglist[SIGLOST] = "Resource lost";
# endif
# if defined (SIGLWP)
      sys_siglist[SIGLWP] = "SIGLWP";
# endif
# if defined (SIGMSG)
      sys_siglist[SIGMSG] = "Monitor mode data available";
# endif
# if defined (SIGPHONE)
      sys_siglist[SIGPHONE] = "SIGPHONE";
# endif
# if defined (SIGPIPE)
      sys_siglist[SIGPIPE] = "Broken pipe";
# endif
# if defined (SIGPOLL)
      sys_siglist[SIGPOLL] = "Pollable event occurred";
# endif
# if defined (SIGPROF)
      sys_siglist[SIGPROF] = "Profiling timer expired";
# endif
# if defined (SIGPTY)
      sys_siglist[SIGPTY] = "PTY I/O interrupt";
# endif
# if defined (SIGPWR)
      sys_siglist[SIGPWR] = "Power-fail restart";
# endif
# if defined (SIGQUIT)
      sys_siglist[SIGQUIT] = "Quit";
# endif
# if defined (SIGRETRACT)
      sys_siglist[SIGRETRACT] = "Need to relinguish monitor mode";
# endif
# if defined (SIGSAK)
      sys_siglist[SIGSAK] = "Secure attention";
# endif
# if defined (SIGSEGV)
      sys_siglist[SIGSEGV] = "Segmentation violation";
# endif
# if defined (SIGSOUND)
      sys_siglist[SIGSOUND] = "Sound completed";
# endif
# if defined (SIGSTKFLT)
      sys_siglist[SIGSTKFLT] = "Stack fault";
# endif
# if defined (SIGSTOP)
      sys_siglist[SIGSTOP] = "Stopped (signal)";
# endif
# if defined (SIGSTP)
      sys_siglist[SIGSTP] = "Stopped (user)";
# endif
# if defined (SIGSYS)
      sys_siglist[SIGSYS] = "Bad argument to system call";
# endif
# if defined (SIGTERM)
      sys_siglist[SIGTERM] = "Terminated";
# endif
# if defined (SIGTHAW)
      sys_siglist[SIGTHAW] = "SIGTHAW";
# endif
# if defined (SIGTRAP)
      sys_siglist[SIGTRAP] = "Trace/breakpoint trap";
# endif
# if defined (SIGTSTP)
      sys_siglist[SIGTSTP] = "Stopped (user)";
# endif
# if defined (SIGTTIN)
      sys_siglist[SIGTTIN] = "Stopped (tty input)";
# endif
# if defined (SIGTTOU)
      sys_siglist[SIGTTOU] = "Stopped (tty output)";
# endif
# if defined (SIGUNUSED)
      sys_siglist[SIGUNUSED] = "SIGUNUSED";
# endif
# if defined (SIGURG)
      sys_siglist[SIGURG] = "Urgent I/O condition";
# endif
# if defined (SIGUSR1)
      sys_siglist[SIGUSR1] = "User defined signal 1";
# endif
# if defined (SIGUSR2)
      sys_siglist[SIGUSR2] = "User defined signal 2";
# endif
# if defined (SIGVTALRM)
      sys_siglist[SIGVTALRM] = "Virtual timer expired";
# endif
# if defined (SIGWAITING)
      sys_siglist[SIGWAITING] = "Process's LWPs are blocked";
# endif
# if defined (SIGWINCH)
      sys_siglist[SIGWINCH] = "Window size changed";
# endif
# if defined (SIGWIND)
      sys_siglist[SIGWIND] = "SIGWIND";
# endif
# if defined (SIGXCPU)
      sys_siglist[SIGXCPU] = "CPU time limit exceeded";
# endif
# if defined (SIGXFSZ)
      sys_siglist[SIGXFSZ] = "File size limit exceeded";
# endif
    }

#endif
}

#if ! defined (HAVE_STRSIGNAL)

char *
strsignal (int code)
{
  char *signame = "";

  if (0 <= code && code < NSIG)
    {
      /* Cast to suppress warning if the table has const char *.  */
      signame = (char *) sys_siglist[code];
    }

  return signame;
}

#endif
