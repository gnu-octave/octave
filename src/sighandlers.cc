// sighandlers.cc                                         -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#include <cnew>
#include <csignal>

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/wait.h>

#include <iostream.h>

#include "sighandlers.h"
#include "octave.h"
#include "error.h"
#include "utils.h"

// Nonzero means we have already printed a message for this series of
// SIGPIPES.  We assume that the writer will eventually give up.
int pipe_handler_error_count = 0;

// Nonzero means we can be interrupted.
int can_interrupt = 0;

static void
my_friendly_exit (const char *sig_name, int sig_number)
{
  error ("%s -- stopping myself...", sig_name);
  clean_up_and_exit (sig_number);
}

// I know, not really a signal handler.

static void
octave_new_handler (void)
{
  error ("new: virtual memory exhausted -- stopping myself");
  clean_up_and_exit (1);
}

static RETSIGTYPE
generic_sig_handler (int i)
{
  my_friendly_exit (sys_siglist[i], i);

#if RETSIGTYPE == void
  return;
#else
  return 0;
#endif
}

// Handle SIGCHLD.  Should use waitpid and ignore stopped jobs.
// Needs to restore state of plotter such that it will be restarted
// again when needed.  Needs to close file descriptors corresponding
// to processes started with execute().

#if 0
static RETSIGTYPE
sigchld_handler (int i)
{
  int status;
  pid_t pid = wait (&status);

  if (pid < 0)
    cerr << "wait error\n";
  else
    {
      cerr << "sigchld caught, PID = " << pid << "; status: ";

      int lo_byte = (status & 0xff);
      int hi_byte = ((status >> 8) & 0xff);
      if (lo_byte == 0177)
	{
	  cerr << "stopped with signal = " << hi_byte << "\n";
	}
      else if (lo_byte)
	{
	  int sig_num = (lo_byte & 0x7f);
	  cerr << "stopped with signal = " << sig_num << "\n";
	  if (lo_byte & 0200)
	    cerr << "child dumped core\n";
	}
      else
	{
	  cerr << "exited with status = " << hi_byte << "\n";
	}
    }

  signal (SIGCHLD, sigchld_handler);
}
#endif

// Handle SIGINT by restarting the parser (see octave.cc).

// XXX FIXME XXX -- it would probably be good to try to use POSIX
// signal interface if it is available.

static RETSIGTYPE
sigint_handler (int i)
{
// Can this ever cause trouble on systems that don't forget signal
// handlers when they are invoked?

  signal (SIGINT, sigint_handler);

  if (can_interrupt)
    {
      jump_to_top_level ();
      panic_impossible ();
    }

#if RETSIGTYPE == void
  return;
#else
  return 0;
#endif
}

static RETSIGTYPE
sigpipe_handler (int i)
{
// Can this ever cause trouble on systems that don't forget signal
// handlers when they are invoked?

  signal (SIGPIPE, sigpipe_handler);

  if (pipe_handler_error_count++ == 0)
    message (0, "broken pipe");

// Don\'t loop forever on account of this.
  if (pipe_handler_error_count  > 100)
    jump_to_top_level ();

#if RETSIGTYPE == void
  return;
#else
  return 0;
#endif
}

// Install all the handlers for the signals we might care about.

void
install_signal_handlers (void)
{
  set_new_handler (octave_new_handler);

#ifdef SIGABRT
  signal (SIGABRT, generic_sig_handler);
#endif

#ifdef SIGALRM
  signal (SIGALRM, generic_sig_handler);
#endif

#ifdef SIGBUS
  signal (SIGBUS, generic_sig_handler);
#endif

#if 0
#ifdef SIGCHLD
  signal (SIGCHLD, sigchld_handler);
#endif
#endif

#ifdef SIGEMT
  signal (SIGEMT, generic_sig_handler);
#endif

#ifdef SIGFPE
  signal (SIGFPE, generic_sig_handler);
#endif

#ifdef SIGHUP
  signal (SIGHUP, generic_sig_handler);
#endif

#ifdef SIGILL
  signal (SIGILL, generic_sig_handler);
#endif

#ifdef SIGINT
  signal (SIGINT, sigint_handler);
#endif

#ifdef SIGIOT
  signal (SIGIOT, generic_sig_handler);
#endif

#ifdef SIGLOST
  signal (SIGLOST, generic_sig_handler);
#endif

#ifdef SIGPIPE
  signal (SIGPIPE, sigpipe_handler);
#endif

#ifdef SIGPOLL
  signal (SIGPOLL, SIG_IGN);
#endif

#ifdef SIGPROF
  signal (SIGPROF, generic_sig_handler);
#endif

#ifdef SIGQUIT
  signal (SIGQUIT, generic_sig_handler);
#endif

#ifdef SIGSEGV
  signal (SIGSEGV, generic_sig_handler);
#endif

#ifdef SIGSYS
  signal (SIGSYS, generic_sig_handler);
#endif

#ifdef SIGTERM
  signal (SIGTERM, generic_sig_handler);
#endif

#ifdef SIGTRAP
  signal (SIGTRAP, generic_sig_handler);
#endif

#ifdef SIGUSR1
  signal (SIGUSR1, generic_sig_handler);
#endif

#ifdef SIGUSR2
  signal (SIGUSR2, generic_sig_handler);
#endif

#ifdef SIGVTALRM
  signal (SIGVTALRM, generic_sig_handler);
#endif

#ifdef SIGIO
  signal (SIGIO, SIG_IGN);
#endif

#ifdef SIGXCPU
  signal (SIGXCPU, generic_sig_handler);
#endif

#ifdef SIGXFSZ
  signal (SIGXFSZ, generic_sig_handler);
#endif
}

#ifndef HAVE_SYS_SIGLIST
char *sys_siglist[NSIG + 1] =
{
#ifdef AIX
/* AIX has changed the signals a bit */
  "bogus signal",			/* 0 */
  "hangup",				/* 1  SIGHUP */
  "interrupt",				/* 2  SIGINT */
  "quit",				/* 3  SIGQUIT */
  "illegal instruction",		/* 4  SIGILL */
  "trace trap",				/* 5  SIGTRAP */
  "IOT instruction",			/* 6  SIGIOT */
  "crash likely",			/* 7  SIGDANGER */
  "floating point exception",		/* 8  SIGFPE */
  "kill",				/* 9  SIGKILL */
  "bus error",				/* 10 SIGBUS */
  "segmentation violation",		/* 11 SIGSEGV */
  "bad argument to system call",	/* 12 SIGSYS */
  "write on a pipe with no one to read it", /* 13 SIGPIPE */
  "alarm clock",			/* 14 SIGALRM */
  "software termination signum",	/* 15 SIGTERM */
  "user defined signal 1",		/* 16 SIGUSR1 */
  "user defined signal 2",		/* 17 SIGUSR2 */
  "death of a child",			/* 18 SIGCLD */
  "power-fail restart",			/* 19 SIGPWR */
  "bogus signal",			/* 20 */
  "bogus signal",			/* 21 */
  "bogus signal",			/* 22 */
  "bogus signal",			/* 23 */
  "bogus signal",			/* 24 */
  "LAN I/O interrupt",			/* 25 SIGAIO */
  "PTY I/O interrupt",			/* 26 SIGPTY */
  "I/O intervention required",		/* 27 SIGIOINT */
  "HFT grant",				/* 28 SIGGRANT */
  "HFT retract",			/* 29 SIGRETRACT */
  "HFT sound done",			/* 30 SIGSOUND */
  "HFT input ready",			/* 31 SIGMSG */
#else /* not AIX */
  "bogus signal",			/* 0 */
  "hangup",				/* 1  SIGHUP */
  "interrupt",				/* 2  SIGINT */
  "quit",				/* 3  SIGQUIT */
  "illegal instruction",		/* 4  SIGILL */
  "trace trap",				/* 5  SIGTRAP */
  "IOT instruction",			/* 6  SIGIOT */
  "EMT instruction",			/* 7  SIGEMT */
  "floating point exception",		/* 8  SIGFPE */
  "kill",				/* 9  SIGKILL */
  "bus error",				/* 10 SIGBUS */
  "segmentation violation",		/* 11 SIGSEGV */
  "bad argument to system call",	/* 12 SIGSYS */
  "write on a pipe with no one to read it", /* 13 SIGPIPE */
  "alarm clock",			/* 14 SIGALRM */
  "software termination signum",	/* 15 SIGTERM */
  "user defined signal 1",		/* 16 SIGUSR1 */
  "user defined signal 2",		/* 17 SIGUSR2 */
  "death of a child",			/* 18 SIGCLD */
  "power-fail restart",			/* 19 SIGPWR */
#ifdef sun
  "window size change",			    /* 20 SIGWINCH */
  "urgent socket condition",		    /* 21 SIGURG */
  "pollable event occured",		    /* 22 SIGPOLL */
  "stop (cannot be caught or ignored)", /*  23 SIGSTOP */
  "user stop requested from tty",	    /* 24 SIGTSTP */
  "stopped process has been continued",	/* 25 SIGCONT */
  "background tty read attempted",	    /* 26 SIGTTIN */
  "background tty write attempted",    /* 27 SIGTTOU */
  "virtual timer expired",		    /* 28 SIGVTALRM */
  "profiling timer expired",		    /* 29 SIGPROF */
  "exceeded cpu limit",			    /* 30 SIGXCPU */
  "exceeded file size limit",		    /* 31 SIGXFSZ */
  "process's lwps are blocked",	    /*  32 SIGWAITING */
  "special signal used by thread library", /* 33 SIGLWP */
#ifdef SIGFREEZE
  "Special Signal Used By CPR",	    /* 34 SIGFREEZE */
#endif
#ifdef SIGTHAW
  "Special Signal Used By CPR",	    /* 35 SIGTHAW */
#endif
#endif /* sun */
#endif /* not AIX */
  0
  };
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
