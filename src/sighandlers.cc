/*

Copyright (C) 1996 John W. Eaton

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

#include <csignal>
#include <new>

#include <iostream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include "error.h"
#include "load-save.h"
#include "pager.h"
#include "sighandlers.h"
#include "syswait.h"
#include "toplev.h"
#include "utils.h"

// Nonzero means we have already printed a message for this series of
// SIGPIPES.  We assume that the writer will eventually give up.
int pipe_handler_error_count = 0;

// Nonzero means we can be interrupted.
int can_interrupt = 0;

// Allow us to save the signal mask and then restore it to the most
// recently saved value.  This is necessary when using the POSIX
// signal handling interface on some systems calling longjmp out of
// the signal handler to get to the top level on an interrupt doesn't
// restore the original signal mask.  Alternatively, we could use
// sigsetjmp/siglongjmp, but saving and restoring the signal mask
// ourselves works ok and seems simpler just now.

#if defined (HAVE_POSIX_SIGNALS)
static sigset_t octave_signal_mask;
#endif

void
octave_save_signal_mask (void)
{
#if defined (HAVE_POSIX_SIGNALS)
  sigprocmask (0, 0, &octave_signal_mask);
#endif
}

void
octave_restore_signal_mask (void)
{
#if defined (HAVE_POSIX_SIGNALS)
  sigprocmask (SIG_SETMASK, &octave_signal_mask, 0);
#endif
}

static void
my_friendly_exit (const char *sig_name, int sig_number)
{
  error ("%s -- stopping myself...", sig_name);

  save_user_variables ();

  clean_up_and_exit (sig_number);
}

// I know, not really a signal handler.

static void
octave_new_handler (void)
{
  error ("memory exhausted -- trying to return to prompt");

  if (can_interrupt)
    {
      jump_to_top_level ();
      panic_impossible ();
    }
  else
    my_friendly_exit ("operator new", 1);
}

sig_handler *
octave_set_signal_handler (int sig, sig_handler *handler)
{
#if defined (HAVE_POSIX_SIGNALS)
  struct sigaction act, oact;
  act.sa_handler = handler;
  act.sa_flags = 0;
  sigemptyset (&act.sa_mask);
  sigemptyset (&oact.sa_mask);
  sigaction (sig, &act, &oact);
  return oact.sa_handler;
#else
  return signal (sig, handler);
#endif
}

static RETSIGTYPE
generic_sig_handler (int sig)
{
  my_friendly_exit (sys_siglist[sig], sig);

#if RETSIGTYPE == void
  return;
#else
  return 0;
#endif
}

// Handle SIGCHLD.

// XXX FIXME XXX -- this should probably be implemented by having a
// global list of pids to check and a corresponding list of functions
// to call if a pid is recognized.  That way, we just have to register
// functions elsewhere and this function doesn't have to change.

static RETSIGTYPE
sigchld_handler (int /* sig */)
{
  // Can this ever cause trouble on systems that don't forget signal
  // handlers when they are invoked?

  octave_set_signal_handler (SIGCHLD, sigchld_handler);

  int status;
  pid_t pid = wait (&status);

  if (pid > 0)
    {
      if (WIFEXITED (status) || WIFSIGNALLED (status))
	{
	  if (pid == octave_pager_pid)
	    {
	      error ("connection to external pager lost --");
	      error ("pending computations have been discarded\n");
	    }
	}
    }
}

#if defined (__alpha__)
static RETSIGTYPE
sigfpe_handler (int /* sig */)
{
  // Can this ever cause trouble on systems that don't forget signal
  // handlers when they are invoked?

  octave_set_signal_handler (SIGFPE, sigfpe_handler);

  error ("floating point exception -- trying to return to prompt");

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
#endif

// Handle SIGINT by restarting the parser (see octave.cc).

static RETSIGTYPE
sigint_handler (int /* sig */)
{
  // Can this ever cause trouble on systems that don't forget signal
  // handlers when they are invoked?

  octave_set_signal_handler (SIGINT, sigint_handler);

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
sigpipe_handler (int /* sig */)
{
  // Can this ever cause trouble on systems that don't forget signal
  // handlers when they are invoked?

  octave_set_signal_handler (SIGPIPE, sigpipe_handler);

  if (pipe_handler_error_count++ == 0)
    message (0, "broken pipe");

  // Don't loop forever on account of this.

  if (pipe_handler_error_count  > 100)
    jump_to_top_level ();

#if RETSIGTYPE == void
  return;
#else
  return 0;
#endif
}

void
catch_interrupts (void)
{
  octave_set_signal_handler (SIGINT, sigint_handler);
}

// Install all the handlers for the signals we might care about.

void
install_signal_handlers (void)
{
  set_new_handler (octave_new_handler);

#ifdef SIGABRT
  octave_set_signal_handler (SIGABRT, generic_sig_handler);
#endif

#ifdef SIGALRM
  octave_set_signal_handler (SIGALRM, generic_sig_handler);
#endif

#ifdef SIGBUS
  octave_set_signal_handler (SIGBUS, generic_sig_handler);
#endif

#ifdef SIGCHLD
  octave_set_signal_handler (SIGCHLD, sigchld_handler);
#endif

#ifdef SIGEMT
  octave_set_signal_handler (SIGEMT, generic_sig_handler);
#endif

#ifdef SIGFPE
#if defined (__alpha__)
  octave_set_signal_handler (SIGFPE, sigfpe_handler);
#else
  octave_set_signal_handler (SIGFPE, generic_sig_handler);
#endif
#endif

#ifdef SIGHUP
  octave_set_signal_handler (SIGHUP, generic_sig_handler);
#endif

#ifdef SIGILL
  octave_set_signal_handler (SIGILL, generic_sig_handler);
#endif

#ifdef SIGINT
  octave_set_signal_handler (SIGINT, sigint_handler);
#endif

#ifdef SIGIOT
  octave_set_signal_handler (SIGIOT, generic_sig_handler);
#endif

#ifdef SIGLOST
  octave_set_signal_handler (SIGLOST, generic_sig_handler);
#endif

#ifdef SIGPIPE
  octave_set_signal_handler (SIGPIPE, sigpipe_handler);
#endif

#ifdef SIGPOLL
  octave_set_signal_handler (SIGPOLL, SIG_IGN);
#endif

#ifdef SIGPROF
  octave_set_signal_handler (SIGPROF, generic_sig_handler);
#endif

#ifdef SIGQUIT
  octave_set_signal_handler (SIGQUIT, generic_sig_handler);
#endif

#ifdef SIGSEGV
  octave_set_signal_handler (SIGSEGV, generic_sig_handler);
#endif

#ifdef SIGSYS
  octave_set_signal_handler (SIGSYS, generic_sig_handler);
#endif

#ifdef SIGTERM
  octave_set_signal_handler (SIGTERM, generic_sig_handler);
#endif

#ifdef SIGTRAP
  octave_set_signal_handler (SIGTRAP, generic_sig_handler);
#endif

#ifdef SIGUSR1
  octave_set_signal_handler (SIGUSR1, generic_sig_handler);
#endif

#ifdef SIGUSR2
  octave_set_signal_handler (SIGUSR2, generic_sig_handler);
#endif

#ifdef SIGVTALRM
  octave_set_signal_handler (SIGVTALRM, generic_sig_handler);
#endif

#ifdef SIGIO
  octave_set_signal_handler (SIGIO, SIG_IGN);
#endif

#ifdef SIGXCPU
  octave_set_signal_handler (SIGXCPU, generic_sig_handler);
#endif

#ifdef SIGXFSZ
  octave_set_signal_handler (SIGXFSZ, generic_sig_handler);
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
;;; End: ***
*/
