// sighandlers.cc                                         -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <new.h>
#include <signal.h>

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
  error ("caught %s -- stopping myself...", sig_name);
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
  signal (SIGPOLL, generic_sig_handler);
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

#ifdef SIGXCPU
  signal (SIGXCPU, generic_sig_handler);
#endif

#ifdef SIGXFSZ
  signal (SIGXFSZ, generic_sig_handler);
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
