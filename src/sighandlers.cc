// sighandlers.cc                                         -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

/*
 * Some of these may eventually perform different actions...
 */

static RETSIGTYPE
sigabrt_handler (int i)
{
  my_friendly_exit ("SIGABRT", i);
}

static RETSIGTYPE
sigalrm_handler (int i)
{
  my_friendly_exit ("SIGALRM", i);
}

static RETSIGTYPE
sigbus_handler (int i)
{
  my_friendly_exit ("SIGBUS", i);
}

static RETSIGTYPE
sigemt_handler (int i)
{
  my_friendly_exit ("SIGEMT", i);
}

static RETSIGTYPE
sigfpe_handler (int i)
{
  my_friendly_exit ("SIGFPE", i);
}

static RETSIGTYPE
sighup_handler (int i)
{
  my_friendly_exit ("SIGHUP", i);
}

static RETSIGTYPE
sigill_handler (int i)
{
  my_friendly_exit ("SIGILL", i);
}

/*
 * Handle SIGINT by restarting the parser (see octave.cc).
 */
static RETSIGTYPE
sigint_handler (int i)
{
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
sigiot_handler (int i)
{
  my_friendly_exit ("SIGIOT", i);
}

static RETSIGTYPE
siglost_handler (int i)
{
  my_friendly_exit ("SIGLOST", i);
}

static RETSIGTYPE
sigpipe_handler (int i)
{
  if (pipe_handler_error_count++ == 0)
    message ((char *) NULL, "broken pipe");

// Don\'t loop forever on account of this.
  if (pipe_handler_error_count  > 100)
    jump_to_top_level ();

#if RETSIGTYPE == void
  return;
#else
  return 0;
#endif
}

static RETSIGTYPE
sigpoll_handler (int i)
{
  my_friendly_exit ("SIGPOLL", i);
}

static RETSIGTYPE
sigprof_handler (int i)
{
  my_friendly_exit ("SIGPROF", i);
}

static RETSIGTYPE
sigquit_handler (int i)
{
  my_friendly_exit ("SIGQUIT", i);
}

static RETSIGTYPE
sigsegv_handler (int i)
{
  my_friendly_exit ("SIGSEGV", i);
}

static RETSIGTYPE
sigsys_handler (int i)
{
  my_friendly_exit ("SIGSYS", i);
}

static RETSIGTYPE
sigterm_handler (int i)
{
  my_friendly_exit ("SIGTERM", i);
}

static RETSIGTYPE
sigtrap_handler (int i)
{
  my_friendly_exit ("SIGTRAP", i);
}

static RETSIGTYPE
sigusr1_handler (int i)
{
  my_friendly_exit ("SIGUSR1", i);
}

static RETSIGTYPE
sigusr2_handler (int i)
{
  my_friendly_exit ("SIGUSR2", i);
}

static RETSIGTYPE
sigvtalrm_handler (int i)
{
  my_friendly_exit ("SIGVTALRM", i);
}

static RETSIGTYPE
sigxcpu_handler (int i)
{
  my_friendly_exit ("SIGXCPU", i);
}

static RETSIGTYPE
sigxfsz_handler (int i)
{
  my_friendly_exit ("SIGXFSZ", i);
}

/*
 * Install all the handlers for the signals we might care about.
 */
void
install_signal_handlers (void)
{
#ifdef SIGABRT
  signal (SIGABRT, sigabrt_handler);
#endif

#ifdef SIGALRM
  signal (SIGALRM, sigalrm_handler);
#endif

#ifdef SIGBUS
  signal (SIGBUS, sigbus_handler);
#endif

#ifdef SIGEMT
  signal (SIGEMT, sigemt_handler);
#endif

#ifdef SIGFPE
  signal (SIGFPE, sigfpe_handler);
#endif

#ifdef SIGHUP
  signal (SIGHUP, sighup_handler);
#endif

#ifdef SIGILL
  signal (SIGILL, sigill_handler);
#endif

#ifdef SIGINT
  signal (SIGINT, sigint_handler);
#endif

#ifdef SIGIOT
  signal (SIGIOT, sigiot_handler);
#endif

#ifdef SIGLOST
  signal (SIGLOST, siglost_handler);
#endif

#ifdef SIGPIPE
  signal (SIGPIPE, sigpipe_handler);
#endif

#ifdef SIGPOLL
  signal (SIGPOLL, sigpoll_handler);
#endif

#ifdef SIGPROF
  signal (SIGPROF, sigprof_handler);
#endif

#ifdef SIGQUIT
  signal (SIGQUIT, sigquit_handler);
#endif

#ifdef SIGSEGV
  signal (SIGSEGV, sigsegv_handler);
#endif

#ifdef SIGSYS
  signal (SIGSYS, sigsys_handler);
#endif

#ifdef SIGTERM
  signal (SIGTERM, sigterm_handler);
#endif

#ifdef SIGTRAP
  signal (SIGTRAP, sigtrap_handler);
#endif

#ifdef SIGUSR1
  signal (SIGUSR1, sigusr1_handler);
#endif

#ifdef SIGUSR2
  signal (SIGUSR2, sigusr2_handler);
#endif

#ifdef SIGVTALRM
  signal (SIGVTALRM, sigvtalrm_handler);
#endif

#ifdef SIGXCPU
  signal (SIGXCPU, sigxcpu_handler);
#endif

#ifdef SIGXFSZ
  signal (SIGXFSZ, sigxfsz_handler);
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
