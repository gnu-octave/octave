////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

// These functions may be provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#  include <windows.h>
#else
#  include <pthread.h>
#endif

#include "signal-wrappers.h"

int
octave_kill_wrapper (pid_t pid, int signum)
{
#if defined (HAVE_KILL)
  return kill (pid, signum);
#elif defined (HAVE_RAISE)
  octave_unused_parameter (pid);

  return raise (signum);
#else
  octave_unused_parameter (pid);
  octave_unused_parameter (signum);

  return -1;
#endif
}

char *
octave_strsignal_wrapper (int signum)
{
  return strsignal (signum);
}

bool
octave_have_kill (void)
{
#if defined (HAVE_KILL)
  return true;
#else
  return false;
#endif
}

bool
octave_get_sig_number (const char *signame, int *signum)
{
  *signum = -1;

  // FIXME: this should probably use a perfect hash function.

  if (! strcmp (signame, "SIGINT"))
    {
#if defined (SIGINT)
      *signum = SIGINT;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGBREAK"))
    {
#if defined (SIGBREAK)
      *signum = SIGBREAK;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGABRT"))
    {
#if defined (SIGABRT)
      *signum = SIGABRT;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGALRM"))
    {
#if defined (SIGALRM)
      *signum = SIGALRM;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGBUS"))
    {
#if defined (SIGBUS)
      *signum = SIGBUS;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGCHLD"))
    {
#if defined (SIGCHLD)
      *signum = SIGCHLD;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGCLD"))
    {
#if defined (SIGCLD)
      *signum = SIGCLD;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGCONT"))
    {
#if defined (SIGCONT)
      *signum = SIGCONT;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGEMT"))
    {
#if defined (SIGEMT)
      *signum = SIGEMT;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGFPE"))
    {
#if defined (SIGFPE)
      *signum = SIGFPE;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGHUP"))
    {
#if defined (SIGHUP)
      *signum = SIGHUP;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGILL"))
    {
#if defined (SIGILL)
      *signum = SIGILL;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGINFO"))
    {
#if defined (SIGINFO)
      *signum = SIGINFO;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGIOT"))
    {
#if defined (SIGIOT)
      *signum = SIGIOT;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGKILL"))
    {
#if defined (SIGKILL)
      *signum = SIGKILL;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGLOST"))
    {
#if defined (SIGLOST)
      *signum = SIGLOST;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGPIPE"))
    {
#if defined (SIGPIPE)
      *signum = SIGPIPE;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGPOLL"))
    {
#if defined (SIGPOLL)
      *signum = SIGPOLL;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGPROF"))
    {
#if defined (SIGPROF)
      *signum = SIGPROF;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGPWR"))
    {
#if defined (SIGPWR)
      *signum = SIGPWR;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGQUIT"))
    {
#if defined (SIGQUIT)
      *signum = SIGQUIT;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGSEGV"))
    {
#if defined (SIGSEGV)
      *signum = SIGSEGV;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGSTOP"))
    {
#if defined (SIGSTOP)
      *signum = SIGSTOP;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGSYS"))
    {
#if defined (SIGSYS)
      *signum = SIGSYS;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGTERM"))
    {
#if defined (SIGTERM)
      *signum = SIGTERM;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGTRAP"))
    {
#if defined (SIGTRAP)
      *signum = SIGTRAP;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGTSTP"))
    {
#if defined (SIGTSTP)
      *signum = SIGTSTP;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGTTIN"))
    {
#if defined (SIGTTIN)
      *signum = SIGTTIN;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGTTOU"))
    {
#if defined (SIGTTOU)
      *signum = SIGTTOU;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGURG"))
    {
#if defined (SIGURG)
      *signum = SIGURG;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGUSR1"))
    {
#if defined (SIGUSR1)
      *signum = SIGUSR1;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGUSR2"))
    {
#if defined (SIGUSR2)
      *signum = SIGUSR2;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGVTALRM"))
    {
#if defined (SIGVTALRM)
      *signum = SIGVTALRM;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGIO"))
    {
#if defined (SIGIO)
      *signum = SIGIO;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGWINCH"))
    {
#if defined (SIGWINCH)
      *signum = SIGWINCH;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGXCPU"))
    {
#if defined (SIGXCPU)
      *signum = SIGXCPU;
      return true;
#endif
    }
  else if (! strcmp (signame, "SIGXFSZ"))
    {
#if defined (SIGXFSZ)
      *signum = SIGXFSZ;
      return true;
#endif
    }

  return false;
}

octave_sig_handler *
octave_set_signal_handler_internal (int sig, octave_sig_handler *handler,
                                    bool restart_syscalls)
{
  struct sigaction act, oact;

  act.sa_handler = handler;
  act.sa_flags = 0;

#if defined (SIGALRM)
  if (sig == SIGALRM)
    {
#  if defined (SA_INTERRUPT)
      act.sa_flags |= SA_INTERRUPT;
#  endif
    }
#endif
#if defined (SA_RESTART)
#  if defined (SIGALRM)
  else
#  endif
  // FIXME: Do we also need to explicitly disable SA_RESTART?
  if (restart_syscalls)
    act.sa_flags |= SA_RESTART;
#endif

  sigemptyset (&act.sa_mask);
  sigemptyset (&oact.sa_mask);

  sigaction (sig, &act, &oact);

  return oact.sa_handler;
}

octave_sig_handler *
octave_set_signal_handler_by_name (const char *signame,
                                   octave_sig_handler *handler,
                                   bool restart_syscalls)
{
  int sig;

  return (octave_get_sig_number (signame, &sig)
          ? octave_set_signal_handler_internal (sig, handler, restart_syscalls)
          : 0);
}

octave_sig_handler *
octave_set_default_signal_handler (int sig)
{
  return octave_set_signal_handler_internal (sig, SIG_DFL, true);
}

octave_sig_handler *
octave_set_default_signal_handler_by_name (const char *signame)
{
  return octave_set_signal_handler_by_name (signame, SIG_DFL, true);
}

int
octave_num_signals (void)
{
  return NSIG;
}

typedef struct
{
  sigset_t nvar;
  sigset_t ovar;
} sigset_info;

void *
octave_block_child (void)
{
#if defined (SIGCHLD) || defined (SIGCLD)

  sigset_info *context = (sigset_info *) malloc (sizeof (sigset_info));

  if (context)
    {
      sigemptyset (&(context->ovar));
      sigemptyset (&(context->nvar));
#if defined (SIGCHLD)
      sigaddset (&(context->nvar), SIGCHLD);
#endif
#if defined (SIGCLD)
      sigaddset (&(context->nvar), SIGCLD);
#endif
      sigprocmask (SIG_BLOCK, &(context->nvar), &(context->ovar));
    }

  return context;

#else

  return 0;

#endif
}

void
octave_unblock_child (void *context_arg)
{
  if (context_arg)
    {
      sigset_info *context = (sigset_info *) context_arg;

      sigprocmask (SIG_SETMASK, &(context->ovar), 0);

      free (context);
    }
}

static void
block_or_unblock_signal (int how, int sig)
{
#if ! defined (__WIN32__) || defined (__CYGWIN__)

  // Blocking/unblocking signals at thread level is only supported
  // on platform with fully compliant POSIX threads. This is not
  // supported on Win32. Moreover, we have to make sure that SIGINT
  // handler is not installed before calling AllocConsole: installing
  // a SIGINT handler internally calls SetConsoleCtrlHandler, which
  // must be called after AllocConsole to be effective.

  sigset_t signal_mask;

  sigemptyset (&signal_mask);

  sigaddset (&signal_mask, sig);

  pthread_sigmask (how, &signal_mask, 0);

#else

  octave_unused_parameter (how);
  octave_unused_parameter (sig);

#endif
}

void
octave_block_interrupt_signal (void)
{
  block_or_unblock_signal (SIG_BLOCK, SIGINT);

#if defined (SIGBREAK)
  block_or_unblock_signal (SIG_BLOCK, SIGBREAK);
#endif
}

void
octave_unblock_interrupt_signal (void)
{
  block_or_unblock_signal (SIG_UNBLOCK, SIGINT);

#if defined (SIGBREAK)
  block_or_unblock_signal (SIG_UNBLOCK, SIGBREAK);
#endif
}

static void
block_or_unblock_signal_by_name (int how, const char *signame)
{
  int sig;

  if (octave_get_sig_number (signame, &sig))
    block_or_unblock_signal (how, sig);
}

void
octave_block_signal_by_name (const char *signame)
{
  block_or_unblock_signal_by_name (SIG_BLOCK, signame);
}

void
octave_unblock_signal_by_name (const char *signame)
{
  block_or_unblock_signal_by_name (SIG_UNBLOCK, signame);
}

/* Allow us to save the signal mask and then restore it to the most
   recently saved value.  This is necessary when using the POSIX signal
   handling interface on some systems calling longjmp out of the signal
   handler to get to the top level on an interrupt doesn't restore the
   original signal mask.  Alternatively, we could use
   sigsetjmp/siglongjmp, but saving and restoring the signal mask
   ourselves works ok and seems simpler just now.  */

static sigset_t octave_signal_mask;

void
octave_save_signal_mask (void)
{
  sigprocmask (0, 0, &octave_signal_mask);
}

void
octave_restore_signal_mask (void)
{
  sigprocmask (SIG_SETMASK, &octave_signal_mask, 0);
}

void *
octave_alloc_signal_mask (void)
{
  return malloc (sizeof (sigset_t));
}

void
octave_free_signal_mask (void *mask)
{
  free (mask);
}

void
octave_get_signal_mask (void *mask)
{
  sigprocmask (0, 0, mask);
}

void
octave_set_signal_mask (void *mask)
{
  sigprocmask (SIG_SETMASK, (sigset_t *) mask, 0);
}

int
octave_raise_wrapper (int signum)
{
  return raise (signum);
}

#if ! defined (__WIN32__)
static void
print_sigset (FILE *of, const char *prefix, const sigset_t *sigset)
{
  int sig;
  int cnt = 0;

  for (sig = 1; sig < NSIG; sig++)
    {
      if (sigismember (sigset, sig))
        {
          cnt++;
          fprintf (of, "%ld: %s%d (%s)\n", (long int) pthread_self (),
                   prefix, sig, strsignal (sig));
        }
    }

  if (cnt == 0)
    fprintf (of, "%ld: %s<empty signal set>\n", (long int) pthread_self (),
             prefix);
}

static int
print_sigmask (FILE *of, const char *msg)
{
  sigset_t sigmask;

  if (msg)
    fprintf (of, "%s", msg);

  if (pthread_sigmask (SIG_BLOCK, NULL, &sigmask) == -1)
    return -1;

  print_sigset (of, "\t\t", &sigmask);

  return 0;
}
#endif

void
octave_show_sigmask (const char *msg)
{
#if ! defined (__WIN32__)
  if (! msg)
    msg = "signal mask\n";

  print_sigmask (stderr, msg);
#else
  octave_unused_parameter (msg);

  fputs ("no signal mask on Windows systems\n", stderr);
#endif
}
