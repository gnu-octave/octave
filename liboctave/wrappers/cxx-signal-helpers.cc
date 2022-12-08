////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2022 The Octave Project Developers
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

#include <sys/types.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#  include <windows.h>
#else
#  include <pthread.h>
#endif

#include "signal-wrappers.h"

#if ! defined (__WIN32__) || defined (__CYGWIN__)

// The following pattern often used in C code to initialize a static
// variable could possibly cause trouble in multi-threaded code:
//
//   TYPE * get_var (void) {
//     static bool initialized = false;
//     static TYPE *var;
//     if (! initialized) {
//       var = ...;
//       initialized = true;
//     }
//     return var;
//   }
//
// Changing this code to
//
//   static TYPE *var = init_var (void);
//
// doesn't work in C because the static variable can't be initialized by
// a function call.  So we have to do this job in C++.  To avoid calling
// new, initialize sigset_t rather than a pointer to allocated storage.

static const sigset_t
init_async_signals (void)
{
  sigset_t sigmask;

  sigemptyset (&sigmask);

  // The signals listed here should match the list of signals that
  // we handle in the signal handler thread.

  // Interrupt signals.

#if defined (SIGINT)
  sigaddset (&sigmask, SIGINT);
#endif

#if defined (SIGBREAK)
  sigaddset (&sigmask, SIGBREAK);
#endif

  // Termination signals.

#if defined (SIGHUP)
  sigaddset (&sigmask, SIGHUP);
#endif

#if defined (SIGQUIT)
  sigaddset (&sigmask, SIGQUIT);
#endif

#if defined (SIGTERM)
  sigaddset (&sigmask, SIGTERM);
#endif

  // Alarm signals.

#if defined (SIGALRM)
  sigaddset (&sigmask, SIGALRM);
#endif

#if defined (SIGVTALRM)
  sigaddset (&sigmask, SIGVTALRM);
#endif

  // I/O signals.

#if defined (SIGLOST)
  sigaddset (&sigmask, SIGLOST);
#endif

#if defined (SIGPIPE)
  sigaddset (&sigmask, SIGPIPE);
#endif

  // Job control signals.

#if defined (SIGCHLD)
  sigaddset (&sigmask, SIGCHLD);
#endif

#if defined (SIGCLD)
  sigaddset (&sigmask, SIGCLD);
#endif

  // Resource limit signals.

#if defined (SIGXCPU)
  sigaddset (&sigmask, SIGXCPU);
#endif

#if defined (SIGXFSZ)
  sigaddset (&sigmask, SIGXFSZ);
#endif

  return sigmask;
}

// Initialized once, is const so we never write to it again and it can
// be accessed by multiple threads without locking.

static const sigset_t async_signals = init_async_signals ();

#endif

void
octave_block_async_signals (void)
{
#if ! defined (__WIN32__) || defined (__CYGWIN__)
  pthread_sigmask (SIG_BLOCK, &async_signals, 0);
#endif
}

void
octave_unblock_async_signals (void)
{
#if ! defined (__WIN32__) || defined (__CYGWIN__)
  pthread_sigmask (SIG_UNBLOCK, &async_signals, 0);
#endif
}

#if ! defined (__WIN32__) || defined (__CYGWIN__)

static void *
signal_watcher (void *arg)
{
  octave_sig_handler *handler = reinterpret_cast<octave_sig_handler *> (arg);

  octave_unblock_async_signals ();

  while (1)
    {
      int sig_caught;

      if (sigwait (&async_signals, &sig_caught))
        {
          // FIXME: what else should we do?
          abort ();
        }

      // Let handler have complete control over what to do.
      (*handler) (sig_caught);
    }
}

#endif

void
octave_create_interrupt_watcher_thread (octave_sig_handler *handler)
{
#if ! defined (__WIN32__)
  pthread_t sighandler_thread_id;

  if (pthread_create (&sighandler_thread_id, 0, signal_watcher,
                      reinterpret_cast<void *> (handler)))
    {
      // FIXME: what else should we do?
      abort ();
    }
#else
  octave_unblock_async_signals ();

  octave_unused_parameter (handler);
#endif
}
