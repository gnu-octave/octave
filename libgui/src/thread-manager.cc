/*

Copyright (C) 2013 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#include <windows.h>
#else
#include <pthread.h>
#endif

#include <sys/types.h>
#include <signal.h>

#include "thread-manager.h"

#if defined (__WIN32__) && ! defined (__CYGWIN__)

class windows_thread_manager : public octave_base_thread_manager
{
public:

  windows_thread_manager (void) : octave_base_thread_manager () { }

  void register_current_thread (void) { }

  void interrupt (void)
  {
    // FIXME: This doesn't work when octave_interrupt_immediately is
    // true.  Octave crashes, presumably in the call to
    // octave_jump_to_enclosing_context.  Does this happen because the
    // jump occurs while Octave is running in the wrong thread?  That
    // was happening on Unixy systems until we started using
    // pthread_kill and blocking interrupts from all threads except the
    // one running the Octave interpreter.

    ::raise (SIGINT);
  }
};

#else

class pthread_thread_manager : public octave_base_thread_manager
{
public:

  pthread_thread_manager (void)
    : octave_base_thread_manager (), my_thread (), initialized (false)
  { }

  void register_current_thread (void)
  {
    my_thread = pthread_self ();
    initialized = true;
  }

  void interrupt (void)
  {
    if (initialized)
      pthread_kill (my_thread, SIGINT);
  }

private:

  pthread_t my_thread;

  bool initialized;
};

#endif

octave_thread_manager::octave_thread_manager (void)
  : rep (octave_thread_manager::create_rep ())
{ }

static void
block_or_unblock_signal (int how, int sig)
{
  sigset_t signal_mask;

  sigemptyset (&signal_mask);

  sigaddset (&signal_mask, sig);

#if defined (__WIN32__) && ! defined (__CYGWIN__)
  sigprocmask (how, &signal_mask, 0);
#else
  pthread_sigmask (how, &signal_mask, 0);
#endif
}

void
octave_thread_manager::block_interrupt_signal (void)
{
  block_or_unblock_signal (SIG_BLOCK, SIGINT);
}

void
octave_thread_manager::unblock_interrupt_signal (void)
{
  block_or_unblock_signal (SIG_UNBLOCK, SIGINT);
}

octave_base_thread_manager *
octave_thread_manager::create_rep (void)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
  return new windows_thread_manager ();
#else
  return new pthread_thread_manager ();
#endif
}
