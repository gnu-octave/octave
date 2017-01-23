/*

Copyright (C) 2013-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <windows.h>
#else
#  include <pthread.h>
#endif

#include "signal-wrappers.h"

#include "thread-manager.h"

#if defined (OCTAVE_USE_WINDOWS_API)

class windows_thread_manager : public octave_base_thread_manager
{
public:

  windows_thread_manager (void) : octave_base_thread_manager () { }

  void register_current_thread (void) { }

  void interrupt (void)
  {
    GenerateConsoleCtrlEvent (CTRL_C_EVENT, 0);
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
      {
        // Send SIGINT to all other processes in our process group.
        // This is needed to interrupt calls to system (), for example.

        // FIXME: What happens if some code inside a
        // {BEGIN,END}_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE block starts
        // additional threads and one of those happens to catch this signal?
        // Would the interrupt handler and the subsequent longjmp and exception
        // all be executed in the wrong thread?  If so, is there any way to
        // prevent that from happening?

        int sigint;
        octave_get_sig_number ("SIGINT", &sigint);

        octave_kill_wrapper (0, sigint);
      }
  }

private:

  pthread_t my_thread;

  bool initialized;
};

#endif

octave_thread_manager::octave_thread_manager (void)
  : rep (octave_thread_manager::create_rep ())
{ }

void
octave_thread_manager::block_interrupt_signal (void)
{
  octave_block_interrupt_signal ();
}

void
octave_thread_manager::unblock_interrupt_signal (void)
{
  octave_unblock_interrupt_signal ();
}

octave_base_thread_manager *
octave_thread_manager::create_rep (void)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  return new windows_thread_manager ();
#else
  return new pthread_thread_manager ();
#endif
}
