/*

Copyright (C) 2011-2012 Jacob Dawid
Copyright (C) 2011-2012 John P. Swensen

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

#ifndef OCTAVELINK_H
#define OCTAVELINK_H

#include <queue>
#include <string>

class octave_mutex;

#include "octave-main-thread.h"
#include "octave-event.h"
#include "octave-event-observer.h"
#include "octave-event-listener.h"

// \class OctaveLink
// \brief Provides threadsafe access to octave.
// \author Jacob Dawid
//
// This class is a wrapper around octave and provides thread safety by
// buffering access operations to octave and executing them in the
// readline event hook, which lives in the octave thread.

class octave_link : public octave_event_observer
{
protected:

  octave_link (void);

public:

  ~octave_link (void) { }

  static void launch_octave (void)
  {
    if (instance_ok ())
      instance->do_launch_octave ();
  }

  static void register_event_listener (octave_event_listener *el)
  {
    if (instance_ok ())
      instance->do_register_event_listener (el);
  }

  static void generate_events (void)
  {
    if (instance_ok ())
      instance->do_generate_events ();
  }

  static void process_events (void)
  {
    if (instance_ok ())
      instance->do_process_events ();
  }

  static void post_event (octave_event *e)
  {
    if (instance_ok ())
      instance->do_post_event (e);
  }

  static void about_to_exit (void)
  {
    if (instance_ok ())
      instance->do_about_to_exit ();
  }

  static void entered_readline_hook (void)
  {
    if (instance_ok ())
      instance->do_entered_readline_hook ();
  }

  static void finished_readline_hook (void)
  {
    if (instance_ok ())
      instance->do_finished_readline_hook ();
  }

  static std::string last_working_directory (void)
  {
    return instance_ok ()
      ? instance->do_last_working_directory () : std::string ();
  }

private:

  static octave_link *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  // No copying!

  octave_link (const octave_link&);

  octave_link& operator = (const octave_link&);

  static bool instance_ok (void);

  octave_event_listener *event_listener;

  // Thread running octave_main.
  octave_main_thread *main_thread;

  // Semaphore to lock access to the event queue.
  octave_mutex *event_queue_mutex;

  // Buffer for queueing events until they will be processed.
  std::queue <octave_event *> event_queue;

  // Stores the last known current working directory of octave.
  std::string last_cwd;

  bool debugging;

  void do_launch_octave (void);
  void do_register_event_listener (octave_event_listener *oel);

  void do_generate_events (void);
  void do_process_events (void);
  void do_post_event (octave_event *e);

  void do_about_to_exit (void);

  void do_entered_readline_hook (void) { }
  void do_finished_readline_hook (void) { }

  std::string do_last_working_directory (void);

  void event_accepted (octave_event *e);
  void event_reject (octave_event *e);
};

#endif // OCTAVELINK_H
