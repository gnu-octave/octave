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

/**
  * \class OctaveLink
  * \brief Provides threadsafe access to octave.
  * \author Jacob Dawid
  * This class is a wrapper around octave and provides threadsafety by
  * buffering access operations to octave and executing them in the readline
  * event hook, which lives in the octave thread.
  */
class octave_link : public octave_event_observer
{
public:
  /** Provides a way to access the unique octave_link object. */
  static octave_link * instance () { return &_singleton; }

  /** Starts octave. */
  void launch_octave ();
  void register_event_listener (octave_event_listener *oel);

  void generate_events ();
  void process_events ();
  void post_event (octave_event *e);
  void event_accepted (octave_event *e);
  void event_reject (octave_event *e);

  void about_to_exit ();

  void entered_readline_hook ();
  void finished_readline_hook ();

  std::string get_last_working_directory ();

private:
  /** Singleton. */
  octave_link ();
  ~octave_link ();

  octave_event_listener *_octave_event_listener;

  /** Thread running octave_main. */
  octave_main_thread *_octave_main_thread;

  /** Semaphore to lock access to the event queue. */
  octave_mutex *_event_queue_mutex;

  /** Buffer for queueing events until they will be processed. */
  std::queue <octave_event *> _event_queue;

  /** Stores the last known current working directory of octave. */
  std::string _last_working_directory;
  bool _debugging_mode_active;

  /** Semaphore to lock access to the performance information. */
  octave_mutex *_performance_information_mutex;

  /** Unique instance. Singelton! */
  static octave_link _singleton;
};
#endif // OCTAVELINK_H
