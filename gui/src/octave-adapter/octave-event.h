/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef OCTAVEEVENT_H
#define OCTAVEEVENT_H

#include <string>
#include "octave-event-observer.h"
#include "oct-env.h"
#include "toplev.h"

/**
  * \class octave_event
  * \brief Base class for an octave event.
  * In order to make communication with octave threadsafe, comunication is
  * implemented via events. An application may create events and post them,
  * however there is no guarantee events will be processed in a given time.
  *
  * In order to create an event, there must be an event observer. The event
  * observer will be given the opportunity to react on the event as soon as
  * it has been processed in the octave thread. Accepting and ignoring takes
  * places in the octave thread.
  */
class octave_event
{
  public:
    octave_event (const octave_event_observer& o)
      : _octave_event_observer (o)
    { }

    virtual ~octave_event ()
    { }

    /** Performs what it necessary to make this event happen.
      * This code is thread-safe since it will be executed in the octave thread.
      * However, you should take care to keep this code as short as possible. */
    virtual bool perform () const = 0;

    /**
      * Accepts this event. This allows the event observer to react properly
      * onto the event.
      */
    void accept ()
    { _octave_event_observer.event_accepted (this); }

    /**
      * Rejects this event. This allows the event observer to react properly
      * onto the event.
      */
    void reject ()
    { _octave_event_observer.event_reject (this); }

  private:
    const octave_event_observer& _octave_event_observer;
};

/** Implements an octave exit event. */
class octave_exit_event : public octave_event
{
  public:
    /** Creates a new octave_exit_event. */
    octave_exit_event (const octave_event_observer& o)
      : octave_event (o)
    { }

    bool perform () const
    { clean_up_and_exit (0); return true; }
};

/** Implements a change directory events. */
class octave_change_directory_event : public octave_event
{
  public:
    /** Creates a new octave_change_directory_event. */
    octave_change_directory_event (const octave_event_observer& o,
                                   std::string directory)
      : octave_event (o)
    { _directory = directory; }

    bool perform () const
    { return octave_env::chdir (_directory); }

  private:
    std::string _directory;
};

#endif // OCTAVEEVENT_H
