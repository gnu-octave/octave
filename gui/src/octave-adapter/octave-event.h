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

/**
  * \class octave_event
  * \brief Base class for an octave event.
  */
class octave_event
{
  public:
    enum event_type
    {
      exit_event,
      change_directory_event
    };

    octave_event (const octave_event_observer& o)
      : _octave_event_observer (o)
    { }

    virtual ~octave_event ()
    { }

    virtual event_type get_event_type () const = 0;

    void accept ()
    { _octave_event_observer.event_accepted (this); }

    void ignore ()
    { _octave_event_observer.event_ignored (this); }

  private:
    const octave_event_observer& _octave_event_observer;
};

class octave_exit_event : public octave_event
{
  public:
    octave_exit_event (const octave_event_observer& o)
      : octave_event (o)
    { }

    event_type get_event_type () const
    { return octave_event::exit_event; }
};

class octave_change_directory_event : public octave_event
{
  public:
    octave_change_directory_event (const octave_event_observer& o,
                                   std::string directory)
      : octave_event (o)
    { _directory = directory; }

    event_type get_event_type () const
    { return octave_event::change_directory_event; }

    std::string get_directory () const
    { return _directory; }

  private:
    std::string _directory;
};

#endif // OCTAVEEVENT_H
