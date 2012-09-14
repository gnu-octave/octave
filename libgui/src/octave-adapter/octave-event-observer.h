/*

Copyright (C) 2011-2012 Jacob Dawid

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

#ifndef OCTAVEEVENTOBSERVER_H
#define OCTAVEEVENTOBSERVER_H

class octave_event;

/**
  * \class octave_event_observer
  * \brief Implements the observer part for the observer pattern.
  */
class octave_event_observer
{
  public:
    octave_event_observer () { }
    virtual ~octave_event_observer () { }

    /**
      * This will be called after the octave_event::perform function
      * has processed an event.  ACCEPT will be true if the event was
      * successfully handled and false otherwise.
      */
    virtual void handle_event (octave_event *e, bool accept) = 0;
};

#include "octave-event.h"

#endif // OCTAVEEVENTOBSERVER_H
