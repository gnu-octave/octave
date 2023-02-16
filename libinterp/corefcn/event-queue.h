////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#if ! defined (octave_event_queue_h)
#define octave_event_queue_h 1

#include "octave-config.h"

#include <queue>
#include <memory>

#include "action-container.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
event_queue : public action_container
{
public:

  event_queue () : m_fifo () { }

  OCTAVE_DISABLE_COPY_MOVE (event_queue)

  // Destructor should not raise an exception, so all actions registered
  // should be exception-safe.  If you're not sure, see event_queue_safe.

  ~event_queue () { run (); }

  void run_first ()
  {
    if (! empty ())
      {
        // No leak on exception!
        std::unique_ptr<elem> ptr (m_fifo.front ());
        m_fifo.pop ();
        ptr->run ();
      }
  }

  void discard_first ()
  {
    if (! empty ())
      {
        elem *ptr = m_fifo.front ();
        m_fifo.pop ();
        delete ptr;
      }
  }

  std::size_t size () const { return m_fifo.size (); }

protected:

  void add_action (elem *new_elem)
  {
    m_fifo.push (new_elem);
  }

  //--------

  std::queue<elem *> m_fifo;
};

// Like event_queue, but this one will guard against the
// possibility of seeing an exception (or interrupt) in the cleanup actions.
// Not that we can do much about it, but at least we won't crash.

class
event_queue_safe : public event_queue
{
public:

  event_queue_safe () : event_queue () { }

  OCTAVE_DISABLE_COPY_MOVE (event_queue_safe)

  ~event_queue_safe ()
  {
    while (! empty ())
      {
        try
          {
            run_first ();
          }
        catch (...) // Yes, the black hole.  Remember we're in a dtor.
          {
            warn_unhandled_exception ();
          }
      }
  }

private:

  void warn_unhandled_exception () const;

};

OCTAVE_END_NAMESPACE(octave)

#endif
