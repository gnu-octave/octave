/*

Copyright (C) 1993-2016 John W. Eaton

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

#if ! defined (octave_child_list_h)
#define octave_child_list_h 1

#include "octave-config.h"

#include <csignal>

#include <sys/types.h>

#include "base-list.h"

namespace octave
{
  class
  OCTAVE_API
  child
  {
  public:

    // Do whatever to handle event for child with PID (might not
    // actually be dead, could just be stopped).  Return true if
    // the list element corresponding to PID should be removed from
    // list.  This function should not call any functions that modify
    // the child_list.

    typedef bool (*child_event_handler) (pid_t, int);

    child (pid_t id = -1, child_event_handler f = 0)
      : pid (id), handler (f), have_status (0), status (0) { }

    child (const child& oc)
      : pid (oc.pid), handler (oc.handler),
      have_status (oc.have_status), status (oc.status) { }

    child& operator = (const child& oc)
      {
        if (&oc != this)
          {
            pid = oc.pid;
            handler = oc.handler;
            have_status = oc.have_status;
            status = oc.status;
          }
        return *this;
      }

    ~child (void) { }

    // The process id of this child.
    pid_t pid;

    // The function we call if an event happens for this child.
    child_event_handler handler;

    // Nonzero if this child has stopped or terminated.
    sig_atomic_t have_status;

    // The status of this child; 0 if running, otherwise a status value
    // from waitpid.
    int status;
  };

  class
  OCTAVE_API
  child_list
  {
  protected:

    child_list (void) { }

    class child_list_rep : public base_list<child>
    {
    public:

      void insert (pid_t pid, child::child_event_handler f);

      void reap (void);

      bool wait (void);
    };

  public:

    ~child_list (void) { }

    static void insert (pid_t pid, child::child_event_handler f);

    static void reap (void);

    static bool wait (void);

    static void remove (pid_t pid);

  private:

    static bool instance_ok (void);

    static child_list_rep *instance;

    static void cleanup_instance (void) { delete instance; instance = 0; }
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::child' instead")
typedef octave::child octave_child;

OCTAVE_DEPRECATED ("use 'octave::child_list' instead")
typedef octave::child_list octave_child_list;

#endif

#endif
