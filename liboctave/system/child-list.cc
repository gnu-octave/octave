/*

Copyright (C) 1993-2016 John W. Eaton

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

#include "child-list.h"
#include "lo-error.h"
#include "oct-syscalls.h"
#include "signal-wrappers.h"
#include "singleton-cleanup.h"

namespace octave
{
  child_list::child_list_rep *child_list::instance = 0;

  bool
  child_list::instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      {
        instance = new child_list_rep ();

        if (instance)
          singleton_cleanup_list::add (cleanup_instance);
      }

    if (! instance)
      (*current_liboctave_error_handler)
        ("unable to create child list object!");

    return retval;
  }

  void
  child_list::insert (pid_t pid, child::child_event_handler f)
  {
    if (instance_ok ())
      instance->insert (pid, f);
  }

  void
  child_list::reap (void)
  {
    if (instance_ok ())
      instance->reap ();
  }

  bool
  child_list::wait (void)
  {
    return (instance_ok ()) ? instance->wait () : false;
  }

  class pid_equal
  {
  public:

    pid_equal (pid_t v) : val (v) { }

    bool operator () (const child& oc) const { return oc.pid == val; }

  private:

    pid_t val;
  };

  void
  child_list::remove (pid_t pid)
  {
    if (instance_ok ())
      instance->remove_if (pid_equal (pid));
  }

  void
  child_list::child_list_rep::insert (pid_t pid, child::child_event_handler f)
  {
    append (child (pid, f));
  }

  void
  child_list::child_list_rep::reap (void)
  {
    // Mark the record for PID invalid.

    for (iterator p = begin (); p != end (); p++)
      {
        // The call to the child::child_event_handler might
        // invalidate the iterator (for example, by calling
        // child_list::remove), so we increment the iterator
        // here.

        child& oc = *p;

        if (oc.have_status)
          {
            oc.have_status = 0;

            child::child_event_handler f = oc.handler;

            if (f && f (oc.pid, oc.status))
              oc.pid = -1;
          }
      }

    remove_if (pid_equal (-1));
  }

  // Wait on our children and record any changes in their status.

  bool
  child_list::child_list_rep::wait (void)
  {
    bool retval = false;

    for (iterator p = begin (); p != end (); p++)
      {
        child& oc = *p;

        pid_t pid = oc.pid;

        if (pid > 0)
          {
            int status;

            if (octave::sys::waitpid (pid, &status, octave::sys::wnohang ()) > 0)
              {
                oc.have_status = 1;

                oc.status = status;

                retval = true;

                break;
              }
          }
      }

    return retval;
  }
}
