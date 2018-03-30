/*

Copyright (C) 1993-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "child-list.h"
#include "oct-syscalls.h"

namespace octave
{
  class pid_equal
  {
  public:

    pid_equal (pid_t v) : val (v) { }

    bool operator () (const child& oc) const { return oc.pid == val; }

  private:

    pid_t val;
  };

  void child_list::remove (pid_t pid)
  {
    m_list.remove_if (pid_equal (pid));
  }

  void child_list::child_list::insert (pid_t pid, child::child_event_handler f)
  {
    m_list.append (child (pid, f));
  }

  void child_list::reap (void)
  {
    // Mark the record for PID invalid.

    for (auto& oc : m_list)
      {
        // The call to the child::child_event_handler might
        // invalidate the iterator (for example, by calling
        // child_list::remove), so we increment the iterator
        // here.

        if (oc.have_status)
          {
            oc.have_status = 0;

            child::child_event_handler f = oc.handler;

            if (f && f (oc.pid, oc.status))
              oc.pid = -1;
          }
      }

    // ??
    remove (-1);
  }

  // Wait on our children and record any changes in their status.

  bool child_list::wait (void)
  {
    bool retval = false;

    for (auto& oc : m_list)
      {
        pid_t pid = oc.pid;

        if (pid > 0)
          {
            int status;

            if (sys::waitpid (pid, &status, sys::wnohang ()) > 0)
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
