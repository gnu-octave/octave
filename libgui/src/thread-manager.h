/*

Copyright (C) 2013-2018 John W. Eaton

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

#if ! defined (octave_thread_manager_h)
#define octave_thread_manager_h 1

#include "octave-config.h"

#include <memory>

namespace octave
{
  class base_thread_manager
  {
  public:

    friend class thread_manager;

    base_thread_manager (void) = default;

    base_thread_manager (const base_thread_manager&) = default;

    virtual ~base_thread_manager (void) = default;

    virtual void register_current_thread (void) = 0;

    virtual void interrupt (void) = 0;
  };

  class thread_manager
  {
  public:

    thread_manager (void);

    ~thread_manager (void) = default;

    thread_manager (const thread_manager&) = default;

    thread_manager& operator = (const thread_manager&) = default;

    void register_current_thread (void) { m_rep->register_current_thread (); }

    void interrupt (void) { m_rep->interrupt (); }

    static void block_interrupt_signal (void);

    static void unblock_interrupt_signal (void);

  private:

    std::shared_ptr<base_thread_manager> m_rep;

    static base_thread_manager * create_rep (void);
  };
}

#endif
