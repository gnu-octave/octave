/*

Copyright (C) 2008-2018 Michael Goffioul

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

#if ! defined (octave_oct_mutex_h)
#define octave_oct_mutex_h 1

#include "octave-config.h"

#include "oct-refcount.h"

namespace octave
{
  class mutex;

  class
  base_mutex
  {
  public:
    friend class mutex;

    base_mutex (void) : count (1) { }

    virtual ~base_mutex (void) = default;

    virtual void lock (void);

    virtual void unlock (void);

    virtual bool try_lock (void);

  private:
    refcount<int> count;
  };

  class
  OCTAVE_API
  mutex
  {
  public:
    mutex (void);

    mutex (const mutex& m)
      : rep (m.rep)
    {
      rep->count++;
    }

    ~mutex (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

    mutex& operator = (const mutex& m)
    {
      if (rep != m.rep)
        {
          if (--rep->count == 0)
            delete rep;

          rep = m.rep;
          rep->count++;
        }

      return *this;
    }

    void lock (void)
    {
      rep->lock ();
    }

    void unlock (void)
    {
      rep->unlock ();
    }

    bool try_lock (void)
    {
      return rep->try_lock ();
    }

  protected:
    base_mutex *rep;
  };

  class
  autolock
  {
  public:
    autolock (const mutex& m, bool block = true)
      : m_mutex (m), m_lock_result (false)
    {
      if (block)
        {
          m_mutex.lock ();
          m_lock_result = true;
        }
      else
        m_lock_result = m_mutex.try_lock ();
    }

    // No copying.

    autolock (const autolock&) = delete;

    autolock& operator = (const autolock&) = delete;

    ~autolock (void)
    {
      if (m_lock_result)
        m_mutex.unlock ();
    }

    bool ok (void) const { return m_lock_result; }

    operator bool (void) const { return ok (); }

  private:

    mutex m_mutex;

    bool m_lock_result;
  };


  class
  OCTAVE_API
  thread
  {
  public:

    static void init (void);

    static bool is_thread (void);

    OCTAVE_DEPRECATED (4.4, "use 'is_thread' instead")
    static bool is_octave_thread (void) { return is_thread (); }
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::mutex' instead")
typedef octave::mutex octave_mutex;

OCTAVE_DEPRECATED (4.4, "use 'octave::base_mutex' instead")
typedef octave::base_mutex octave_base_mutex;

OCTAVE_DEPRECATED (4.4, "use 'octave::autolock' instead")
typedef octave::autolock octave_autolock;

OCTAVE_DEPRECATED (4.4, "use 'octave::thread' instead")
typedef octave::thread octave_thread;

#endif

#endif
