////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2024 The Octave Project Developers
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

#if ! defined (octave_oct_mutex_h)
#define octave_oct_mutex_h 1

#include "octave-config.h"

#include <memory>

OCTAVE_BEGIN_NAMESPACE(octave)

class mutex;

class OCTAVE_API base_mutex
{
public:
  friend class mutex;

  OCTAVE_DEFAULT_CONSTRUCT_COPY_MOVE (base_mutex)

  virtual ~base_mutex () = default;

  virtual void lock ();

  virtual void unlock ();

  virtual bool try_lock ();
};

class OCTAVE_API mutex
{
public:
  mutex ();

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (mutex)

  void lock ()
  {
    m_rep->lock ();
  }

  void unlock ()
  {
    m_rep->unlock ();
  }

  bool try_lock ()
  {
    return m_rep->try_lock ();
  }

protected:
  std::shared_ptr<base_mutex> m_rep;
};

class OCTAVE_API autolock
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

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (autolock)

  ~autolock ()
  {
    if (m_lock_result)
      m_mutex.unlock ();
  }

  bool ok () const { return m_lock_result; }

  operator bool () const { return ok (); }

private:

  mutex m_mutex;

  bool m_lock_result;
};


class OCTAVE_API thread
{
public:

  static void init ();

  static bool is_thread ();
};

OCTAVE_END_NAMESPACE(octave)

#endif
