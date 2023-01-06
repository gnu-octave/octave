////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "oct-mutex.h"
#include "lo-error.h"

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <windows.h>
#elif defined (HAVE_PTHREAD_H)
#  include <pthread.h>
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

void
base_mutex::lock (void)
{
  (*current_liboctave_error_handler) ("mutex not supported on this platform");
}

void
base_mutex::unlock (void)
{
  (*current_liboctave_error_handler) ("mutex not supported on this platform");
}

bool
base_mutex::try_lock (void)
{
  (*current_liboctave_error_handler) ("mutex not supported on this platform");

  return false;
}

#if defined (OCTAVE_USE_WINDOWS_API)

class
w32_mutex : public base_mutex
{
public:
  w32_mutex (void)
    : base_mutex ()
  {
    InitializeCriticalSection (&cs);
  }

  ~w32_mutex (void)
  {
    DeleteCriticalSection (&cs);
  }

  void lock (void)
  {
    EnterCriticalSection (&cs);
  }

  void unlock (void)
  {
    LeaveCriticalSection (&cs);
  }

  bool try_lock (void)
  {
    return (TryEnterCriticalSection (&cs) != 0);
  }

private:
  CRITICAL_SECTION cs;
};

static DWORD thread_id = 0;

void
thread::init (void)
{
  thread_id = GetCurrentThreadId ();
}

bool
thread::is_thread (void)
{
  return (GetCurrentThreadId () == thread_id);
}

#elif defined (HAVE_PTHREAD_H)

class
pthread_mutex : public base_mutex
{
public:
  pthread_mutex (void)
    : base_mutex (), m_pm ()
  {
    pthread_mutexattr_t attr;

    pthread_mutexattr_init (&attr);
    pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init (&m_pm, &attr);
    pthread_mutexattr_destroy (&attr);
  }

  ~pthread_mutex (void)
  {
    pthread_mutex_destroy (&m_pm);
  }

  void lock (void)
  {
    pthread_mutex_lock (&m_pm);
  }

  void unlock (void)
  {
    pthread_mutex_unlock (&m_pm);
  }

  bool try_lock (void)
  {
    return (pthread_mutex_trylock (&m_pm) == 0);
  }

private:
  pthread_mutex_t m_pm;
};

static pthread_t thread_id = 0;

void
thread::init (void)
{
  thread_id = pthread_self ();
}

bool
thread::is_thread (void)
{
  return (pthread_equal (thread_id, pthread_self ()) != 0);
}

#endif

static base_mutex *
init_rep (void)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  return new w32_mutex ();
#elif defined (HAVE_PTHREAD_H)
  return new pthread_mutex ();
#else
  return new base_mutex ();
#endif
}

mutex::mutex (void) : m_rep (init_rep ()) { }

OCTAVE_END_NAMESPACE(octave)
