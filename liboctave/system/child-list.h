////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_child_list_h)
#define octave_child_list_h 1

#include "octave-config.h"

#include <csignal>

#include <sys/types.h>

#include "base-list.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class OCTAVE_API child
{
public:

  // Do whatever to handle event for child with PID (might not
  // actually be dead, could just be stopped).  Return true if
  // the list element corresponding to PID should be removed from
  // list.  This function should not call any functions that modify
  // the child_list.

  typedef bool (*child_event_handler) (pid_t, int);

  child (pid_t pid = -1, child_event_handler f = nullptr)
    : m_pid (pid), m_handler (f), m_have_status (0), m_status (0)
  { }

  child (const child&) = default;

  child& operator = (const child&) = default;

  ~child (void) = default;

  // The process ID of this child.
  pid_t m_pid;

  // The function we call if an event happens for this child.
  child_event_handler m_handler;

  // Nonzero if this child has stopped or terminated.
  sig_atomic_t m_have_status;

  // The m_status of this child; 0 if running, otherwise a m_status value
  // from waitpid.
  int m_status;
};

class OCTAVE_API child_list
{
public:

  child_list (void) { }

  void insert (pid_t pid, child::child_event_handler f);

  void remove (pid_t pid);

  void reap (void);

  bool wait (void);

private:

  base_list<child> m_list;
};

OCTAVE_END_NAMESPACE(octave)

#endif
