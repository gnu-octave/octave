////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

// This class is based on the procbuf class from libg++, written by
// Per Bothner, Copyright (C) 1993 Free Software Foundation.

#if ! defined (octave_oct_procbuf_h)
#define octave_oct_procbuf_h 1

#include "octave-config.h"

#include <sys/types.h>

#include "c-file-ptr-stream.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
procbuf : public c_file_ptr_buf
{
public:

  procbuf (void)
    : c_file_ptr_buf (nullptr), m_wstatus (-1), m_open_p (false),
      m_proc_pid (-1), m_next (nullptr)
  { }

  procbuf (const char *command, int mode)
    : c_file_ptr_buf (nullptr), m_wstatus (-1), m_open_p (false),
      m_proc_pid (-1), m_next (nullptr)
  { open (command, mode); }

  // No copying!

  procbuf (const procbuf&) = delete;

  procbuf& operator = (const procbuf&) = delete;

  ~procbuf (void) { close (); }

  procbuf * open (const char *command, int mode);

  procbuf * close (void);

  int wait_status (void) const { return m_wstatus; }

  bool is_open (void) const { return m_open_p; }

  pid_t pid (void) const { return m_proc_pid; }

protected:

  int m_wstatus;

  bool m_open_p;

  pid_t m_proc_pid;

  procbuf *m_next;
};

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use octave::procbuf' instead")
typedef octave::procbuf procbuf;

#endif

#endif
