////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2005-2023 The Octave Project Developers
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

#if ! defined (octave_oct_uname_h)
#define octave_oct_uname_h 1

#include "octave-config.h"

#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

class
OCTAVE_API
uname
{
public:

  uname (void)
    : m_sysname ("unknown"), m_nodename ("unknown"),
      m_release ("unknown"), m_version ("unknown"),
      m_machine ("unknown"),
      m_errmsg ("uname not supported on this system"), m_errno (-1)
  { init (); }

  uname (const uname& unm)
    : m_sysname (unm.m_sysname), m_nodename (unm.m_nodename),
      m_release (unm.m_release), m_version (unm.m_version),
      m_machine (unm.m_machine),
      m_errmsg (unm.m_errmsg), m_errno (unm.m_errno)
  { }

  uname& operator = (const uname& unm)
  {
    if (this != &unm)
      {
        m_sysname = unm.m_sysname;
        m_nodename = unm.m_nodename;
        m_release = unm.m_release;
        m_version = unm.m_version;
        m_machine = unm.m_machine;

        m_errmsg = unm.m_errmsg;
        m_errno = unm.m_errno;
      }

    return *this;
  }

  ~uname (void) = default;

  std::string sysname (void) const { return m_sysname; }
  std::string nodename (void) const { return m_nodename; }
  std::string release (void) const { return m_release; }
  std::string version (void) const { return m_version; }
  std::string machine (void) const { return m_machine; }

  std::string message (void) const { return m_errmsg; }
  int error (void) const { return m_errno; }

private:

  std::string m_sysname;
  std::string m_nodename;
  std::string m_release;
  std::string m_version;
  std::string m_machine;

  std::string m_errmsg;
  int m_errno;

  void init (void);
};

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
