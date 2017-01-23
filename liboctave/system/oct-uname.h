/*

Copyright (C) 2005-2016 John W. Eaton

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

#if ! defined (octave_oct_uname_h)
#define octave_oct_uname_h 1

#include "octave-config.h"

#include <string>

namespace octave
{
  namespace sys
  {
    class
    OCTAVE_API
    uname
    {
    public:

      uname (void)
        : m_sysname ("unknown"), m_nodename ("unknown"),
          m_release ("unknown"), m_version ("unknown"),
          m_machine ("unknown"),
          msg ("uname not supported on this system"), err (-1)
      { init (); }

      uname (const uname& unm)
        : m_sysname (unm.m_sysname), m_nodename (unm.m_nodename),
          m_release (unm.m_release), m_version (unm.m_version),
          m_machine (unm.m_machine), msg (unm.msg), err (unm.err)
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

            msg = unm.msg;
            err = unm.err;
          }

        return *this;
      }

      ~uname (void) = default;

      std::string sysname (void) const { return m_sysname; }
      std::string nodename (void) const { return m_nodename; }
      std::string release (void) const { return m_release; }
      std::string version (void) const { return m_version; }
      std::string machine (void) const { return m_machine; }

      std::string message (void) const { return msg; }
      int error (void) const { return err; }

    private:

      std::string m_sysname;
      std::string m_nodename;
      std::string m_release;
      std::string m_version;
      std::string m_machine;

      std::string msg;
      int err;

      void init (void);
    };
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::sys::uname' instead")
typedef octave::sys::uname octave_uname;

#endif

#endif
