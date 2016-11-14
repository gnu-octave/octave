/*

Copyright (C) 1996-2016 John W. Eaton

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

#if ! defined (octave_oct_passwd_h)
#define octave_oct_passwd_h 1

#include "octave-config.h"

#include <string>

#include <sys/types.h>

namespace octave
{
  namespace sys
  {
    class
    OCTAVE_API
    password
    {
    public:

      password (void)
        : m_name (), m_passwd (), m_uid (0), m_gid (0), m_gecos (),
          m_dir (), m_shell (), valid (false)
      { }

      password (const password& pw)
        : m_name (pw.m_name), m_passwd (pw.m_passwd),
          m_uid (pw.m_uid), m_gid (pw.m_gid), m_gecos (pw.m_gecos),
          m_dir (pw.m_dir), m_shell (pw.m_shell), valid (pw.valid)
      { }

      password& operator = (const password& pw)
      {
        if (this != &pw)
          {
            m_name = pw.m_name;
            m_passwd = pw.m_passwd;
            m_uid = pw.m_uid;
            m_gid = pw.m_gid;
            m_gecos = pw.m_gecos;
            m_dir = pw.m_dir;
            m_shell = pw.m_shell;
            valid = pw.valid;
          }

        return *this;
      }

      ~password (void) { }

      std::string name (void) const;

      std::string passwd (void) const;

      uid_t uid (void) const;

      gid_t gid (void) const;

      std::string gecos (void) const;

      std::string dir (void) const;

      std::string shell (void) const;

      bool ok (void) const { return valid; }

      operator bool () const { return ok (); }

      static password getpwent (void);
      static password getpwent (std::string& msg);

      static password getpwuid (uid_t uid);
      static password getpwuid (uid_t uid, std::string& msg);

      static password getpwnam (const std::string& nm);
      static password getpwnam (const std::string& nm, std::string& msg);

      static int setpwent (void);
      static int setpwent (std::string& msg);

      static int endpwent (void);
      static int endpwent (std::string& msg);

    private:

      // User name.
      std::string m_name;

      // Encrypted password.
      std::string m_passwd;

      // Numeric user id.
      uid_t m_uid;

      // Numeric group id.
      gid_t m_gid;

      // Miscellaneous junk.
      std::string m_gecos;

      // Home directory.
      std::string m_dir;

      // Login shell.
      std::string m_shell;

      // Flag that says whether we have been properly initialized.
      bool valid;

      // This is how we will create a password object from a pointer
      // to a struct passwd.
      password (void *p, std::string& msg);
    };
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::sys::password' instead")
typedef octave::sys::password octave_passwd;

#endif

#endif

