/*

Copyright (C) 1996-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_oct_group_h)
#define octave_oct_group_h 1

#include "octave-config.h"

#include <string>

#include <sys/types.h>

#include "str-vec.h"

namespace octave
{
  namespace sys
  {
    class
    OCTAVE_API
    group
    {
    public:

      group (void)
        : m_name (), m_passwd (), m_gid (0), m_mem (), valid (false)
        { }

      group (const group& gr)
        : m_name (gr.m_name), m_passwd (gr.m_passwd),
        m_gid (gr.m_gid), m_mem (gr.m_mem), valid (gr.valid)
        { }

      group& operator = (const group& gr)
        {
          if (this != &gr)
            {
              m_name = gr.m_name;
              m_passwd = gr.m_passwd;
              m_gid = gr.m_gid;
              m_mem = gr.m_mem;
              valid = gr.valid;
            }

          return *this;
        }

      std::string name (void) const;

      std::string passwd (void) const;

      gid_t gid (void) const;

      string_vector mem (void) const;

      bool ok (void) const { return valid; }

      operator bool () const { return ok (); }

      static group getgrent (void);
      static group getgrent (std::string& msg);

      static group getgrgid (gid_t gid);
      static group getgrgid (gid_t gid, std::string& msg);

      static group getgrnam (const std::string& nm);
      static group getgrnam (const std::string& nm, std::string& msg);

      static int setgrent (void);
      static int setgrent (std::string& msg);

      static int endgrent (void);
      static int endgrent (std::string& msg);

    private:

      // The group name.
      std::string m_name;

      // The group password.
      std::string m_passwd;

      // The numeric group id.
      gid_t m_gid;

      // The members of the group;
      string_vector m_mem;

      // Flag that says whether we have been properly initialized.
      bool valid;

      // This is how we will create an group object from a pointer
      // to a struct group.
      group (void *p, std::string& msg);
    };
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use octave::sys::group instead")
typedef octave::sys::group octave_group;

#endif

#endif
