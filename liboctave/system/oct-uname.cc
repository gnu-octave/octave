/*

Copyright (C) 2005-2015 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>
#include <cstring>

#include <sys/utsname.h>

#include "oct-uname.h"

namespace
octave
{
  namespace
  sys
  {
    void
    uname::init (void)
    {
      struct utsname unm;

      err = ::uname (&unm);

      if (err < 0)
        msg = gnulib::strerror (errno);
      else
        {
          m_sysname = unm.sysname;
          m_nodename = unm.nodename;
          m_release = unm.release;
          m_version = unm.version;
          m_machine = unm.machine;
        }
    }
  }
}
