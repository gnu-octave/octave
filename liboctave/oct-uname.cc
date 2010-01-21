/*

Copyright (C) 2005, 2007 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <cstring>

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif

#include "oct-uname.h"

void
octave_uname::init (void)
{
#if defined (HAVE_UNAME) && defined (HAVE_SYS_UTSNAME_H)
  struct utsname unm;

  err = ::uname (&unm);

  if (err < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
  else
    {
      utsname_sysname = unm.sysname;
      utsname_nodename = unm.nodename;
      utsname_release = unm.release;
      utsname_version = unm.version;
      utsname_machine = unm.machine;
    }
#endif
}
