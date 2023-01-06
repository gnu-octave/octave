////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

// uname may be provided by gnulib.  We don't include gnulib headers
// directly in Octave's C++ source files to avoid problems that may be
// caused by the way that gnulib overrides standard library functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sys/utsname.h>

#include "uname-wrapper.h"

// Returns pointers to static storage.

int
octave_uname_wrapper (char **sysname, char **nodename,
                      char **release, char **version, char **machine)
{
  static struct utsname unm;

  int err = uname (&unm);

  if (err < 0)
    {
      *sysname = 0;
      *nodename = 0;
      *release = 0;
      *version = 0;
      *machine = 0;
    }
  else
    {
      *sysname = unm.sysname;
      *nodename = unm.nodename;
      *release = unm.release;
      *version = unm.version;
      *machine = unm.machine;
    }

  return err;
}
