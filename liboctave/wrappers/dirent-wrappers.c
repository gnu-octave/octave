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

// These functions may be provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <dirent.h>

#include "dirent-wrappers.h"

void *
octave_opendir_wrapper (const char *dname)
{
  return opendir (dname);
}

char *
octave_readdir_wrapper (void *dir)
{
  struct dirent *d_ent = readdir ((DIR *) dir);

  return d_ent ? d_ent->d_name : 0;
}

void
octave_rewinddir_wrapper (void *dir)
{
  rewinddir ((DIR *) dir);
}

int
octave_closedir_wrapper (void *dir)
{
  return closedir ((DIR *) dir);
}

// Define NAME_MAX, the maximum length of a single component in a
// filename.  No such limit may exist, or may vary depending on the
// filesystem.  This value should be avoided for creating character
// strings and used only to truncate given file names to this length if
// attempts to get info about the file fail with errno == ENAMETOOLONG.

// Most likely the system will truncate filenames if it is not POSIX,
// and so we can use the BSD value here.

#if ! defined (_POSIX_NAME_MAX)
#  define _POSIX_NAME_MAX 255
#endif

#if ! defined (NAME_MAX)
#  define NAME_MAX _POSIX_NAME_MAX
#endif

unsigned int
octave_name_max_wrapper (void)
{
  return NAME_MAX;
}
