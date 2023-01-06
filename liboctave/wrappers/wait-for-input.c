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

// select may be provided by gnulib.  We don't include gnulib headers
// directly in Octave's C++ source files to avoid problems that may be
// caused by the way that gnulib overrides standard library functions.

// We don't need select directly, we just need to use it to wait for
// input on an open file descriptor.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sys/select.h>

#include "wait-for-input.h"

int
octave_wait_for_input (int fid)
{
  int retval = -1;

#if defined (HAVE_SELECT)
  if (fid >= 0)
    {
      fd_set set;

      FD_ZERO (&set);
      FD_SET (fid, &set);

      retval = select (FD_SETSIZE, &set, 0, 0, 0);
    }
#else
  octave_unused_parameter (fid);

  retval = 1;
#endif

  return retval;
}
