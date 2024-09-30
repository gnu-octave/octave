////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sys/select.h>

#include "select-wrappers.h"

int
octave_select (int nfds, /* fd_set *restrict */ void *restrict readfds,
               /* fd_set *restrict */ void *restrict writefds,
               /* fd_set *restrict */ void *restrict errorfds,
               /* struct timeval * */ void *timeout)
{
  return select (nfds, (fd_set *restrict) readfds, (fd_set *restrict) writefds,
                 (fd_set *restrict) errorfds, (struct timeval *) timeout);
}

void
octave_fd_set (int fd, /* fd_set * */ void *set)
{
  FD_SET (fd, (fd_set *) set);
}

void
octave_fd_clr (int fd, /* fd_set * */ void *set)
{
  FD_CLR (fd, (fd_set *) set);
}

int
octave_fd_isset (int fd, /* fd_set * */ void *set)
{
  return FD_ISSET (fd, (fd_set *) set);
}

void
octave_fd_zero (/* fd_set * */ void *set)
{
  FD_ZERO ((fd_set *) set);
}
