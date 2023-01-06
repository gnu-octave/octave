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

#include <fcntl.h>

#include "fcntl-wrappers.h"

int
octave_fcntl_wrapper (int fd, int cmd, int arg)
{
  return fcntl (fd, cmd, arg);
}

int
octave_open_wrapper (const char *nm, int flags, mode_t mode)
{
  return open (nm, flags, mode);
}

int
octave_f_dupfd_wrapper (void)
{
#if defined (F_DUPFD)
  return F_DUPFD;
#else
  return -1;
#endif
}

int
octave_f_getfd_wrapper (void)
{
#if defined (F_GETFD)
  return F_GETFD;
#else
  return -1;
#endif
}

int
octave_f_getfl_wrapper (void)
{
#if defined (F_GETFL)
  return F_GETFL;
#else
  return -1;
#endif
}

int
octave_f_setfd_wrapper (void)
{
#if defined (F_SETFD)
  return F_SETFD;
#else
  return -1;
#endif
}

int
octave_f_setfl_wrapper (void)
{
#if defined (F_SETFL)
  return F_SETFL;
#else
  return -1;
#endif
}

int
octave_o_append_wrapper (void)
{
#if defined (O_APPEND)
  return O_APPEND;
#else
  return -1;
#endif
}

int
octave_o_async_wrapper (void)
{
#if defined (O_ASYNC)
  return O_ASYNC;
#else
  return -1;
#endif
}

int
octave_o_creat_wrapper (void)
{
#if defined (O_CREAT)
  return O_CREAT;
#else
  return -1;
#endif
}

int
octave_o_excl_wrapper (void)
{
#if defined (O_EXCL)
  return O_EXCL;
#else
  return -1;
#endif
}

int
octave_o_nonblock_wrapper (void)
{
#if defined (O_NONBLOCK)
  return O_NONBLOCK;
#else
  return -1;
#endif
}

int
octave_o_rdonly_wrapper (void)
{
#if defined (O_RDONLY)
  return O_RDONLY;
#else
  return -1;
#endif
}

int
octave_o_rdwr_wrapper (void)
{
#if defined (O_RDWR)
  return O_RDWR;
#else
  return -1;
#endif
}

int
octave_o_sync_wrapper (void)
{
#if defined (O_SYNC)
  return O_SYNC;
#else
  return -1;
#endif
}

int
octave_o_trunc_wrapper (void)
{
#if defined (O_TRUNC)
  return O_TRUNC;
#else
  return -1;
#endif
}

int
octave_o_wronly_wrapper (void)
{
#if defined (O_WRONLY)
  return O_WRONLY;
#else
  return -1;
#endif
}
