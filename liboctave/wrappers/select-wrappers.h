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

#if ! defined (octave_wait_for_input_h)
#define octave_wait_for_input_h 1

#if defined (__cplusplus)
extern "C" {
#endif

// We'd need to include gnulib headers for the 'fd_set', 'struct timeval',
// and 'SOCKET' types. Use 'void *' or 'int' in the wrapper interfaces instead.

extern OCTAVE_API int
octave_select (int nfds, /* fd_set *restrict */ void *restrict readfds,
               /* fd_set *restrict */ void *restrict writefds,
               /* fd_set *restrict */ void *restrict errorfds,
               /* struct timeval * */ void *timeout);

extern OCTAVE_API void
octave_fd_set (/* SOCKET */ int fd, /* fd_set * */ void *set);

extern OCTAVE_API void
octave_fd_clr (/* SOCKET */ int fd, /* fd_set * */ void *set);

extern OCTAVE_API int
octave_fd_isset (/* SOCKET */ int fd, /* fd_set * */ void *set);

extern OCTAVE_API void
octave_fd_zero (/* fd_set * */ void *set);

#if defined (__cplusplus)
}
#endif

#endif
