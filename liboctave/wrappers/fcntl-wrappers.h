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

#if ! defined (octave_fcntl_wrappers_h)
#define octave_fcntl_wrappers_h 1

#include <sys/types.h>

#if defined __cplusplus
extern "C" {
#endif

extern OCTAVE_API int octave_fcntl_wrapper (int fd, int cmd, int arg);

extern OCTAVE_API int
octave_open_wrapper (const char *nm, int flags, mode_t mode);

extern OCTAVE_API int octave_f_dupfd_wrapper (void);

extern OCTAVE_API int octave_f_getfd_wrapper (void);

extern OCTAVE_API int octave_f_getfl_wrapper (void);

extern OCTAVE_API int octave_f_setfd_wrapper (void);

extern OCTAVE_API int octave_f_setfl_wrapper (void);

extern OCTAVE_API int octave_o_append_wrapper (void);

extern OCTAVE_API int octave_o_async_wrapper (void);

extern OCTAVE_API int octave_o_creat_wrapper (void);

extern OCTAVE_API int octave_o_excl_wrapper (void);

extern OCTAVE_API int octave_o_nonblock_wrapper (void);

extern OCTAVE_API int octave_o_rdonly_wrapper (void);

extern OCTAVE_API int octave_o_rdwr_wrapper (void);

extern OCTAVE_API int octave_o_sync_wrapper (void);

extern OCTAVE_API int octave_o_trunc_wrapper (void);

extern OCTAVE_API int octave_o_wronly_wrapper (void);

#if defined __cplusplus
}
#endif

#endif
