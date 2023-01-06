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

#if ! defined (octave_unistd_wrappers_h)
#define octave_unistd_wrappers_h 1

#if ! defined (__cplusplus)
#  include <stdbool.h>
#endif

#include <sys/types.h>

#if defined __cplusplus
extern "C" {
#endif

extern OCTAVE_API int octave_access_f_ok (void);

extern OCTAVE_API int octave_access_r_ok (void);

extern OCTAVE_API int octave_access_w_ok (void);

extern OCTAVE_API int octave_access_x_ok (void);

extern OCTAVE_API int octave_access_wrapper (const char *nm, int mode);

extern OCTAVE_API int octave_chdir_wrapper (const char *nm);

extern OCTAVE_API int octave_close_wrapper (int fd);

extern OCTAVE_API const char * octave_ctermid_wrapper (void);

extern OCTAVE_API int octave_dup2_wrapper (int fd1, int fd2);

extern OCTAVE_API int octave_execv_wrapper (const char *file, char *const *argv);

extern OCTAVE_API int octave_execvp_wrapper (const char *file, char *const *argv);

extern OCTAVE_API pid_t octave_fork_wrapper (void);

extern OCTAVE_API int octave_ftruncate_wrapper (int fd, off_t sz);

extern OCTAVE_API char * octave_getcwd_wrapper (char *nm, size_t len);

extern OCTAVE_API gid_t octave_getegid_wrapper (void);

extern OCTAVE_API uid_t octave_geteuid_wrapper (void);

extern OCTAVE_API gid_t octave_getgid_wrapper (void);

extern OCTAVE_API int octave_gethostname_wrapper (char *nm, size_t len);

extern OCTAVE_API pid_t octave_getpgrp_wrapper (void);

extern OCTAVE_API pid_t octave_getpid_wrapper (void);

extern OCTAVE_API pid_t octave_getppid_wrapper (void);

extern OCTAVE_API uid_t octave_getuid_wrapper (void);

extern OCTAVE_API int octave_isatty_wrapper (int fd);

extern OCTAVE_API int octave_link_wrapper (const char *nm1, const char *nm2);

extern OCTAVE_API int octave_pipe_wrapper (int *fd);

extern OCTAVE_API int octave_rmdir_wrapper (const char *nm);

extern OCTAVE_API pid_t octave_setsid_wrapper (void);

extern OCTAVE_API int octave_stdin_fileno (void);

extern OCTAVE_API int octave_stdout_fileno (void);

extern OCTAVE_API int octave_symlink_wrapper (const char *nm1, const char *nm2);

extern OCTAVE_API int octave_unlink_wrapper (const char *nm);

extern OCTAVE_API pid_t octave_vfork_wrapper (void);

extern OCTAVE_API bool octave_have_fork (void);

extern OCTAVE_API bool octave_have_vfork (void);

#if defined __cplusplus
}
#endif

#endif
