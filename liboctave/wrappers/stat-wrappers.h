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

#if ! defined (octave_stat_wrappers_h)
#define octave_stat_wrappers_h 1

#if defined __cplusplus
#  include <ctime>
#else
#  include <stdbool.h>
#  include <time.h>
#endif

#include <sys/types.h>

#if defined __cplusplus
extern "C" {
#endif

extern OCTAVE_API int octave_mkdir_wrapper (const char *name, mode_t mode);

extern OCTAVE_API int octave_mkfifo_wrapper (const char *name, mode_t mode);

extern OCTAVE_API int octave_umask_wrapper (mode_t mode);

extern OCTAVE_API int
octave_stat_wrapper (const char *fname, mode_t *mode, ino_t *ino,
                     dev_t *dev, nlink_t *nlink, uid_t *uid,
                     gid_t *gid, off_t *size, time_t *atime,
                     time_t *mtime, time_t *ctime, dev_t *rdev,
                     long *blksize, long *blocks);

extern OCTAVE_API int
octave_lstat_wrapper (const char *lname, mode_t *mode, ino_t *ino,
                      dev_t *dev, nlink_t *nlink, uid_t *uid,
                      gid_t *gid, off_t *size, time_t *atime,
                      time_t *mtime, time_t *ctime, dev_t *rdev,
                      long *blksize, long *blocks);

extern OCTAVE_API int
octave_fstat_wrapper (int fid, mode_t *mode, ino_t *ino,
                      dev_t *dev, nlink_t *nlink, uid_t *uid,
                      gid_t *gid, off_t *size, time_t *atime,
                      time_t *mtime, time_t *ctime, dev_t *rdev,
                      long *blksize, long *blocks);

extern OCTAVE_API bool octave_is_blk_wrapper (mode_t mode);
extern OCTAVE_API bool octave_is_chr_wrapper (mode_t mode);
extern OCTAVE_API bool octave_is_dir_wrapper (mode_t mode);
extern OCTAVE_API bool octave_is_fifo_wrapper (mode_t mode);
extern OCTAVE_API bool octave_is_lnk_wrapper (mode_t mode);
extern OCTAVE_API bool octave_is_reg_wrapper (mode_t mode);
extern OCTAVE_API bool octave_is_sock_wrapper (mode_t mode);

extern OCTAVE_API bool octave_have_struct_stat_st_rdev (void);
extern OCTAVE_API bool octave_have_struct_stat_st_blksize (void);
extern OCTAVE_API bool octave_have_struct_stat_st_blocks (void);

#if defined __cplusplus
}
#endif

#endif
