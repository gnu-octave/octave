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

#include <time.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "stat-wrappers.h"
#include "uniconv-wrappers.h"

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <windows.h>
#  include <wchar.h>
#endif

int
octave_mkdir_wrapper (const char *name, mode_t mode)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  wchar_t *wname = u8_to_wchar (name);
  int status = _wmkdir (wname);
  free ((void *) wname);
  octave_unused_parameter (mode);
  return status;
#else
  return mkdir (name, mode);
#endif
}

int
octave_mkfifo_wrapper (const char *name, mode_t mode)
{
  return mkfifo (name, mode);
}

int
octave_umask_wrapper (mode_t mode)
{
  return umask (mode);
}

static inline void
assign_stat_fields (struct stat *buf, mode_t *mode, ino_t *ino,
                    dev_t *dev, nlink_t *nlink, uid_t *uid,
                    gid_t *gid, off_t *size, time_t *atime,
                    time_t *mtime, time_t *ctime, dev_t *rdev,
                    long *blksize, long *blocks)
{
  *mode = buf->st_mode;
  *ino = buf->st_ino;
  *dev = buf->st_dev;
  *nlink = buf->st_nlink;
  *uid = buf->st_uid;
  *gid = buf->st_gid;
  *size = buf->st_size;
  *atime = buf->st_atime;
  *mtime = buf->st_mtime;
  *ctime = buf->st_ctime;

#if defined (HAVE_STRUCT_STAT_ST_RDEV)
  *rdev = buf->st_rdev;
#else
  *rdev = 0;
#endif

#if defined (HAVE_STRUCT_STAT_ST_BLKSIZE)
  *blksize = buf->st_blksize;
#else
  *blksize = 0;
#endif

#if defined (HAVE_STRUCT_STAT_ST_BLOCKS)
  *blocks = buf->st_blocks;
#else
  *blocks = 0;
#endif
}

int
octave_stat_wrapper (const char *fname, mode_t *mode, ino_t *ino,
                     dev_t *dev, nlink_t *nlink, uid_t *uid,
                     gid_t *gid, off_t *size, time_t *atime,
                     time_t *mtime, time_t *ctime, dev_t *rdev,
                     long *blksize, long *blocks)
{
  struct stat buf;

#if defined (OCTAVE_USE_WINDOWS_API)
  wchar_t *wfname = u8_to_wchar (fname);
  int status = _wstati64 (wfname, &buf);
  free ((void *) wfname);
#else
  int status = stat (fname, &buf);
#endif

  assign_stat_fields (&buf, mode, ino, dev, nlink, uid, gid, size,
                      atime, mtime, ctime, rdev, blksize, blocks);

  return status;
}

int
octave_lstat_wrapper (const char *lname, mode_t *mode, ino_t *ino,
                      dev_t *dev, nlink_t *nlink, uid_t *uid,
                      gid_t *gid, off_t *size, time_t *atime,
                      time_t *mtime, time_t *ctime, dev_t *rdev,
                      long *blksize, long *blocks)
{
  struct stat buf;

#if defined (OCTAVE_USE_WINDOWS_API)
  // Windows doesn't have an lstat. Use stat instead
  wchar_t *wlname = u8_to_wchar (lname);
  int status = _wstati64 (wlname, &buf);
  free ((void *) wlname);
#else
  int status = lstat (lname, &buf);
#endif

  assign_stat_fields (&buf, mode, ino, dev, nlink, uid, gid, size,
                      atime, mtime, ctime, rdev, blksize, blocks);

  return status;
}

int
octave_fstat_wrapper (int fid, mode_t *mode, ino_t *ino,
                      dev_t *dev, nlink_t *nlink, uid_t *uid,
                      gid_t *gid, off_t *size, time_t *atime,
                      time_t *mtime, time_t *ctime, dev_t *rdev,
                      long *blksize, long *blocks)
{
  struct stat buf;

  int status = fstat (fid, &buf);

  assign_stat_fields (&buf, mode, ino, dev, nlink, uid, gid, size,
                      atime, mtime, ctime, rdev, blksize, blocks);

  return status;
}

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
// Disable the unused parameter warning for the following wrapper functions.
// The <sys/stat.h> header provided by gnulib may define some of the S_IS*
// macros to expand to a constant and ignore the parameter.
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

bool
octave_is_blk_wrapper (mode_t mode)
{
#if defined (S_ISBLK)
  return S_ISBLK (mode);
#else
  return false;
#endif
}

bool
octave_is_chr_wrapper (mode_t mode)
{
#if defined (S_ISCHR)
  return S_ISCHR (mode);
#else
  return false;
#endif
}

bool
octave_is_dir_wrapper (mode_t mode)
{
#if defined (S_ISDIR)
  return S_ISDIR (mode);
#else
  return false;
#endif
}

bool
octave_is_fifo_wrapper (mode_t mode)
{
#if defined (S_ISFIFO)
  return S_ISFIFO (mode);
#else
  return false;
#endif
}

bool
octave_is_lnk_wrapper (mode_t mode)
{
#if defined (S_ISLNK)
  return S_ISLNK (mode);
#else
  return false;
#endif
}

bool
octave_is_reg_wrapper (mode_t mode)
{
#if defined (S_ISREG)
  return S_ISREG (mode);
#else
  return false;
#endif
}

bool
octave_is_sock_wrapper (mode_t mode)
{
#if defined (S_ISSOCK)
  return S_ISSOCK (mode);
#else
  return false;
#endif
}

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
// Restore prevailing warning state for remainder of the file.
#  pragma GCC diagnostic pop
#endif

bool
octave_have_struct_stat_st_rdev (void)
{
#if defined (HAVE_STRUCT_STAT_ST_RDEV)
  return true;
#else
  return false;
#endif
}

bool
octave_have_struct_stat_st_blksize (void)
{
#if defined (HAVE_STRUCT_STAT_ST_BLKSIZE)
  return true;
#else
  return false;
#endif
}

bool
octave_have_struct_stat_st_blocks (void)
{
#if defined (HAVE_STRUCT_STAT_ST_BLOCKS)
  return true;
#else
  return false;
#endif
}
