/*

Copyright (C) 1996-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>
#include <cstring>

#include "file-ops.h"
#include "file-stat.h"
#include "stat-wrappers.h"
#include "strmode-wrapper.h"

namespace octave
{
  namespace sys
  {
    // FIXME: the is_* and mode_as_string functions are only valid
    // for initialized objects.  If called for an object that is not
    // initialized, they should throw an exception.

    bool
    base_file_stat::is_blk (void) const
    {
      return exists () && is_blk (m_mode);
    }

    bool
    base_file_stat::is_chr (void) const
    {
      return exists () && is_chr (m_mode);
    }

    bool
    base_file_stat::is_dir (void) const
    {
      return exists () && is_dir (m_mode);
    }

    bool
    base_file_stat::is_fifo (void) const
    {
      return exists () && is_fifo (m_mode);
    }

    bool
    base_file_stat::is_lnk (void) const
    {
      return exists () && is_lnk (m_mode);
    }

    bool
    base_file_stat::is_reg (void) const
    {
      return exists () && is_reg (m_mode);
    }

    bool
    base_file_stat::is_sock (void) const
    {
      return exists () && is_sock (m_mode);
    }

    bool
    base_file_stat::is_blk (mode_t mode)
    {
      return octave_is_blk_wrapper (mode);
    }

    bool
    base_file_stat::is_chr (mode_t mode)
    {
      return octave_is_chr_wrapper (mode);
    }

    bool
    base_file_stat::is_dir (mode_t mode)
    {
      return octave_is_dir_wrapper (mode);
    }

    bool
    base_file_stat::is_fifo (mode_t mode)
    {
      return octave_is_fifo_wrapper (mode);
    }

    bool
    base_file_stat::is_lnk (mode_t mode)
    {
      return octave_is_lnk_wrapper (mode);
    }

    bool
    base_file_stat::is_reg (mode_t mode)
    {
      return octave_is_reg_wrapper (mode);
    }

    bool
    base_file_stat::is_sock (mode_t mode)
    {
      return octave_is_sock_wrapper (mode);
    }

    bool
    base_file_stat::have_struct_stat_st_rdev (void)
    {
      return ::octave_have_struct_stat_st_rdev ();
    }

    bool
    base_file_stat::have_struct_stat_st_blksize (void)
    {
      return octave_have_struct_stat_st_blksize ();
    }

    bool
    base_file_stat::have_struct_stat_st_blocks (void)
    {
      return octave_have_struct_stat_st_blocks ();
    }

    std::string
    base_file_stat::mode_as_string (void) const
    {
      char buf[12];

      octave_strmode_wrapper (m_mode, buf);

      return std::string (buf);
    }

    // Has FILE been modified since TIME?  Returns 1 for yes, 0 for no,
    // and -1 for any error.

    int
    base_file_stat::is_newer (const std::string& file,
                              const octave::sys::time& time)
    {
      file_stat fs (file);

      return fs ? fs.is_newer (time) : -1;
    }

    // Private stuff:

    void
    file_stat::update_internal (bool force)
    {
      if (! initialized || force)
        {
          initialized = false;
          fail = false;

          std::string full_file_name = octave::sys::file_ops::tilde_expand (file_name);

#if defined (__WIN32__)
          // Remove trailing slash.
          if (octave::sys::file_ops::is_dir_sep (full_file_name[full_file_name.length () - 1])
              && full_file_name.length () != 1
              && ! (full_file_name.length () == 3 && full_file_name[1] == ':'))
            full_file_name.resize (full_file_name.length () - 1);
#endif

          const char *cname = full_file_name.c_str ();

          time_t sys_atime, sys_mtime, sys_ctime;

          int status
            = (follow_links
               ? octave_stat_wrapper (cname, &m_mode, &m_ino, &m_dev,
                                      &m_nlink, &m_uid, &m_gid, &m_size,
                                      &sys_atime, &sys_mtime, &sys_ctime,
                                      &m_rdev, &m_blksize, &m_blocks)
               : octave_lstat_wrapper (cname, &m_mode, &m_ino, &m_dev,
                                       &m_nlink, &m_uid, &m_gid, &m_size,
                                       &sys_atime, &sys_mtime, &sys_ctime,
                                       &m_rdev, &m_blksize, &m_blocks));

          if (status < 0)
            {
              fail = true;
              errmsg = std::strerror (errno);
            }
          else
            {
              m_atime = octave::sys::time (sys_atime);
              m_mtime = octave::sys::time (sys_mtime);
              m_ctime = octave::sys::time (sys_ctime);
            }

          initialized = true;
        }
    }

    void
    file_fstat::update_internal (bool force)
    {
      if (! initialized || force)
        {
          initialized = false;
          fail = false;

          time_t sys_atime, sys_mtime, sys_ctime;

          int status
            = octave_fstat_wrapper (fid, &m_mode, &m_ino, &m_dev,
                                    &m_nlink, &m_uid, &m_gid, &m_size,
                                    &sys_atime, &sys_mtime, &sys_ctime,
                                    &m_rdev, &m_blksize, &m_blocks);

          if (status < 0)
            {
              fail = true;
              errmsg = std::strerror (errno);
            }
          else
            {
              m_atime = octave::sys::time (sys_atime);
              m_mtime = octave::sys::time (sys_mtime);
              m_ctime = octave::sys::time (sys_ctime);
            }

          initialized = true;
        }
    }
  }
}
