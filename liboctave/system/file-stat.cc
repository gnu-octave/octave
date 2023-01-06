////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <ctime>
#include <cerrno>
#include <cstring>

#include "file-ops.h"
#include "file-stat.h"
#include "stat-wrappers.h"
#include "strmode-wrapper.h"

#if defined (OCTAVE_USE_WINDOWS_API)
#  include "lo-regexp.h"
#  include "oct-env.h"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

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
                          const sys::time& time)
{
  file_stat fs (file);

  return fs ? fs.is_newer (time) : -1;
}

// Private stuff:

file_stat::file_stat (const std::string& n, bool fl)
  : base_file_stat (), m_file_name (n), m_follow_links (fl)
{
  if (! m_file_name.empty ())
    update_internal ();
}

file_stat::~file_stat () { }

void
file_stat::update_internal (bool force)
{
  if (! m_initialized || force)
    {
      m_initialized = false;
      m_fail = false;

      std::string full_file_name = sys::file_ops::tilde_expand (m_file_name);

#if defined (OCTAVE_USE_WINDOWS_API)
      full_file_name = sys::env::make_absolute (full_file_name);

      // Remove trailing slashes
      while (full_file_name.length () > 1
             && sys::file_ops::is_dir_sep (full_file_name.back ()))
        full_file_name.pop_back ();

      // If path is a root (like "C:" or "\\SERVER\share"), add a
      // trailing backslash.
      // FIXME: This pattern does not match all possible UNC roots:
      //        https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-dtyp/62e862f4-2a51-452e-8eeb-dc4ff5ee33cc
      static const regexp pat (R"(^\\\\[\w.-]+\\[\w\$-]+$)");
      if ((full_file_name.length () == 2 && full_file_name[1] == ':')
          || (full_file_name.length () > 4  && full_file_name[0] == '\\'
              && full_file_name[1] == '\\' && pat.is_match (full_file_name)))
        full_file_name.push_back ('\\');
#endif

      const char *cname = full_file_name.c_str ();

      time_t sys_atime, sys_mtime, sys_ctime;

      int status
        = (m_follow_links
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
          m_fail = true;
          m_errmsg = std::strerror (errno);
        }
      else
        {
          m_atime = sys::time (static_cast<OCTAVE_TIME_T> (sys_atime));
          m_mtime = sys::time (static_cast<OCTAVE_TIME_T> (sys_mtime));
          m_ctime = sys::time (static_cast<OCTAVE_TIME_T> (sys_ctime));
        }

      m_initialized = true;
    }
}

void
file_fstat::update_internal (bool force)
{
  if (! m_initialized || force)
    {
      m_initialized = false;
      m_fail = false;

      time_t sys_atime, sys_mtime, sys_ctime;

      int status
        = octave_fstat_wrapper (m_fid, &m_mode, &m_ino, &m_dev,
                                &m_nlink, &m_uid, &m_gid, &m_size,
                                &sys_atime, &sys_mtime, &sys_ctime,
                                &m_rdev, &m_blksize, &m_blocks);

      if (status < 0)
        {
          m_fail = true;
          m_errmsg = std::strerror (errno);
        }
      else
        {
          m_atime = sys::time (static_cast<OCTAVE_TIME_T> (sys_atime));
          m_mtime = sys::time (static_cast<OCTAVE_TIME_T> (sys_mtime));
          m_ctime = sys::time (static_cast<OCTAVE_TIME_T> (sys_ctime));
        }

      m_initialized = true;
    }
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
