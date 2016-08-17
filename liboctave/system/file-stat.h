/*

Copyright (C) 1996-2016 John W. Eaton

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

#if ! defined (octave_file_stat_h)
#define octave_file_stat_h 1

#include "octave-config.h"

#include <string>

#include "oct-time.h"

#include <sys/types.h>

namespace octave
{
  namespace sys
  {
    class
    OCTAVE_API
    base_file_stat
    {
    public:

      base_file_stat (void)
        : initialized (false), fail (false), errmsg (), m_mode (),
          m_ino (), m_dev (), m_nlink (), m_uid (), m_gid (),
          m_size (), m_atime (), m_mtime (), m_ctime (), m_rdev (),
          m_blksize (), m_blocks () { }

      base_file_stat (const base_file_stat& fs)
        : initialized (fs.initialized), fail (fs.fail), errmsg (fs.errmsg),
          m_mode (fs.m_mode), m_ino (fs.m_ino), m_dev (fs.m_dev),
          m_nlink (fs.m_nlink), m_uid (fs.m_uid), m_gid (fs.m_gid),
          m_size (fs.m_size), m_atime (fs.m_atime), m_mtime (fs.m_mtime),
          m_ctime (fs.m_ctime), m_rdev (fs.m_rdev),
          m_blksize (fs.m_blksize), m_blocks (fs.m_blocks) { }

      base_file_stat& operator = (const base_file_stat& fs)
        {
          if (this != &fs)
            {
              initialized = fs.initialized;
              fail = fs.fail;
              errmsg = fs.errmsg;
              m_mode = fs.m_mode;
              m_ino = fs.m_ino;
              m_dev = fs.m_dev;
              m_nlink = fs.m_nlink;
              m_uid = fs.m_uid;
              m_gid = fs.m_gid;
              m_size = fs.m_size;
              m_atime = fs.m_atime;
              m_mtime = fs.m_mtime;
              m_ctime = fs.m_ctime;
              m_rdev = fs.m_rdev;
              m_blksize = fs.m_blksize;
              m_blocks = fs.m_blocks;
            }

          return *this;
        }

      // The minimum difference in file time stamp values.
      // FIXME: This value should come from the filesystem itself.
      //        How can we get that info?
      octave::sys::time time_resolution (void) const
      {
        static octave::sys::time resolution (1.0);
        return resolution;
      }

      // File status and info.  The is_XXX functions will return false for
      // file_stat objects that are not properly initialized.  The others
      // should all return 0 (or the equivalent, for the given object)
      // which is likely not meaningful.

      bool is_blk (void) const;
      bool is_chr (void) const;
      bool is_dir (void) const;
      bool is_fifo (void) const;
      bool is_lnk (void) const;
      bool is_reg (void) const;
      bool is_sock (void) const;

      static bool is_blk (mode_t mode);
      static bool is_chr (mode_t mode);
      static bool is_dir (mode_t mode);
      static bool is_fifo (mode_t mode);
      static bool is_lnk (mode_t mode);
      static bool is_reg (mode_t mode);
      static bool is_sock (mode_t mode);

      static bool have_struct_stat_st_rdev (void);
      static bool have_struct_stat_st_blksize (void);
      static bool have_struct_stat_st_blocks (void);

      ino_t ino (void) const { return m_ino; }
      dev_t dev (void) const { return m_dev; }

      nlink_t nlink (void) const { return m_nlink; }

      uid_t uid (void) const { return m_uid; }
      gid_t gid (void) const { return m_gid; }

      off_t size (void) const { return m_size; }

      octave::sys::time atime (void) const { return m_atime; }
      octave::sys::time mtime (void) const { return m_mtime; }
      octave::sys::time ctime (void) const { return m_ctime; }

      dev_t rdev (void) const { return m_rdev; }

      long blksize (void) const { return m_blksize; }
      long blocks (void) const { return m_blocks; }

      mode_t mode (void) const { return m_mode; }

      std::string mode_as_string (void) const;

      bool ok (void) const { return initialized && ! fail; }

      operator bool () const { return ok (); }

      bool exists (void) const { return ok (); }

      std::string error (void) const { return ok () ? "" : errmsg; }

      // Has the file referenced by this object been modified since TIME?
      bool is_newer (const octave::sys::time& time) const { return m_mtime > time; }

      // It's nice to be able to hide the file_stat object if we don't
      // really care about it.
      static int is_newer (const std::string&, const octave::sys::time&);

    protected:

      virtual ~base_file_stat (void) { }

      // TRUE means we have already called stat.
      bool initialized;

      // TRUE means the stat for this file failed.
      bool fail;

      // If a failure occurs, this contains the system error text.
      std::string errmsg;

      // file type and permissions
      mode_t m_mode;

      // serial number
      ino_t m_ino;

      // device number
      dev_t m_dev;

      // number of links
      nlink_t m_nlink;

      // user ID of owner
      uid_t m_uid;

      // group ID of owner
      gid_t m_gid;

      // size in bytes, for regular files
      off_t m_size;

      // time of last access
      octave::sys::time m_atime;

      // time of last modification
      octave::sys::time m_mtime;

      // time of last file status change
      octave::sys::time m_ctime;

      // device number for special files
      dev_t m_rdev;

      // best I/O block size
      long m_blksize;

      // number of 512-byte blocks allocated
      long m_blocks;
    };

    class
    OCTAVE_API
    file_stat : public base_file_stat
    {
    public:

      file_stat (const std::string& n = "", bool fl = true)
        : base_file_stat (), file_name (n), follow_links (fl)
      {
        if (! file_name.empty ())
          update_internal ();
      }

      file_stat (const file_stat& fs)
        : base_file_stat (fs), file_name (fs.file_name),
        follow_links (fs.follow_links) { }

      file_stat& operator = (const file_stat& fs)
        {
          if (this != &fs)
            {
              base_file_stat::operator = (fs);

              file_name = fs.file_name;
              follow_links = fs.follow_links;
            }

          return *this;
        }

      ~file_stat (void) { }

      void get_stats (bool force = false)
      {
        if (! initialized || force)
          update_internal (force);
      }

      void get_stats (const std::string& n, bool force = false)
      {
        if (n != file_name || ! initialized || force)
          {
            initialized = false;

            file_name = n;

            update_internal (force);
          }
      }

    private:

      // Name of the file.
      std::string file_name;

      // TRUE means follow symbolic links to the ultimate file (stat).
      // FALSE means get information about the link itself (lstat).
      bool follow_links;

      void update_internal (bool force = false);
    };

    class
    OCTAVE_API
    file_fstat : public base_file_stat
    {
    public:

      file_fstat (int n) : base_file_stat (), fid (n)
      {
        update_internal ();
      }

      file_fstat (const file_fstat& fs)
        : base_file_stat (fs), fid (fs.fid) { }

      file_fstat& operator = (const file_fstat& fs)
        {
          if (this != &fs)
            {
              base_file_stat::operator = (fs);

              fid = fs.fid;
            }

          return *this;
        }

      ~file_fstat (void) { }

      void get_stats (bool force = false)
      {
        if (! initialized || force)
          update_internal (force);
      }

      void get_stats (int n, bool force = false)
      {
        if (n != fid || ! initialized || force)
          {
            initialized = false;

            fid = n;

            update_internal (force);
          }
      }

    private:

      // Open file descriptor.
      int fid;

      void update_internal (bool force = false);
    };
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::sys::base_file_stat' instead")
typedef octave::sys::base_file_stat base_file_stat;

OCTAVE_DEPRECATED ("use 'octave::sys::file_stat' instead")
typedef octave::sys::file_stat file_stat;

OCTAVE_DEPRECATED ("use 'octave::sys::file_fstat' instead")
typedef octave::sys::file_fstat file_fstat;

#endif

#endif
