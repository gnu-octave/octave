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

#if ! defined (octave_file_stat_h)
#define octave_file_stat_h 1

#include "octave-config.h"

#include <string>

#include "oct-time.h"

#include <sys/types.h>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

class
OCTAVE_API
base_file_stat
{
public:

  base_file_stat (void)
    : m_initialized (false), m_fail (false), m_errmsg (), m_mode (),
      m_ino (), m_dev (), m_nlink (), m_uid (), m_gid (),
      m_size (), m_atime (), m_mtime (), m_ctime (), m_rdev (),
      m_blksize (), m_blocks () { }

  base_file_stat (const base_file_stat& fs)
    : m_initialized (fs.m_initialized), m_fail (fs.m_fail),
      m_errmsg (fs.m_errmsg), m_mode (fs.m_mode), m_ino (fs.m_ino),
      m_dev (fs.m_dev), m_nlink (fs.m_nlink), m_uid (fs.m_uid),
      m_gid (fs.m_gid), m_size (fs.m_size), m_atime (fs.m_atime),
      m_mtime (fs.m_mtime), m_ctime (fs.m_ctime), m_rdev (fs.m_rdev),
      m_blksize (fs.m_blksize), m_blocks (fs.m_blocks) { }

  base_file_stat& operator = (const base_file_stat& fs)
  {
    if (this != &fs)
      {
        m_initialized = fs.m_initialized;
        m_fail = fs.m_fail;
        m_errmsg = fs.m_errmsg;
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
  sys::time time_resolution (void) const
  {
    static sys::time resolution (1.0);
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

  sys::time atime (void) const { return m_atime; }
  sys::time mtime (void) const { return m_mtime; }
  sys::time ctime (void) const { return m_ctime; }

  dev_t rdev (void) const { return m_rdev; }

  long blksize (void) const { return m_blksize; }
  long blocks (void) const { return m_blocks; }

  mode_t mode (void) const { return m_mode; }

  std::string mode_as_string (void) const;

  bool ok (void) const { return m_initialized && ! m_fail; }

  operator bool () const { return ok (); }

  bool exists (void) const { return ok (); }

  std::string error (void) const { return ok () ? "" : m_errmsg; }

  // Has the file referenced by this object been modified since TIME?
  bool is_newer (const sys::time& time) const { return m_mtime > time; }

  // It's nice to be able to hide the file_stat object if we don't
  // really care about it.
  static int is_newer (const std::string&, const sys::time&);

protected:

  virtual ~base_file_stat (void) = default;

  // TRUE means we have already called stat.
  bool m_initialized;

  // TRUE means the stat for this file failed.
  bool m_fail;

  // If a failure occurs, this contains the system error text.
  std::string m_errmsg;

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
  sys::time m_atime;

  // time of last modification
  sys::time m_mtime;

  // time of last file status change
  sys::time m_ctime;

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

  // This constructor must remain defined in the cpp file rather than in
  // the header file (bug #50234).
  file_stat (const std::string& n = "", bool fl = true);

  file_stat (const file_stat& fs)
    : base_file_stat (fs), m_file_name (fs.m_file_name),
      m_follow_links (fs.m_follow_links) { }

  file_stat& operator = (const file_stat& fs)
  {
    if (this != &fs)
      {
        base_file_stat::operator = (fs);

        m_file_name = fs.m_file_name;
        m_follow_links = fs.m_follow_links;
      }

    return *this;
  }

  // This destructor must remain as an empty destructor defined in the
  // cpp file rather than in the header file (bug #50234).
  ~file_stat (void);

  void get_stats (bool force = false)
  {
    if (! m_initialized || force)
      update_internal (force);
  }

  void get_stats (const std::string& n, bool force = false)
  {
    if (n != m_file_name || ! m_initialized || force)
      {
        m_initialized = false;

        m_file_name = n;

        update_internal (force);
      }
  }

private:

  // Name of the file.
  std::string m_file_name;

  // TRUE means follow symbolic links to the ultimate file (stat).
  // FALSE means get information about the link itself (lstat).
  bool m_follow_links;

  void update_internal (bool force = false);
};

class
OCTAVE_API
file_fstat : public base_file_stat
{
public:

  file_fstat (int n) : base_file_stat (), m_fid (n)
  {
    update_internal ();
  }

  file_fstat (const file_fstat& fs)
    : base_file_stat (fs), m_fid (fs.m_fid) { }

  file_fstat& operator = (const file_fstat& fs)
  {
    if (this != &fs)
      {
        base_file_stat::operator = (fs);

        m_fid = fs.m_fid;
      }

    return *this;
  }

  ~file_fstat (void) = default;

  void get_stats (bool force = false)
  {
    if (! m_initialized || force)
      update_internal (force);
  }

  void get_stats (int n, bool force = false)
  {
    if (n != m_fid || ! m_initialized || force)
      {
        m_initialized = false;

        m_fid = n;

        update_internal (force);
      }
  }

private:

  // Open file descriptor.
  int m_fid;

  void update_internal (bool force = false);
};

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
