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

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <vector>

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <cctype>

#  include <windows.h>
#  include "unwind-prot.h"
#else
#  include "canonicalize-file-name-wrapper.h"
#endif

#include "areadlink-wrapper.h"
#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "gen-tempname-wrapper.h"
#include "lo-sysdep.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "oct-password.h"
#include "quit.h"
#include "stat-wrappers.h"
#include "str-vec.h"
#include "unistd-wrappers.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// The following tilde-expansion code was stolen and adapted from
// readline.

// The default value of tilde_additional_prefixes.  This is set to
// whitespace preceding a tilde so that simple programs which do not
// perform any word separation get desired behavior.
static const char *default_prefixes[] = { " ~", "\t~", ":~", nullptr };

// The default value of tilde_additional_suffixes.  This is set to
// whitespace or newline so that simple programs which do not perform
// any word separation get desired behavior.
static const char *default_suffixes[] = { " ", "\n", ":", nullptr };

static std::size_t
tilde_find_prefix (const std::string& s, std::size_t& len)
{
  len = 0;

  std::size_t s_len = s.length ();

  if (s_len == 0 || s[0] == '~')
    return 0;

  string_vector prefixes = sys::file_ops::tilde_additional_prefixes;

  if (! prefixes.empty ())
    {
      for (std::size_t i = 0; i < s_len; i++)
        {
          for (int j = 0; j < prefixes.numel (); j++)
            {
              std::size_t pfx_len = prefixes[j].length ();

              if (prefixes[j] == s.substr (i, pfx_len))
                {
                  len = pfx_len - 1;
                  return i + len;
                }
            }
        }
    }

  return s_len;
}

// Find the end of a tilde expansion in S, and return the index
// of the character which ends the tilde definition.

static std::size_t
tilde_find_suffix (const std::string& s)
{
  std::size_t s_len = s.length ();

  string_vector suffixes = sys::file_ops::tilde_additional_suffixes;

  std::size_t i = 0;

  for ( ; i < s_len; i++)
    {
      if (sys::file_ops::is_dir_sep (s[i]))
        break;

      if (! suffixes.empty ())
        {
          for (int j = 0; j < suffixes.numel (); j++)
            {
              std::size_t sfx_len = suffixes[j].length ();

              if (suffixes[j] == s.substr (i, sfx_len))
                return i;
            }
        }
    }

  return i;
}

// Take FNAME and return the tilde prefix we want expanded.

static std::string
isolate_tilde_prefix (const std::string& fname)
{
  std::size_t f_len = fname.length ();

  std::size_t len = 1;

  while (len < f_len && ! sys::file_ops::is_dir_sep (fname[len]))
    len++;

  return fname.substr (1, len);
}

// Do the work of tilde expansion on FILENAME.  FILENAME starts with a
// tilde.

static std::string
tilde_expand_word (const std::string& filename)
{
  std::size_t f_len = filename.length ();

  if (f_len == 0 || filename[0] != '~')
    return std::string (filename);

  // A leading '~/' or a bare '~' is *always* translated to the value
  // of $HOME or the home directory of the current user, regardless of
  // any preexpansion hook.

  if (f_len == 1 || sys::file_ops::is_dir_sep (filename[1]))
    return sys::env::get_home_directory () + filename.substr (1);

  std::string username = isolate_tilde_prefix (filename);

  std::size_t user_len = username.length ();

  std::string dirname;

  if (sys::file_ops::tilde_expansion_preexpansion_hook)
    {
      std::string expansion
        = sys::file_ops::tilde_expansion_preexpansion_hook (username);

      if (! expansion.empty ())
        return expansion + filename.substr (user_len+1);
    }

  // No preexpansion hook, or the preexpansion hook failed.  Look in the
  // password database.

  sys::password pw = sys::password::getpwnam (username);

  if (! pw)
    {
      // If the calling program has a special syntax for expanding tildes,
      // and we couldn't find a standard expansion, then let them try.

      if (sys::file_ops::tilde_expansion_failure_hook)
        {
          std::string expansion
            = sys::file_ops::tilde_expansion_failure_hook (username);

          if (! expansion.empty ())
            dirname = expansion + filename.substr (user_len+1);
        }

      // If we don't have a failure hook, or if the failure hook did not
      // expand the tilde, return a copy of what we were passed.

      if (dirname.empty ())
        dirname = filename;
    }
  else
    dirname = pw.dir () + filename.substr (user_len+1);

  return dirname;
}

OCTAVE_BEGIN_NAMESPACE(sys)

OCTAVE_BEGIN_NAMESPACE(file_ops)

char dev_sep_char (void)
{
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
  return ':';
#else
  return 0;
#endif
}

char dir_sep_char (void)
{
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
  return '\\';
#else
  return '/';
#endif
}

std::string dir_sep_str (void)
{
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
  return R"(\)";
#else
  return "/";
#endif
}

std::string dir_sep_chars (void)
{
#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)
  return R"(/\)";
#else
  return dir_sep_str ();
#endif
}

tilde_expansion_hook tilde_expansion_preexpansion_hook = nullptr;

tilde_expansion_hook tilde_expansion_failure_hook = nullptr;

string_vector tilde_additional_prefixes = default_prefixes;

string_vector tilde_additional_suffixes = default_suffixes;

bool is_dev_sep (char c)
{
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
  return c == dev_sep_char ();
#else
  octave_unused_parameter (c);

  return false;
#endif
}

bool is_dir_sep (char c)
{
  std::string tmp = dir_sep_chars ();
  return tmp.find (c) != std::string::npos;
}

std::string tilde_expand (const std::string& name)
{
  if (name.find ('~') == std::string::npos)
    return std::string (name);
  else
    {
      std::string result;

      std::size_t name_len = name.length ();

      // Scan through S expanding tildes as we come to them.

      std::size_t pos = 0;

      while (1)
        {
          if (pos > name_len)
            break;

          std::size_t len;

          // Make START point to the tilde which starts the expansion.

          std::size_t start = tilde_find_prefix (name.substr (pos), len);

          result.append (name.substr (pos, start));

          // Advance STRING to the starting tilde.

          pos += start;

          // Make FINI be the index of one after the last character of the
          // username.

          std::size_t fini = tilde_find_suffix (name.substr (pos));

          // If both START and FINI are zero, we are all done.

          if (! (start || fini))
            break;

          // Expand the entire tilde word, and copy it into RESULT.

          std::string tilde_word = name.substr (pos, fini);

          pos += fini;

          std::string expansion = tilde_expand_word (tilde_word);

          result.append (expansion);
        }

      return result;
    }
}

string_vector tilde_expand (const string_vector& names)
{
  int n = names.numel ();

  string_vector retval (n);

  for (int i = 0; i < n; i++)
    retval[i] = tilde_expand (names[i]);

  return retval;
}

std::string concat (const std::string& dir, const std::string& file)
{
  return dir.empty ()
         ? file
         : (is_dir_sep (dir.back ())
            ? dir + file
            : dir + dir_sep_char () + file);
}

std::string dirname (const std::string& path)
{
  std::size_t ipos = path.find_last_of (dir_sep_chars ());

  return (ipos != std::string::npos) ? path.substr (0, ipos) : "";
}

std::string tail (const std::string& path)
{
  std::size_t ipos = path.find_last_of (dir_sep_chars ());

  if (ipos != std::string::npos)
    ipos++;
  else
    ipos = 0;

  return path.substr (ipos);
}

std::string native_separator_path (const std::string& path)
{
  std::string retval;

  if (dir_sep_char () == '/')
    retval = path;
  else
    {
      std::size_t n = path.length ();
      for (std::size_t i = 0; i < n; i++)
        {
          if (path[i] == '/')
            retval += dir_sep_char();
          else
            retval += path[i];
        }
    }

  return retval;
}

OCTAVE_END_NAMESPACE(file_ops)

int mkdir (const std::string& nm, mode_t md)
{
  std::string msg;
  return mkdir (nm, md, msg);
}

int mkdir (const std::string& name, mode_t mode, std::string& msg)
{
  msg = "";

  int status = octave_mkdir_wrapper (name.c_str (), mode);

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

int recursive_mkdir (const std::string& name, mode_t mode)
{
  std::string msg;
  return recursive_mkdir (name, mode, msg);
}

int recursive_mkdir (const std::string& name, mode_t mode, std::string& msg)
{
  int status;

  // account for root in absolute directories
#if defined (OCTAVE_USE_WINDOWS_API)
  // root of current drive
  std::size_t skip_root = 0;
  if (name.size () > 1)
    {
      if (name[1] == ':')
        // drive root (e.g., C:\)
        skip_root = 2;
      else if (file_ops::is_dir_sep (name[0])
               && file_ops::is_dir_sep (name[1]))
        {
          // UNC path root (e.g., \\SERVER\share\)
          skip_root = name.find_first_of (file_ops::dir_sep_chars (), 2);
          skip_root = name.find_first_of (file_ops::dir_sep_chars (),
                                          skip_root + 1);
        }
    }

  std::size_t delim = name.find_first_of (file_ops::dir_sep_chars (),
                                          skip_root + 1);
#else
  std::size_t delim = name.find_first_of (file_ops::dir_sep_chars (), 1);
#endif

  // iterate over all componenents of NAME and make directories
  while (delim != std::string::npos)
    {
      std::string base = name.substr (0, delim);
      sys::file_stat fs (base);
      if (! fs.is_dir ())
        {
          status = mkdir (base, mode, msg);
          if (status < 0)
            return status;
        }
      delim = name.find_first_of (file_ops::dir_sep_chars (), delim + 1);
    }

  // finally, create requested directory
  return mkdir (name, mode, msg);
}

int mkfifo (const std::string& nm, mode_t md)
{
  std::string msg;
  return mkfifo (nm, md, msg);
}

int mkfifo (const std::string& name, mode_t mode, std::string& msg)
{
  msg = "";

  int status = octave_mkfifo_wrapper (name.c_str (), mode);

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

int link (const std::string& old_name, const std::string& new_name)
{
  std::string msg;
  return link (old_name, new_name, msg);
}

int link (const std::string& old_name, const std::string& new_name,
          std::string& msg)
{
  msg = "";

  int status = -1;

  status = octave_link_wrapper (old_name.c_str (), new_name.c_str ());

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

int symlink (const std::string& old_name, const std::string& new_name)
{
  std::string msg;
  return symlink (old_name, new_name, msg);
}

int symlink (const std::string& old_name, const std::string& new_name,
             std::string& msg)
{
  msg = "";

  int status = -1;

  status = octave_symlink_wrapper (old_name.c_str (), new_name.c_str ());

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

int readlink (const std::string& path, std::string& result)
{
  std::string msg;
  return readlink (path, result, msg);
}

int readlink (const std::string& path, std::string& result, std::string& msg)
{
  int status = -1;

  msg = "";

  char *buf = octave_areadlink_wrapper (path.c_str ());

  if (! buf)
    msg = std::strerror (errno);
  else
    {
      result = buf;
      ::free (buf);
      status = 0;
    }

  return status;
}

int rename (const std::string& from, const std::string& to)
{
  std::string msg;
  return rename (from, to, msg);
}

int rename (const std::string& from, const std::string& to,
            std::string& msg)
{
  int status = -1;

  msg = "";

#if defined (OCTAVE_USE_WINDOWS_API)
  std::wstring wfrom = u8_to_wstring (from);
  std::wstring wto = u8_to_wstring (to);
  status = _wrename (wfrom.c_str (), wto.c_str ());
#else
  status = std::rename (from.c_str (), to.c_str ());
#endif

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

int rmdir (const std::string& name)
{
  std::string msg;
  return rmdir (name, msg);
}

int rmdir (const std::string& name, std::string& msg)
{
  msg = "";

  int status = -1;

  status = octave_rmdir_wrapper (name.c_str ());

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

// And a version that works recursively.

int recursive_rmdir (const std::string& name)
{
  std::string msg;
  return recursive_rmdir (name, msg);
}

int recursive_rmdir (const std::string& name, std::string& msg)
{
  msg = "";

  int status = 0;

  string_vector dirlist;

  if (get_dirlist (name, dirlist, msg))
    {
      for (octave_idx_type i = 0; i < dirlist.numel (); i++)
        {
          octave_quit ();

          std::string nm = dirlist[i];

          // Skip current directory and parent.
          if (nm == "." || nm == "..")
            continue;

          std::string fullnm = name + file_ops::dir_sep_str () + nm;

          // Get info about the file.  Don't follow links.
          file_stat fs (fullnm, false);

          if (fs)
            {
              if (fs.is_dir ())
                {
                  status = recursive_rmdir (fullnm, msg);

                  if (status < 0)
                    break;
                }
              else
                {
                  status = unlink (fullnm, msg);

                  if (status < 0)
                    break;
                }
            }
          else
            {
              msg = fs.error ();
              break;
            }
        }

      if (status >= 0)
        status = rmdir (name, msg);
    }
  else
    status = -1;

  return status;
}

int umask (mode_t mode)
{
  return octave_umask_wrapper (mode);
}

int unlink (const std::string& name)
{
  std::string msg;
  return unlink (name, msg);
}

int unlink (const std::string& name, std::string& msg)
{
  msg = "";

  int status = -1;

  status = octave_unlink_wrapper (name.c_str ());

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

std::string tempnam (const std::string& dir, const std::string& pfx)
{
  std::string msg;
  return tempnam (dir, pfx, msg);
}

std::string tempnam (const std::string& dir, const std::string& pfx,
                     std::string& msg)
{
  msg = "";

  std::string retval;

  // get dir path to use for template
  std::string templatename;
  if (dir.empty ())
    templatename = env::get_temp_directory ();
  else if (! file_stat (dir, false).is_dir ())
    templatename = env::get_temp_directory ();
  else
    templatename = dir;

  // add dir sep char if it is not there
  if (*templatename.rbegin () != file_ops::dir_sep_char ())
    templatename += file_ops::dir_sep_char ();

  if (pfx.empty ())
    templatename += "file";
  else
    templatename += pfx;

  // add the required XXXXXX for the template
  templatename += "XXXXXX";

  // create and copy template to char array for call to gen_tempname
  char tname [templatename.length () + 1];

  strcpy (tname, templatename.c_str ());

  if (octave_gen_tempname_wrapper (tname) == -1)
    msg = std::strerror (errno);
  else
    retval = tname;

  return retval;
}

std::string canonicalize_file_name (const std::string& name)
{
  std::string msg;
  return canonicalize_file_name (name, msg);
}

std::string canonicalize_file_name (const std::string& name, std::string& msg)
{
  msg = "";

  std::string retval;

  // FIXME:  Consider replacing this with std::filesystem::canonical on all
  // platforms once we allow using C++17.

#if defined (OCTAVE_USE_WINDOWS_API)
  // open file handle
  std::wstring wname = u8_to_wstring (name);
  HANDLE h_file = CreateFileW (wname.c_str (), GENERIC_READ,
                               FILE_SHARE_READ, nullptr, OPEN_EXISTING,
                               FILE_FLAG_BACKUP_SEMANTICS, nullptr);

  // Might have been a symbolic link that points to a network share.
  // It looks like opening a network share itself (not a file or folder
  // *on* a share) might return an invalid handle. As a workaround, try to
  // open a handle to the symbolic link itself (and do not resolve it).
  if (h_file == INVALID_HANDLE_VALUE)
    h_file = CreateFileW (wname.c_str (), GENERIC_READ,
                          FILE_SHARE_READ, nullptr, OPEN_EXISTING,
                          FILE_FLAG_BACKUP_SEMANTICS
                          | FILE_FLAG_OPEN_REPARSE_POINT, nullptr);

  if (h_file == INVALID_HANDLE_VALUE)
    {
      msg = "Unable to open file \"" + name + "\"";
      return retval;
    }

  unwind_action close_file_handle (CloseHandle, h_file);

  const std::size_t buf_size = 32767;
  wchar_t buffer[buf_size] = L"";

  // query canonical name
  DWORD len = GetFinalPathNameByHandleW (h_file, buffer, buf_size,
                                         FILE_NAME_NORMALIZED);
  if (len >= buf_size)
    {
      msg = "Error querying normalized name for \"" + name + "\"";
      return retval;
    }

  retval = u8_from_wstring (std::wstring (buffer, len));

  // remove prefix
  // "Normal" paths are prefixed by "\\?\".
  // UNC paths are prefixed by "\\?\UNC\".
  if (retval.compare (0, 8, R"(\\?\UNC\)") == 0)
    {
      retval = retval.erase (2, 6);

      // If the initial path looked like a mapped network drive, replace
      // portion of path that corresponds to mapped root with drive root.
      if (name.size () < 3 || name[1] != ':')
        return retval;

      // UNC path corresponding to original drive letter (mappped drive)
      std::wstring orig_map = wname.substr (0, 3);
      orig_map[2] = L'\\';
      HANDLE h_map = CreateFileW (orig_map.c_str (), GENERIC_READ,
                                  FILE_SHARE_READ, nullptr, OPEN_EXISTING,
                                  FILE_FLAG_BACKUP_SEMANTICS
                                  | FILE_FLAG_OPEN_REPARSE_POINT,
                                  nullptr);

      if (h_map == INVALID_HANDLE_VALUE)
        // cannot determine common root
        return retval;

      unwind_action close_map_handle (CloseHandle, h_map);
      len = GetFinalPathNameByHandleW (h_map, buffer, buf_size,
                                       FILE_NAME_NORMALIZED);

      std::string orig_root
        = u8_from_wstring (std::wstring (buffer, len));

      if (orig_root.compare (0, 8, R"(\\?\UNC\)"))
        // root was not a mapped share
        return retval;

      orig_root = orig_root.erase (2, 6);
      // trim trailing file separators from UNC path corresponding to root
      std::string file_seps = file_ops::dir_sep_chars ();
      while (file_seps.find (orig_root.back ()) != std::string::npos)
        orig_root.pop_back ();

      if (retval.compare (0, orig_root.size (), orig_root))
        // start of UNC path doesn't match mapped drive root
        return retval;

      // file is on mapped share
      size_t sep_pos = orig_root.size ();
      if (sep_pos != retval.size ())
        retval = retval.substr (sep_pos-2);
      else
        retval.resize (2);  // no file component
      retval[0] = std::toupper (name[0]);
      retval[1] = ':';
    }
  else if (retval.compare (0, 4, R"(\\?\)") == 0)
    retval = retval.erase (0, 4);
#else
  char *tmp = octave_canonicalize_file_name_wrapper (name.c_str ());

  if (tmp)
    {
      retval = tmp;
      free (tmp);
    }

  if (retval.empty ())
    msg = std::strerror (errno);
#endif

  return retval;
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
