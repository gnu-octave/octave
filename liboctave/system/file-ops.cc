////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2021 The Octave Project Developers
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
#  include <windows.h>
#  include <shlwapi.h>
#endif

#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
#  include <algorithm>
#  include "localcharset-wrapper.h"
#  include "uniconv-wrappers.h"
#endif

#include "areadlink-wrapper.h"
#include "canonicalize-file-name-wrapper.h"
#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "gen-tempname-wrapper.h"
#include "lo-sysdep.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "oct-passwd.h"
#include "quit.h"
#include "stat-wrappers.h"
#include "str-vec.h"
#include "unistd-wrappers.h"

namespace octave
{
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

  static size_t
  tilde_find_prefix (const std::string& s, size_t& len)
  {
    len = 0;

    size_t s_len = s.length ();

    if (s_len == 0 || s[0] == '~')
      return 0;

    string_vector prefixes = sys::file_ops::tilde_additional_prefixes;

    if (! prefixes.empty ())
      {
        for (size_t i = 0; i < s_len; i++)
          {
            for (int j = 0; j < prefixes.numel (); j++)
              {
                size_t pfx_len = prefixes[j].length ();

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

  static size_t
  tilde_find_suffix (const std::string& s)
  {
    size_t s_len = s.length ();

    string_vector suffixes = sys::file_ops::tilde_additional_suffixes;

    size_t i = 0;

    for ( ; i < s_len; i++)
      {
        if (sys::file_ops::is_dir_sep (s[i]))
          break;

        if (! suffixes.empty ())
          {
            for (int j = 0; j < suffixes.numel (); j++)
              {
                size_t sfx_len = suffixes[j].length ();

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
    size_t f_len = fname.length ();

    size_t len = 1;

    while (len < f_len && ! sys::file_ops::is_dir_sep (fname[len]))
      len++;

    return fname.substr (1, len);
  }

  // Do the work of tilde expansion on FILENAME.  FILENAME starts with a
  // tilde.

  static std::string
  tilde_expand_word (const std::string& filename)
  {
    size_t f_len = filename.length ();

    if (f_len == 0 || filename[0] != '~')
      return std::string (filename);

    // A leading '~/' or a bare '~' is *always* translated to the value
    // of $HOME or the home directory of the current user, regardless of
    // any preexpansion hook.

    if (f_len == 1 || sys::file_ops::is_dir_sep (filename[1]))
      return sys::env::get_home_directory () + filename.substr (1);

    std::string username = isolate_tilde_prefix (filename);

    size_t user_len = username.length ();

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

  namespace sys
  {
    namespace file_ops
    {
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

            size_t name_len = name.length ();

            // Scan through S expanding tildes as we come to them.

            size_t pos = 0;

            while (1)
              {
                if (pos > name_len)
                  break;

                size_t len;

                // Make START point to the tilde which starts the expansion.

                size_t start = tilde_find_prefix (name.substr (pos), len);

                result.append (name.substr (pos, start));

                // Advance STRING to the starting tilde.

                pos += start;

                // Make FINI be the index of one after the last character of the
                // username.

                size_t fini = tilde_find_suffix (name.substr (pos));

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
        size_t ipos = path.find_last_of (dir_sep_chars ());

        return (ipos != std::string::npos) ? path.substr (0, ipos) : "";
      }

      std::string tail (const std::string& path)
      {
        size_t ipos = path.find_last_of (dir_sep_chars ());

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
            size_t n = path.length ();
            for (size_t i = 0; i < n; i++)
              {
                if (path[i] == '/')
                  retval += dir_sep_char();
                else
                  retval += path[i];
              }
          }

        return retval;
      }
    }

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

#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
      // This is a work-around to allow running files in the current path if it
      // contains characters that cannot be encoded in the locale charset.
      if (name.compare (".") == 0)
        return env::get_current_directory ();

      // On Windows, convert to locale charset before passing to
      // canonicalize_file_name, and convert back to UTF-8 after that.

      // FIXME: This only allows non-ASCII characters in the file or path that
      // can be encoded in the locale charset.
      // Consider replacing this with std::filesystem::canonical once we allow
      // using C++17.

      const char *locale = octave_locale_charset_wrapper ();
      const uint8_t *name_u8 = reinterpret_cast<const uint8_t *>
                                 (name.c_str ());
      size_t length = 0;
      char *name_locale = octave_u8_conv_to_encoding (locale, name_u8,
                                                      name.length () + 1,
                                                      &length);

      if (name_locale)
        {
          char *tmp_locale =
            octave_canonicalize_file_name_wrapper (name_locale);
          free (name_locale);

          if (tmp_locale)
            {
              char *tmp = reinterpret_cast<char *>
                            (octave_u8_conv_from_encoding (locale, tmp_locale,
                                                           strlen (tmp_locale),
                                                           &length));
              free (tmp_locale);

              if (tmp)
                {
                  retval = std::string (tmp, length);
                  free (tmp);
                }
            }
        }
#else
      char *tmp = octave_canonicalize_file_name_wrapper (name.c_str ());

      if (tmp)
        {
          retval = tmp;
          free (tmp);
        }
#endif

#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
      // Canonical Windows file separator is backslash.
      std::replace (retval.begin (), retval.end (), '/', '\\');
#endif

#if defined (OCTAVE_USE_WINDOWS_API)
      std::wstring w_tmp;
      bool strip_marker = true;
      if (retval.empty ())
        {
          // For UNC paths, take the input as is.
          // Also translate forward slashes to backslashes.
          std::string name_backsl = name;
          std::replace (name_backsl.begin (), name_backsl.end (), '/', '\\');
          if (name_backsl.compare (0, 2, "\\\\") == 0)
            {
              w_tmp = u8_to_wstring (name_backsl);
              strip_marker = false;
              wchar_t canon_path[MAX_PATH];
              if (PathCanonicalizeW (canon_path, w_tmp.c_str ()))
                w_tmp = std::wstring (canon_path);
            }
        }
      else
        w_tmp = L"\\\\?\\" + u8_to_wstring (retval);

      if (! w_tmp.empty ())
        {
          // Get a more canonical name wrt case and full names
          // FIXME: To make this work on partitions that don't store short file
          // names, use FindFirstFileW on each component of the path.
          // Insufficient access permissions on parent folders might make this
          // tricky.

          // Parts of the path that wouldn't fit into a short 8.3 file name are
          // copied as is by GetLongPathNameW.  To also get the correct case
          // for these parts, first convert to short file names and than back
          // to long.
          wchar_t buffer[32767] = L"";
          int w_len = GetShortPathNameW (w_tmp.c_str (), buffer, 32767);
          w_len = GetLongPathNameW (buffer, buffer, 32767);

          if (! strip_marker)
            retval = u8_from_wstring (std::wstring (buffer, w_len));
          else if (w_len > 4)
            retval = u8_from_wstring (std::wstring (buffer+4, w_len-4));

          // If root is a drive, use an upper case letter for the drive letter.
          if (retval.length () > 1 && retval[1] == ':')
            retval[0] = toupper (retval[0]);
        }
#endif

      if (retval.empty ())
        msg = std::strerror (errno);

      return retval;
    }
  }
}
