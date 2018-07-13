/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include "dir-ops.h"
#include "file-ops.h"
#include "lo-error.h"
#include "lo-sysdep.h"
#include "uniconv-wrappers.h"
#include "unistd-wrappers.h"

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <windows.h>
#  include <wchar.h>

#  include "lo-hash.h"
#endif

namespace octave
{
  namespace sys
  {
    std::string
    getcwd (void)
    {
      std::string retval;

      // Using octave_getcwd_wrapper ensures that we have a getcwd that
      // will allocate a buffer as large as necessary if buf and size are
      // both 0.

      char *tmp = octave_getcwd_wrapper (nullptr, 0);

      if (! tmp)
        (*current_liboctave_error_handler) ("unable to find current directory");

      retval = tmp;
      free (tmp);

      return retval;
    }

    int
    chdir (const std::string& path_arg)
    {
      std::string path = sys::file_ops::tilde_expand (path_arg);

#if defined (OCTAVE_USE_WINDOWS_API)
      if (path.length () == 2 && path[1] == ':')
        path += '\\';
#endif

      return octave_chdir_wrapper (path.c_str ());
    }

    bool
    get_dirlist (const std::string& dirname, string_vector& dirlist, std::string& msg)
    {
      dirlist = "";
      msg = "";
#if defined (OCTAVE_USE_WINDOWS_API)
      _WIN32_FIND_DATAW ffd;

      std::string path_name (dirname);
      if (path_name.empty ())
        return true;

      if (path_name.back () == '\\' || path_name.back () == '/')
        path_name.push_back ('*');
      else
        path_name.append (R"(\*)");

      // Find first file in directory.
      HANDLE hFind = FindFirstFileW (u8_to_wstring (path_name).c_str (),
                              &ffd);
      if (INVALID_HANDLE_VALUE == hFind)
        {
          DWORD errCode = GetLastError ();
          char *errorText;
          FormatMessageA (FORMAT_MESSAGE_FROM_SYSTEM |
                         FORMAT_MESSAGE_ALLOCATE_BUFFER |
                         FORMAT_MESSAGE_IGNORE_INSERTS,
                         NULL, errCode,
                         MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
                         errorText, 0, NULL);
          if (errorText != NULL)
            {
              msg = std::string (errorText);
              LocalFree (errorText);
            }
          return false;
        }

      std::list<std::string> dirlist_str;
      do
        dirlist_str.push_back (u8_from_wstring (ffd.cFileName));
      while (FindNextFileW (hFind, &ffd) != 0);

      FindClose(hFind);

      dirlist = string_vector (dirlist_str);

#else

      dir_entry dir (dirname);

      if (! dir)
        {
          msg = dir.error ();
          return false;
        }

      dirlist = dir.read ();

      dir.close ();
#endif

      return true;
    }

    std::FILE *
    fopen (const std::string& filename, const std::string& mode)
    {
#if defined (OCTAVE_USE_WINDOWS_API)
      return _wfopen (u8_to_wstring (filename).c_str (),
                      u8_to_wstring (mode).c_str ());
#else
      return std::fopen (filename.c_str (), mode.c_str ());
#endif
    }

    std::wstring
    u8_to_wstring (const std::string& utf8_string)
    {
      size_t srclen = utf8_string.length ();
      const uint8_t *src = reinterpret_cast<const uint8_t *>
                           (utf8_string.c_str ());

      size_t length = 0;
      wchar_t *wchar = reinterpret_cast<wchar_t *>
                       (octave_u8_conv_to_encoding ("wchar_t", src, srclen,
                                                    &length));

      std::wstring retval = L"";
      if (wchar != nullptr)
        {
          retval = std::wstring (wchar, length / sizeof (wchar_t));
          free (static_cast<void *> (wchar));
        }

      return retval;
    }

    std::string
    u8_from_wstring (const std::wstring& wchar_string)
    {
      size_t srclen = wchar_string.length () * sizeof (wchar_t);
      const char *src = reinterpret_cast<const char *> (wchar_string.c_str ());

      size_t length = 0;
      char *mbchar = reinterpret_cast<char *>
                     (octave_u8_conv_from_encoding ("wchar_t", src, srclen,
                                                    &length));

      std::string retval = "";
      if (mbchar != nullptr)
        {
          retval = std::string (mbchar, length);
          free (static_cast<void *> (mbchar));
        }

      return retval;
    }

    std::string
    get_ASCII_filename (const std::string& orig_file_name)
    {
#if defined (OCTAVE_USE_WINDOWS_API)
      // Return file name that only contains ASCII characters that can be used
      // to access the file orig_file_name.  The original file must exist in the
      // file system before calling this function.
      // This is useful for passing file names to functions that are not aware
      // of the character encoding we are using.

      // 1. Check whether filename contains non-ASCII (UTF-8) characters
      std::string::const_iterator first_non_ASCII =
                std::find_if (orig_file_name.begin (), orig_file_name.end (),
                [](char c){return (c < 0 || c >= 128);});
      if (first_non_ASCII == orig_file_name.end ())
        return orig_file_name;

      // 2. Check if file system stores short filenames (always ASCII-only).
      const wchar_t *w_orig_file_name = u8_to_wstring (orig_file_name).c_str ();
      // get short filename (8.3) from UTF-16 filename
      long length = GetShortPathNameW (w_orig_file_name, NULL, 0);

      // Dynamically allocate the correct size
      // (terminating null char was included in length)
      wchar_t *w_short_file_name = new wchar_t[length];
      length = GetShortPathNameW (w_orig_file_name, w_short_file_name, length);

      std::string short_file_name = u8_from_wstring (std::wstring (w_short_file_name));

      if (short_file_name.compare (orig_file_name) != 0)
        return short_file_name;

      // 3. Create hard link with only-ASCII characters
      // Get longest possible part of path that only contains ASCII chars.
      size_t pos = (std::string (orig_file_name.begin (), first_non_ASCII)).
                        find_last_of (octave::sys::file_ops::dir_sep_chars ());
      std::string par_dir = orig_file_name.substr (0, pos+1);

      // create .oct_ascii directory
      // FIXME: We need to have write permission in this location.
      std::string oct_ascii_dir = par_dir + ".oct_ascii";
      std::string test_dir = canonicalize_file_name (oct_ascii_dir);
      if (test_dir.empty ())
      {
        std::string msg;
        int status = octave::sys::mkdir (oct_ascii_dir, 0777, msg);
        if (status < 0)
          return orig_file_name;
        // set hidden property
        SetFileAttributesA (oct_ascii_dir.c_str (), FILE_ATTRIBUTE_HIDDEN);
      }

      // create file from hash of full filename
      std::string filename_hash = oct_ascii_dir + file_ops::dir_sep_str () +
                                  octave::crypto::hash ("SHA1", orig_file_name);
      std::string _filename_hash_ = canonicalize_file_name (filename_hash);
      if (! _filename_hash_.empty ())
        return _filename_hash_;

      wchar_t w_filename_hash[filename_hash.length ()+1] = {0};
      for (size_t i=0; i < filename_hash.length (); i++)
        w_filename_hash[i] = filename_hash.at (i);
      if (CreateHardLinkW (w_filename_hash, w_orig_file_name, NULL))
        return filename_hash;

      return orig_file_name;

#else
      return orig_file_name;
#endif
    }

  }
}
