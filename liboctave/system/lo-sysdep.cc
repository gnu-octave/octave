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
      wchar_t *wchar = nullptr;

      wchar = u8_to_wchar (utf8_string.c_str ());

      std::wstring retval = L"";
      if (wchar != nullptr)
        {
          retval = std::wstring (wchar);
          free (static_cast<void *> (wchar));
        }

      return retval;
    }

    std::string
    u8_from_wstring (const std::wstring& wchar_string)
    {
      char *mbchar = nullptr;

      mbchar = u8_from_wchar (wchar_string.c_str ());

      std::string retval = "";
      if (mbchar != nullptr)
        {
          retval = std::string (mbchar);
          free (static_cast<void *> (mbchar));
        }

      return retval;
    }

  }
}
