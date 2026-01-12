////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2026 The Octave Project Developers
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

// These functions are also defined in liboctave or libinterp.  They
// are repeated here to avoid having to link the main Octave program
// with the Octave libraries.

#if ! defined (octave_shared_sysdep_h)
#define octave_shared_sysdep_h 1

#include <cctype>

#if defined (OCTAVE_USE_WINDOWS_API)

#  include <windows.h>
#  include <tlhelp32.h>

#  if defined (_MSC_VER)
#    define popen _popen
#    define pclose _pclose
#  endif

char *
convert_utf16_to_utf8 (const wchar_t *utf16, const int size16, int &size8)
{
  // convert UTF-16 encoded wide character string to multibyte UTF-8 string

  // get required size of converted string
  size8 = WideCharToMultiByte (CP_UTF8, 0, utf16, size16, nullptr, 0, nullptr,
                               nullptr);

  if (size8 <= 0)
    return nullptr;

  // allocate buffer for UTF-8 string - must be freed by calling function
  char *utf8 = new char[size8];

  // actually convert to UTF-8
  WideCharToMultiByte (CP_UTF8, 0, utf16, size16, utf8, size8, nullptr,
                       nullptr);

  return utf8;
}

wchar_t *
convert_utf8_to_utf16 (const char *utf8, const int size8, int &size16)
{
  // convert multibyte UTF-8 string to UTF-16 encoded wide character string

  // get required size of converted string
  size16 = MultiByteToWideChar (CP_UTF8, 0, utf8, size8, nullptr, 0);

  if (size16 <= 0)
    return nullptr;

  // allocate buffer for UTF-16 string - must be freed by calling function
  wchar_t *utf16 = new wchar_t[size16];

  // actually convert to UTF-16
  MultiByteToWideChar (CP_UTF8, 0, utf8, size8, utf16, size16);

  return utf16;
}

#if defined (_UNICODE)
static char **
convert_wargv_to_utf8 (int argc, wchar_t **wargv)
{
    // allocate array of char* (argv-style)
    char **argv = new char*[argc + 1];
    int size8;

    for (int i = 0; i < argc; ++i)
      {
        argv[i] = convert_utf16_to_utf8 (wargv[i], -1, size8);
      }

    argv[argc] = nullptr;

    return argv;
}
#endif

std::string
w32_get_octave_home ()
{
  std::string retval;

  std::string bin_dir;

  wchar_t namebuf[MAX_PATH+1];
  DWORD size16
    = GetModuleFileNameW (GetModuleHandle (nullptr), namebuf, MAX_PATH);
  if (size16 < MAX_PATH)
    {
      // convert wide character string to multibyte UTF-8 string
      int size8;
      char *name_u8 = convert_utf16_to_utf8 (namebuf, size16, size8);
      if (name_u8)
        {
          std::string exe_name (name_u8, size8);
          delete[] name_u8;

          std::size_t pos = exe_name.rfind ('\\');

          if (pos != std::string::npos)
            bin_dir = exe_name.substr (0, pos + 1);
        }
    }

  if (! bin_dir.empty ())
    {
      std::size_t pos = bin_dir.rfind (R"(\bin\)");

      if (pos != std::string::npos)
        retval = bin_dir.substr (0, pos);
    }

  return retval;
}

#endif

// Find the directory where the octave binary is supposed to be
// installed.

#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)           \
     && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
static const char dir_sep_char = '\\';
#else
static const char dir_sep_char = '/';
#endif

#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)
static std::string dir_sep_chars = R"(/\)";
#else
static std::string dir_sep_chars = "/";
#endif

#endif
