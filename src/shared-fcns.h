////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#if ! defined (octave_shared_fcns_h)
#define octave_shared_fcns_h 1

#include <cctype>

#if defined (OCTAVE_USE_WINDOWS_API)

#  include <windows.h>
#  include <tlhelp32.h>
#  include <locale>
#  include <codecvt>

#  if defined (_MSC_VER)
#    define popen _popen
#    define pclose _pclose
#  endif

static std::string
w32_get_octave_home (void)
{
  std::string retval;

  std::string bin_dir;

  wchar_t namebuf[MAX_PATH+1];
  DWORD n_size
    = GetModuleFileNameW (GetModuleHandle (nullptr), namebuf, MAX_PATH);
  if (n_size < MAX_PATH)
    {
      // convert wide character string to multibyte UTF-8 string
      std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> wchar_conv;
      std::string exe_name
        = wchar_conv.to_bytes (std::wstring (namebuf, n_size));

      std::size_t pos = exe_name.rfind ('\\');

      if (pos != std::string::npos)
        bin_dir = exe_name.substr (0, pos + 1);
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

static std::string
octave_getenv (const std::string& name)
{
  char *value = ::getenv (name.c_str ());

  return value ? value : "";
}

static std::string Voctave_home;
static std::string Voctave_exec_home;

static void
set_octave_home (void)
{
  std::string op = OCTAVE_PREFIX;
  std::string oep = OCTAVE_EXEC_PREFIX;

  std::string oh = octave_getenv ("OCTAVE_HOME");
  std::string oeh = octave_getenv ("OCTAVE_EXEC_HOME");

#if defined (OCTAVE_USE_WINDOWS_API)
  if (oh.empty ())
    oh = w32_get_octave_home ();
#endif

  // If OCTAVE_HOME is set in the environment, use that.  Otherwise,
  // default to ${prefix} from configure.

  Voctave_home = (oh.empty () ? op : oh);

  // If OCTAVE_EXEC_HOME is set in the environment, use that.
  // Otherwise, if ${prefix} and ${exec_prefix} from configure are set
  // to the same value, use OCTAVE_HOME from the environment if it is set.
  // Otherwise, default to ${exec_prefix} from configure.

  if (! oeh.empty ())
    Voctave_exec_home = oeh;
  else if (op == oep && ! oh.empty ())
    Voctave_exec_home = oh;
  else
    Voctave_exec_home = oep;
}

static bool is_dir_sep (char c)
{
  return dir_sep_chars.find (c) != std::string::npos;
}

static bool
absolute_pathname (const std::string& s)
{
  std::size_t len = s.length ();

  if (len == 0)
    return false;

  if (is_dir_sep (s[0]))
    return true;

#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)
  if ((len == 2 && isalpha (s[0]) && s[1] == ':')
      || (len > 2 && isalpha (s[0]) && s[1] == ':'
          && is_dir_sep (s[2])))
    return true;
#endif

  return false;
}

static std::string
prepend_home_dir (const std::string& hd, const std::string& s)
{
  std::string retval = s;

  if (! absolute_pathname (retval))
    retval = hd + dir_sep_char + s;

  if (dir_sep_char != '/')
    std::replace (retval.begin (), retval.end (), '/', dir_sep_char);

  return retval;
}

// prepend_octave_home is used in mkoctfile.in.cc and
// octave-config.in.cc but not in main.in.cc.  Tagging it as unused
// avoids warnings from GCC about an unused function but should not
// cause trouble in the event that it actually is used.

OCTAVE_UNUSED
static std::string
prepend_octave_home (const std::string& s)
{
  return prepend_home_dir (Voctave_home, s);
}

static std::string
prepend_octave_exec_home (const std::string& s)
{
  return prepend_home_dir (Voctave_exec_home, s);
}

#endif
