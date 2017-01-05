/*

Copyright (C) 2008-2016 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_shared_fcns_h)
#define octave_shared_fcns_h 1

#if defined (OCTAVE_USE_WINDOWS_API)

#include <windows.h>
#include <tlhelp32.h>

#if defined (_MSC_VER)
#  define popen _popen
#  define pclose _pclose
#endif

static std::string
w32_get_octave_home (void)
{
  std::string retval;

  std::string bin_dir;

  char namebuf[MAX_PATH+1];
  if (GetModuleFileName (GetModuleHandle (NULL), namebuf, MAX_PATH))
    {
      namebuf[MAX_PATH] = '\0';

      std::string exe_name = namebuf; 
      size_t pos = exe_name.rfind ("\\");

      if (pos != std::string::npos)
        bin_dir = exe_name.substr (0, pos + 1);
    }

  if (! bin_dir.empty ())
    {
      size_t pos = bin_dir.rfind ("\\bin\\");

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

static std::string
octave_getenv (const std::string& name)
{
  char *value = ::getenv (name.c_str ());

  return value ? value : "";
}

static std::string
get_octave_home (void)
{
  std::string oh = octave_getenv ("OCTAVE_HOME");

#if defined (OCTAVE_USE_WINDOWS_API)
  if (oh.empty ())
    oh = w32_get_octave_home ();
#endif

  return oh.empty () ? std::string (OCTAVE_PREFIX) : oh;
}

static std::string
subst_octave_home (const std::string& s)
{
  std::string retval;

  std::string octave_home = get_octave_home ();

  std::string prefix = OCTAVE_PREFIX;

  retval = s;

  if (octave_home != prefix)
    {
      size_t len = prefix.length ();

      if (s.substr (0, len) == prefix)
        retval.replace (0, len, octave_home);
    }

  if (dir_sep_char != '/')
    std::replace (retval.begin (), retval.end (), '/', dir_sep_char);

  return retval;
}

#endif

