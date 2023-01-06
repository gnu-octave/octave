////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#include <cstdlib>

#include <iostream>
#include <string>

#if defined (OCTAVE_USE_WINDOWS_API) && defined (_UNICODE)
#  include <vector>
#  include <locale>
#  include <codecvt>
#  include <windows.h>
#  include <versionhelpers.h>
#endif

#include "liboctave-build-info.h"

#include "liboctinterp-build-info.h"

#include "liboctgui-build-info.h"

#include "oct-env.h"

#include "octave.h"
#include "octave-build-info.h"
#include "qt-application.h"
#include "sysdep.h"

static void
check_hg_versions (void)
{
  bool ok = true;

  // Each library and executable has its own definition of the hg
  // id.  They should always match but may be different because of a
  // botched installation or incorrect LD_LIBRARY_PATH or some other
  // unusual problem.

  std::string octave_id = octave_hg_id ();
  std::string liboctave_id = liboctave_hg_id ();
  std::string liboctinterp_id = liboctinterp_hg_id ();
  std::string liboctgui_id = liboctgui_hg_id ();

  if (octave_id != liboctave_id)
    {
      std::cerr << "octave hg id ("
                << octave_id
                << ") does not match liboctave hg id ("
                << liboctave_id
                << ')' << std::endl;
      ok = false;
    }

  if (octave_id != liboctinterp_id)
    {
      std::cerr << "octave hg id ("
                << octave_id
                << ") does not match liboctinterp hg id ("
                << liboctinterp_id
                << ')' << std::endl;
      ok = false;
    }

  if (octave_id != liboctgui_id)
    {
      std::cerr << "octave hg id ("
                << octave_id
                << ") does not match liboctgui hg id ("
                << liboctgui_id
                << ')' << std::endl;
      ok = false;
    }

  if (! ok)
    exit (1);
}

#if defined (OCTAVE_USE_WINDOWS_API) && defined (_UNICODE)
extern "C"
int
wmain (int argc, wchar_t **wargv)
{
  static char **argv = new char * [argc + 1];
  std::vector<std::string> argv_str;

  // convert wide character strings to multibyte UTF-8 strings
  std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> wchar_conv;
  for (int i_arg = 0; i_arg < argc; i_arg++)
    argv_str.push_back (wchar_conv.to_bytes (wargv[i_arg]));

  // Get pointers to C strings not before vector is stable.
  for (int i_arg = 0; i_arg < argc; i_arg++)
    argv[i_arg] = &argv_str[i_arg][0];
  argv[argc] = nullptr;

  unsigned int old_console_codepage = 0;
  unsigned int old_console_output_codepage = 0;

  if (IsWindows7OrGreater ())
    {
      // save old console input and output codepages
      old_console_codepage = GetConsoleCP ();
      old_console_output_codepage = GetConsoleOutputCP ();

      // set console input and output codepages to UTF-8
      SetConsoleCP (65001);
      SetConsoleOutputCP (65001);
    }

#else
int
main (int argc, char **argv)
{
#endif
  check_hg_versions ();

  octave::sys::env::set_program_name (argv[0]);

  octave::qt_application app (argc, argv);

  int ret = app.execute ();

#if defined (OCTAVE_USE_WINDOWS_API) && defined (_UNICODE)
  if (IsWindows7OrGreater ())
    {
      // restore previous console input and output codepages
      if (old_console_codepage)
        SetConsoleCP (old_console_codepage);
      if (old_console_output_codepage)
        SetConsoleOutputCP (old_console_output_codepage);
    }
#endif

  return ret;
}
