// %NO_EDIT_WARNING%

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>
#include <map>
#include <iostream>
#include <algorithm>
#include <cstdlib>

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <vector>
#  include <locale>
#  include <codecvt>
#endif

#if ! defined (OCTAVE_PREFIX)
#  define OCTAVE_PREFIX %OCTAVE_PREFIX%
#endif

#if ! defined (OCTAVE_EXEC_PREFIX)
#  define OCTAVE_EXEC_PREFIX %OCTAVE_EXEC_PREFIX%
#endif

#ifndef OCTAVE_UNUSED
#  define OCTAVE_UNUSED
#endif

#include "shared-fcns.h"

static std::map<std::string, std::string> vars;

static std::string usage_msg = "usage: octave-config [options]";

static std::string help_msg =
  "\n"
  "Options:\n"
  "\n"
  "  -h, -?, --help        Print this message.\n"
  "\n"
  "  --m-site-dir          Print the name of the directory where Octave\n"
  "                        expects to find locally installed .m files.\n"
  "\n"
  "  --oct-site-dir        Print the name of the directory where Octave\n"
  "                        expects to find locally installed .oct files.\n"
  "\n"
  "  -p VAR, --print VAR   Print the value of the given configuration\n"
  "                        variable VAR.  Recognized variables are:\n"
  "\n"
  "                          API_VERSION            LOCALFCNFILEDIR\n"
  "                          ARCHLIBDIR             LOCALOCTFILEDIR\n"
  "                          BINDIR                 LOCALSTARTUPFILEDIR\n"
  "                          CANONICAL_HOST_TYPE    LOCALVERARCHLIBDIR\n"
  "                          DATADIR                LOCALVERFCNFILEDIR\n"
  "                          DATAROOTDIR            LOCALVEROCTFILEDIR\n"
  "                          DEFAULT_PAGER          MAN1DIR\n"
  "                          EXEC_PREFIX            MAN1EXT\n"
  "                          EXEEXT                 MANDIR\n"
  "                          FCNFILEDIR             OCTAVE_EXEC_HOME\n"
  "                          IMAGEDIR               OCTAVE_HOME\n"
  "                          INCLUDEDIR             OCTDATADIR\n"
  "                          INFODIR                OCTDOCDIR\n"
  "                          INFOFILE               OCTFILEDIR\n"
  "                          LIBDIR                 OCTFONTSDIR\n"
  "                          LIBEXECDIR             OCTINCLUDEDIR\n"
  "                          LOCALAPIARCHLIBDIR     OCTLIBDIR\n"
  "                          LOCALAPIFCNFILEDIR     STARTUPFILEDIR\n"
  "                          LOCALAPIOCTFILEDIR     VERSION\n"
  "                          LOCALARCHLIBDIR\n"
  "\n"
  "  -v, --version         Print the Octave version number.\n"
  "\n";

static void
initialize (void)
{
  set_octave_home ();

  vars["OCTAVE_HOME"] = Voctave_home;
  vars["OCTAVE_EXEC_HOME"] = Voctave_exec_home;

  vars["API_VERSION"] = %OCTAVE_API_VERSION%;
  vars["CANONICAL_HOST_TYPE"] = %OCTAVE_CANONICAL_HOST_TYPE%;
  vars["DEFAULT_PAGER"] = %OCTAVE_DEFAULT_PAGER%;
  vars["EXEEXT"] = %OCTAVE_EXEEXT%;
  vars["MAN1EXT"] = %OCTAVE_MAN1EXT%;
  vars["VERSION"] = %OCTAVE_VERSION%;

  vars["ARCHLIBDIR"] = prepend_octave_exec_home (%OCTAVE_ARCHLIBDIR%);
  vars["BINDIR"] = prepend_octave_exec_home (%OCTAVE_BINDIR%);
  vars["DATADIR"] = prepend_octave_home (%OCTAVE_DATADIR%);
  vars["DATAROOTDIR"] = prepend_octave_home (%OCTAVE_DATAROOTDIR%);
  vars["FCNFILEDIR"] = prepend_octave_home (%OCTAVE_FCNFILEDIR%);
  vars["IMAGEDIR"] = prepend_octave_home (%OCTAVE_IMAGEDIR%);
  vars["INCLUDEDIR"] = prepend_octave_home (%OCTAVE_INCLUDEDIR%);
  vars["INFODIR"] = prepend_octave_home (%OCTAVE_INFODIR%);
  vars["INFOFILE"] = prepend_octave_home (%OCTAVE_INFOFILE%);
  vars["LIBDIR"] = prepend_octave_exec_home (%OCTAVE_LIBDIR%);
  vars["LIBEXECDIR"] = prepend_octave_exec_home (%OCTAVE_LIBEXECDIR%);
  vars["LOCALAPIARCHLIBDIR"] = prepend_octave_exec_home (%OCTAVE_LOCALAPIARCHLIBDIR%);
  vars["LOCALAPIFCNFILEDIR"] = prepend_octave_home (%OCTAVE_LOCALAPIFCNFILEDIR%);
  vars["LOCALAPIOCTFILEDIR"] = prepend_octave_exec_home (%OCTAVE_LOCALAPIOCTFILEDIR%);
  vars["LOCALARCHLIBDIR"] = prepend_octave_exec_home (%OCTAVE_LOCALARCHLIBDIR%);
  vars["LOCALFCNFILEDIR"] = prepend_octave_home (%OCTAVE_LOCALFCNFILEDIR%);
  vars["LOCALOCTFILEDIR"] = prepend_octave_exec_home (%OCTAVE_LOCALOCTFILEDIR%);
  vars["LOCALSTARTUPFILEDIR"] = prepend_octave_home (%OCTAVE_LOCALSTARTUPFILEDIR%);
  vars["LOCALVERARCHLIBDIR"] = prepend_octave_exec_home (%OCTAVE_LOCALVERARCHLIBDIR%);
  vars["LOCALVERFCNFILEDIR"] = prepend_octave_home (%OCTAVE_LOCALVERFCNFILEDIR%);
  vars["LOCALVEROCTFILEDIR"] = prepend_octave_exec_home (%OCTAVE_LOCALVEROCTFILEDIR%);
  vars["MAN1DIR"] = prepend_octave_home (%OCTAVE_MAN1DIR%);
  vars["MANDIR"] = prepend_octave_home (%OCTAVE_MANDIR%);
  vars["OCTDATADIR"] = prepend_octave_home (%OCTAVE_OCTDATADIR%);
  vars["OCTDOCDIR"] = prepend_octave_home (%OCTAVE_OCTDOCDIR%);
  vars["OCTFILEDIR"] = prepend_octave_exec_home (%OCTAVE_OCTFILEDIR%);
  vars["OCTFONTSDIR"] = prepend_octave_home (%OCTAVE_OCTFONTSDIR%);
  vars["OCTINCLUDEDIR"] = prepend_octave_home (%OCTAVE_OCTINCLUDEDIR%);
  vars["OCTLIBDIR"] = prepend_octave_exec_home (%OCTAVE_OCTLIBDIR%);
  vars["STARTUPFILEDIR"] = prepend_octave_home (%OCTAVE_STARTUPFILEDIR%);
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

#else
int
main (int argc, char **argv)
{
#endif
  initialize ();

  if (argc == 1)
    {
      std::cout << usage_msg << std::endl;
      return 1;
    }

  for (int i = 1; i < argc; i++)
    {
      std::string arg (argv[i]);

      if (arg == "-h" || arg == "-?" || arg == "--help")
        {
          std::cout << usage_msg << std::endl;
          std::cout << help_msg;
          return 0;
        }
      else if (arg == "--m-site-dir")
        std::cout << vars["LOCALVERFCNFILEDIR"] << std::endl;
      else if (arg == "--oct-site-dir")
        std::cout << vars["LOCALVEROCTFILEDIR"] << std::endl;
      else if (arg == "-v" || arg == "--version")
        std::cout << vars["VERSION"] << std::endl;
      else if (arg == "-p" || arg == "--print")
        {
          if (i < argc-1)
            {
              arg = argv[++i];
              std::cout << vars[arg] << std::endl;
            }
          else
            {
              std::cerr << "octave-config: " << arg
                        << " options requires argument" << std::endl;
              return 1;
            }
        }
      else
        {
          std::cerr << "octave-config: unrecognized argument " << arg
                    << std::endl;
          return 1;
        }
    }

  return 0;
}
