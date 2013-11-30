/*

Copyright (C) 2008-2013 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#include <config.h>
#endif

#include <string>
#include <map>
#include <iostream>
#include <algorithm>
#include <cstdlib>

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)
#include <windows.h>
#endif

static bool initialized = false;
static std::map<std::string,std::string> vars;
static std::string OCTAVE_HOME, PREFIX;
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
"                          API_VERSION            LOCALAPIARCHLIBDIR\n"
"                          CANONICAL_HOST_TYPE    LOCALAPIFCNFILEDIR\n"
"                          DEFAULT_PAGER          LOCALAPIOCTFILEDIR\n"
"                          ARCHLIBDIR             LOCALARCHLIBDIR\n"
"                          BINDIR                 LOCALFCNFILEDIR\n"
"                          DATADIR                LOCALOCTFILEDIR\n"
"                          DATAROOTDIR            LOCALSTARTUPFILEDIR\n"
"                          EXEC_PREFIX            LOCALVERARCHLIBDIR\n"
"                          FCNFILEDIR             LOCALVERFCNFILEDIR\n"
"                          LOCALVEROCTFILEDIR     MAN1DIR\n"
"                          IMAGEDIR               MANDIR\n"
"                          MAN1EXT                OCTFILEDIR\n"
"                          INCLUDEDIR             OCTINCLUDEDIR\n"
"                          INFODIR                OCTLIBDIR\n"
"                          INFOFILE               PREFIX\n"
"                          LIBDIR                 STARTUPFILEDIR\n"
"                          LIBEXECDIR             VERSION\n"
"\n"
"  -v, --version         Print the Octave version number.\n"
"\n";

static std::string
substitute_prefix (const std::string& s, const std::string& prefix,
                   const std::string new_prefix)
{
  std::string retval = s;

  if (!prefix.empty () && new_prefix != prefix)
    {
      int len = prefix.length ();
      if (retval.find (prefix) == 0)
        retval.replace (0, len, new_prefix);
    }

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)
  std::replace (retval.begin (), retval.end (), '/', '\\');
#endif

  return retval;
}

static void
initialize (void)
{
  if (initialized)
    return;

  initialized = true;

  const char *homestr = getenv ("OCTAVE_HOME");
  OCTAVE_HOME = (homestr ? homestr : "");
  PREFIX = %OCTAVE_PREFIX%;

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)
  int n = 1024;

  std::string bin_dir (n, '\0');

  while (true)
    {
      int status = GetModuleFileName (0, &bin_dir[0], n);

      if (status < n)
        {
          bin_dir.resize (status);
          break;
        }
      else
        {
          n *= 2;
          bin_dir.resize (n);
        }
    }

  if (! bin_dir.empty ())
    {
      size_t pos = bin_dir.rfind ("\\bin\\");

      if (pos != std::string::npos)
        OCTAVE_HOME = bin_dir.substr (0, pos);
    }
#endif

  vars["API_VERSION"] = %OCTAVE_API_VERSION%;
  vars["CANONICAL_HOST_TYPE"] = %OCTAVE_CANONICAL_HOST_TYPE%;
  vars["DEFAULT_PAGER"] = %OCTAVE_DEFAULT_PAGER%;
  vars["ARCHLIBDIR"] = substitute_prefix (%OCTAVE_ARCHLIBDIR%,
                                          PREFIX, OCTAVE_HOME);
  vars["BINDIR"] = substitute_prefix (%OCTAVE_BINDIR%, PREFIX, OCTAVE_HOME);
  vars["DATADIR"] = substitute_prefix (%OCTAVE_DATADIR%, PREFIX, OCTAVE_HOME);
  vars["DATAROOTDIR"] = substitute_prefix (%OCTAVE_DATAROOTDIR%,
                                           PREFIX, OCTAVE_HOME);
  vars["EXEC_PREFIX"] = substitute_prefix (%OCTAVE_EXEC_PREFIX%,
                                           PREFIX, OCTAVE_HOME);
  vars["FCNFILEDIR"] = substitute_prefix (%OCTAVE_FCNFILEDIR%,
                                          PREFIX, OCTAVE_HOME);
  vars["IMAGEDIR"] = substitute_prefix (%OCTAVE_IMAGEDIR%, PREFIX, OCTAVE_HOME);
  vars["INCLUDEDIR"] = substitute_prefix (%OCTAVE_INCLUDEDIR%,
                                          PREFIX, OCTAVE_HOME);
  vars["INFODIR"] = substitute_prefix (%OCTAVE_INFODIR%, PREFIX, OCTAVE_HOME);
  vars["INFOFILE"] = substitute_prefix (%OCTAVE_INFOFILE%, PREFIX, OCTAVE_HOME);
  vars["LIBDIR"] = substitute_prefix (%OCTAVE_LIBDIR%, PREFIX, OCTAVE_HOME);
  vars["LIBEXECDIR"] = substitute_prefix (%OCTAVE_LIBEXECDIR%,
                                          PREFIX, OCTAVE_HOME);
  vars["LOCALAPIARCHLIBDIR"] = substitute_prefix (%OCTAVE_LOCALAPIARCHLIBDIR%,
                                                  PREFIX, OCTAVE_HOME);
  vars["LOCALAPIFCNFILEDIR"] = substitute_prefix (%OCTAVE_LOCALAPIFCNFILEDIR%,
                                                  PREFIX, OCTAVE_HOME);
  vars["LOCALAPIOCTFILEDIR"] = substitute_prefix (%OCTAVE_LOCALAPIOCTFILEDIR%,
                                                  PREFIX, OCTAVE_HOME);
  vars["LOCALARCHLIBDIR"] = substitute_prefix (%OCTAVE_LOCALARCHLIBDIR%,
                                               PREFIX, OCTAVE_HOME);
  vars["LOCALFCNFILEDIR"] = substitute_prefix (%OCTAVE_LOCALFCNFILEDIR%,
                                               PREFIX, OCTAVE_HOME);
  vars["LOCALOCTFILEDIR"] = substitute_prefix (%OCTAVE_LOCALOCTFILEDIR%,
                                               PREFIX, OCTAVE_HOME);
  vars["LOCALSTARTUPFILEDIR"] = substitute_prefix (%OCTAVE_LOCALSTARTUPFILEDIR%,
                                                   PREFIX, OCTAVE_HOME);
  vars["LOCALVERARCHLIBDIR"] = substitute_prefix (%OCTAVE_LOCALVERARCHLIBDIR%,
                                                  PREFIX, OCTAVE_HOME);
  vars["LOCALVERFCNFILEDIR"] = substitute_prefix (%OCTAVE_LOCALVERFCNFILEDIR%,
                                                  PREFIX, OCTAVE_HOME);
  vars["LOCALVEROCTFILEDIR"] = substitute_prefix (%OCTAVE_LOCALVEROCTFILEDIR%,
                                                  PREFIX, OCTAVE_HOME);
  vars["MAN1DIR"] = substitute_prefix (%OCTAVE_MAN1DIR%, PREFIX, OCTAVE_HOME);
  vars["MAN1EXT"] = %OCTAVE_MAN1EXT%;
  vars["MANDIR"] = substitute_prefix (%OCTAVE_MANDIR%, PREFIX, OCTAVE_HOME);
  vars["OCTFILEDIR"] = substitute_prefix (%OCTAVE_OCTFILEDIR%,
                                          PREFIX, OCTAVE_HOME);
  vars["OCTINCLUDEDIR"] = substitute_prefix (%OCTAVE_OCTINCLUDEDIR%,
                                             PREFIX, OCTAVE_HOME);
  vars["OCTLIBDIR"] = substitute_prefix (%OCTAVE_OCTLIBDIR%,
                                         PREFIX, OCTAVE_HOME);
  vars["PREFIX"] = (OCTAVE_HOME.empty () ? PREFIX : OCTAVE_HOME);
  vars["STARTUPFILEDIR"] = substitute_prefix (%OCTAVE_STARTUPFILEDIR%,
                                              PREFIX, OCTAVE_HOME);
  vars["VERSION"] = %OCTAVE_VERSION%;
}

int
main (int argc, char **argv)
{
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
