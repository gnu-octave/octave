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
#include <cstring>
#include <map>
#include <list>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cstdlib>

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <locale>
#  include <codecvt>
#endif

// Programming note:  The CROSS macro here refers to building a
// cross-compiler aware version of mkoctfile that can be used to cross
// compile .oct file for Windows builds of Octave, not that mkoctfile
// itself is being cross compiled.
//
// We don't use the wrapper and gnulib functions when we are building
// with CROSS defined.  This configuration is only expected to work on
// modern systems that should not need to have gnulib to fix POSIX
// portability problems.  So we just assume a working POSIX system when
// CROSS is defined.

#if defined (CROSS)
#  include <stdlib.h>
#  include <sys/types.h>
#  include <sys/wait.h>
#  include <unistd.h>
#  ifndef OCTAVE_UNUSED
#    define OCTAVE_UNUSED
#  endif
#else
// We are linking against static libs so do not decorate with dllimport.
// FIXME: This should be done by the build system.
#  undef OCTAVE_API
#  define OCTAVE_API
#  include "mkostemps-wrapper.h"
#  include "uniconv-wrappers.h"
#  include "unistd-wrappers.h"
#  include "wait-wrappers.h"
#endif

#if ! defined (OCTAVE_VERSION)
#  define OCTAVE_VERSION %OCTAVE_CONF_VERSION%
#endif

#if ! defined (OCTAVE_PREFIX)
#  define OCTAVE_PREFIX %OCTAVE_CONF_PREFIX%
#endif

#if ! defined (OCTAVE_EXEC_PREFIX)
#  define OCTAVE_EXEC_PREFIX %OCTAVE_CONF_EXEC_PREFIX%
#endif

#include "shared-fcns.h"

#if defined (CROSS)

static int
octave_mkostemps_wrapper (char *tmpl, int suffixlen)
{
  return mkostemps (tmpl, suffixlen, 0);
}

static int
octave_unlink_wrapper (const char *nm)
{
  return unlink (nm);
}

static bool
octave_wifexited_wrapper (int status)
{
  return WIFEXITED (status);
}

static int
octave_wexitstatus_wrapper (int status)
{
  return WEXITSTATUS (status);
}

#endif

static std::string
get_line (FILE *fp)
{
  std::ostringstream buf;

  while (true)
    {
      int c = std::fgetc (fp);

      if (c == '\n' || c == EOF)
        break;

      buf << static_cast<char> (c);
    }

  return buf.str ();
}

static std::string
get_variable (const char *name, const std::string& defval)
{
  const char *val = getenv (name);

  if (val && *val)
    return std::string (val);
  else
    return defval;
}

static std::string
quote_path (const std::string& s)
{
  if (s.find (' ') != std::string::npos && s[0] != '"')
    return '"' + s + '"';
  else
    return s;
}

static std::string
replace_prefix (std::string s)
{
#if defined (OCTAVE_REPLACE_PREFIX)
  const std::string match = "${prefix}";
  const std::string repl = Voctave_exec_home;
  std::size_t pos = s.find (match);
  while (pos != std::string::npos )
    {
      s.replace (pos, match.length (), repl);
      pos = s.find (match);
    }
#endif

  return s;
}

static std::map<std::string, std::string>
make_vars_map (bool link_stand_alone, bool verbose, bool debug)
{
  set_octave_home ();

  std::map<std::string, std::string> vars;

  vars["OCTAVE_HOME"] = Voctave_home;
  vars["OCTAVE_EXEC_HOME"] = Voctave_exec_home;

  vars["API_VERSION"] = %OCTAVE_API_VERSION%;
  vars["CANONICAL_HOST_TYPE"] = %OCTAVE_CANONICAL_HOST_TYPE%;
  vars["DEFAULT_PAGER"] = %OCTAVE_DEFAULT_PAGER%;
  vars["EXEEXT"] = %OCTAVE_EXEEXT%;
  vars["MAN1EXT"] = %OCTAVE_MAN1EXT%;
  vars["OCTAVE_VERSION"] = %OCTAVE_VERSION%;

  vars["ARCHLIBDIR"] = prepend_octave_exec_home (%OCTAVE_ARCHLIBDIR%);
  vars["BINDIR"] = prepend_octave_exec_home (%OCTAVE_BINDIR%);
  vars["DATADIR"] = prepend_octave_home (%OCTAVE_DATADIR%);
  vars["DATAROOTDIR"] = prepend_octave_home (%OCTAVE_DATAROOTDIR%);
  vars["FCNFILEDIR"] = prepend_octave_home (%OCTAVE_FCNFILEDIR%);
  vars["IMAGEDIR"] = prepend_octave_home (%OCTAVE_IMAGEDIR%);
  vars["INFODIR"] = prepend_octave_home (%OCTAVE_INFODIR%);
  vars["INFOFILE"] = prepend_octave_home (%OCTAVE_INFOFILE%);
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
  vars["STARTUPFILEDIR"] = prepend_octave_home (%OCTAVE_STARTUPFILEDIR%);

  vars["OCTINCLUDEDIR"]
    = get_variable ("OCTINCLUDEDIR",
                    prepend_octave_home (%OCTAVE_CONF_OCTINCLUDEDIR%));

  vars["INCLUDEDIR"]
    = get_variable ("INCLUDEDIR",
                    prepend_octave_home (%OCTAVE_CONF_INCLUDEDIR%));

  vars["LIBDIR"]
    = get_variable ("LIBDIR", prepend_octave_exec_home (%OCTAVE_CONF_LIBDIR%));

  vars["OCTLIBDIR"]
    = get_variable ("OCTLIBDIR",
                    prepend_octave_exec_home (%OCTAVE_CONF_OCTLIBDIR%));

  std::string DEFAULT_INCFLAGS;

#if defined (OCTAVE_USE_WINDOWS_API)
  DEFAULT_INCFLAGS = "-I" + quote_path (vars["OCTINCLUDEDIR"] + R"(\..)")
                     + " -I" + quote_path (vars["OCTINCLUDEDIR"]);
#else
  DEFAULT_INCFLAGS = "-I" + quote_path (vars["OCTINCLUDEDIR"] + "/..")
                     + " -I" + quote_path (vars["OCTINCLUDEDIR"]);
#endif

  if (vars["INCLUDEDIR"] != "/usr/include")
    DEFAULT_INCFLAGS += " -I" + quote_path (vars["INCLUDEDIR"]);

  std::string DEFAULT_LDFLAGS;

#if (defined (OCTAVE_USE_WINDOWS_API) || defined (CROSS) || defined (OCTAVE_LINK_ALL_DEPS))
  // We'll be linking the files we compile with -loctinterp and -loctave,
  // so we need to know where to find them.
  DEFAULT_LDFLAGS += "-L" + quote_path (vars["OCTLIBDIR"]);
#endif

  if (vars["LIBDIR"] != "/usr/lib")
    DEFAULT_LDFLAGS += " -L" + quote_path (vars["LIBDIR"]);

  vars["CPPFLAGS"] = get_variable ("CPPFLAGS",
                                   replace_prefix (%OCTAVE_CONF_CPPFLAGS%));

  vars["INCFLAGS"] = get_variable ("INCFLAGS", DEFAULT_INCFLAGS);

  vars["F77"] = get_variable ("F77", %OCTAVE_CONF_MKOCTFILE_F77%);

  vars["FFLAGS"] = get_variable ("FFLAGS", %OCTAVE_CONF_FFLAGS%);

  vars["FPICFLAG"] = get_variable ("FPICFLAG", %OCTAVE_CONF_FPICFLAG%);

  vars["CC"] = get_variable ("CC", %OCTAVE_CONF_MKOCTFILE_CC%);
  if (verbose && vars["CC"] == "cc-msvc")
    vars["CC"] += " -d";

  vars["CFLAGS"] = get_variable ("CFLAGS", %OCTAVE_CONF_CFLAGS%);

  vars["CPICFLAG"] = get_variable ("CPICFLAG", %OCTAVE_CONF_CPICFLAG%);

  vars["CXX"] = get_variable ("CXX", %OCTAVE_CONF_MKOCTFILE_CXX%);
  if (verbose && vars["CXX"] == "cc-msvc")
    vars["CXX"] += " -d";

  vars["CXXFLAGS"] = get_variable ("CXXFLAGS", %OCTAVE_CONF_CXXFLAGS%);

  vars["CXXLD"] = get_variable ("CXXLD", vars["CXX"]);
  if (verbose && vars["CXXLD"] == "cc-msvc")
    vars["CXXLD"] += " -d";

  vars["CXXPICFLAG"] = get_variable ("CXXPICFLAG", %OCTAVE_CONF_CXXPICFLAG%);

  vars["XTRA_CFLAGS"] = get_variable ("XTRA_CFLAGS", %OCTAVE_CONF_XTRA_CFLAGS%);

  vars["XTRA_CXXFLAGS"] = get_variable ("XTRA_CXXFLAGS",
                                        %OCTAVE_CONF_XTRA_CXXFLAGS%);

  vars["AR"] = get_variable ("AR", %OCTAVE_CONF_MKOCTFILE_AR%);

  vars["RANLIB"] = get_variable ("RANLIB", %OCTAVE_CONF_MKOCTFILE_RANLIB%);

  vars["DEPEND_FLAGS"] = get_variable ("DEPEND_FLAGS",
                                       %OCTAVE_CONF_DEPEND_FLAGS%);

  vars["DEPEND_EXTRA_SED_PATTERN"]
    = get_variable ("DEPEND_EXTRA_SED_PATTERN",
                    %OCTAVE_CONF_DEPEND_EXTRA_SED_PATTERN%);

  vars["DL_LDFLAGS"] = get_variable ("DL_LDFLAGS",
                                     %OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS%);

  if (! link_stand_alone)
    DEFAULT_LDFLAGS += ' ' + vars["DL_LDFLAGS"];

  vars["RDYNAMIC_FLAG"] = get_variable ("RDYNAMIC_FLAG",
                                        %OCTAVE_CONF_RDYNAMIC_FLAG%);

  vars["LIBOCTAVE"] = "-loctave";

  vars["LIBOCTINTERP"] = "-loctinterp";

  vars["READLINE_LIBS"] = %OCTAVE_CONF_READLINE_LIBS%;

  vars["LAPACK_LIBS"] = get_variable ("LAPACK_LIBS", %OCTAVE_CONF_LAPACK_LIBS%);

  vars["BLAS_LIBS"] = get_variable ("BLAS_LIBS", %OCTAVE_CONF_BLAS_LIBS%);

  vars["FFTW3_LDFLAGS"]
    = get_variable ("FFTW3_LDFLAGS",
                    replace_prefix (%OCTAVE_CONF_FFTW3_LDFLAGS%));

  vars["FFTW3_LIBS"] = get_variable ("FFTW3_LIBS", %OCTAVE_CONF_FFTW3_LIBS%);

  vars["FFTW3F_LDFLAGS"]
    = get_variable ("FFTW3F_LDFLAGS",
                    replace_prefix (%OCTAVE_CONF_FFTW3F_LDFLAGS%));

  vars["FFTW3F_LIBS"] = get_variable ("FFTW3F_LIBS", %OCTAVE_CONF_FFTW3F_LIBS%);

  vars["LIBS"] = get_variable ("LIBS", %OCTAVE_CONF_LIBS%);

  vars["FLIBS"] = get_variable ("FLIBS",
                                replace_prefix (%OCTAVE_CONF_FLIBS%));

  vars["OCTAVE_LINK_DEPS"] = get_variable ("OCTAVE_LINK_DEPS",
                                           %OCTAVE_CONF_MKOCTFILE_OCTAVE_LINK_DEPS%);

  vars["OCTAVE_LINK_OPTS"] = get_variable ("OCTAVE_LINK_OPTS",
                                           %OCTAVE_CONF_OCTAVE_LINK_OPTS%);

  vars["OCT_LINK_DEPS"] = get_variable ("OCT_LINK_DEPS",
                                        %OCTAVE_CONF_MKOCTFILE_OCT_LINK_DEPS%);

  vars["OCT_LINK_OPTS"]
    = get_variable ("OCT_LINK_OPTS",
                    replace_prefix (%OCTAVE_CONF_OCT_LINK_OPTS%));

  vars["LDFLAGS"] = get_variable ("LDFLAGS", DEFAULT_LDFLAGS);

  vars["LD_STATIC_FLAG"] = get_variable ("LD_STATIC_FLAG",
                                         %OCTAVE_CONF_LD_STATIC_FLAG%);

  // FIXME: Remove LFLAGS in Octave 9
  vars["LFLAGS"] = get_variable ("LFLAGS", DEFAULT_LDFLAGS);
  if (vars["LFLAGS"] != DEFAULT_LDFLAGS)
    std::cerr << "mkoctfile: warning: LFLAGS is deprecated and will be removed in a future version of Octave, use LDFLAGS instead" << std::endl;

  vars["F77_INTEGER8_FLAG"] = get_variable ("F77_INTEGER8_FLAG",
                                            %OCTAVE_CONF_F77_INTEGER_8_FLAG%);
  vars["ALL_FFLAGS"] = vars["FFLAGS"] + ' ' + vars["F77_INTEGER8_FLAG"];
  if (debug)
    vars["ALL_FFLAGS"] += " -g";

  vars["ALL_CFLAGS"]
    = vars["INCFLAGS"] + ' ' + vars["XTRA_CFLAGS"] + ' ' + vars["CFLAGS"];
  if (debug)
    vars["ALL_CFLAGS"] += " -g";

  vars["ALL_CXXFLAGS"]
    = vars["INCFLAGS"] + ' ' + vars["XTRA_CXXFLAGS"] + ' ' + vars["CXXFLAGS"];
  if (debug)
    vars["ALL_CXXFLAGS"] += " -g";

  vars["ALL_LDFLAGS"]
    = vars["LD_STATIC_FLAG"] + ' ' + vars["CPICFLAG"] + ' ' + vars["LDFLAGS"];

  vars["OCTAVE_LIBS"]
    = (vars["LIBOCTINTERP"] + ' ' + vars["LIBOCTAVE"] + ' '
       + vars["SPECIAL_MATH_LIB"]);

  vars["FFTW_LIBS"] = vars["FFTW3_LDFLAGS"] + ' ' + vars["FFTW3_LIBS"] + ' '
                      + vars["FFTW3F_LDFLAGS"] + ' ' + vars["FFTW3F_LIBS"];

  return vars;
}

static std::string usage_msg = "usage: mkoctfile [options] file ...";

static std::string version_msg = "mkoctfile, version " OCTAVE_VERSION;

static std::string help_msg =
  "\n"
  "Options:\n"
  "\n"
  "  -h, -?, --help          Print this message.\n"
  "\n"
  "  -IDIR                   Add -IDIR to compile commands.\n"
  "\n"
  "  -idirafter DIR          Add -idirafter DIR to compile commands.\n"
  "\n"
  "  -DDEF                   Add -DDEF to compile commands.\n"
  "\n"
  "  -lLIB                   Add library LIB to link command.\n"
  "\n"
  "  -LDIR                   Add -LDIR to link command.\n"
  "\n"
  "  -M, --depend            Generate dependency files (.d) for C and C++\n"
  "                          source files.\n"
#if ! defined (OCTAVE_USE_WINDOWS_API)
  "\n"
  "  -pthread                Add -pthread to link command.\n"
#endif
  "\n"
  "  -RDIR                   Add -RDIR to link command.\n"
  "\n"
  "  -Wl,...                 Pass flags though the linker like -Wl,-rpath=...\n"
  "\n"
  "  -W...                   Pass flags though the compiler like -Wa,OPTION.\n"
  "\n"
  "  -c, --compile           Compile, but do not link.\n"
  "\n"
  "  -o FILE, --output FILE  Output filename.  Default extension is .oct\n"
  "                          (or .mex if --mex is specified) unless linking\n"
  "                          a stand-alone executable.\n"
  "\n"
  "  -g                      Enable debugging options for compilers.\n"
  "\n"
  "  -p VAR, --print VAR     Print configuration variable VAR.  There are\n"
  "                          three categories of variables:\n"
  "\n"
  "                          Octave configuration variables that users may\n"
  "                          override with environment variables.  These are\n"
  "                          used in commands that mkoctfile executes.\n"
  "\n"
  "                            ALL_CFLAGS                  INCLUDEDIR\n"
  "                            ALL_CXXFLAGS                LAPACK_LIBS\n"
  "                            ALL_FFLAGS                  LDFLAGS\n"
  "                            ALL_LDFLAGS                 LD_STATIC_FLAG\n"
  "                            BLAS_LIBS                   LIBDIR\n"
  "                            CC                          LIBOCTAVE\n"
  "                            CFLAGS                      LIBOCTINTERP\n"
  "                            CPICFLAG                    OCTAVE_LINK_OPTS\n"
  "                            CPPFLAGS                    OCTINCLUDEDIR\n"
  "                            CXX                         OCTAVE_LIBS\n"
  "                            CXXFLAGS                    OCTAVE_LINK_DEPS\n"
  "                            CXXLD                       OCTLIBDIR\n"
  "                            CXXPICFLAG                  OCT_LINK_DEPS\n"
  "                            DL_LDFLAGS                  OCT_LINK_OPTS\n"
  "                            F77                         RDYNAMIC_FLAG\n"
  "                            F77_INTEGER8_FLAG           SPECIAL_MATH_LIB\n"
  "                            FFLAGS                      XTRA_CFLAGS\n"
  "                            FPICFLAG                    XTRA_CXXFLAGS\n"
  "                            INCFLAGS\n"
  "\n"
  "                          Octave configuration variables as above, but\n"
  "                          currently unused by mkoctfile.\n"
  "\n"
  "                            AR\n"
  "                            DEPEND_EXTRA_SED_PATTERN\n"
  "                            DEPEND_FLAGS\n"
  "                            FFTW3F_LDFLAGS\n"
  "                            FFTW3F_LIBS\n"
  "                            FFTW3_LDFLAGS\n"
  "                            FFTW3_LIBS\n"
  "                            FFTW_LIBS\n"
  "                            FLIBS\n"
  "                            LIBS\n"
  "                            RANLIB\n"
  "                            READLINE_LIBS\n"
  "\n"
  "                          Octave configuration variables that are provided\n"
  "                          for informational purposes only.  Except for\n"
  "                          OCTAVE_HOME and OCTAVE_EXEC_HOME, users may not\n"
  "                          override these variables.\n"
  "\n"
  "                          If OCTAVE_HOME or OCTAVE_EXEC_HOME are set in\n"
  "                          the environment, then other variables are adjusted\n"
  "                          accordingly with OCTAVE_HOME or OCTAVE_EXEC_HOME\n"
  "                          substituted for the original value of the directory\n"
  "                          specified by the --prefix or --exec-prefix options\n"
  "                          that were used when Octave was configured.\n"
  "\n"
  "                            API_VERSION                 LOCALFCNFILEDIR\n"
  "                            ARCHLIBDIR                  LOCALOCTFILEDIR\n"
  "                            BINDIR                      LOCALSTARTUPFILEDIR\n"
  "                            CANONICAL_HOST_TYPE         LOCALVERARCHLIBDIR\n"
  "                            DATADIR                     LOCALVERFCNFILEDIR\n"
  "                            DATAROOTDIR                 LOCALVEROCTFILEDIR\n"
  "                            DEFAULT_PAGER               MAN1DIR\n"
  "                            EXEC_PREFIX                 MAN1EXT\n"
  "                            EXEEXT                      MANDIR\n"
  "                            FCNFILEDIR                  OCTAVE_EXEC_HOME\n"
  "                            IMAGEDIR                    OCTAVE_HOME\n"
  "                            INFODIR                     OCTAVE_VERSION\n"
  "                            INFOFILE                    OCTDATADIR\n"
  "                            LIBEXECDIR                  OCTDOCDIR\n"
  "                            LOCALAPIARCHLIBDIR          OCTFILEDIR\n"
  "                            LOCALAPIFCNFILEDIR          OCTFONTSDIR\n"
  "                            LOCALAPIOCTFILEDIR          STARTUPFILEDIR\n"
  "                            LOCALARCHLIBDIR\n"
  "\n"
  "  --link-stand-alone      Link a stand-alone executable file.\n"
  "\n"
  "  --mex                   Assume we are creating a MEX file.  Set the\n"
  "                          default output extension to \".mex\".\n"
  "\n"
  "  -s, --strip             Strip output file.\n"
  "\n"
  "  -n, --just-print, --dry-run\n"
  "                          Print commands, but do not execute them.\n"
  "\n"
  "  -v, --verbose           Echo commands as they are executed.\n"
  "\n"
  "  --silent                Ignored.  Intended to suppress output from\n"
  "                          compiler steps.\n"
  "\n"
  "  FILE                    Compile or link FILE.  Recognized file types are:\n"
  "\n"
  "                            .c    C source\n"
  "                            .cc   C++ source\n"
  "                            .cp   C++ source\n"
  "                            .cpp  C++ source\n"
  "                            .CPP  C++ source\n"
  "                            .cxx  C++ source\n"
  "                            .c++  C++ source\n"
  "                            .C    C++ source\n"
  "                            .f    Fortran source (fixed form)\n"
  "                            .F    Fortran source (fixed form)\n"
  "                            .f90  Fortran source (free form)\n"
  "                            .F90  Fortran source (free form)\n"
  "                            .o    object file\n"
  "                            .a    library file\n"
#if defined (_MSC_VER)
  "                            .lib  library file\n"
#endif
  "\n";

static std::string
basename (const std::string& s, bool strip_path = false)
{
  std::string retval;

  std::size_t pos = s.rfind ('.');

  if (pos == std::string::npos)
    retval = s;
  else
    retval = s.substr (0, pos);

  if (strip_path)
    {
      std::size_t p1 = retval.rfind ('/'), p2 = retval.rfind ('\\');

      pos = (p1 != std::string::npos && p2 != std::string::npos
             ? std::max (p1, p2) : (p2 != std::string::npos ? p2 : p1));

      if (pos != std::string::npos)
        retval = retval.substr (++pos, std::string::npos);
    }

  return retval;
}

inline bool
starts_with (const std::string& s, const std::string& prefix)
{
  return (s.length () >= prefix.length () && s.find (prefix) == 0);
}

inline bool
ends_with (const std::string& s, const std::string& suffix)
{
  return (s.length () >= suffix.length ()
          && s.rfind (suffix) == s.length () - suffix.length ());
}

static int
run_command (const std::string& cmd, bool verbose, bool printonly = false)
{
  if (printonly)
    {
      std::cout << cmd << std::endl;
      return 0;
    }

  if (verbose)
    std::cout << cmd << std::endl;

  // FIXME: Call _wsystem on Windows or octave::sys::system.
  int result = system (cmd.c_str ());

  if (octave_wifexited_wrapper (result))
    result = octave_wexitstatus_wrapper (result);

  return result;
}

bool
is_true (const std::string& s)
{
  return (s == "yes" || s == "true");
}

static std::string
get_temp_directory (void)
{
  std::string tempd;

  tempd = octave_getenv ("TMPDIR");

#if defined (__MINGW32__) || defined (_MSC_VER)

  if (tempd.empty ())
    tempd = octave_getenv ("TEMP");

  if (tempd.empty ())
    tempd = octave_getenv ("TMP");

#if defined (P_tmpdir)
  if (tempd.empty ())
    tempd = P_tmpdir;
#endif

  // Some versions of MinGW and MSVC either don't define P_tmpdir, or
  // define it to a single backslash.  In such cases just use C:\temp.
  if (tempd.empty () || tempd == R"(\)")
    tempd = R"(c:\temp)";

#else

#if defined (P_tmpdir)
  if (tempd.empty ())
    tempd = P_tmpdir;
#else
  if (tempd.empty ())
    tempd = "/tmp";
#endif

#endif

  return tempd;
}

static std::string
create_interleaved_complex_file (void)
{
  std::string tmpl = get_temp_directory () + "/oct-XXXXXX.c";

  char *ctmpl = new char [tmpl.length () + 1];

  ctmpl = strcpy (ctmpl, tmpl.c_str ());

  // mkostemps will open the file and return a file descriptor.  We
  // won't worry about closing it because we will need the file until we
  // are done and then the file will be closed when mkoctfile exits.
  int fd = octave_mkostemps_wrapper (ctmpl, 2);

  // Make C++ string from filled-in template.
  std::string retval (ctmpl);
  delete [] ctmpl;

  // Write symbol definition to file.
  FILE *fid = fdopen (fd, "w");
  fputs ("const int __mx_has_interleaved_complex__ = 1;\n", fid);
  fclose (fid);

  return retval;
}

static std::string
tmp_objfile_name (void)
{
  std::string tmpl = get_temp_directory () + "/oct-XXXXXX.o";

  char *ctmpl = new char [tmpl.length () + 1];

  ctmpl = strcpy (ctmpl, tmpl.c_str ());

  // mkostemps will open the file and return a file descriptor.  We
  // won't worry about closing it because we will need the file until we
  // are done and then the file will be closed when mkoctfile exits.
  octave_mkostemps_wrapper (ctmpl, 2);

  std::string retval (ctmpl);  // make C++ string from filled-in template
  delete [] ctmpl;

  return retval;
}

static void
clean_up_tmp_files (const std::list<std::string>& tmp_files)
{
  for (const auto& file : tmp_files)
    octave_unlink_wrapper (file.c_str ());
}

#if defined (OCTAVE_USE_WINDOWS_API) && defined (_UNICODE)
extern "C"
int
wmain (int argc, wchar_t **sys_argv)
{
  std::vector<std::string> argv;

  // Convert wide character strings to multibyte UTF-8 strings and save
  // them in a vector of std::string objects for later processing.

  std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> wchar_conv;
  for (int i_arg = 0; i_arg < argc; i_arg++)
    argv.push_back (wchar_conv.to_bytes (sys_argv[i_arg]));
#else
int
main (int argc, char **sys_argv)
{
  std::vector<std::string> argv;

  // Save args as vector of std::string objects for later processing.
  for (int i_arg = 0; i_arg < argc; i_arg++)
    argv.push_back (sys_argv[i_arg]);
#endif

  if (argc == 1)
    {
      std::cout << usage_msg << std::endl;
      return 1;
    }

  if (argc == 2 && (argv[1] == "-v" || argv[1] == "-version"
                    || argv[1] == "--version"))
    {
      std::cout << version_msg << std::endl;
      return 0;
    }

  std::list<std::string> cfiles, ccfiles, f77files, tmp_objfiles;
  std::string output_ext = ".oct";
  std::string objfiles, libfiles, octfile, outputfile;
  std::string incflags, defs, ldflags, pass_on_options;
  std::string var_to_print;
  bool debug = false;
  bool verbose = false;
  bool strip = false;
  bool no_oct_file_strip_on_this_platform = is_true ("%NO_OCT_FILE_STRIP%");
  bool compile_only = false;
  bool link_stand_alone = false;
  bool depend = false;
  bool printonly = false;
  bool output_file_option = false;
  bool creating_mex_file = false;
  bool r2017b_option = false;
  bool r2018a_option = false;
  // The default for this may change in the future.
  bool mx_has_interleaved_complex = false;

  for (int i = 1; i < argc; i++)
    {
      std::string arg = argv[i];

      std::string file;

      if (ends_with (arg, ".c"))
        {
          file = arg;
          cfiles.push_back (file);
        }
      else if (ends_with (arg, ".cc") || ends_with (arg, ".cp")
               || ends_with (arg, ".cpp") || ends_with (arg, ".CPP")
               || ends_with (arg, ".cxx") || ends_with (arg, ".c++")
               || ends_with (arg, ".C"))
        {
          file = arg;
          ccfiles.push_back (file);
        }
      else if (ends_with (arg, ".f") || ends_with (arg, ".F")
               || ends_with (arg, "f90") || ends_with (arg, ".F90"))
        {
          file = arg;
          f77files.push_back (file);
        }
      else if (ends_with (arg, ".o") || ends_with (arg, ".obj"))
        {
          file = arg;
          objfiles += (' ' + quote_path (arg));
        }
      else if (ends_with (arg, ".lib") || ends_with (arg, ".a"))
        {
          file = arg;
          libfiles += (' ' + quote_path (arg));
        }
      else if (arg == "-d" || arg == "-debug" || arg == "--debug"
               || arg == "-v" || arg == "-verbose" ||  arg == "--verbose")
        {
          verbose = true;
        }
      else if (arg == "-silent" ||  arg == "--silent")
        {
          // Ignored for now.
        }
      else if (arg == "-h" || arg == "-?" || arg == "-help" || arg == "--help")
        {
          std::cout << usage_msg << std::endl;
          std::cout << help_msg << std::endl;
          return 0;
        }
      else if (starts_with (arg, "-I"))
        {
          incflags += (' ' + quote_path (arg));
        }
      else if (arg == "-idirafter")
        {
          if (i < argc-1)
            {
              arg = argv[++i];
              incflags += (" -idirafter " + arg);
            }
          else
            std::cerr << "mkoctfile: include directory name missing"
                      << std::endl;
        }
      else if (starts_with (arg, "-D"))
        {
          defs += (' ' + arg);
        }
      else if (arg == "-largeArrayDims" || arg == "-compatibleArrayDims")
        {
          std::cerr << "mkoctfile: warning: -largeArrayDims and -compatibleArrayDims are accepted for compatibility, but ignored" << std::endl;
        }
      else if (arg == "-R2017b")
        {
          if (r2018a_option)
            {
              std::cerr << "mkoctfile: only one of -R2017b and -R2018a may be used" << std::endl;
              return 1;
            }

          r2017b_option = true;
        }
      else if (arg == "-R2018a")
        {
          if (r2017b_option)
            {
              std::cerr << "mkoctfile: only one of -R2017b and -R2018a may be used" << std::endl;
              return 1;
            }

          r2018a_option = true;
          mx_has_interleaved_complex = true;
        }
      else if (starts_with (arg, "-Wl,") || starts_with (arg, "-l")
               || starts_with (arg, "-L") || starts_with (arg, "-R"))
        {
          ldflags += (' ' + quote_path (arg));
        }
#if ! defined (OCTAVE_USE_WINDOWS_API)
      else if (arg == "-pthread")
        {
          ldflags += (' ' + arg);
        }
#endif
      else if (arg == "-M" || arg == "-depend" || arg == "--depend")
        {
          depend = true;
        }
      else if (arg == "-o" || arg == "-output" || arg == "--output")
        {
          output_file_option = true;

          if (i < argc-1)
            {
              arg = argv[++i];
              outputfile = arg;
            }
          else
            std::cerr << "mkoctfile: output filename missing" << std::endl;
        }
      else if (arg == "-n" || arg == "--dry-run" || arg == "--just-print")
        {
          printonly = true;
        }
      else if (arg == "-p" || arg == "-print" || arg == "--print")
        {
          if (i < argc-1)
            {
              ++i;

              // FIXME: Remove LFLAGS checking in Octave 9
              if (argv[i] == "LFLAGS")
                std::cerr << "mkoctfile: warning: LFLAGS is deprecated and will be removed in a future version of Octave, use LDFLAGS instead" << std::endl;

              if (! var_to_print.empty ())
                std::cerr << "mkoctfile: warning: only one '" << arg
                          << "' option will be processed" << std::endl;
              else
                var_to_print = argv[i];
            }
          else
            std::cerr << "mkoctfile: --print requires argument" << std::endl;
        }
      else if (arg == "-s" || arg == "-strip" || arg == "--strip")
        {
          if (no_oct_file_strip_on_this_platform)
            std::cerr << "mkoctfile: stripping disabled on this platform"
                      << std::endl;
          else
            strip = true;
        }
      else if (arg == "-c" || arg == "-compile" || arg == "--compile")
        {
          compile_only = true;
        }
      else if (arg == "-g")
        {
          debug = true;
        }
      else if (arg == "-link-stand-alone" || arg == "--link-stand-alone")
        {
          link_stand_alone = true;
        }
      else if (arg == "-mex" || arg == "--mex")
        {
          creating_mex_file = true;

          incflags += " -I.";
#if defined (_MSC_VER)
          ldflags += " -Wl,-export:mexFunction";
#endif
          output_ext = ".mex";
        }
      else if (starts_with (arg, "-W"))
        {
          pass_on_options += (' ' + arg);
        }
      else if (starts_with (arg, "-O"))
        {
          pass_on_options += (' ' + arg);
        }
      else if (starts_with (arg, "-"))
        {
          // Pass through any unrecognized options.
          pass_on_options += (' ' + arg);
          // Check for an additional argument following the option.
          // However, don't check the final position which is typically a file
          if (i < argc-2)
            {
              arg = argv[i+1];
              if (arg[0] != '-')
                {
                  pass_on_options += (' ' + arg);
                  i++;
                }
            }
        }
      else
        {
          std::cerr << "mkoctfile: unrecognized argument " << arg << std::endl;
          return 1;
        }

      if (! file.empty () && octfile.empty ())
        octfile = file;
    }

  std::map<std::string, std::string> vars
    = make_vars_map (link_stand_alone, verbose, debug);

  if (! var_to_print.empty ())
    {
      if (vars.find (var_to_print) == vars.end ())
        {
          std::cerr << "mkoctfile: unknown variable '" << var_to_print << "'"
                    << std::endl;
          return 1;
        }

      std::cout << vars[var_to_print] << std::endl;

      return 0;
    }

  if (creating_mex_file)
    {
      if (vars["ALL_CFLAGS"].find ("-g") != std::string::npos)
        defs += " -DMEX_DEBUG";

      if (mx_has_interleaved_complex)
        {
          defs += " -DMX_HAS_INTERLEAVED_COMPLEX=1";

          if (! compile_only)
            {
              // Create tmp C source file that defines an extern symbol
              // that can be checked when loading the mex file to
              // determine that the file was compiled expecting
              // interleaved complex values.

              std::string tmp_file = create_interleaved_complex_file ();

              cfiles.push_back (tmp_file);
            }
        }
    }
  else
    {
      if (r2017b_option)
        std::cerr << "mkoctfile: warning: -R2017b option ignored unless creating mex file"
                  << std::endl;

      if (r2018a_option)
        std::cerr << "mkoctfile: warning: -R2018a option ignored unless creating mex file"
                  << std::endl;
    }

  if (compile_only && output_file_option
      && (cfiles.size () + ccfiles.size () + f77files.size ()) > 1)
    {
      std::cerr << "mkoctfile: may not use -c and -o with multiple source files"
                << std::endl;
      return 1;
    }

  std::string output_option;

  if (link_stand_alone)
    {
      if (! outputfile.empty ())
        output_option = "-o " + outputfile;
    }
  else
    {
      if (! outputfile.empty ())
        {
          // FIXME: should probably do a better job of finding the
          // filename extension instead of just looking at the filename
          // length.

          octfile = outputfile;
          std::size_t len = octfile.length ();
          std::size_t len_ext = output_ext.length ();
          if (len <= len_ext || octfile.substr (len-len_ext) != output_ext)
            octfile += output_ext;
        }
      else
        octfile = basename (octfile, true) + output_ext;
    }

  if (depend)
    {
#if defined (OCTAVE_USE_WINDOWS_API) && ! defined (_UNICODE)
      std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> wchar_conv;
#endif

      for (const auto& f : cfiles)
        {
          std::string dfile = basename (f, true) + ".d", line;

          octave_unlink_wrapper (dfile.c_str ());

          std::string cmd
            = (vars["CC"] + ' ' + vars["DEPEND_FLAGS"] + ' '
               + vars["CPPFLAGS"] + ' ' + vars["ALL_CFLAGS"] + ' '
               + incflags  + ' ' + defs + ' ' + quote_path (f));

#if defined (OCTAVE_USE_WINDOWS_API)
          FILE *fd;
          try
            {
              std::wstring wcmd = wchar_conv.from_bytes (cmd);
              fd = ::_wpopen (wcmd.c_str (), L"r");
            }
          catch (const std::range_error& e)
            {
              fd = ::popen (cmd.c_str (), "r");
            }

          std::ofstream fo;
          try
            {
              std::wstring wfile = wchar_conv.from_bytes (dfile);
              fo.open (wfile.c_str ());
            }
          catch (const std::range_error& e)
            {
              fo.open (dfile.c_str ());
            }
#else
          FILE *fd = popen (cmd.c_str (), "r");

          std::ofstream fo (dfile.c_str ());
#endif

          std::size_t pos;
          while (! feof (fd))
            {
              line = get_line (fd);
              if ((pos = line.rfind (".o:")) != std::string::npos)
                {
                  std::size_t spos = line.rfind ('/', pos);
                  std::string ofile
                    = (spos == std::string::npos
                       ? line.substr (0, pos+2)
                       : line.substr (spos+1, pos-spos+1));
                  fo << "pic/" << ofile << ' ' << ofile << ' '
                     << dfile << line.substr (pos) << std::endl;
                }
              else
                fo << line << std::endl;
            }
          pclose (fd);
          fo.close ();
        }

      for (const auto& f : ccfiles)
        {
          std::string dfile = basename (f, true) + ".d", line;

          octave_unlink_wrapper (dfile.c_str ());

          std::string cmd
            = (vars["CXX"] + ' ' + vars["DEPEND_FLAGS"] + ' '
               + vars["CPPFLAGS"] + ' ' + vars["ALL_CXXFLAGS"] + ' '
               + incflags  + ' ' + defs + ' ' + quote_path (f));

#if defined (OCTAVE_USE_WINDOWS_API)
          FILE *fd;
          try
            {
              std::wstring wcmd = wchar_conv.from_bytes (cmd);
              fd = ::_wpopen (wcmd.c_str (), L"r");
            }
          catch (const std::range_error& e)
            {
              fd = ::popen (cmd.c_str (), "r");
            }

          std::ofstream fo;
          try
            {
              std::wstring wfile = wchar_conv.from_bytes (dfile);
              fo.open (wfile.c_str ());
            }
          catch (const std::range_error& e)
            {
              fo.open (dfile.c_str ());
            }
#else
          FILE *fd = popen (cmd.c_str (), "r");

          std::ofstream fo (dfile.c_str ());
#endif

          std::size_t pos;
          while (! feof (fd))
            {
              line = get_line (fd);
              if ((pos = line.rfind (".o:")) != std::string::npos)
                {
                  std::size_t spos = line.rfind ('/', pos);
                  std::string ofile
                    = (spos == std::string::npos
                       ? line.substr (0, pos+2)
                       : line.substr (spos+1, pos-spos+1));
                  fo << "pic/" << ofile << ' ' << ofile << ' '
                     << dfile << line.substr (pos+2) << std::endl;
                }
              else
                fo << line << std::endl;
            }
          pclose (fd);
          fo.close ();
        }

      return 0;
    }

  for (const auto& f : f77files)
    {
      if (! vars["F77"].empty ())
        {
          std::string o;
          if (compile_only)
            {
              if (! outputfile.empty ())
                o = outputfile;
              else
                o = basename (f, true) + ".o";
            }
          else
            {
              o = tmp_objfile_name ();

              tmp_objfiles.push_back (o);

              objfiles += (' ' + o);
            }

          std::string cmd
            = (vars["F77"] + " -c " + vars["FPICFLAG"] + ' '
               + vars["ALL_FFLAGS"] + ' ' + incflags + ' ' + defs + ' '
               + pass_on_options + ' ' + quote_path (f)
               + " -o " + quote_path (o));

          int status = run_command (cmd, verbose, printonly);

          if (status)
            return status;
        }
      else
        {
          std::cerr << "mkoctfile: no way to compile Fortran file " << f
                    << std::endl;
          return 1;
        }
    }

  for (const auto& f : cfiles)
    {
      if (! vars["CC"].empty ())
        {
          std::string o;
          if (compile_only)
            {
              if (! outputfile.empty ())
                o = outputfile;
              else
                o = basename (f, true) + ".o";
            }
          else
            {
              o = tmp_objfile_name ();

              tmp_objfiles.push_back (o);

              objfiles += (' ' + o);
            }

          std::string cmd
            = (vars["CC"] + " -c " + vars["CPPFLAGS"] + ' '
               + vars["CPICFLAG"] + ' ' + vars["ALL_CFLAGS"] + ' '
               + pass_on_options + ' ' + incflags + ' ' + defs + ' '
               + quote_path (f) + " -o " + quote_path (o));

          int status = run_command (cmd, verbose, printonly);

          if (status)
            return status;
        }
      else
        {
          std::cerr << "mkoctfile: no way to compile C file " << f
                    << std::endl;
          return 1;
        }
    }

  for (const auto& f : ccfiles)
    {
      if (! vars["CXX"].empty ())
        {
          std::string o;
          if (compile_only)
            {
              if (! outputfile.empty ())
                o = outputfile;
              else
                o = basename (f, true) + ".o";
            }
          else
            {
              o = tmp_objfile_name ();

              tmp_objfiles.push_back (o);

              objfiles += (' ' + o);
            }

          std::string cmd
            = (vars["CXX"] + " -c " + vars["CPPFLAGS"] + ' '
               + vars["CXXPICFLAG"] + ' ' + vars["ALL_CXXFLAGS"] + ' '
               + pass_on_options + ' ' + incflags + ' ' + defs + ' '
               + quote_path (f) + " -o " + quote_path (o));

          int status = run_command (cmd, verbose, printonly);

          if (status)
            return status;
        }
      else
        {
          std::cerr << "mkoctfile: no way to compile C++ file " << f
                    << std::endl;
          return 1;
        }
    }

  // If we are only compiling, we are done.

  if (compile_only)
    return 0;

  if (objfiles.empty ())
    {
      std::cerr << "mkoctfile: no objects to link" << std::endl;
      return 1;
    }

  std::string octave_libs;

  if (link_stand_alone)
    {
      if (! vars["CXXLD"].empty ())
        {
          octave_libs = "-L" + quote_path (vars["OCTLIBDIR"])
                        + ' ' + vars["OCTAVE_LIBS"];

          // FIXME: Remove LFLAGS in Octave 9
          std::string cmd
            = (vars["CXXLD"] + ' ' + vars["CPPFLAGS"] + ' '
               + vars["ALL_CXXFLAGS"] + ' ' + vars["RDYNAMIC_FLAG"] + ' '
               + pass_on_options + ' ' + output_option + ' ' + objfiles + ' '
               + libfiles + ' ' + ldflags + ' ' + vars["ALL_LDFLAGS"] + ' '
               + vars["LFLAGS"] + ' ' + octave_libs + ' '
               + vars["OCTAVE_LINK_OPTS"] + ' ' + vars["OCTAVE_LINK_DEPS"]);

          int status = run_command (cmd, verbose, printonly);

          clean_up_tmp_files (tmp_objfiles);

          if (status)
            return status;
        }
      else
        {
          std::cerr
            << "mkoctfile: no way to link stand-alone executable file"
            << std::endl;
          return 1;
        }
    }
  else
    {
#if defined (OCTAVE_USE_WINDOWS_API) || defined(CROSS)
      octave_libs = "-L" + quote_path (vars["OCTLIBDIR"])
                    + ' ' + vars["OCTAVE_LIBS"];
#endif

      // FIXME: Remove LFLAGS in Octave 9
      std::string cmd
        = (vars["CXXLD"] + ' ' + vars["ALL_CXXFLAGS"] + ' '
           + pass_on_options + " -o " + octfile + ' ' + objfiles + ' '
           + libfiles + ' ' + ldflags + ' ' + vars["DL_LDFLAGS"] + ' '
           + vars["LDFLAGS"] + ' ' + vars["LFLAGS"] + ' ' + octave_libs + ' '
           + vars["OCT_LINK_OPTS"] + ' ' + vars["OCT_LINK_DEPS"]);

#if defined (OCTAVE_USE_WINDOWS_API) || defined(CROSS)
      if (! f77files.empty () && ! vars["FLIBS"].empty ())
        cmd += ' ' + vars["FLIBS"];
#endif

      int status = run_command (cmd, verbose, printonly);

      clean_up_tmp_files (tmp_objfiles);

      if (status)
        return status;
    }

  if (strip)
    {
      std::string cmd = "strip " + octfile;

      int status = run_command (cmd, verbose, printonly);

      if (status)
        return status;
    }

  return 0;
}
