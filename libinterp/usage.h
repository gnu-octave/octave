////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_usage_h)
#define octave_usage_h 1

#include "octave-config.h"

#include <iosfwd>

#include "version.h"

// Usage message
static const char *usage_string =
  "octave [-HVWdfhiqvx] [--debug] [--doc-cache-file file] [--echo-commands]\n\
       [--eval CODE] [--exec-path path] [--experimental-terminal-widget]\n\
       [--gui] [--help] [--image-path path] [--info-file file]\n\
       [--info-program prog] [--interactive] [--line-editing] [--no-gui]\n\
       [--no-history] [--no-init-file] [--no-init-path] [--no-line-editing]\n\
       [--no-site-file] [--no-window-system] [--norc] [-p path]\n\
       [--path path] [--persist] [--server] [--silent] [--traditional]\n\
       [--verbose] [--version] [file]";

// Usage message with extra help.

static void
octave_print_verbose_usage_and_exit (void)
{
  std::cout << octave_name_version_copyright_copying_and_warranty ()
            << "\n\
\n\
Usage: octave [options] [FILE]\n\
\n\
Options:\n\
\n\
  --built-in-docstrings-file FILE Use docs for built-ins from FILE.\n\
  --debug, -d             Enter parser debugging mode.\n\
  --doc-cache-file FILE   Use doc cache file FILE.\n\
  --echo-commands, -x     Echo commands as they are executed.\n\
  --eval CODE             Evaluate CODE.  Exit when done unless --persist.\n\
  --exec-path PATH        Set path for executing subprograms.\n\
  --experimental-terminal-widget\n\
                          Use new experimental terminal widget in the GUI.\n\
  --gui                   Start the graphical user interface.\n\
  --help, -h,             Print short help message and exit.\n\
  --image-path PATH       Add PATH to head of image search path.\n\
  --info-file FILE        Use top-level info file FILE.\n\
  --info-program PROGRAM  Use PROGRAM for reading info files.\n\
  --interactive, -i       Force interactive behavior.\n\
  --line-editing          Force readline use for command-line editing.\n\
  --no-gui                Disable the graphical user interface.\n\
  --no-history, -H        Don't save commands to the history list\n\
  --no-init-file          Don't read the ~/.octaverc or .octaverc files.\n\
  --no-init-path          Don't initialize function search path.\n\
  --no-line-editing       Don't use readline for command-line editing.\n\
  --no-site-file          Don't read the site-wide octaverc file.\n\
  --no-window-system, -W  Disable window system, including graphics.\n\
  --norc, -f              Don't read any initialization files.\n\
  --path PATH, -p PATH    Add PATH to head of function search path.\n\
  --persist               Go interactive after --eval or reading from FILE.\n\
  --server                Enter server mode at startup.\n\
  --silent, --quiet, -q   Don't print message at startup.\n\
  --texi-macros-file FILE Use Texinfo macros in FILE for makeinfo command.\n\
  --traditional           Set variables for closer MATLAB compatibility.\n\
  --verbose, -V           Enable verbose output in some cases.\n\
  --version, -v           Print version number and exit.\n\
\n\
  FILE                    Execute commands from FILE.  Exit when done\n\
                          unless --persist is also specified.\n\
\n"
            << octave_www_statement ()
            << "\n\n"
            << octave_contrib_statement ()
            << "\n\n"
            << octave_bugs_statement ()
            << "\n";

  exit (0);
}

// Terse usage message.

static void
octave_print_terse_usage_and_exit (void)
{
  std::cerr << "\nusage: " << usage_string << "\n\n";

  exit (1);
}

static void
octave_print_version_and_exit (void)
{
  std::cout << octave_name_version_copyright_copying_warranty_and_bugs ()
            << "\n";
  exit (0);
}

#endif
