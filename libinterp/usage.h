////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2024 The Octave Project Developers
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
  "octave [-GHVWefghiqvx] [--echo-commands] [--eval CODE]\n\
       [--experimental-terminal-widget] [--gui] [--help] [--interactive]\n\
       [--line-editing] [--no-gui] [--no-history] [--no-init-file]\n\
       [--no-init-path] [--no-line-editing] [--no-site-file]\n\
       [--no-window-system] [--norc] [--path path] [--persist] [--server]\n\
       [--silent] [--traditional] [--version] [file]";

// Usage message with extra help.

static void
octave_print_verbose_usage_and_exit ()
{
  std::cout << octave_name_version_copyright_copying_and_warranty ()
            << "\n\
\n\
Usage: octave [options] [FILE]\n\
\n\
Options:\n\
\n"
// FIXME: Disabled debug option for parser 2023-12-29.
// Uncomment and restore code if Octave adds capability to
// immediately enter debug mode for a script.
//--debug, -d             Enter debugging mode.\n\ //
"\
  --echo-commands, -x     Echo commands as they are executed.\n\
  --eval, -e CODE         Evaluate CODE.  Exit when done unless --persist.\n\
  --experimental-terminal-widget\n\
                          Use new experimental terminal widget in the GUI.\n\
  --gui, -g               Start the graphical user interface.\n\
  --help, -h,             Print short help message and exit.\n\
  --interactive, -i       Force interactive behavior.\n\
  --line-editing          Force readline use for command-line editing.\n\
  --no-gui, -G            Disable the graphical user interface.\n\
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
  --traditional           Set variables for closer MATLAB compatibility.\n\
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
octave_print_terse_usage_and_exit ()
{
  std::cerr << "\nusage: " << usage_string << "\n\n";

  exit (1);
}

static void
octave_print_version_and_exit ()
{
  std::cout << octave_name_version_copyright_copying_warranty_and_bugs ()
            << "\n";
  exit (0);
}

#endif
