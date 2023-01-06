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

#if ! defined (octave_options_h)
#define octave_options_h 1

#include "octave-config.h"

#include <iosfwd>

// This is here so that it's more likely that the usage message and
// the real set of options will agree.  Note: the '+' must come first
// to prevent getopt from permuting arguments!
static const char *short_opts = "+HWVdfhip:qvx";

// Long options.  See the comments in getopt.h for the meanings of the
// fields in this structure.
#define BUILT_IN_DOCSTRINGS_FILE_OPTION 1
#define DOC_CACHE_FILE_OPTION 2
#define EVAL_OPTION 3
#define EXEC_PATH_OPTION 4
#define EXPERIMENTAL_TERMINAL_WIDGET_OPTION 5
#define GUI_OPTION 6
#define IMAGE_PATH_OPTION 7
#define INFO_FILE_OPTION 8
#define INFO_PROG_OPTION 9
#define LINE_EDITING_OPTION 10
#define NO_GUI_OPTION 11
#define NO_GUI_LIBS_OPTION 12
#define NO_INIT_FILE_OPTION 13
#define NO_INIT_PATH_OPTION 14
#define NO_LINE_EDITING_OPTION 15
#define NO_SITE_FILE_OPTION 16
#define PERSIST_OPTION 17
#define SERVER_OPTION 18
#define TEXI_MACROS_FILE_OPTION 19
#define TRADITIONAL_OPTION 20
struct octave_getopt_options long_opts[] =
{
  { "braindead",                octave_no_arg,       nullptr, TRADITIONAL_OPTION },
  { "built-in-docstrings-file", octave_required_arg, nullptr, BUILT_IN_DOCSTRINGS_FILE_OPTION },
  { "debug",                    octave_no_arg,       nullptr, 'd' },
  { "doc-cache-file",           octave_required_arg, nullptr, DOC_CACHE_FILE_OPTION },
  { "echo-commands",            octave_no_arg,       nullptr, 'x' },
  { "eval",                     octave_required_arg, nullptr, EVAL_OPTION },
  { "exec-path",                octave_required_arg, nullptr, EXEC_PATH_OPTION },
  { "experimental-terminal-widget", octave_no_arg,   nullptr, EXPERIMENTAL_TERMINAL_WIDGET_OPTION },
  { "force-gui",                octave_no_arg,       nullptr, GUI_OPTION },
  { "gui",                      octave_no_arg,       nullptr, GUI_OPTION },
  { "help",                     octave_no_arg,       nullptr, 'h' },
  { "image-path",               octave_required_arg, nullptr, IMAGE_PATH_OPTION },
  { "info-file",                octave_required_arg, nullptr, INFO_FILE_OPTION },
  { "info-program",             octave_required_arg, nullptr, INFO_PROG_OPTION },
  { "interactive",              octave_no_arg,       nullptr, 'i' },
  { "line-editing",             octave_no_arg,       nullptr, LINE_EDITING_OPTION },
  { "no-gui",                   octave_no_arg,       nullptr, NO_GUI_OPTION },
  { "no-gui-libs",              octave_no_arg,       nullptr, NO_GUI_LIBS_OPTION },
  { "no-history",               octave_no_arg,       nullptr, 'H' },
  { "no-init-file",             octave_no_arg,       nullptr, NO_INIT_FILE_OPTION },
  { "no-init-path",             octave_no_arg,       nullptr, NO_INIT_PATH_OPTION },
  { "no-line-editing",          octave_no_arg,       nullptr, NO_LINE_EDITING_OPTION },
  { "no-site-file",             octave_no_arg,       nullptr, NO_SITE_FILE_OPTION },
  { "no-window-system",         octave_no_arg,       nullptr, 'W' },
  { "norc",                     octave_no_arg,       nullptr, 'f' },
  { "path",                     octave_required_arg, nullptr, 'p' },
  { "persist",                  octave_no_arg,       nullptr, PERSIST_OPTION },
  { "quiet",                    octave_no_arg,       nullptr, 'q' },
  { "server",                   octave_no_arg,       nullptr, SERVER_OPTION },
  { "silent",                   octave_no_arg,       nullptr, 'q' },
  { "texi-macros-file",         octave_required_arg, nullptr, TEXI_MACROS_FILE_OPTION },
  { "traditional",              octave_no_arg,       nullptr, TRADITIONAL_OPTION },
  { "verbose",                  octave_no_arg,       nullptr, 'V' },
  { "version",                  octave_no_arg,       nullptr, 'v' },
  { nullptr,                    0,                   nullptr, 0 }
};

#endif
