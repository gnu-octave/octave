// octave.h                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_octave_h)
#define octave_octave_h 1

#include <cstdio>

class tree_constant;
class tree_function;
class tree_statement_list;
class Octave_str_obj;

extern void clean_up_and_exit (int) NORETURN;

extern void parse_and_execute (FILE *f, int print = 0);
extern void parse_and_execute (char *s, int print = 0, int verbose = 0);

extern tree_constant eval_string (const char *string, int print,
				  int& parse_status);

// argv[0] for this program.
extern char *raw_prog_name;

// Cleaned-up name of this program, not including path information.
extern char *prog_name;

// Login name for user running this program.
extern char *user_name;

// Name of the host we are running on.
extern char *host_name;

// User's home directory.
extern char *home_directory;

// Guess what?
extern char *the_current_working_directory;

// Load path specified on command line.
extern char *load_path;

// Name of the info file specified on command line.
extern char *info_file;

// Name of the editor to be invoked by the edit_history command.
extern char *editor;

// If nonzero, don't do fancy line editing.
extern int no_line_editing;

// If nonzero, print verbose info in some cases.
extern int verbose_flag;

// Command number, counting from the beginning of this session.
extern int current_command_number;

// Nonzero means we are exiting via the builtin exit or quit functions.
extern int quitting_gracefully;

// Current command to execute.
extern tree_statement_list *global_command;

// Pointer to function that is currently being evaluated.
extern tree_function *curr_function;

// Nonzero means input is coming from startup file.
extern int input_from_startup_file;

// The command-line options.
extern Octave_str_obj octave_argv;

// Nonzero means that input is coming from a file that was named on
// the command line.
extern int input_from_command_line_file;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
