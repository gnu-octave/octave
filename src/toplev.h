/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_toplev_h)
#define octave_toplev_h 1

#include <cstdio>

class octave_value;
class octave_value_list;
class tree_function;
class tree_statement_list;
class charMatrix;

#include <string>

extern void
clean_up_and_exit (int) GCC_ATTR_NORETURN;

extern void
parse_and_execute (FILE *f, bool print = false);

extern void
parse_and_execute (const string& s, bool print = false, bool verbose = false,
		   const char *warn_for = 0);

extern octave_value
eval_string (const string&, bool print, int& parse_status);

extern int
main_loop (void);

extern void
do_octave_atexit (void);

extern void
symbols_of_toplev (void);

// argv[0] for this program.
extern string Vprogram_invocation_name;

// Cleaned-up name of this program, not including path information.
extern string Vprogram_name;

// Login name for user running this program.
extern string Vuser_name;

// Name of the host we are running on.
extern string Vhost_name;

// Home directory for the current user.
extern string Vhome_directory;

// Nonzero means we are using readline.
extern int using_readline;

// Nonzero means we printed messages about reading startup files.
extern int reading_startup_message_printed;

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

// Nonzero means that input is coming from a file that was named on
// the command line.
extern int input_from_command_line_file;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
