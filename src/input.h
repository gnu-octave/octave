/*

Copyright (C) 1996 John W. Eaton

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

// Use the GNU readline library for command line editing and hisory.

#if !defined (octave_input_h)
#define octave_input_h 1

#include <cstdio>

#include <string>

extern int octave_read (char *buf, unsigned max_size);
extern FILE *get_input_from_file (const string& name, int warn = 1);
extern FILE *get_input_from_stdin (void);
extern void initialize_readline (void);

// Global pointer for eval().
extern string current_eval_string;

// Nonzero means get input from current_eval_string.
extern int get_input_from_eval_string;

// Nonzero means we're parsing a function file.
extern int reading_fcn_file;

// Simple name of function file we are reading.
extern string curr_fcn_file_name;

// Full name of file we are reading.
extern string curr_fcn_file_full_name;

// Nonzero means we're parsing a script file.
extern int reading_script_file;

// If we are reading from an M-file, this is it.
extern FILE *ff_instream;

// Nonzero means this is an interactive shell.
extern int interactive;

// Nonzero means the user forced this shell to be interactive (-i).
extern int forced_interactive;

// Should we issue a prompt?
extern int promptflag;

// A line of input.
extern string current_input_line;

char *gnu_readline (const char *s);

extern string Vps4;

extern void symbols_of_input (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
