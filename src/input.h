// input.h                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Use the GNU readline library for command line editing and hisory.

#if !defined (_input_h)
#define _input_h 1

#include <stdio.h>

extern char *octave_gets (void);
extern int octave_read (char *buf, int max_size);
extern FILE *get_input_from_file (char *name, int warn = 1);
extern FILE *get_input_from_stdin (void);
extern void initialize_readline (void);

// Global pointer for eval().
extern const char *current_eval_string;

// Nonzero means get input from current_eval_string.
extern int get_input_from_eval_string;

// Nonzero means we're parsing an M-file.
extern int reading_m_file;

// Simple name of M-file we are reading.
extern char *curr_m_file_name;

// Nonzero means we're parsing a script file.
extern int reading_script_file;

// If we are reading from an M-file, this is it.
extern FILE *mf_instream;

// Nonzero means we are using readline.
extern int using_readline;

// Nonzero means commands are echoed as they are executed (-x).
extern int echo_input;

// Nonzero means this is an interactive shell.
extern int interactive;

// Nonzero means the user forced this shell to be interactive (-i).
extern int forced_interactive;

// Should we issue a prompt?
extern int promptflag;

// A line of input.
extern char *current_input_line;

extern "C"
{
char *gnu_readline (char *s);
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
