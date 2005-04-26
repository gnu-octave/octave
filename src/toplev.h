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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_toplev_h)
#define octave_toplev_h 1

#include <cstdio>

#include <string>

class octave_value;
class octave_value_list;
class octave_user_function;
class tree_statement_list;
class charMatrix;

extern void
clean_up_and_exit (int) GCC_ATTR_NORETURN;

extern int main_loop (void);

extern void
do_octave_atexit (void);

// Current command to execute.
extern tree_statement_list *global_command;

// Pointer to function that is currently being evaluated.
extern octave_function *curr_function;

// Pointer to caller of curr_function.
extern octave_function *curr_caller_function;

// Pointer to parent function that is currently being evaluated.
extern octave_function *curr_parent_function;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
extern bool octave_interpreter_ready;

// TRUE means we've processed all the init code and we are good to go.
extern bool octave_initialized;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
