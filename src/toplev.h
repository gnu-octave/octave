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

#include <string>

class octave_value;
class octave_value_list;
class octave_user_function;
class tree_statement_list;
class charMatrix;

extern void
clean_up_and_exit (int) GCC_ATTR_NORETURN;

extern int
main_loop (void);

extern void
do_octave_atexit (void);

// Current command to execute.
extern tree_statement_list *global_command;

// Pointer to function that is currently being evaluated.
extern octave_user_function *curr_function;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
