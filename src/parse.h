// parse.h                                                 -*- C++ -*-
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

#if !defined (octave_parse_h)
#define octave_parse_h 1

#include <string>

#include "SLStack.h"

extern void reset_parser (void);
extern int yylex (void);
extern int yyparse (void);

class tree;
class tree_matrix;
class tree_identifier;
class symbol_table;

// Temporary symbol table pointer used to cope with bogus function syntax.
extern symbol_table *tmp_local_sym_tab;

// Stack to hold list of literal matrices.
extern SLStack <tree_matrix *> ml;

// A nonzero element corresponding to an element of ml means we just
// started reading a new matrix.  This should probably be part of a
// new struct for matrix lists...
extern SLStack <int> mlnm;

// Nonzero means print parser debugging info (-d).
extern int yydebug;

// The current input line number.
extern int input_line_number;

// The column of the current token.
extern int current_input_column;

// Buffer for help text snagged from function files.
extern string help_buf;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
