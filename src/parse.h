// parse.h                                                 -*- C++ -*-
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

#if !defined (_parse_h)
#define _parse_h 1

#include "SLStack.h"

extern void discard_until (char c);
extern void reset_parser (void);
extern int yylex (void);
extern int yyparse (void);

class tree;
class tree_matrix;
class tree_identifier;
class symbol_table;

// Nonzero means we're in the middle of defining a function.
extern int defining_func;

// Nonzero means we're in the middle of defining a loop.
extern int looping;

// Nonzero means we're in the middle of defining a conditional expression.
extern int iffing;

// Nonzero means we need to do some extra lookahead to avoid being
// screwed by bogus function syntax.
extern int maybe_screwed;

// Nonzero means we need to do some extra lookahead to avoid being
// screwed by bogus function syntax.
extern int maybe_screwed_again;

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

#define HELP_BUF_LENGTH 8192

// Buffer for help text snagged from function files.
extern char help_buf [HELP_BUF_LENGTH];

// Nonzero means we're working on a plot command.
extern int plotting;

// Nonzero means we've seen something that means we must be past the
// range part of a plot command.
extern int past_plot_range;

// Nonzero means we're looking at the range part of a plot command.
extern int in_plot_range;

// Nonzero means we're looking at the using part of a plot command.
extern int in_plot_using;

// Nonzero means we're looking at the style part of a plot command.
extern int in_plot_style;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
