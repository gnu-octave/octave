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

#if !defined (octave_parse_h)
#define octave_parse_h 1

#include <cstdio>

#include <string>

#include <stack>

extern void reset_parser (void);
extern int octave_lex (void);
extern int octave_parse (void);

class tree;
class tree_matrix;
class tree_identifier;
class octaev_function;
class symbol_record;
class symbol_table;

#include "oct-obj.h"

// Temporary symbol table pointer used to cope with bogus function syntax.
extern symbol_table *tmp_local_sym_tab;

// Nonzero means print parser debugging info (-d).
extern int octave_debug;

// The current input line number.
extern int input_line_number;

// The column of the current token.
extern int current_input_column;

// Buffer for help text snagged from function files.
extern std::stack<std::string> help_buf;

// TRUE means we are using readline.
extern bool line_editing;

// TRUE means we printed messages about reading startup files.
extern bool reading_startup_message_printed;

// TRUE means input is coming from startup file.
extern bool input_from_startup_file;

// TRUE means that we are in the process of evaluating a function
// body.  The parser might be called in that case if we are looking at
// an eval() statement.
extern bool evaluating_function_body;

// Keep track of symbol table information when parsing functions.
extern std::stack<symbol_table*> symtab_context;

// Name of parent function when parsing function files that might
// contain nested functions.
extern std::string parent_function_name;

// TRUE means warn about function files that have time stamps in the future.
extern bool Vwarn_future_time_stamp;

// Keep a count of how many END tokens we expect.
extern int end_tokens_expected;

extern void
parse_and_execute (FILE *f);

extern void
parse_and_execute (const std::string& s, bool verbose = false,
		   const char *warn_for = 0);

extern std::string get_help_from_file (const std::string& f);

extern bool
load_fcn_from_file (symbol_record *sym_rec, bool exec_script);

extern void
source_file (const std::string file_name);

extern octave_value_list
feval (const std::string& name,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern octave_value_list
feval (octave_function *fcn,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern octave_value_list
feval (const octave_value_list& args, int nargout = 0);

extern octave_value_list
eval_string (const std::string&, bool silent, int& parse_status, int hargout);

extern octave_value
eval_string (const std::string&, bool silent, int& parse_status);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
