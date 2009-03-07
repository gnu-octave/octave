/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002,
              2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

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
class octave_function;

#include "oct-obj.h"

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

// Keep track of symbol table information when parsing functions.
extern std::stack<symbol_table::scope_id> symtab_context;

// Name of parent function when parsing function files that might
// contain nested functions.
extern std::string parent_function_name;

// Name of the current class when we are parsing class methods or
// constructors.
extern std::string current_class_name;

// Keep a count of how many END tokens we expect.
extern int end_tokens_expected;

extern OCTINTERP_API std::string
get_help_from_file (const std::string& nm, bool& symbol_found,
		    std::string& file);

extern OCTINTERP_API std::string
get_help_from_file (const std::string& nm, bool& symbol_found);

extern OCTINTERP_API std::string lookup_autoload (const std::string& nm);

extern OCTINTERP_API string_vector autoloaded_functions (void);

extern OCTINTERP_API string_vector reverse_lookup_autoload (const std::string& nm);

extern OCTINTERP_API octave_function *
load_fcn_from_file (const std::string& file_name,
		    const std::string& dir_name = std::string (),
		    const std::string& dispatch_type = std::string (),
		    const std::string& fcn_name = std::string (),
		    bool autoload = false);

extern OCTINTERP_API void
source_file (const std::string& file_name,
	     const std::string& context = std::string (),
	     bool verbose = false, bool require_file = true,
	     const std::string& warn_for = std::string ());

extern OCTINTERP_API octave_value_list
feval (const std::string& name,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern OCTINTERP_API octave_value_list
feval (octave_function *fcn,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern OCTINTERP_API octave_value_list
feval (const octave_value_list& args, int nargout = 0);

extern OCTINTERP_API octave_value_list
eval_string (const std::string&, bool silent, int& parse_status, int hargout);

extern OCTINTERP_API octave_value
eval_string (const std::string&, bool silent, int& parse_status);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
