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

#if !defined (octave_variables_h)
#define octave_variables_h 1

class octave_function;
class octave_user_function;
class symbol_record;
class symbol_table;

class tree_identifier;
class octave_value;
class octave_value_list;
class octave_builtin;
class octave_mapper;
class string_vector;

#include <string>

#include "ov.h"
#include "ov-builtin.h"
#include "symtab.h"

extern bool at_top_level (void);

extern void initialize_symbol_tables (void);

extern bool is_builtin_variable (const std::string&);
extern bool is_command_name (const std::string&);

// The next three are here temporarily...
extern bool is_marked_as_rawcommand (const std::string& s);
extern void mark_as_rawcommand (const std::string& s);
extern void unmark_rawcommand (const std::string& s);

extern bool is_rawcommand_name (const std::string&);
extern bool is_mapper_function_name (const std::string&);
extern bool is_builtin_function_name (const std::string&);
extern bool is_globally_visible (const std::string&);

extern octave_function *
is_valid_function (const octave_value&, const std::string& = std::string (),
		   bool warn = false); 

extern octave_function *
is_valid_function (const std::string&, const std::string& = std::string (),
		   bool warn = false); 

extern octave_function *
extract_function (const octave_value& arg, const std::string& warn_for,
		  const std::string& fname, const std::string& header,
		  const std::string& trailer);

extern string_vector
get_struct_elts (const std::string& text);

extern string_vector
generate_struct_completions (const std::string& text, std::string& prefix,
			     std::string& hint);

extern bool
looks_like_struct (const std::string& text);

extern int
symbol_exist (const std::string& name, const std::string& type = "any");

extern std::string
unique_symbol_name (const std::string& basename);

extern bool
fcn_out_of_date (octave_function *fcn, const std::string& ff, time_t tp);

extern bool lookup (symbol_record *s, bool exec_script = true);

extern symbol_record *
lookup_by_name (const std::string& nm, bool exec_script = true);

extern octave_value lookup_function (const std::string& nm);

extern octave_value lookup_user_function (const std::string& nm);

extern octave_value lookup_function_handle (const std::string& nm);

extern octave_value
get_global_value (const std::string& nm, bool silent = false);

extern void set_global_value (const std::string& nm, const octave_value& val);

extern std::string builtin_string_variable (const std::string&);
extern int builtin_real_scalar_variable (const std::string&, double&);
extern octave_value builtin_any_variable (const std::string&);

extern void link_to_global_variable (symbol_record *sr);
extern void link_to_builtin_or_function (symbol_record *sr);

extern void force_link_to_function (const std::string&);

extern void bind_ans (const octave_value& val, bool print);

extern void
bind_builtin_constant (const std::string&, const octave_value&,
		       bool protect = false, bool eternal = false,
		       const std::string& help = std::string ());

extern void
bind_builtin_variable (const std::string&, const octave_value&,
		       bool protect = false, bool eternal = false,
		       symbol_record::change_function f = 0,
		       const std::string& help = std::string ());

extern void mlock (const std::string&);
extern void munlock (const std::string&);
extern bool mislocked (const std::string&);

extern bool clear_function (const std::string& nm);
extern bool clear_variable (const std::string& nm);
extern bool clear_symbol (const std::string& nm);

// Symbol table for symbols at the top level.
extern symbol_table *top_level_sym_tab;

// Symbol table for the current scope.
extern symbol_table *curr_sym_tab;

// Symbol table for the current caller scope.
extern symbol_table *curr_caller_sym_tab;

// Symbol table for global symbols.
extern symbol_table *global_sym_tab;

// Symbol table for functions and built-in symbols.
extern symbol_table *fbi_sym_tab;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
