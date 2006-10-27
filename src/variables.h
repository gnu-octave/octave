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

#include <climits>
#include <cfloat>

#include <string>

#include "ov.h"
#include "ov-builtin.h"
#include "symtab.h"

extern OCTINTERP_API bool at_top_level (void);

extern OCTINTERP_API void initialize_symbol_tables (void);
extern OCTINTERP_API void clear_mex_functions (void);

extern OCTINTERP_API bool is_command_name (const std::string&);

// The next three are here temporarily...
extern OCTINTERP_API bool is_marked_as_rawcommand (const std::string& s);
extern OCTINTERP_API void mark_as_rawcommand (const std::string& s);
extern OCTINTERP_API void unmark_rawcommand (const std::string& s);

extern OCTINTERP_API bool is_rawcommand_name (const std::string&);
extern OCTINTERP_API bool is_mapper_function_name (const std::string&);
extern OCTINTERP_API bool is_builtin_function_name (const std::string&);
extern OCTINTERP_API bool is_globally_visible (const std::string&);

extern OCTINTERP_API octave_function *
is_valid_function (const octave_value&, const std::string& = std::string (),
		   bool warn = false); 

extern OCTINTERP_API octave_function *
is_valid_function (const std::string&, const std::string& = std::string (),
		   bool warn = false); 

extern OCTINTERP_API octave_function *
extract_function (const octave_value& arg, const std::string& warn_for,
		  const std::string& fname, const std::string& header,
		  const std::string& trailer);

extern OCTINTERP_API string_vector
get_struct_elts (const std::string& text);

extern OCTINTERP_API string_vector
generate_struct_completions (const std::string& text, std::string& prefix,
			     std::string& hint);

extern OCTINTERP_API bool
looks_like_struct (const std::string& text);

extern OCTINTERP_API int
symbol_exist (const std::string& name, const std::string& type = "any");

extern OCTINTERP_API std::string
unique_symbol_name (const std::string& basename);

extern OCTINTERP_API bool
fcn_out_of_date (octave_function *fcn, const std::string& ff, time_t tp);

extern OCTINTERP_API bool lookup (symbol_record *s, bool exec_script = true);

extern OCTINTERP_API symbol_record *
lookup_by_name (const std::string& nm, bool exec_script = true);

extern OCTINTERP_API octave_value lookup_function (const std::string& nm);

extern OCTINTERP_API octave_value lookup_user_function (const std::string& nm);

extern OCTINTERP_API octave_value lookup_function_handle (const std::string& nm);

extern OCTINTERP_API octave_value
get_global_value (const std::string& nm, bool silent = false);

extern OCTINTERP_API void set_global_value (const std::string& nm, const octave_value& val);

extern OCTINTERP_API octave_value
set_internal_variable (bool& var, const octave_value_list& args,
		       int nargout, const char *nm);

extern OCTINTERP_API octave_value
set_internal_variable (char& var, const octave_value_list& args,
		       int nargout, const char *nm);

extern OCTINTERP_API octave_value
set_internal_variable (int& var, const octave_value_list& args,
		       int nargout, const char *nm,
		       int minval = INT_MIN, int maxval = INT_MAX);

extern OCTINTERP_API octave_value
set_internal_variable (double& var, const octave_value_list& args,
		       int nargout, const char *nm,
		       double minval = DBL_MIN, double maxval = DBL_MAX);

extern OCTINTERP_API octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
		       int nargout, const char *nm, bool empty_ok = true);

#define SET_INTERNAL_VARIABLE(NM) \
  set_internal_variable (V ## NM, args, nargout, #NM)

#define SET_NONEMPTY_INTERNAL_STRING_VARIABLE(NM) \
  set_internal_variable (V ## NM, args, nargout, #NM, false)

#define SET_INTERNAL_VARIABLE_WITH_LIMITS(NM, MINVAL, MAXVAL) \
  set_internal_variable (V ## NM, args, nargout, #NM, MINVAL, MAXVAL)

extern OCTINTERP_API std::string builtin_string_variable (const std::string&);
extern OCTINTERP_API int builtin_real_scalar_variable (const std::string&, double&);
extern OCTINTERP_API octave_value builtin_any_variable (const std::string&);

extern OCTINTERP_API void link_to_global_variable (symbol_record *sr);
extern OCTINTERP_API void link_to_builtin_or_function (symbol_record *sr);

extern OCTINTERP_API void force_link_to_function (const std::string&);

extern OCTINTERP_API void bind_ans (const octave_value& val, bool print);

extern OCTINTERP_API void
bind_internal_variable (const std::string& fname, const octave_value& val);

extern OCTINTERP_API void mlock (const std::string&);
extern OCTINTERP_API void munlock (const std::string&);
extern OCTINTERP_API bool mislocked (const std::string&);

extern OCTINTERP_API bool clear_function (const std::string& nm);
extern OCTINTERP_API bool clear_variable (const std::string& nm);
extern OCTINTERP_API bool clear_symbol (const std::string& nm);

// Symbol table for symbols at the top level.
extern OCTINTERP_API symbol_table *top_level_sym_tab;

// Symbol table for the current scope.
extern OCTINTERP_API symbol_table *curr_sym_tab;

// Symbol table for the current caller scope.
extern OCTINTERP_API symbol_table *curr_caller_sym_tab;

// Symbol table for global symbols.
extern OCTINTERP_API symbol_table *global_sym_tab;

// Symbol table for functions and built-in symbols.
extern OCTINTERP_API symbol_table *fbi_sym_tab;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
