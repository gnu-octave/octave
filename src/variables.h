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

#if !defined (octave_variables_h)
#define octave_variables_h 1

class octave_symbol;
class symbol_record;
class symbol_table;

class tree_identifier;
class tree_indirect_ref;
class octave_value;
class octave_value_list;
class octave_builtin;
class octave_mapper;
class string_vector;

#include <string>

#include "ov.h"

typedef int (*sv_Function)(void);

struct builtin_variable
{
  builtin_variable (const string& n, const octave_value& v, bool iaf,
		    bool p, bool e, sv_Function svf, const string& h)
    : name (n), value (v), install_as_function (iaf), protect (p),
      eternal (e), sv_function (svf), help_string (h) { }

  string name;
  octave_value value;
  bool install_as_function;
  bool protect;
  bool eternal;
  sv_Function sv_function;
  string help_string;
};

class
octave_variable_reference
{
public:

  octave_variable_reference (tree_identifier *i) : id (i), indir (0) { }

  octave_variable_reference (tree_indirect_ref *i);

  ~octave_variable_reference (void) { }

  void assign (octave_value::assign_op, const octave_value&);

  void assign (octave_value::assign_op, const octave_value_list&,
	       const octave_value&);

  octave_value value (void);

private:

  tree_identifier *id;

  tree_indirect_ref *indir;

  // No copying!

  octave_variable_reference (const octave_variable_reference&);

  octave_variable_reference& operator = (const octave_variable_reference&);
};

typedef octave_value_list (*Octave_builtin_fcn)(const octave_value_list&, int);

extern void initialize_symbol_tables (void);

extern bool lookup (symbol_record *s, bool exec_script = true);

extern symbol_record *lookup_by_name (const string& nm,
				      bool exec_script = true);

extern octave_value get_global_value (const string& nm);

extern void set_global_value (const string& nm, const octave_value& val);

extern string get_help_from_file (const string& f);

extern string builtin_string_variable (const string&);
extern int builtin_real_scalar_variable (const string&, double&);
extern octave_value builtin_any_variable (const string&);

extern void link_to_global_variable (symbol_record *sr);
extern void link_to_builtin_variable (symbol_record *sr);
extern void link_to_builtin_or_function (symbol_record *sr);

extern void force_link_to_function (const string&);

extern bool is_builtin_variable (const string&);
extern bool is_text_function_name (const string&);
extern bool is_mapper_function_name (const string&);
extern bool is_builtin_function_name (const string&);
extern bool is_globally_visible (const string&);

extern octave_symbol *
is_valid_function (const octave_value&, const string&, bool warn = false); 

extern octave_symbol *
extract_function (const octave_value& arg, const string& warn_for,
		  const string& fname, const string& header,
		  const string& trailer);

extern string_vector
get_struct_elts (const string& text);

extern bool
looks_like_struct (const string& text);

extern string_vector
generate_struct_completions (const string& text, string& prefix,
			     string& hint);

extern string_vector make_name_list (void);

extern void
install_builtin_mapper (octave_mapper *mf);

extern void
install_builtin_function (octave_builtin *f, bool is_text_fcn = false);

extern void
install_builtin_variable (const builtin_variable& v);

extern void
install_builtin_variable_as_function (const string& name,
				      const octave_value& val,
				      bool protect = false,
				      bool eternal = false,
				      const string& help = string ());

extern void alias_builtin (const string& alias, const string& name);

extern void bind_ans (const octave_value& val, bool print);

extern void bind_global_error_variable (void);

extern void clear_global_error_variable (void *);

extern void
bind_builtin_variable (const string&, const octave_value&,
		       bool protect = false, bool eternal = false,
		       sv_Function f = (sv_Function) 0,
		       const string& help = string ());

// Symbol table for symbols at the top level.
extern symbol_table *top_level_sym_tab;

// Symbol table for the current scope.
extern symbol_table *curr_sym_tab;

// Symbol table for global symbols.
extern symbol_table *global_sym_tab;

enum echo_state
{
  ECHO_OFF = 0,
  ECHO_SCRIPTS = 1,
  ECHO_FUNCTIONS = 2,
  ECHO_CMD_LINE = 4
};

extern int Vecho_executing_commands;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
