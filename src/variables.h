// variables.h                                              -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

class symbol_record;
class symbol_table;

class tree_fvc;
class tree_constant;
class Octave_object;
class string_vector;

#include <string>

struct builtin_mapper_function;

typedef int (*sv_Function)(void);

struct builtin_variable
{
  builtin_variable (const string& n, tree_constant *v, int iaf, int p,
		    int e, sv_Function svf, const string& h)
    : name (n), value (v), install_as_function (iaf), protect (p),
      eternal (e), sv_function (svf), help_string (h) { }

  string name;
  tree_constant *value;
  int install_as_function;
  int protect;
  int eternal;
  sv_Function sv_function;
  string help_string;
};

typedef Octave_object (*Octave_builtin_fcn)(const Octave_object&, int);

struct builtin_function
{
  builtin_function (const string& n, int itf, Octave_builtin_fcn f,
		    const string& h)
    : name (n), is_text_fcn (itf), fcn (f), help_string (h) { }

  string name;
  int is_text_fcn;
  Octave_builtin_fcn fcn;
  string help_string;
};

extern void initialize_symbol_tables (void);

extern bool lookup (symbol_record *s, int exec_script = 1);

extern symbol_record *lookup_by_name (const string& nm, int exec_script = 1);

extern string get_help_from_file (const string& f);

extern string builtin_string_variable (const string&);
extern int builtin_real_scalar_variable (const string&, double&);
extern tree_constant builtin_any_variable (const string&);

extern void link_to_global_variable (symbol_record *sr);
extern void link_to_builtin_variable (symbol_record *sr);
extern void link_to_builtin_or_function (symbol_record *sr);

extern void force_link_to_function (const string&);

extern bool is_builtin_variable (const string&);
extern bool is_text_function_name (const string&);
extern bool is_globally_visible (const string&);

extern tree_fvc *is_valid_function (const tree_constant&, const string&,
				    int warn = 0); 

extern string_vector make_name_list (void);

extern void install_builtin_mapper (const builtin_mapper_function& mf);

extern void install_builtin_function (const builtin_function& gf);

extern void install_builtin_variable (const builtin_variable& v);

extern void
install_builtin_variable_as_function
  (const string& name, tree_constant *val, int protect = 0,
   int eternal = 0, const string& help = string ());

extern void alias_builtin (const string& alias, const string& name);

#if 0
extern void bind_nargin_and_nargout (symbol_table *sym_tab,
				     int nargin, int nargout);
#endif

extern void bind_ans (const tree_constant& val, int print);

extern void bind_global_error_variable (void);

extern void clear_global_error_variable (void *);

extern void bind_builtin_variable (const string&, tree_constant *,
				   int protect = 0, int eternal = 0,
				   sv_Function f = (sv_Function) 0,
				   const string& help = string ());

extern void bind_builtin_variable (const string&, const tree_constant&,
				   int protect = 0, int eternal = 0,
				   sv_Function f = (sv_Function) 0,
				   const string& help = string ());

extern void install_builtin_variables (void);

extern string maybe_add_default_load_path (const string& p);

extern string octave_lib_dir (void);
extern string octave_arch_lib_dir (void);
extern string octave_fcn_file_dir (void);
extern string octave_bin_dir (void);
extern string default_exec_path (void);
extern string default_path (void);
extern string default_info_file (void);
extern string default_info_prog (void);
extern string default_editor (void);
extern string get_local_site_defaults (void);
extern string get_site_defaults (void);

// Symbol table for symbols at the top level.
extern symbol_table *top_level_sym_tab;

// Symbol table for the current scope.
extern symbol_table *curr_sym_tab;

// Symbol table for global symbols.
extern symbol_table *global_sym_tab;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
