// variables.h                                              -*- C++ -*-
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

#if !defined (octave_variables_h)
#define octave_variables_h 1

class istream;
class ostrstream;
class symbol_record;
class symbol_table;
class tree;
class tree_fvc;
class tree_constant;
class Octave_object;

struct builtin_mapper_function;
struct builtin_function;
struct builtin_variable;

typedef int (*sv_Function)(void);

struct builtin_variable
{
  char *name;
  tree_constant *value;
  int install_as_function;
  int protect;
  int eternal;
  sv_Function sv_function;
  char *help_string;
};

typedef Octave_object (*Octave_builtin_fcn)(const Octave_object&, int);

struct builtin_function
{
  char *name;
  int nargin_max;
  int nargout_max;
  int is_text_fcn;
  Octave_builtin_fcn fcn;
  char *help_string;
};

extern void initialize_symbol_tables (void);

extern int lookup (symbol_record *s, int exec_script = 1);

extern symbol_record *lookup_by_name (const char *nm, int exec_script = 1);

extern char *get_help_from_file (const char *f);

extern char *builtin_string_variable (const char *);
extern int builtin_real_scalar_variable (const char *, double&);
extern tree_constant builtin_any_variable (const char *);

extern void link_to_global_variable (symbol_record *sr);
extern void link_to_builtin_variable (symbol_record *sr);
extern void link_to_builtin_or_function (symbol_record *sr);

extern void force_link_to_function (const char *s);

extern int is_builtin_variable (const char *name);
extern int is_text_function_name (const char *name);
extern int is_globally_visible (const char *name);

extern tree_fvc *is_valid_function (const tree_constant&, char *,
				    int warn = 0); 
extern int takes_correct_nargs (tree_fvc *, int, char *, int warn = 0);

extern char **make_name_list (void);

extern void install_builtin_mapper (builtin_mapper_function *mf);

extern void install_builtin_function (builtin_function *gf);

extern void install_builtin_variable (builtin_variable *v);

extern void install_builtin_variable_as_function (const char *name,
						  tree_constant *val,
						  int protect = 0,
						  int eternal = 0,
						  const char *help = 0);

extern void alias_builtin (const char *alias, const char *name);

#if 0
extern void bind_nargin_and_nargout (symbol_table *sym_tab,
				     int nargin, int nargout);
#endif

extern void bind_ans (const tree_constant& val, int print);

extern void bind_builtin_variable (const char *, tree_constant *,
				   int protect = 0, int eternal = 0,
				   sv_Function f = (sv_Function) 0,
				   const char *help = 0);

extern void bind_builtin_variable (const char *, const tree_constant&,
				   int protect = 0, int eternal = 0,
				   sv_Function f = (sv_Function) 0,
				   const char *help = 0);

extern void install_builtin_variables (void);

extern char *maybe_add_default_load_path (const char *p);

extern char *octave_lib_dir (void);
extern char *octave_arch_lib_dir (void);
extern char *octave_bin_dir (void);
extern char *default_path (void);
extern char *default_info_file (void);
extern char *default_editor (void);
extern char *get_site_defaults (void);

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
