// variables.h                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#if !defined (_variables_h)
#define _variables_h 1

class istream;
class symbol_record;
class symbol_table;
class tree;
class tree_constant;

struct builtin_mapper_functions;
struct builtin_text_functions;
struct builtin_general_functions;
struct builtin_string_variables;

#ifndef SV_FUNCTION_TYPEDEFS
#define SV_FUNCTION_TYPEDEFS 1

typedef int (*sv_Function)(void);

#endif

extern void initialize_symbol_tables (void);

extern int symbol_out_of_date (symbol_record *sr);

extern void document_symbol (const char *name, const char *help);

extern void install_builtin_mapper_function (builtin_mapper_functions *mf);

extern void install_builtin_text_function (builtin_text_functions *tf);

extern void install_builtin_general_function (builtin_general_functions *gf);

extern void install_builtin_variable (builtin_string_variables *sv);

extern void install_builtin_variable_as_function (const char *name,
						  tree_constant *val,
						  int protect = 0,
						  int eternal = 0);  

extern void bind_nargin_and_nargout (symbol_table *sym_tab,
				     int nargin, int nargout);

extern void bind_builtin_variable (const char *, tree_constant *,
				   int protect = 0, int eternal = 0,
				   sv_Function f = (sv_Function) 0,
				   const char *help = (char *) 0);

extern char *builtin_string_variable (const char *);
extern int builtin_real_scalar_variable (const char *, double&);

extern void link_to_global_variable (symbol_record *sr);
extern void link_to_builtin_variable (symbol_record *sr);
extern void link_to_builtin_or_function (symbol_record *sr);

extern void force_link_to_function (const char *s);

extern int is_globally_visible (const char *nm);

extern char *extract_keyword (istream&, char *);
extern int extract_keyword (istream&, char *, int&);

extern void skip_comments (istream&);
extern int valid_identifier (char *);
extern int identifier_exists (char *);
extern int is_builtin_variable (const char *name);
extern tree *is_valid_function (tree_constant&, char *, int warn = 0);
extern int takes_correct_nargs (tree *, int, char *, int warn = 0);
extern char **make_name_list (void);

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
