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

#ifdef __GNUG__
#pragma interface
#endif

class istream;
class symbol_record;
class symbol_table;
class tree;
class tree_constant;

extern int symbol_out_of_date (symbol_record *sr);
extern symbol_record *force_global (char *name);
extern int bind_variable (char *, tree_constant *);
extern int bind_protected_variable (char *, tree_constant *);
extern char *octave_string_variable (char *);
extern int octave_real_scalar_variable (char *, double&);
extern int extract_keyword (istream&, char *, char *);
extern int extract_keyword (istream&, char *, int&);
extern void skip_comments (istream&);
extern int valid_identifier (char *);
extern int identifier_exists (char *);
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
