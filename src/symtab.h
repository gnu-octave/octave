// Symbol table classes.                              -*- C++ -*-
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

#if !defined (_symtab_h)
#define _symtab_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include <assert.h>

#ifndef SV_FUNCTION_TYPEDEFS
#define SV_FUNCTION_TYPEDEFS 1

typedef int (*sv_Function)(void);

#endif

#define HASH_TABLE_SIZE 1024             /* Must be multiple of 2 */
#define HASH_MASK (HASH_TABLE_SIZE - 1)

class tree;
class tree_builtin;
class tree_constant;
class tree_function;

class symbol_def;
class symbol_record;
class symbol_record_info;
class symbol_table;

/*
 * Variables or functions.
 */
class symbol_def
{
  friend class symbol_record;
  friend class symbol_record_info;

public:

  symbol_def (void);
  symbol_def (tree_constant *t);
  symbol_def (tree_builtin *t);
  symbol_def (tree_function *t);

  ~symbol_def (void);

  int is_variable (void) const;
  int is_function (void) const;
  int is_user_variable (void) const;
  int is_user_function (void) const;
  int is_builtin_variable (void) const;
  int is_builtin_function (void) const;

  void define (tree_constant *t);
  void define (tree_builtin *t);
  void define (tree_function *t);

  void protect (void);
  void unprotect (void);
  void make_eternal (void);

  tree *def (void) const;
  char *help (void) const;
  void document (const char *h);

  int save (ostream& os, int mark_as_global);

  enum TYPE
    {
      UNKNOWN = 0,
      USER_FUNCTION = 1,
      USER_VARIABLE = 2,
      BUILTIN_FUNCTION = 4,
      BUILTIN_VARIABLE = 8
    };

  friend maybe_delete (symbol_def *def);

private:

  unsigned type : 4;
  unsigned eternal : 1;
  unsigned read_only : 1;

  char *help_string;
  tree *definition;
  symbol_def *next_elem;
  int count;

  void init_state (void);

  symbol_def (const symbol_def& sd);
  symbol_def& operator = (const symbol_def& sd);
};

/*
 * Individual records in a symbol table.
 */
class
symbol_record
{
  friend class symbol_record_info;

public:
  symbol_record (void);
  symbol_record (const char *n, symbol_record *nxt = (symbol_record *) NULL);

 ~symbol_record (void);

  char *name (void) const;
  char *help (void) const; 
  tree *def (void) const;

  void rename (const char *n);

  int is_function (void) const;
  int is_user_function (void) const;
  int is_builtin_function (void) const;
  int is_variable (void) const;
  int is_user_variable (void) const;
  int is_builtin_variable (void) const;

  unsigned type (void) const;

  int is_defined (void) const;
  int is_read_only (void) const;
  int is_eternal (void) const;

  void protect (void);
  void unprotect (void);
  void make_eternal (void);

  void set_sv_function (sv_Function f);

  int define (tree_constant *t);
  int define (tree_builtin *t);
  int define (tree_function *t);
  int define_as_fcn (tree_constant *t);
  int define_builtin_var (tree_constant *t);

  void document (const char *h);

  int save (ostream& os, int mark_as_global = 0);

  int clear (void);

  void alias (symbol_record *s, int force = 0);

  void mark_as_formal_parameter (void);
  int is_formal_parameter (void) const;

  void mark_as_linked_to_global (void);
  int is_linked_to_global (void) const;

  symbol_record *next (void) const;

  void chain (symbol_record *s);

private:

  unsigned formal_param : 1;
  unsigned linked_to_global : 1;

  char *nm;
  sv_Function sv_fcn;
  symbol_def *definition;
  symbol_record *next_elem;

  void init_state (void);

  int read_only_error (void);

  void push_def (symbol_def *sd);
  symbol_def *pop_def (void);

  symbol_record& operator = (const symbol_record& s);
};

/*
 * A structure for handling verbose information about a symbol_record.
 */

class
symbol_record_info
{
public:

  symbol_record_info (void);
  symbol_record_info (const symbol_record& s);

  symbol_record_info (const symbol_record_info& s);

  ~symbol_record_info (void);

  symbol_record_info& operator = (const symbol_record_info& s);

  int is_defined (void) const;
  int is_read_only (void) const;
  int is_eternal (void) const;
  int hides_fcn (void) const;
  int hides_builtin (void) const;
  char *type_as_string (void) const;
  int is_function (void) const;
  int rows (void) const;
  int columns (void) const;
  char *name (void) const;

  enum HIDES
    {
      SR_INFO_NONE = 0,
      SR_INFO_USER_FUNCTION = 1,
      SR_INFO_BUILTIN_FUNCTION = 2
    };

  enum CONST_TYPE
    {
      SR_INFO_UNKNOWN = 0,
      SR_INFO_SCALAR = 1,
      SR_INFO_COMPLEX_SCALAR = 2,
      SR_INFO_MATRIX = 4,
      SR_INFO_COMPLEX_MATRIX = 8,
      SR_INFO_RANGE = 16,
      SR_INFO_STRING = 32
    };

private:

  void init_state (void);

  unsigned type : 4;
  unsigned const_type : 6;
  unsigned hides : 2;
  unsigned eternal : 1;
  unsigned read_only : 1;
  int nr;
  int nc;
  char *nm;
  
  int initialized;
};

/*
 * A symbol table.
 */

#define SYMTAB_LOCAL_SCOPE 1
#define SYMTAB_GLOBAL_SCOPE 2

#define SYMTAB_ALL_SCOPES (SYMTAB_LOCAL_SCOPE | SYMTAB_GLOBAL_SCOPE)

#define SYMTAB_ALL_TYPES (symbol_def::USER_FUNCTION \
			  | symbol_def::USER_VARIABLE \
			  | symbol_def::BUILTIN_FUNCTION \
			  | symbol_def::BUILTIN_VARIABLE)

class
symbol_table
{
public:

  symbol_table (void);

  symbol_record *lookup (const char *nm, int insert = 0, int warn = 0);

  void clear (int clear_user_functions = 1);
  int clear (const char *nm, int clear_user_functions = 1);

  int save (ostream& os, int mark_as_global = 0);
  int save (ostream& os, const char *name, int mark_as_global = 0);

  int size (void) const;

  symbol_record_info *long_list (int& count, int sort = 0,
				 unsigned type = SYMTAB_ALL_TYPES,
				 unsigned scope = SYMTAB_ALL_SCOPES) const;

  char **list (int& count, int sort = 0,
	       unsigned type = SYMTAB_ALL_TYPES,
	       unsigned scope = SYMTAB_ALL_SCOPES) const;  

private:

  unsigned int hash (const char *s);

  symbol_record table[HASH_TABLE_SIZE];
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
