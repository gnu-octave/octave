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
class symbol_table;

/*
 * Variables or functions.
 */
class symbol_def
{
  friend class symbol_record;

public:

  enum symbol_type
    {
      unknown_type,
      variable,
      builtin_function,
      user_function,
    };

  enum symbol_lifespan
    {
      temporary,
      eternal,
    };

  enum symbol_class
    {
      read_write,
      read_only,
    };

  symbol_def (void);
  symbol_def (tree_constant *t);
  symbol_def (tree_builtin *t);
  symbol_def (tree_function *t);

  ~symbol_def (void);

  void define (tree_constant *t);
  void define (tree_builtin *t);
  void define (tree_function *t);

  tree *def (void);
  char *help (void);
  void document (char *h);

  int save (ostream& os, int mark_as_global);

private:

  char *help_string;
  symbol_lifespan lifespan;
  symbol_class sym_class;
  symbol_type type;
  tree *definition;
  int count;
  int preserve;

  symbol_def (const symbol_def& sd);
  symbol_def& operator = (const symbol_def& sd);
};

/*
 * Individual records in a symbol table.
 */
class
symbol_record
{
 friend class symbol_table;

public:
  symbol_record (void);
  symbol_record (char *n);
  symbol_record (char *n, symbol_record *nxt);

 ~symbol_record (void);

  char *name (void);
  char *help (void); 
  tree *def (void);

  int is_function (void);
  int is_variable (void);

  int is_defined (void);

  void set_sv_function (sv_Function f);

  int var_read_only (void);
  int read_only (void);

  int define (tree_constant *t);
  int define (tree_builtin *t);
  int define (tree_function *t);
  int define_as_fcn (tree_constant *t);

  void document (char *h);

  void protect (void);
  void unprotect (void);
  void make_eternal (void);

  int save (ostream& os, int mark_as_global = 0);

  void clear_visible (void);
  void clear_all (void);
  void undefine (void);

  void mark_as_formal_parameter (void);
  int is_formal_parameter (void);

  void mark_as_forced_global (void);
  int is_forced_global (void);

  void alias (symbol_record *s, int force = 0);

  symbol_record *next (void);

private:

  char *nm;
  int formal_param;
  int forced_global;
  symbol_def *var;
  symbol_def *fcn;
  sv_Function sv_fcn;
  symbol_record *next_elem;

  symbol_record (const symbol_record& s);
  symbol_record& operator = (const symbol_record& s);
};

/*
 * A symbol table.
 */
class
symbol_table
{
public:

  symbol_table (void);

  symbol_record *lookup (char *nm, int insert = 0, int warn = 0);

  void clear (void);
  int clear (char *nm);
  void undefine (void);

  void bind_globals (void);

  int save (ostream& os, int mark_as_global = 0);
  int save (ostream& os, char *name, int mark_as_global = 0);

  int size (void);

  char **list (void);
  char **var_list (void);
  char **fcn_list (void);

  char **list (int& count);
  char **var_list (int& count);
  char **fcn_list (int& count);

  char **sorted_list (void);
  char **sorted_var_list (void);
  char **sorted_fcn_list (void);

  char **sorted_list (int& count);
  char **sorted_var_list (int& count);
  char **sorted_fcn_list (int& count);

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
