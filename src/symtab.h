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

#if !defined (octave_symtab_h)
#define octave_symtab_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

#include "SLStack.h"

#include "str-vec.h"

#include "ov.h"

// Must be multiple of 2.
#define HASH_TABLE_SIZE 1024
#define HASH_MASK (HASH_TABLE_SIZE - 1)

class octave_lvalue;

class string_vector;

class symbol_def;
class symbol_record;
class symbol_record_info;
class symbol_table;

// Variables or functions.

class symbol_def
{
  friend class symbol_record;
  friend class symbol_record_info;

public:

  symbol_def (void);

  symbol_def (const octave_value& val, unsigned int sym_type = 0);

  ~symbol_def (void) { }

  bool is_variable (void) const;
  bool is_function (void) const;
  bool is_text_function (void) const;
  bool is_mapper_function (void) const;
  bool is_user_variable (void) const;
  bool is_user_function (void) const;
  bool is_builtin_variable (void) const;
  bool is_builtin_function (void) const;
  bool is_map_element (const string& elts) const;

  void define (const octave_value& val, unsigned int sym_type);

  void protect (void);
  void unprotect (void);
  void make_eternal (void);

  octave_value& def (void);
  string help (void) const;
  void document (const string& h);

  enum TYPE
    {
      UNKNOWN = 0,
      USER_FUNCTION = 1,
      USER_VARIABLE = 2,
      BUILTIN_FUNCTION = 4,
      TEXT_FUNCTION = 8,
      MAPPER_FUNCTION = 16,
      BUILTIN_VARIABLE = 32
    };

  unsigned int symbol_type (void) { return type; }

  friend maybe_delete (symbol_def *def);

private:

  unsigned int type : 6;
  unsigned int eternal : 1;
  unsigned int read_only : 1;

  string help_string;
  octave_value definition;
  symbol_def *next_elem;
  int count;

  void init_state (void);

  symbol_def (const symbol_def& sd);
  symbol_def& operator = (const symbol_def& sd);
};

// Individual records in a symbol table.

class
symbol_record
{
  friend class symbol_record_info;

public:

  typedef int (*sv_function) (void);

  symbol_record (void);
  symbol_record (const string& n, symbol_record *nxt = 0);

  ~symbol_record (void) { }

  string name (void) const;
  string help (void) const; 

  octave_value& def (void);

  void rename (const string& new_name);

  bool is_function (void) const;
  bool is_user_function (void) const;
  bool is_text_function (void) const;
  bool is_mapper_function (void) const;
  bool is_builtin_function (void) const;
  bool is_variable (void) const;
  bool is_user_variable (void) const;
  bool is_builtin_variable (void) const;
  bool is_map_element (const string& elts) const;

  unsigned int type (void) const;

  bool is_defined (void) const;
  bool is_read_only (void) const;
  bool is_eternal (void) const;

  void protect (void);
  void unprotect (void);
  void make_eternal (void);

  void set_sv_function (sv_function f);

  int define (const octave_value& v,
	      unsigned int sym_type = symbol_def::USER_VARIABLE);

  int define_as_fcn (const octave_value& v);

  int define_builtin_var (const octave_value& v);

  int define (octave_function *f, unsigned int sym_type);

  void document (const string& h);

  int clear (void);

  void alias (symbol_record *s, bool force = false);

  void mark_as_formal_parameter (void);
  bool is_formal_parameter (void) const;

  void mark_as_linked_to_global (void);
  bool is_linked_to_global (void) const;

  void mark_as_static (void);
  bool is_static (void) const;

  octave_value& variable_value (void);
  octave_lvalue variable_reference (void);

  symbol_record *next (void) const;

  void chain (symbol_record *s);

  void push_context (void);
  void pop_context (void);

private:

  unsigned int formal_param : 1;
  unsigned int linked_to_global : 1;
  unsigned int tagged_static : 1;

  string nm;
  sv_function sv_fcn;
  symbol_def *definition;
  symbol_record *next_elem;

// This should maybe be one stack with a structure containing all the
// items we need to save for recursive calls...
  SLStack <symbol_def *> context;
  SLStack <unsigned int> global_link_context;

  void init_state (void);

  int read_only_error (const char *action);

  void push_def (symbol_def *sd);
  symbol_def *pop_def (void);

  symbol_record& operator = (const symbol_record& s);
};

// A structure for handling verbose information about a symbol_record.

class
symbol_record_info
{
public:

  symbol_record_info (void);
  symbol_record_info (symbol_record& s);

  symbol_record_info (const symbol_record_info& s);

  ~symbol_record_info (void) { }

  symbol_record_info& operator = (const symbol_record_info& s);

  bool is_defined (void) const;
  bool is_read_only (void) const;
  bool is_eternal (void) const;
  bool hides_fcn (void) const;
  bool hides_builtin (void) const;
  string type_name (void) const;
  bool is_function (void) const;
  int rows (void) const;
  int columns (void) const;
  string name (void) const;

  enum HIDES
    {
      SR_INFO_NONE = 0,
      SR_INFO_USER_FUNCTION = 1,
      SR_INFO_BUILTIN_FUNCTION = 2
    };

private:

  bool initialized;
  int nr;
  int nc;
  unsigned int type : 6;
  unsigned int hides : 2;
  unsigned int eternal : 1;
  unsigned int read_only : 1;
  string nm;
  string const_type;
};

// A symbol table.

#define SYMTAB_LOCAL_SCOPE 1
#define SYMTAB_GLOBAL_SCOPE 2

#define SYMTAB_ALL_SCOPES (SYMTAB_LOCAL_SCOPE | SYMTAB_GLOBAL_SCOPE)

#define SYMTAB_ALL_TYPES (symbol_def::USER_FUNCTION \
			  | symbol_def::USER_VARIABLE \
			  | symbol_def::BUILTIN_FUNCTION \
			  | symbol_def::TEXT_FUNCTION \
			  | symbol_def::MAPPER_FUNCTION \
			  | symbol_def::BUILTIN_VARIABLE)

#define SYMTAB_VARIABLES (symbol_def::USER_VARIABLE \
			  | symbol_def::BUILTIN_VARIABLE)

class
symbol_table
{
public:

  symbol_table (void);

  symbol_record *lookup (const string& nm, bool insert = false,
			 bool warn = false);

  void rename (const string& old_name, const string& new_name);

  void clear (bool clear_user_functions = true);
  int clear (const string& nm, bool clear_user_functions = true);

  int size (void) const;

  symbol_record_info *
  long_list (int& count, const string_vector& pats = string_vector (),
	     int npats = 0, bool sort = false,
	     unsigned int type = SYMTAB_ALL_TYPES,
	     unsigned int scope = SYMTAB_ALL_SCOPES) const;

  string_vector
  list (int& count, const string_vector& pats = string_vector (),
	int npats = 0, bool sort = false,
	unsigned int type = SYMTAB_ALL_TYPES,
	unsigned int scope = SYMTAB_ALL_SCOPES) const;

  symbol_record **glob (int& count, const string& pat = string ("*"),
			unsigned int type = SYMTAB_ALL_TYPES,
			unsigned int scope = SYMTAB_ALL_SCOPES) const;

  void push_context (void);
  void pop_context (void);

private:

  unsigned int hash (const string& s);

  symbol_record table[HASH_TABLE_SIZE];
};

extern bool valid_identifier (const char *s);

extern bool valid_identifier (const string& s);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
