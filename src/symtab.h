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

#include <cassert>

#include <string>

#include "SLStack.h"

#include "oct-alloc.h"
#include "str-vec.h"

#include "ov.h"

class octave_lvalue;

class string_vector;

class symbol_record;
class symbol_table;

// Individual records in a symbol table.

class
symbol_record
{
public:

  // If you add or delete an entry here, you'll also need to change
  // the width parameter in the declaration for symbol_type below...

  enum TYPE
    {
      UNKNOWN = 0,
      USER_FUNCTION = 1,
      USER_VARIABLE = 2,
      DLD_FUNCTION = 4,
      BUILTIN_FUNCTION = 8,
      TEXT_FUNCTION = 16,
      MAPPER_FUNCTION = 32,
      BUILTIN_VARIABLE = 64,
      BUILTIN_CONSTANT = 128
    };

private:

  // Variables or functions.

  class symbol_def
  {
  public:

    symbol_def (const octave_value& val = octave_value (),
		unsigned int sym_type = 0)
      : symbol_type (sym_type), eternal (0), read_only (0), help_string (),
	definition (val), next_elem (0), count (1) { }

    ~symbol_def (void) { }

    bool is_constant (void) const
      { return (symbol_type & symbol_record::BUILTIN_CONSTANT); }

    bool is_variable (void) const
      {
	return (symbol_type & symbol_record::USER_VARIABLE
		|| symbol_type & symbol_record::BUILTIN_VARIABLE);
      }

    bool is_function (void) const
      {
	return (symbol_type & symbol_record::USER_FUNCTION
		|| symbol_type & symbol_record::DLD_FUNCTION
		|| symbol_type & symbol_record::BUILTIN_FUNCTION);
      }

    bool is_user_variable (void) const
      { return (symbol_type & symbol_record::USER_VARIABLE); }

    bool is_text_function (void) const
      { return (symbol_type & symbol_record::TEXT_FUNCTION); }

    bool is_mapper_function (void) const
      { return (symbol_type & symbol_record::MAPPER_FUNCTION); }

    bool is_user_function (void) const
      { return (symbol_type & symbol_record::USER_FUNCTION); }

    bool is_builtin_constant (void) const
      { return (symbol_type & symbol_record::BUILTIN_CONSTANT); }

    bool is_builtin_variable (void) const
      { return (symbol_type & symbol_record::BUILTIN_VARIABLE); }

    bool is_builtin_function (void) const
      { return (symbol_type & symbol_record::BUILTIN_FUNCTION); }

    bool is_dld_function (void) const
      { return (symbol_type & symbol_record::DLD_FUNCTION); }

    // XXX FIXME XXX
    bool is_map_element (const string& /* elts */) const
      { return false; }

    bool is_defined (void) const
      { return definition.is_defined (); }

    bool is_read_only (void) const
      { return read_only; }

    bool is_eternal (void) const
      { return eternal; }

    int rows (void) const { return definition.rows (); }
    int columns (void) const { return definition.columns (); }

    string type_name (void) const { return definition.type_name (); }

    void define (const octave_value& val, unsigned int sym_type)
      {
	definition = val;
	symbol_type = sym_type;
      }

    void protect (void) { read_only = 1; }

    void unprotect (void) { read_only = 0; }

    void make_eternal (void) { eternal = 1; }

    octave_value& def (void) { return definition; }

    string help (void) const { return help_string; }

    void document (const string& h) { help_string = h; }

    unsigned int type (void) { return symbol_type; }

    void *operator new (size_t size)
      { return allocator.alloc (size); }

    void operator delete (void *p, size_t size)
      { allocator.free (p, size); }

    static octave_allocator allocator;

    // The type of this symbol (see the enum above).
    unsigned int symbol_type : 8;

    // Nonzero means this variable cannot be cleared.
    unsigned int eternal : 1;

    // Nonzero means this variable cannot be given a new value.
    unsigned int read_only : 1;

    // The doc string associated with this variable.
    string help_string;

    // The value of this definition.  See ov.h and related files.
    octave_value definition;

    // Pointer to next definition in chain.  This is used so that
    // variables can hide function definitions, and so that the function
    // definitions can reappear if the variable is cleared.
    symbol_def *next_elem;

    // Reference count.
    int count;

    void dump_symbol_info (void);

    // No copying!

    symbol_def (const symbol_def& sd);

    symbol_def& operator = (const symbol_def& sd);
  };

public:

  typedef int (*change_function) (void);

  symbol_record (void)
    : formal_param (0), linked_to_global (0), tagged_static (0),
      nm (), chg_fcn (0), definition (new symbol_def ()),
      next_elem (0) { }

  symbol_record (const string& n, symbol_record *nxt)
    : formal_param (0), linked_to_global (0), tagged_static (0),
      nm (n), chg_fcn (0), definition (new symbol_def ()),
      next_elem (nxt) { }

  ~symbol_record (void) { }

  string name (void) const { return nm; }

  string help (void) const { return definition->help (); }

  octave_value& def (void) { return definition->def (); }

  void rename (const string& new_name);

  bool is_function (void) const
    { return definition->is_function (); }

  bool is_text_function (void) const
    { return definition->is_text_function (); }

  bool is_mapper_function (void) const
    { return definition->is_mapper_function (); }

  bool is_user_function (void) const
    { return definition->is_user_function (); }

  bool is_builtin_function (void) const
    { return definition->is_builtin_function (); }

  bool is_dld_function (void) const
    { return definition->is_dld_function (); }

  bool is_constant (void) const
    { return definition->is_constant (); }

  bool is_builtin_constant (void) const
    { return definition->is_builtin_constant (); }

  bool is_variable (void) const
    { return definition->is_variable (); }

  bool is_user_variable (void) const
    { return definition->is_user_variable (); }

  bool is_builtin_variable (void) const
    { return definition->is_builtin_variable (); }

  bool is_map_element (const string& elts) const
    { return definition->is_map_element (elts); }

  unsigned int type (void) const { return definition->type (); }

  bool is_defined (void) const { return definition->is_defined (); }

  bool is_read_only (void) const { return definition->is_read_only (); }

  bool is_eternal (void) const { return definition->is_eternal (); }

  void protect (void) { definition->protect (); }

  void unprotect (void) { definition->unprotect (); }

  void make_eternal (void) { definition->make_eternal (); }

  void set_change_function (change_function f) { chg_fcn = f; }

  void define (const octave_value& v, unsigned int sym_type = USER_VARIABLE);

  void define_builtin_var (const octave_value& v);

  bool define_builtin_const (const octave_value& v);

  bool define (octave_function *f, unsigned int sym_type);

  void document (const string& h) { definition->document (h); }

  void clear (void);

  void alias (symbol_record *s, bool force = false);

  void mark_as_formal_parameter (void);
  bool is_formal_parameter (void) const { return formal_param; }

  void mark_as_linked_to_global (void);
  bool is_linked_to_global (void) const { return linked_to_global; }

  void mark_as_static (void);
  bool is_static (void) const { return tagged_static; }

  bool hides_fcn (void) const;
  bool hides_builtin (void) const;

  int rows (void) const { return definition->rows (); }
  int columns (void) const { return definition->columns (); }

  string type_name (void) const { return definition->type_name (); }

  octave_value& variable_value (void);
  octave_lvalue variable_reference (void);

  symbol_record *next (void) const { return next_elem; }

  void chain (symbol_record *s) { next_elem = s; }

  void push_context (void);

  void pop_context (void);

  void print_symbol_info_line (ostream& os);

  void dump_symbol_info (void);

private:

  unsigned int formal_param : 1;
  unsigned int linked_to_global : 1;
  unsigned int tagged_static : 1;

  string nm;
  change_function chg_fcn;
  symbol_def *definition;
  symbol_record *next_elem;

  // This should maybe be one stack with a structure containing all the
  // items we need to save for recursive calls...
  SLStack <symbol_def *> context;
  SLStack <unsigned int> global_link_context;

  bool read_only_error (const char *action);

  void push_def (symbol_def *sd);

  void remove_top_def (void);

  void replace_all_defs (symbol_def *sd);

  void link_to_builtin_variable (void);

  // No copying!

  symbol_record (const symbol_record& s);

  symbol_record& operator = (const symbol_record& s);
};

// A symbol table.

#define SYMTAB_LOCAL_SCOPE 1
#define SYMTAB_GLOBAL_SCOPE 2

#define SYMTAB_ALL_SCOPES (SYMTAB_LOCAL_SCOPE | SYMTAB_GLOBAL_SCOPE)

#define SYMTAB_ALL_TYPES (symbol_record::USER_FUNCTION \
			  | symbol_record::USER_VARIABLE \
			  | symbol_record::DLD_FUNCTION \
			  | symbol_record::BUILTIN_FUNCTION \
			  | symbol_record::TEXT_FUNCTION \
			  | symbol_record::MAPPER_FUNCTION \
			  | symbol_record::BUILTIN_VARIABLE \
			  | symbol_record::BUILTIN_CONSTANT)

#define SYMTAB_VARIABLES (symbol_record::USER_VARIABLE \
			  | symbol_record::BUILTIN_VARIABLE)

class
symbol_table
{
public:

  symbol_table (unsigned int tab_size = 128)
    : table_size (tab_size), table (new symbol_record [table_size])
  {
    assert ((tab_size % 2) == 0);
  }

  ~symbol_table (void)
  {
    delete [] table;
  }

  symbol_record *lookup (const string& nm, bool insert = false,
			 bool warn = false);

  void rename (const string& old_name, const string& new_name);

  void clear (bool clear_user_functions = true);
  bool clear (const string& nm, bool clear_user_functions = true);

  int size (void) const;

  Array<symbol_record *>
  symbol_list (int& count, const string_vector& pats = string_vector (),
	       unsigned int type = SYMTAB_ALL_TYPES,
	       unsigned int scope = SYMTAB_ALL_SCOPES) const;


  string_vector
  name_list (int& count, const string_vector& pats = string_vector (),
	     bool sort = false, unsigned int type = SYMTAB_ALL_TYPES,
	     unsigned int scope = SYMTAB_ALL_SCOPES) const;


  int maybe_list (const char *header, const string_vector& argv,
		  ostream& os, bool show_verbose,
		  unsigned type, unsigned scope);
  
  symbol_record **glob (int& count, const string& pat = string ("*"),
			unsigned int type = SYMTAB_ALL_TYPES,
			unsigned int scope = SYMTAB_ALL_SCOPES) const;

  void push_context (void);

  void pop_context (void);

  void print_stats (void);

private:

  unsigned int table_size;

  symbol_record *table;

  unsigned int hash (const string& s);

  // No copying!

  symbol_table (const symbol_table&);

  symbol_table& operator = (const symbol_table&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
