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

#if !defined (octave_symtab_h)
#define octave_symtab_h 1

#include <cassert>

#include <string>
#include <stack>
#include <sstream>

#include "oct-alloc.h"
#include "str-vec.h"

#include "ov.h"

class octave_lvalue;

class string_vector;

class symbol_record;
class symbol_table;

struct
whos_parameter
{
  char command;
  char modifier;
  int parameter_length;
  int first_parameter_length;
  int dimensions;
  int balance;
  std::string text;
  std::string line;
};

// Individual records in a symbol table.

class
OCTINTERP_API
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
      COMMAND = 16,
      RAWCOMMAND = 32,
      MAPPER_FUNCTION = 64,
      MEX_FUNCTION = 128,
    };

private:

  // Variables or functions.

  class symbol_def
  {
  public:

    symbol_def (const octave_value& val = octave_value (),
		unsigned int sym_type = 0)
      : symbol_type (sym_type), eternal (0), read_only (0), help_string (),
	definition (val), count (1) { }

    ~symbol_def (void) { }

    bool is_variable (void) const
      { return (symbol_type & symbol_record::USER_VARIABLE); }

    // It's not necessary to check for COMMAND and MAPPER_FUNCTION
    // here.  Those tags are just used as additional qualifiers for
    // the other types of functions.

    bool is_function (void) const
      {
	return (symbol_type & symbol_record::USER_FUNCTION
		|| symbol_type & symbol_record::DLD_FUNCTION
		|| symbol_type & symbol_record::MEX_FUNCTION
		|| symbol_type & symbol_record::BUILTIN_FUNCTION);
      }

    bool is_user_variable (void) const
      { return (symbol_type & symbol_record::USER_VARIABLE); }

    // Don't use |= here to avoid error with AIX compiler.
    void mark_as_command (void)
      { symbol_type = symbol_type | symbol_record::COMMAND; }

    void unmark_command (void)
      { symbol_type &= ~symbol_record::COMMAND; }

    bool is_command (void) const
      { return (symbol_type & symbol_record::COMMAND); }

    void mark_as_rawcommand (void)
      { symbol_type |= (symbol_record::COMMAND
			| symbol_record::RAWCOMMAND); }

    void unmark_rawcommand (void)
      { symbol_type &= ~symbol_record::RAWCOMMAND; }

    bool is_rawcommand (void) const
      { return (symbol_type & symbol_record::RAWCOMMAND); }      

    bool is_mapper_function (void) const
      { return (symbol_type & symbol_record::MAPPER_FUNCTION); }

    bool is_user_function (void) const
      { return (symbol_type & symbol_record::USER_FUNCTION); }

    bool is_builtin_function (void) const
      { return (symbol_type & symbol_record::BUILTIN_FUNCTION); }

    bool is_dld_function (void) const
      { return (symbol_type & symbol_record::DLD_FUNCTION); }

    bool is_mex_function (void) const
      { return (symbol_type & symbol_record::MEX_FUNCTION); }

    // FIXME
    bool is_map_element (const std::string& /* elts */) const
      { return false; }

    bool is_defined (void) const
      { return definition.is_defined (); }

    bool is_read_only (void) const
      { return read_only; }

    bool is_eternal (void) const
      { return eternal; }

    bool is_matrix_type (void) const 
      { return definition.is_matrix_type (); }

    bool is_sparse_type (void) const
      { return definition.is_sparse_type (); }

    bool is_complex_type (void) const
      { return definition.is_complex_type (); }

    std::string class_name (void) const
      { return definition.class_name (); }

    Matrix size (void) const
      { return definition.size (); }

    size_t byte_size (void) const
      { return definition.byte_size (); };

    octave_idx_type numel (void) const
      { return definition.numel (); };

    octave_idx_type capacity (void) const
      { return definition.capacity (); };

    dim_vector dims (void) const 
      { return definition.dims (); }

    octave_idx_type rows (void) const { return definition.rows (); }
    octave_idx_type columns (void) const { return definition.columns (); }

    std::string type_name (void) const { return definition.type_name (); }

    std::string type_as_string (void) const;

    void type (std::ostream& os, const std::string& name, bool pr_type_info,
	       bool quiet, bool pr_orig_txt);

    std::string which (const std::string& name);

    void which (std::ostream& os, const std::string& name);

    void define (const octave_value& val, unsigned int sym_type)
      {
	definition = val;
	symbol_type = sym_type;
      }

    void protect (void) { read_only = 1; }

    void unprotect (void) { read_only = 0; }

    void make_eternal (void) { eternal = 1; }

    octave_value& def (void) { return definition; }

    std::string help (void) const { return help_string; }

    void document (const std::string& h);

    unsigned int type (void) { return symbol_type; }

    void *operator new (size_t size)
      { return allocator.alloc (size); }

    void operator delete (void *p, size_t size)
      { allocator.free (p, size); }

    static octave_allocator allocator;

    // The type of this symbol (see the enum above).
    unsigned int symbol_type : 9;

    // Nonzero means this variable cannot be cleared.
    unsigned int eternal : 1;

    // Nonzero means this variable cannot be given a new value.
    unsigned int read_only : 1;

    // The doc string associated with this variable.
    std::string help_string;

    // The value of this definition.  See ov.h and related files.
    octave_value definition;

    // Reference count.
    int count;

    void print_info (std::ostream& os,
		     const std::string& prefix = std::string ()) const;

    // No copying!

    symbol_def (const symbol_def& sd);

    symbol_def& operator = (const symbol_def& sd);
  };

public:

  typedef int (*change_function) (void);

  symbol_record (void)
    : formal_param (false), automatic_variable (false),
      linked_to_global (false), tagged_static (false),
      can_hide_function (true), visible (true), nm (), chg_fcn (0),
      definition (new symbol_def ()), next_elem (0) { }

  // FIXME -- kluge alert!  We obviously need a better way of
  // handling allow_shadow!

  symbol_record (const std::string& n, symbol_record *nxt)
    : formal_param (false), automatic_variable (false),
      linked_to_global (false), tagged_static (false),
      can_hide_function (n != "__end__"), visible (true), nm (n),
      chg_fcn (0), definition (new symbol_def ()), next_elem (nxt) { }

  ~symbol_record (void)
    {
      if (--definition->count <= 0)
	delete definition;
    }

  std::string name (void) const { return nm; }

  std::string help (void) const { return definition->help (); }

  octave_value& def (void) { return definition->def (); }

  void rename (const std::string& new_name);

  bool is_function (void) const
    { return definition->is_function (); }

  void mark_as_command (void)
    { definition->mark_as_command (); }

  void unmark_command (void)
    { definition->unmark_command (); }

  bool is_command (void) const
    { return definition->is_command (); }

  void mark_as_rawcommand (void)
    { definition->mark_as_rawcommand (); }

  void unmark_rawcommand (void)
    { definition->unmark_rawcommand (); }

  bool is_rawcommand (void) const
    { return definition->is_rawcommand (); }    

  bool is_mapper_function (void) const
    { return definition->is_mapper_function (); }

  bool is_user_function (void) const
    { return definition->is_user_function (); }

  bool is_builtin_function (void) const
    { return definition->is_builtin_function (); }

  bool is_dld_function (void) const
    { return definition->is_dld_function (); }

  bool is_mex_function (void) const
    { return definition->is_mex_function (); }

  bool is_variable (void) const
    { return definition->is_variable (); }

  bool is_user_variable (void) const
    { return definition->is_user_variable (); }

  bool is_map_element (const std::string& elts) const
    { return definition->is_map_element (elts); }

  unsigned int type (void) const { return definition->type (); }

  bool is_defined (void) const { return definition->is_defined (); }

  bool is_read_only (void) const { return definition->is_read_only (); }

  bool is_eternal (void) const { return definition->is_eternal (); }

  void protect (void) { definition->protect (); }

  void unprotect (void) { definition->unprotect (); }

  void make_eternal (void) { definition->make_eternal (); }

  void hide (void) { visible = false; }
  void show (void) { visible = true; }
  bool is_visible (void) const { return visible; }

  void set_change_function (change_function f) { chg_fcn = f; }

  void define (const octave_value& v, unsigned int sym_type = USER_VARIABLE);

  bool define (octave_function *f, unsigned int sym_type);

  void document (const std::string& h) { definition->document (h); }

  void clear (void);

  void alias (symbol_record *s, bool mark_to_clear = false);

  void mark_as_formal_parameter (void);
  bool is_formal_parameter (void) const { return formal_param; }

  void mark_as_automatic_variable (void);
  bool is_automatic_variable (void) const { return automatic_variable; }

  void mark_as_linked_to_global (void);
  bool is_linked_to_global (void) const { return linked_to_global; }

  void mark_as_static (void);
  bool is_static (void) const { return tagged_static; }
  void unmark_static (void) { tagged_static = false; }

  bool is_matrix_type (void) const 
    { return definition->is_matrix_type (); }

  bool is_sparse_type (void) const
    { return definition->is_sparse_type (); }

  bool is_complex_type (void) const
    { return definition->is_complex_type (); }

  std::string class_name (void) const
    { return definition->class_name (); }

  Matrix size (void) const
    { return definition->size (); }

  size_t byte_size (void) const
    { return definition->byte_size (); };

  octave_idx_type numel (void) const
    { return definition->numel (); };

  octave_idx_type capacity (void) const
    { return definition->capacity (); };

  dim_vector dims (void) const { return definition->dims (); }

  int dimensions_string_req_first_space (int print_dims) const;

  int dimensions_string_req_total_space (int print_dims) const;

  std::string make_dimensions_string (int print_dims) const;

  octave_idx_type rows (void) const { return definition->rows (); }
  octave_idx_type columns (void) const { return definition->columns (); }

  std::string type_name (void) const { return definition->type_name (); }

  std::string type_as_string (void) const
    { return definition->type_as_string (); }

  void type (std::ostream& os, bool pr_type_info, bool quiet, bool pr_orig_txt)
    { definition->type (os, name (), pr_type_info, quiet, pr_orig_txt); }

  std::string which (void) { return definition->which (name ()); }

  void which (std::ostream& os) { definition->which (os, name ()); }

  octave_value& variable_value (void);
  octave_lvalue variable_reference (void);

  symbol_record *next (void) const { return next_elem; }

  void chain (symbol_record *s) { next_elem = s; }

  void push_context (void);

  void pop_context (void);

  void print_symbol_info_line (std::ostream& os,
			       std::list<whos_parameter>& params) const;

  void print_info (std::ostream& os,
		   const std::string& prefix = std::string ()) const;

private:

  unsigned int formal_param : 1;
  unsigned int automatic_variable : 1;
  unsigned int linked_to_global : 1;
  unsigned int tagged_static : 1;
  unsigned int can_hide_function : 1;
  unsigned int visible : 1;

  std::string nm;
  change_function chg_fcn;
  symbol_def *definition;
  symbol_record *next_elem;

  // This should maybe be one stack with a structure containing all the
  // items we need to save for recursive calls...
  std::stack <symbol_def *> context;
  std::stack <unsigned int> global_link_context;

  std::stack <symbol_record *> aliases_to_clear;

  void push_alias_to_clear (symbol_record *s)
    { aliases_to_clear.push (s); }

  bool read_only_error (const char *action);

  void maybe_delete_def (void)
    {
      if (--definition->count <= 0)
        delete definition;
    }

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
			  | symbol_record::COMMAND \
  			  | symbol_record::RAWCOMMAND \
			  | symbol_record::MAPPER_FUNCTION \
			  | symbol_record::MEX_FUNCTION)

#define SYMTAB_VARIABLES (symbol_record::USER_VARIABLE)

class
OCTINTERP_API
symbol_table
{
public:

  symbol_table (unsigned int tab_size = 128,
		const std::string& nm = std::string ())
    : table_size (tab_size), table (new symbol_record [table_size]),
      table_name (nm)
    {
      assert ((tab_size % 2) == 0);

      if (table_name.empty ())
	{
	  std::ostringstream buf;
	  buf << symtab_count++;
	  table_name = buf.str ();
	}
    }

  ~symbol_table (void);

  symbol_record *lookup (const std::string& nm, bool insert = false,
			 bool warn = false);

  void rename (const std::string& old_name, const std::string& new_name);

  void clear (void);

  void clear_variables (void);
  void clear_functions (void);
  void clear_mex_functions (void);
  void clear_globals (void);

  bool clear (const std::string& nm);

  bool clear_variable (const std::string& nm);
  bool clear_function (const std::string& nm);
  bool clear_global (const std::string& nm);

  bool clear_variable_pattern (const std::string& pat);
  bool clear_function_pattern (const std::string& pat);
  bool clear_global_pattern (const std::string& pat);

  int size (void) const;

  Array<symbol_record *>
  subsymbol_list (const string_vector& pats = string_vector (),
		  unsigned int type = SYMTAB_ALL_TYPES,
		  unsigned int scope = SYMTAB_ALL_SCOPES) const;

  Array<symbol_record *>
  symbol_list (const string_vector& pats = string_vector (),
	       unsigned int type = SYMTAB_ALL_TYPES,
	       unsigned int scope = SYMTAB_ALL_SCOPES) const;


  string_vector
  name_list (const string_vector& pats = string_vector (),
	     bool sort = false, unsigned int type = SYMTAB_ALL_TYPES,
	     unsigned int scope = SYMTAB_ALL_SCOPES) const;

  string_vector
  user_function_name_list (void) const
    {
      return name_list
	(string_vector (), false,
	 symbol_record::USER_FUNCTION|symbol_record::DLD_FUNCTION|symbol_record::MEX_FUNCTION,
	 SYMTAB_ALL_SCOPES);
    }

  string_vector
  global_variable_name_list (void) const
    {
      return name_list
	(string_vector (), false, SYMTAB_VARIABLES, SYMTAB_GLOBAL_SCOPE);
    }

  string_vector
  variable_name_list (void) const
    {
      return name_list
	(string_vector (), false, SYMTAB_VARIABLES, SYMTAB_LOCAL_SCOPE);
    }

  int maybe_list (const char *header, const string_vector& argv,
		  std::ostream& os, bool show_verbose,
		  unsigned type, unsigned scope);
  
  Array<symbol_record *> glob (const std::string& pat = std::string ("*"),
			       unsigned int type = SYMTAB_ALL_TYPES,
			       unsigned int scope = SYMTAB_ALL_SCOPES) const;

  void push_context (void);

  void pop_context (void);

  // Create a new symbol table with the same entries.  Only the symbol
  // names and some attributes are copied, not values.
  symbol_table *dup (void);

  // Inherit some values from the parent_sym_tab.
  void inherit (symbol_table *parent_sym_tab);

  void print_info (std::ostream& os) const;

private:

  unsigned int table_size;

  symbol_record *table;

  std::string table_name;

  static unsigned long int symtab_count;

  void
  print_descriptor (std::ostream& os, 
		    std::list<whos_parameter> params) const;

  std::list<whos_parameter>
  parse_whos_line_format (Array<symbol_record *>& symbols) const;

  unsigned int hash (const std::string& s);

  // No copying!

  symbol_table (const symbol_table&);

  symbol_table& operator = (const symbol_table&);
};

// Defines layout for the whos/who -long command.
extern std::string Vwhos_line_format;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
