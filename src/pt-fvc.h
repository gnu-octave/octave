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

#if !defined (octave_tree_fvc2_h)
#define octave_tree_fvc2_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include <SLList.h>

class symbol_record;
class tree_function;

#include "mappers.h"
#include "pt-fvc-base.h"
#include "variables.h"

// Symbols from the symbol table.

class
tree_identifier : public tree_fvc
{
  friend class tree_index_expression;

public:
  tree_identifier (int l = -1, int c = -1)
    : tree_fvc (l, c), sym (0), maybe_do_ans_assign (false) { }

  tree_identifier (symbol_record *s, int l = -1, int c = -1)
    : tree_fvc (l, c), sym (s), maybe_do_ans_assign (false) { }

  ~tree_identifier (void) { }

  bool is_identifier (void) const
    { return true; }

  string name (void) const;

  tree_identifier *define (tree_constant *t);
  tree_identifier *define (tree_function *t);

  void document (const string& s);

  tree_constant assign (tree_constant& t);
  tree_constant assign (tree_constant& t, const Octave_object& args);

  tree_constant assign (SLList<string> list, tree_constant& t);
  tree_constant assign (SLList<string> list, tree_constant& t,
			const Octave_object& args); 

  bool is_defined (void);

  void bump_value (tree_expression::type);

  tree_fvc *do_lookup (bool& script_file_executed, bool exec_script = true);

  void link_to_global (void);

  void mark_as_formal_parameter (void);

  void mark_for_possible_ans_assign (void)
    { maybe_do_ans_assign = true; }

  tree_constant eval (bool print);

  Octave_object eval (bool print, int nargout, const Octave_object& args);

  void eval_undefined_error (void);

  void print_code (ostream& os);

private:
  symbol_record *sym;
  bool maybe_do_ans_assign;
};

// Indirect references to values (structure references).

class
tree_indirect_ref : public tree_fvc
{
public:
  tree_indirect_ref (int l = -1, int c = -1)
    : tree_fvc (l, c), id (0), preserve_ident (false) { }

  tree_indirect_ref (tree_identifier *i, int l = -1, int c = -1)
    : tree_fvc (l, c), id (i), preserve_ident (false) { }

  ~tree_indirect_ref (void);

  tree_indirect_ref *chain (const string& s);

  bool is_indirect_ref (void) const
    { return true; }

  bool is_identifier_only (void) const
    { return (id && refs.empty ()); }

  tree_identifier *ident (void)
    { return id; }

  void preserve_identifier (void)
    { preserve_ident = true; }

  string name (void) const;

  tree_constant assign (tree_constant& t);
  tree_constant assign (tree_constant& t, const Octave_object& args);

  void mark_for_possible_ans_assign (void)
    { id->mark_for_possible_ans_assign (); }

  tree_constant eval (bool print);

  Octave_object eval (bool print, int nargout, const Octave_object& args);

  void print_code (ostream& os);

private:
  tree_identifier *id;
  SLList<string> refs;
  bool preserve_ident;
};

// Builtin functions.

class
tree_builtin : public tree_fvc
{
public:
  tree_builtin (const string& nm = string ());

  tree_builtin (Mapper_fcn& m_fcn, const string& nm = string ());

  tree_builtin (Octave_builtin_fcn f, const string& nm = string ());

  ~tree_builtin (void) { }  // XXX ?? XXX

//  int is_builtin (void) const;

  bool is_mapper_function (void) const
    { return is_mapper; }

  tree_constant eval (bool print);

  Octave_object eval (bool print, int nargout, const Octave_object& args);

  string name (void) const
    { return my_name; }

  void print_code (ostream& os);

private:
  bool is_mapper;
  Mapper_fcn mapper_fcn;
  Octave_builtin_fcn fcn;
  string my_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
