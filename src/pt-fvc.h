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
class tree_constant;
class tree_function;

class tree_walker;

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

  octave_value assign (const octave_value& t);
  octave_value assign (const octave_value_list& args, const octave_value& t);

#if 0
  octave_value assign (SLList<string> list, const octave_value& t);
  octave_value assign (SLList<string> list, const octave_value_list& args,
		       const octave_value& t);
#endif

  bool is_defined (void);

  void increment (void);

  void decrement (void);

  tree_fvc *do_lookup (bool& script_file_executed, bool exec_script = true);

  void link_to_global (void);

  void mark_as_formal_parameter (void);

  void mark_for_possible_ans_assign (void)
    { maybe_do_ans_assign = true; }

  octave_value eval (bool print);

  octave_value_list eval (bool print, int nargout, const octave_value_list& args);

  void eval_undefined_error (void);

  void accept (tree_walker& tw);

  octave_value value (void) const;

  octave_value& reference (void);

private:

  // The symbol record that this identifier references.
  symbol_record *sym;

  // True if we should consider assigning the result of evaluating
  // this identifier to the built-in variable ans.
  bool maybe_do_ans_assign;
};

// Indirect references to values (structure references).

class
tree_indirect_ref : public tree_fvc
{
public:

  tree_indirect_ref (int l = -1, int c = -1)
    : tree_fvc (l, c), id (0), indir (0), nm (),
      preserve_ident (false), preserve_indir (false),
      maybe_do_ans_assign (false) { }

  tree_indirect_ref (tree_identifier *i, int l = -1, int c = -1)
    : tree_fvc (l, c), id (i), indir (0), nm (),
      preserve_ident (false), preserve_indir (false),
      maybe_do_ans_assign (false) { }

  tree_indirect_ref (tree_indirect_ref *i, const string& n,
		     int l = -1, int c = -1)
    : tree_fvc (l, c), id (0), indir (i), nm (n),
      preserve_ident (false), preserve_indir (false),
      maybe_do_ans_assign (false) { }

  ~tree_indirect_ref (void);

  bool is_indirect_ref (void) const
    { return true; }

  bool is_identifier_only (void) const
    { return (id && nm.empty ()); }

  tree_identifier *ident (void)
    { return id; }

  tree_indirect_ref *indirect (void)
    { return indir; }

  void preserve_identifier (void)
    { preserve_ident = true; }

  void preserve_indirect (void)
    { preserve_indir = true; }

  void mark_for_possible_ans_assign (void)
    {
      maybe_do_ans_assign = true;

      if (is_identifier_only ())
	id->mark_for_possible_ans_assign ();
    }

  string name (void) const;

#if 0
  octave_value& assign (const octave_value& t);
  octave_value& assign (const octave_value_list& args, const octave_value& t);
#endif

  octave_value eval (bool print);

  octave_value_list eval (bool print, int nargout,
			  const octave_value_list& args);

  octave_value value (void) const;
  octave_value& reference (void);

  string elt_name (void)
    { return nm; }

  void accept (tree_walker& tw);

private:

  // The identifier for this structure reference.  For example, in
  // a.b.c, a is the id.
  tree_identifier *id;

  // This element just points to another indirect reference.
  tree_indirect_ref *indir;

  // The sub-element name.
  string nm;

  // True if we should not delete the identifier.
  bool preserve_ident;

  // True if we should not delete the indirect reference.
  bool preserve_indir;

  // True if we should consider assigning the result of evaluating
  // this identifier to the built-in variable ans.
  bool maybe_do_ans_assign;
};

// Builtin functions.

class
tree_builtin : public tree_fvc
{
public:

  tree_builtin (const string& nm = string ());

  tree_builtin (const builtin_mapper_function& m_fcn,
		const string& nm = string ());

  tree_builtin (Octave_builtin_fcn f, const string& nm = string ());

  ~tree_builtin (void) { }  // XXX ?? XXX

//  int is_builtin (void) const;

  bool is_mapper_function (void) const
    { return is_mapper; }

  octave_value eval (bool print);

  octave_value_list eval (bool print, int nargout, const octave_value_list& args);

  string name (void) const
    { return my_name; }

  void accept (tree_walker& tw);

private:

  // True if this is a mapper function.
  bool is_mapper;

  // A structure describing the mapper function.
  builtin_mapper_function mapper_fcn;

  // The actual function, if it is not a mapper.
  Octave_builtin_fcn fcn;

  // The name of this function.
  string my_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
