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

#if !defined (octave_tree_identifier_h)
#define octave_tree_identifier_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include <string>

class octave_symbol;
class octave_function;
class symbol_record;

class tree_walker;

#include "pt-mvr-base.h"

// Symbols from the symbol table.

class
tree_identifier : public tree_multi_val_ret
{
  friend class tree_index_expression;

public:

  tree_identifier (int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), sym (0), maybe_do_ans_assign (false) { }

  tree_identifier (symbol_record *s, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), sym (s), maybe_do_ans_assign (false) { }

  ~tree_identifier (void) { }

  bool is_identifier (void) const
    { return true; }

  string name (void) const;

  tree_identifier *define (octave_symbol *s, unsigned int sym_type);

  tree_identifier *define (octave_function *f, unsigned int sym_type);

  tree_identifier *define (const octave_value& v);

  void document (const string& s);

  octave_value assign (octave_value::assign_op op,
		       const octave_value& t);

  octave_value assign (octave_value::assign_op op,
		       const octave_value_list& args,
		       const octave_value& t);

  bool is_defined (void);

  void increment (void);

  void decrement (void);

  octave_symbol *do_lookup (bool& script_file_executed, bool
			    exec_script = true);

  void link_to_global (void);

  void mark_as_static (void);

  void mark_as_formal_parameter (void);

  void mark_for_possible_ans_assign (void)
    { maybe_do_ans_assign = true; }

  octave_value eval (bool print = false);

  octave_value_list eval (bool print, int nargout,
			  const octave_value_list& args); 

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

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
