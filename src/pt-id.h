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

#if !defined (octave_tree_identifier_h)
#define octave_tree_identifier_h 1

#include <iostream>
#include <string>

class octave_value;
class octave_value_list;
class octave_function;
class symbol_record;

class tree_walker;

#include "pt-exp.h"

// Symbols from the symbol table.

class
tree_identifier : public tree_expression
{
  friend class tree_index_expression;

public:

  tree_identifier (int l = -1, int c = -1)
    : tree_expression (l, c), sym (0) { }

  tree_identifier (symbol_record *s, int l = -1, int c = -1)
    : tree_expression (l, c), sym (s) { }

  ~tree_identifier (void) { }

  bool has_magic_end (void) const { return (name () == "__end__"); }

  bool is_identifier (void) const { return true; }

  std::string name (void) const;

  tree_identifier *define (octave_function *f, unsigned int sym_type);

  void document (const std::string& s);

  bool is_defined (void);

  bool is_function (void);

  octave_value
  do_lookup (bool& script_file_executed, bool exec_script = true);

  void link_to_global (void);

  void mark_as_static (void);

  void mark_as_formal_parameter (void);

  // We really need to know whether this symbol referst to a variable
  // or a function, but we may not know that yet.

  bool lvalue_ok (void) const { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  octave_lvalue lvalue (void);

  void eval_undefined_error (void);

  void accept (tree_walker& tw);

private:

  // The symbol record that this identifier references.
  symbol_record *sym;

  // No copying!

  tree_identifier (const tree_identifier&);

  tree_identifier& operator = (const tree_identifier&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
