/*

Copyright (C) 1996, 1997, 2000, 2002, 2003, 2004, 2005, 2006, 2007,
              2008, 2009 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_tree_expr_h)
#define octave_tree_expr_h 1

#include <string>

class octave_value;
class octave_lvalue;

#include "pt.h"
#include "symtab.h"

// A base class for expressions.

class
tree_expression : public tree
{
public:

  tree_expression (int l = -1, int c = -1)
    : tree (l, c), num_parens (0), postfix_indexed (false),
      print_flag (false) { }

  virtual ~tree_expression (void) { }

  virtual bool has_magic_end (void) const = 0;

  virtual tree_expression *dup (symbol_table::scope_id,
				symbol_table::context_id context) const = 0;

  virtual bool is_constant (void) const { return false; }

  virtual bool is_matrix_constant (void) const { return false; }

  virtual bool is_identifier (void) const { return false; }

  virtual bool is_index_expression (void) const { return false; }

  virtual bool is_assignment_expression (void) const { return false; }

  virtual bool is_prefix_expression (void) const { return false; }

  virtual bool is_unary_expression (void) const { return false; }

  virtual bool is_binary_expression (void) const { return false; }

  virtual bool is_boolean_expression (void) const { return false; }

  virtual bool is_logically_true (const char *);

  virtual bool lvalue_ok (void) const { return false; }

  virtual bool rvalue_ok (void) const { return false; }

  virtual octave_value rvalue1 (int nargout = 1);

  virtual octave_value_list rvalue (int nargout);

  virtual octave_lvalue lvalue (void);

  int paren_count (void) const { return num_parens; }

  bool is_postfix_indexed (void) const { return postfix_indexed; }

  bool print_result (void) const { return print_flag; }

  virtual std::string oper (void) const { return "<unknown>"; }

  virtual std::string name (void) const { return "<unknown>"; }

  virtual std::string original_text (void) const;

  tree_expression *mark_in_parens (void)
    {
      num_parens++;
      return this;
    }

  tree_expression *mark_postfix_indexed (void)
    {
      postfix_indexed = true;
      return this;
    }

  tree_expression *set_print_flag (bool print)
    {
      print_flag = print;
      return this;
    }

  virtual void copy_base (const tree_expression& e)
    {
      num_parens = e.num_parens;
      postfix_indexed = e.postfix_indexed;
      print_flag = e.print_flag;
    }

protected:

  // A count of the number of times this expression appears directly
  // inside a set of parentheses.
  //
  //   (((e1)) + e2)  ==> 2 for expression e1
  //                  ==> 1 for expression ((e1)) + e2
  //                  ==> 0 for expression e2
  int num_parens;

  // A flag that says whether this expression has an index associated
  // with it.  See the code in tree_identifier::rvalue for the rationale.
  bool postfix_indexed;

  // Print result of rvalue for this expression?
  bool print_flag;

private:

  // No copying!

  tree_expression (const tree_expression&);

  tree_expression& operator = (const tree_expression&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
