////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_pt_exp_h)
#define octave_pt_exp_h 1

#include "octave-config.h"

#include <string>
#include <list>

class octave_value;

#include "pt.h"
#include "pt-eval.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class octave_lvalue;

// A base class for expressions.

class tree_expression : public tree
{
public:

  tree_expression (int l = -1, int c = -1)
    : tree (l, c), m_num_parens (0), m_postfix_index_type ('\0'),
      m_for_cmd_expr (false), m_print_flag (false) { }

  // No copying!

  tree_expression (const tree_expression&) = delete;

  tree_expression& operator = (const tree_expression&) = delete;

  virtual ~tree_expression (void) = default;

  virtual tree_expression * dup (symbol_scope& scope) const = 0;

  virtual bool is_constant (void) const { return false; }

  virtual bool is_matrix (void) const { return false; }

  virtual bool iscell (void) const { return false; }

  virtual bool is_identifier (void) const { return false; }

  virtual bool is_index_expression (void) const { return false; }

  virtual bool is_assignment_expression (void) const { return false; }

  virtual bool is_prefix_expression (void) const { return false; }

  virtual bool is_unary_expression (void) const { return false; }

  virtual bool is_binary_expression (void) const { return false; }

  virtual bool is_boolean_expression (void) const { return false; }

  virtual bool is_colon_expression (void) const { return false; }

  virtual bool lvalue_ok (void) const { return false; }

  virtual bool rvalue_ok (void) const { return false; }

  virtual octave_lvalue lvalue (tree_evaluator&);

  int paren_count (void) const { return m_num_parens; }

  bool is_postfix_indexed (void) const
  { return (m_postfix_index_type != '\0'); }

  char postfix_index (void) const { return m_postfix_index_type; }

  // Check if the result of the expression should be printed.
  // Should normally be used in conjunction with
  // tree_evaluator::statement_printing_enabled.
  bool print_result (void) const { return m_print_flag; }

  virtual std::string oper (void) const { return "<unknown>"; }

  virtual std::string name (void) const { return "<unknown>"; }

  virtual std::string original_text (void) const;

  void mark_as_for_cmd_expr (void) { m_for_cmd_expr = true; }

  bool is_for_cmd_expr (void) const { return m_for_cmd_expr; }

  tree_expression * mark_in_parens (void)
  {
    m_num_parens++;
    return this;
  }

  tree_expression * set_postfix_index (char type)
  {
    m_postfix_index_type = type;
    return this;
  }

  tree_expression * set_print_flag (bool print)
  {
    m_print_flag = print;
    return this;
  }

  virtual void copy_base (const tree_expression& e)
  {
    m_num_parens = e.m_num_parens;
    m_postfix_index_type = e.m_postfix_index_type;
    m_print_flag = e.m_print_flag;
  }

  virtual octave_value evaluate (tree_evaluator& tw, int nargout = 1) = 0;

  virtual octave_value_list
  evaluate_n (tree_evaluator& tw, int nargout = 1) = 0;

protected:

  // A count of the number of times this expression appears directly
  // inside a set of parentheses.
  //
  //   (((e1)) + e2)  ==> 2 for expression e1
  //                  ==> 1 for expression ((e1)) + e2
  //                  ==> 0 for expression e2
  int m_num_parens;

  // The first index type associated with this expression.  This field
  // is 0 (character '\0') if the expression has no associated index.
  // See the code in tree_identifier::rvalue for the rationale.
  char m_postfix_index_type;

  // TRUE if this expression is the EXPR in for loop:
  // FOR i = EXPR ... END
  bool m_for_cmd_expr;

  // Print result of rvalue for this expression?
  bool m_print_flag;
};

OCTAVE_END_NAMESPACE(octave)

#endif
