////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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
#include "pt-delimiter-list.h"
#include "pt-eval.h"
#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class octave_lvalue;

// A base class for expressions.

class tree_expression : public tree
{
public:

  tree_expression (int l = -1, int c = -1)
    : tree (l, c), m_postfix_index_type ('\0'), m_for_cmd_expr (false), m_print_flag (false) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_expression)

  virtual ~tree_expression () = default;

  virtual tree_expression * dup (symbol_scope& scope) const = 0;

  virtual bool is_constant () const { return false; }

  virtual bool is_matrix () const { return false; }

  virtual bool iscell () const { return false; }

  virtual bool is_identifier () const { return false; }

  virtual bool is_index_expression () const { return false; }

  virtual bool is_assignment_expression () const { return false; }

  virtual bool is_prefix_expression () const { return false; }

  virtual bool is_unary_expression () const { return false; }

  virtual bool is_binary_expression () const { return false; }

  virtual bool is_boolean_expression () const { return false; }

  virtual bool is_colon_expression () const { return false; }

  virtual bool lvalue_ok () const { return false; }

  virtual bool rvalue_ok () const { return false; }

  virtual octave_lvalue lvalue (tree_evaluator&);

  // The number of times this expression appears directly inside a set
  // of delimiters.
  //
  //   (([e1]) + e2)  ==> 2 for expression e1
  //                  ==> 1 for expression ([e1]) + e2
  //                  ==> 0 for expression e2

  size_t delim_count () const { return m_delims.count (); }

  bool is_postfix_indexed () const
  { return (m_postfix_index_type != '\0'); }

  char postfix_index () const { return m_postfix_index_type; }

  // Check if the result of the expression should be printed.
  // Should normally be used in conjunction with
  // tree_evaluator::statement_printing_enabled.
  bool print_result () const { return m_print_flag; }

  virtual std::string oper () const { return "<unknown>"; }

  virtual std::string name () const { return "<unknown>"; }

  virtual std::string original_text () const;

  void mark_as_for_cmd_expr () { m_for_cmd_expr = true; }

  bool is_for_cmd_expr () const { return m_for_cmd_expr; }

  tree_expression * mark_in_delims (const token& open_delim, const token& close_delim)
  {
    m_delims.push (open_delim, close_delim);
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
    m_postfix_index_type = e.m_postfix_index_type;
    m_print_flag = e.m_print_flag;
    m_delims = e.m_delims;
  }

  virtual octave_value evaluate (tree_evaluator& tw, int nargout = 1) = 0;

  virtual octave_value_list
  evaluate_n (tree_evaluator& tw, int nargout = 1) = 0;

protected:

  // The first index type associated with this expression.  This field
  // is 0 (character '\0') if the expression has no associated index.
  // See the code in tree_identifier::rvalue for the rationale.
  char m_postfix_index_type;

  // TRUE if this expression is the EXPR in for loop:
  // FOR i = EXPR ... END
  bool m_for_cmd_expr;

  // Print result of rvalue for this expression?
  bool m_print_flag;

  // Tokens for any delimiters surrounding this expression.
  tree_delimiter_list m_delims;
};

OCTAVE_END_NAMESPACE(octave)

#endif
