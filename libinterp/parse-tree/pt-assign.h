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

#if ! defined (octave_pt_assign_h)
#define octave_pt_assign_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

class octave_value;
class octave_value_list;

#include "ov.h"
#include "pt-exp.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class octave_lvalue;
class tree_argument_list;

// Simple assignment expressions.

class tree_simple_assignment : public tree_expression
{
public:

  tree_simple_assignment (bool plhs = false, int l = -1, int c = -1,
                          octave_value::assign_op t = octave_value::op_asn_eq)
    : tree_expression (l, c), m_lhs (nullptr), m_rhs (nullptr),
      m_preserve (plhs), m_ans_assign (), m_etype (t)
  { }

  tree_simple_assignment (tree_expression *le, tree_expression *re,
                          bool plhs = false, int l = -1, int c = -1,
                          octave_value::assign_op t = octave_value::op_asn_eq);

  // No copying!

  tree_simple_assignment (const tree_simple_assignment&) = delete;

  tree_simple_assignment& operator = (const tree_simple_assignment&) = delete;

  ~tree_simple_assignment (void);

  bool rvalue_ok (void) const { return true; }

  bool is_assignment_expression (void) const { return true; }

  std::string oper (void) const;

  tree_expression * left_hand_side (void) { return m_lhs; }

  tree_expression * right_hand_side (void) { return m_rhs; }

  tree_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator& tw, int nargout = 1);

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

  void accept (tree_walker& tw)
  {
    tw.visit_simple_assignment (*this);
  }

  octave_value::assign_op op_type (void) const { return m_etype; }

private:

  void do_assign (octave_lvalue& ult, const octave_value_list& args,
                  const octave_value& rhs_val);

  void do_assign (octave_lvalue& ult, const octave_value& rhs_val);

  // The left hand side of the assignment.
  tree_expression *m_lhs;

  // The right hand side of the assignment.
  tree_expression *m_rhs;

  // True if we should not delete the lhs.
  bool m_preserve;

  // True if this is an assignment to the automatic variable ans.
  bool m_ans_assign;

  // The type of the expression.
  octave_value::assign_op m_etype;
};

// Multi-valued assignment expressions.

class tree_multi_assignment : public tree_expression
{
public:

  tree_multi_assignment (bool plhs = false, int l = -1, int c = -1)
    : tree_expression (l, c), m_lhs (nullptr), m_rhs (nullptr),
      m_preserve (plhs)
  { }

  tree_multi_assignment (tree_argument_list *lst, tree_expression *r,
                         bool plhs = false, int l = -1, int c = -1);

  // No copying!

  tree_multi_assignment (const tree_multi_assignment&) = delete;

  tree_multi_assignment& operator = (const tree_multi_assignment&) = delete;

  ~tree_multi_assignment (void);

  bool is_assignment_expression (void) const { return true; }

  bool rvalue_ok (void) const { return true; }

  std::string oper (void) const;

  tree_argument_list * left_hand_side (void) { return m_lhs; }

  tree_expression * right_hand_side (void) { return m_rhs; }

  tree_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator& tw, int nargout = 1)
  {
    octave_value_list retval = evaluate_n (tw, nargout);

    return retval.length () > 0 ? retval(0) : octave_value ();
  }

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1);

  void accept (tree_walker& tw)
  {
    tw.visit_multi_assignment (*this);
  }

  octave_value::assign_op op_type (void) const
  {
    return octave_value::op_asn_eq;
  }

private:

  // The left hand side of the assignment.
  tree_argument_list *m_lhs;

  // The right hand side of the assignment.
  tree_expression *m_rhs;

  // True if we should not delete the lhs.
  bool m_preserve;
};

OCTAVE_END_NAMESPACE(octave)

#endif
