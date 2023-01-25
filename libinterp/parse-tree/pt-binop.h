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

#if ! defined (octave_pt_binop_h)
#define octave_pt_binop_h 1

#include "octave-config.h"

#include <string>

class octave_value;
class octave_value_list;

#include "ov.h"
#include "pt-exp.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;

// Binary expressions.

class tree_binary_expression : public tree_expression
{
public:

  tree_binary_expression (int l = -1, int c = -1,
                          octave_value::binary_op t
                          = octave_value::unknown_binary_op)
    : tree_expression (l, c), m_lhs (nullptr), m_rhs (nullptr), m_etype (t),
      m_preserve_operands (false)
  { }

  tree_binary_expression (tree_expression *a, tree_expression *b,
                          int l = -1, int c = -1,
                          octave_value::binary_op t
                          = octave_value::unknown_binary_op)
    : tree_expression (l, c), m_lhs (a), m_rhs (b), m_etype (t),
      m_preserve_operands (false)
  { }

  // No copying!

  tree_binary_expression (const tree_binary_expression&) = delete;

  tree_binary_expression& operator = (const tree_binary_expression&) = delete;

  ~tree_binary_expression (void)
  {
    if (! m_preserve_operands)
      {
        delete m_lhs;
        delete m_rhs;
      }
  }

  void preserve_operands (void) { m_preserve_operands = true; }

  bool is_binary_expression (void) const { return true; }

  bool rvalue_ok (void) const { return true; }

  std::string oper (void) const;

  octave_value::binary_op op_type (void) const { return m_etype; }

  tree_expression * lhs (void) { return m_lhs; }
  tree_expression * rhs (void) { return m_rhs; }

  void lhs (tree_expression *expr) { m_lhs = expr; }
  void rhs (tree_expression *expr) { m_rhs = expr; }

  tree_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator&, int nargout = 1);

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

  void accept (tree_walker& tw)
  {
    tw.visit_binary_expression (*this);
  }

  std::string profiler_name (void) const { return "binary " + oper (); }

  void matlab_style_short_circuit_warning (const char *op);

protected:

  // The operands for the expression.
  tree_expression *m_lhs;
  tree_expression *m_rhs;

private:

  // The type of the expression.
  octave_value::binary_op m_etype;

  // If TRUE, don't delete m_lhs and m_rhs in destructor;
  bool m_preserve_operands;
};

class tree_braindead_shortcircuit_binary_expression
  : public tree_binary_expression
{
public:

  tree_braindead_shortcircuit_binary_expression (tree_expression *a,
      tree_expression *b,
      int l, int c,
      octave_value::binary_op t)
    : tree_binary_expression (a, b, l, c, t)
  { }

  // No copying!

  tree_braindead_shortcircuit_binary_expression
  (const tree_braindead_shortcircuit_binary_expression&) = delete;

  tree_braindead_shortcircuit_binary_expression&
  operator = (const tree_braindead_shortcircuit_binary_expression&) = delete;

  ~tree_braindead_shortcircuit_binary_expression (void) = default;

  tree_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator&, int nargout = 1);

  using tree_binary_expression::evaluate_n;
};

// Boolean expressions.

class tree_boolean_expression : public tree_binary_expression
{
public:

  enum type
  {
    unknown,
    bool_and,
    bool_or
  };

  tree_boolean_expression (int l = -1, int c = -1, type t = unknown)
    : tree_binary_expression (l, c), m_etype (t) { }

  tree_boolean_expression (tree_expression *a, tree_expression *b,
                           int l = -1, int c = -1, type t = unknown)
    : tree_binary_expression (a, b, l, c), m_etype (t) { }

  // No copying!

  tree_boolean_expression (const tree_boolean_expression&) = delete;

  tree_boolean_expression& operator = (const tree_boolean_expression&) = delete;

  ~tree_boolean_expression (void) = default;

  bool is_boolean_expression (void) const { return true; }

  bool rvalue_ok (void) const { return true; }

  std::string oper (void) const;

  type op_type (void) const { return m_etype; }

  tree_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator&, int nargout = 1);

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

  void accept (tree_walker& tw)
  {
    tw.visit_boolean_expression (*this);
  }

private:

  // The type of the expression.
  type m_etype;
};

OCTAVE_END_NAMESPACE(octave)

#endif
