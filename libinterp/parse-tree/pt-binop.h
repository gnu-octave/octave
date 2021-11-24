////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2021 The Octave Project Developers
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

namespace octave
{
  class symbol_scope;

  // Binary expressions.

  class tree_binary_expression : public tree_expression
  {
  public:

    tree_binary_expression (int l = -1, int c = -1,
                            octave_value::binary_op t
                            = octave_value::unknown_binary_op)
      : tree_expression (l, c), m_lhs (nullptr), m_rhs (nullptr), m_etype (t),
        m_eligible_for_braindead_shortcircuit (false),
        m_braindead_shortcircuit_warning_issued (false) { }

    tree_binary_expression (tree_expression *a, tree_expression *b,
                            int l = -1, int c = -1,
                            octave_value::binary_op t
                            = octave_value::unknown_binary_op)
      : tree_expression (l, c), m_lhs (a), m_rhs (b), m_etype (t),
        m_eligible_for_braindead_shortcircuit (false),
        m_braindead_shortcircuit_warning_issued (false) { }

    // No copying!

    tree_binary_expression (const tree_binary_expression&) = delete;

    tree_binary_expression& operator = (const tree_binary_expression&) = delete;

    ~tree_binary_expression (void)
    {
      delete m_lhs;
      delete m_rhs;
    }

    void mark_braindead_shortcircuit (void)
    {
      if (m_etype == octave_value::op_el_and
          || m_etype == octave_value::op_el_or)
        {
          m_eligible_for_braindead_shortcircuit = true;

          m_lhs->mark_braindead_shortcircuit ();
          m_rhs->mark_braindead_shortcircuit ();
        }
    }

    bool is_binary_expression (void) const { return true; }

    bool rvalue_ok (void) const { return true; }

    std::string oper (void) const;

    octave_value::binary_op op_type (void) const { return m_etype; }

    tree_expression * lhs (void) { return m_lhs; }
    tree_expression * rhs (void) { return m_rhs; }

    bool is_eligible_for_braindead_shortcircuit (void) const
    {
      return m_eligible_for_braindead_shortcircuit;
    }

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

    // TRUE if this is an | or & expression in the condition of an IF
    // or WHILE statement.
    bool m_eligible_for_braindead_shortcircuit;

    // TRUE if we have already issued a warning about short circuiting
    // for this operator.
    bool m_braindead_shortcircuit_warning_issued;
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
}

#endif
