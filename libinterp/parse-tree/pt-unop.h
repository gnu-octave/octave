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

#if ! defined (octave_pt_unop_h)
#define octave_pt_unop_h 1

#include "octave-config.h"

#include <string>

class octave_value;
class octave_value_list;

#include "ov.h"
#include "pt-exp.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;

// Unary expressions.

class tree_unary_expression : public tree_expression
{
protected:

  tree_unary_expression (int l = -1, int c = -1,
                         octave_value::unary_op t
                         = octave_value::unknown_unary_op)
    : tree_expression (l, c), m_op (nullptr), m_etype (t)  { }

  tree_unary_expression (tree_expression *e, int l = -1, int c = -1,
                         octave_value::unary_op t
                         = octave_value::unknown_unary_op)
    : tree_expression (l, c), m_op (e), m_etype (t) { }

public:

  OCTAVE_DISABLE_COPY_MOVE (tree_unary_expression)

  ~tree_unary_expression () { delete m_op; }

  bool is_unary_expression () const { return true; }

  tree_expression * operand () { return m_op; }

  std::string oper () const;

  octave_value::unary_op op_type () const { return m_etype; }

protected:

  // The operand for the expression.
  tree_expression *m_op;

  // The type of the expression.
  octave_value::unary_op m_etype;
};

// Prefix expressions.

class tree_prefix_expression : public tree_unary_expression
{
public:

  tree_prefix_expression (int l = -1, int c = -1)
    : tree_unary_expression (l, c, octave_value::unknown_unary_op) { }

  tree_prefix_expression (tree_expression *e, int l = -1, int c = -1,
                          octave_value::unary_op t
                          = octave_value::unknown_unary_op)
    : tree_unary_expression (e, l, c, t) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_prefix_expression)

  ~tree_prefix_expression () = default;

  bool rvalue_ok () const { return true; }

  tree_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator&, int nargout = 1);

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

  void accept (tree_walker& tw)
  {
    tw.visit_prefix_expression (*this);
  }

  std::string profiler_name () const { return "prefix " + oper (); }
};

// Postfix expressions.

class tree_postfix_expression : public tree_unary_expression
{
public:

  tree_postfix_expression (int l = -1, int c = -1)
    : tree_unary_expression (l, c, octave_value::unknown_unary_op) { }

  tree_postfix_expression (tree_expression *e, int l = -1, int c = -1,
                           octave_value::unary_op t
                           = octave_value::unknown_unary_op)
    : tree_unary_expression (e, l, c, t) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_postfix_expression)

  ~tree_postfix_expression () = default;

  bool rvalue_ok () const { return true; }

  tree_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator&, int nargout = 1);

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

  void accept (tree_walker& tw)
  {
    tw.visit_postfix_expression (*this);
  }

  std::string profiler_name () const { return "postfix " + oper (); }
};

OCTAVE_END_NAMESPACE(octave)

#endif
