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

#if ! defined (octave_pt_colon_h)
#define octave_pt_colon_h 1

#include "octave-config.h"

#include <string>

class octave_value;
class octave_value_list;

#include "pt-exp.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;

// Colon expressions.

class tree_colon_expression : public tree_expression
{
public:

  tree_colon_expression (tree_expression *base, const token& colon_1_tok, tree_expression *limit)
    : m_base (base), m_colon_1_tok (colon_1_tok), m_limit (limit)
  { }

  tree_colon_expression (tree_expression *base, const token& colon_1_tok, tree_expression *increment, const token& colon_2_tok, tree_expression *limit)
    : m_base (base), m_colon_1_tok (colon_1_tok), m_increment (increment), m_colon_2_tok (colon_2_tok), m_limit (limit)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_colon_expression)

  ~tree_colon_expression ()
  {
    if (! m_save_base)
      delete m_base;

    delete m_limit;
    delete m_increment;
  }

  filepos beg_pos () const { return m_base->beg_pos (); }
  filepos end_pos () const { return m_limit->end_pos (); }

  void preserve_base () { m_save_base = true; }

  bool rvalue_ok () const { return true; }

  void eval_error (const std::string& s) const;

  tree_expression * base () { return m_base; }

  tree_expression * limit () { return m_limit; }

  tree_expression * increment () { return m_increment; }

  tree_expression * dup (symbol_scope& scope) const;

  bool is_colon_expression () const { return true; }

  octave_value evaluate (tree_evaluator&, int nargout = 1);

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

  void accept (tree_walker& tw)
  {
    tw.visit_colon_expression (*this);
  }

private:

  // The components of the expression.
  tree_expression *m_base;
  token m_colon_1_tok;
  tree_expression *m_increment {nullptr};
  token m_colon_2_tok;
  tree_expression *m_limit;

  bool m_save_base {false};
};

OCTAVE_END_NAMESPACE(octave)

#endif
