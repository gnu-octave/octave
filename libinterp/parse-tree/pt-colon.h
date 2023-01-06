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

  tree_colon_expression (int l = -1, int c = -1)
    : tree_expression (l, c), m_base (nullptr), m_limit (nullptr),
      m_increment (nullptr), m_save_base (false) { }

  tree_colon_expression (tree_expression *bas, tree_expression *lim,
                         int l = -1, int c = -1)
    : tree_expression (l, c), m_base (bas), m_limit (lim),
      m_increment (nullptr), m_save_base (false) { }

  tree_colon_expression (tree_expression *bas, tree_expression *lim,
                         tree_expression *inc, int l = -1, int c = -1)
    : tree_expression (l, c), m_base (bas), m_limit (lim),
      m_increment (inc), m_save_base (false) { }

  // No copying!

  tree_colon_expression (const tree_colon_expression&) = delete;

  tree_colon_expression& operator = (const tree_colon_expression&) = delete;

  ~tree_colon_expression (void)
  {
    if (! m_save_base)
      delete m_base;

    delete m_limit;
    delete m_increment;
  }

  void preserve_base (void) { m_save_base = true; }

  bool rvalue_ok (void) const { return true; }

  void eval_error (const std::string& s) const;

  tree_expression * base (void) { return m_base; }

  tree_expression * limit (void) { return m_limit; }

  tree_expression * increment (void) { return m_increment; }

  tree_expression * dup (symbol_scope& scope) const;

  bool is_colon_expression (void) const { return true; }

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
  tree_expression *m_limit;
  tree_expression *m_increment;

  bool m_save_base;
};

OCTAVE_END_NAMESPACE(octave)

#endif
