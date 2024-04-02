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

#if ! defined (octave_pt_idx_h)
#define octave_pt_idx_h 1

#include "octave-config.h"

#include <list>

class octave_map;
class octave_value;
class octave_value_list;

#include "str-vec.h"

#include "pt-exp.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class octave_lvalue;
class symbol_scope;
class tree_argument_list;
class tree_evaluator;

// Index expressions.

class tree_index_expression : public tree_expression
{
public:

  tree_index_expression (tree_expression *e, const token& open_delim, tree_argument_list *lst, const token& close_delim, int l, int c, char t);

  tree_index_expression (tree_expression *e, const token& dot_tok, const token& struct_elt_tok, int l = -1, int c = -1);

  tree_index_expression (tree_expression *e, const token& dot_tok, const token& open_paren, tree_expression *df, const token& close_paren, int l = -1, int c = -1);

  OCTAVE_DISABLE_COPY_MOVE (tree_index_expression)

  ~tree_index_expression ();

  tree_index_expression *
  append (const token& open_delim, tree_argument_list *lst, const token& close_delim, char t = '(');

  tree_index_expression * append (const token& dot_tok, const token& struct_elt_tok);

  tree_index_expression * append (const token& dot_tok, const token& open_paren, tree_expression *df, const token& close_paren);

  bool is_index_expression () const { return true; }

  std::string name () const;

  tree_expression * expression () { return m_expr; }

  std::list<tree_argument_list *> arg_lists () { return m_args; }

  std::string type_tags () { return m_type; }

  std::list<string_vector> arg_names () { return m_arg_nm; }

  std::list<tree_expression *> dyn_fields () { return m_dyn_field; }

  void mark_word_list_cmd () { m_word_list_cmd = true; }

  bool is_word_list_cmd () const { return m_word_list_cmd; }

  bool lvalue_ok () const { return m_expr->lvalue_ok (); }

  bool rvalue_ok () const { return true; }

  octave_lvalue lvalue (tree_evaluator& tw);

  tree_index_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator& tw, int nargout = 1)
  {
    octave_value_list retval = evaluate_n (tw, nargout);

    return retval.length () > 0 ? retval(0) : octave_value ();
  }

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1);

  void accept (tree_walker& tw)
  {
    tw.visit_index_expression (*this);
  }

  std::string
  get_struct_index
  (tree_evaluator& tw, std::list<string_vector>::const_iterator p_arg_nm,
   std::list<tree_expression *>::const_iterator p_dyn_field) const;

private:

  // The LHS of this index expression.
  tree_expression *m_expr {nullptr};

  // The indices (only valid if type == paren || type == brace).
  std::list<tree_argument_list *> m_args;

  // The type of this index expression.
  std::string m_type;

  // The names of the arguments.  Used for constant struct element
  // references.
  std::list<string_vector> m_arg_nm;

  // The list of dynamic field names, if any.
  std::list<tree_expression *> m_dyn_field;

  // TRUE if this expression was parsed as a word list command.
  bool m_word_list_cmd {false};

  tree_index_expression (int l, int c);

  octave_map make_arg_struct () const;
};

OCTAVE_END_NAMESPACE(octave)

#endif
