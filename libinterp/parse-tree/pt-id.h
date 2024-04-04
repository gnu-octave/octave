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

#if ! defined (octave_pt_id_h)
#define octave_pt_id_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

class octave_value;
class octave_value_list;
class octave_function;

#include "comment-list.h"
#include "oct-lvalue.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-walk.h"
#include "symscope.h"
#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;

// Symbols from the symbol table.

class tree_identifier : public tree_expression
{
  friend class tree_index_expression;

public:

  tree_identifier (const token& tok) : m_token (tok) { }

  tree_identifier (symbol_scope& scope, const token& tok)
    : m_sym (scope ? scope.insert (tok.text ()) : symbol_record (tok.text ())),
      m_token (tok)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_identifier)

  ~tree_identifier () = default;

  bool is_identifier () const { return true; }

  std::string name () const { return m_sym.name (); }

  comment_list leading_comments () const { return m_token.leading_comments (); }

  filepos beg_pos () const { return m_token.beg_pos (); }
  filepos end_pos () const { return m_token.end_pos (); }

  virtual bool is_black_hole () const { return false; }

  void mark_as_formal_parameter () { m_sym.mark_formal (); }

  // We really need to know whether this symbol refers to a variable
  // or a function, but we may not know that yet.

  bool lvalue_ok () const { return true; }

  octave_lvalue lvalue (tree_evaluator& tw);

  void eval_undefined_error ();

  void static_workspace_error ()
  {
    error (R"(can not add variable "%s" to a static workspace)",
           name ().c_str ());
  }

  tree_identifier * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator& tw, int nargout = 1)
  {
    octave_value_list retval = evaluate_n (tw, nargout);

    return retval.length () > 0 ? retval(0) : octave_value ();
  }

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1);

  void accept (tree_walker& tw)
  {
    tw.visit_identifier (*this);
  }

  symbol_record symbol () const { return m_sym; }

  tree_identifier * mark_get_set (const token& get_set_tok, const token& dot_tok)
  {
    m_get_set_tok = get_set_tok;
    m_dot_tok = dot_tok;

    return this;
  }

protected:

  tree_identifier (symbol_record& sym, const token& tok)
    : m_sym (sym), m_token (tok)
  { }

  // The symbol record that this identifier references.
  symbol_record m_sym;

  // These will be defined for get.ID or set.ID function names.
  token m_get_set_tok;
  token m_dot_tok;

  // The IDENT token from the lexer.
  token m_token;
};

class tree_black_hole : public tree_identifier
{
public:

  tree_black_hole (const token& token)
    : tree_identifier (token)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_black_hole)

  ~tree_black_hole () = default;

  std::string name () const { return "~"; }

  bool is_black_hole () const { return true; }

  tree_black_hole * dup (symbol_scope&) const
  {
    return new tree_black_hole (m_token);
  }

  octave_lvalue lvalue (tree_evaluator& tw);
};

OCTAVE_END_NAMESPACE(octave)

#endif
