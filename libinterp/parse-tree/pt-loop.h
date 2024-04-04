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

#if ! defined (octave_pt_loop_h)
#define octave_pt_loop_h 1

#include "octave-config.h"

class octave_value;

#include "pt-cmd.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class comment_list;
class tree_argument_list;
class tree_expression;
class tree_statement_list;

// While.

class tree_while_command : public tree_command
{
public:

  tree_while_command (const token& while_tok, tree_expression *expr, tree_statement_list *body, const token& end_tok)
    : m_while_tok (while_tok), m_expr (expr), m_body (body), m_end_tok (end_tok)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_while_command)

  ~tree_while_command ();

  filepos beg_pos () const { return m_while_tok.beg_pos (); }
  filepos end_pos () const { return m_end_tok.end_pos (); }

  tree_expression * condition () { return m_expr; }

  tree_statement_list * body () { return m_body; }

  void accept (tree_walker& tw)
  {
    tw.visit_while_command (*this);
  }

private:

  token m_while_tok;

  // Expression to test.
  tree_expression *m_expr;

  // List of commands to execute.
  tree_statement_list *m_body {nullptr};

  token m_end_tok;
};

// Do-Until.

class tree_do_until_command : public tree_command
{
public:

  tree_do_until_command (const token& do_tok, tree_statement_list *body, const token& until_tok, tree_expression *expr)
    : m_do_tok (do_tok), m_body (body), m_until_tok (until_tok), m_expr (expr)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_do_until_command)

  ~tree_do_until_command ();

  filepos beg_pos () const { return m_do_tok.beg_pos (); }
  filepos end_pos () const { return m_expr->end_pos (); }

  tree_statement_list * body () { return m_body; }

  tree_expression * condition () { return m_expr; }

  void accept (tree_walker& tw)
  {
    tw.visit_do_until_command (*this);
  }

private:

  token m_do_tok;

  // List of commands to execute.
  tree_statement_list *m_body {nullptr};

  token m_until_tok;

  // Expression to test.
  tree_expression *m_expr;
};

// For.

class tree_simple_for_command : public tree_command
{
public:

  tree_simple_for_command (bool parfor, const token& for_tok, const token& open_paren, tree_expression *le, const token& eq_tok,
                           tree_expression *re, const token& sep_tok, tree_expression *maxproc_arg, const token& close_paren,
                           tree_statement_list *body, const token& end_tok)
    : m_parfor (parfor), m_for_tok (for_tok), m_open_paren (open_paren), m_lhs (le), m_eq_tok (eq_tok),
      m_expr (re), m_sep_tok (sep_tok), m_maxproc (maxproc_arg), m_close_paren (close_paren),
      m_body (body), m_end_tok (end_tok)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_simple_for_command)

  ~tree_simple_for_command ();

  bool in_parallel () { return m_parfor; }

  filepos beg_pos () const { return m_for_tok.beg_pos (); }
  filepos end_pos () const { return m_end_tok.end_pos (); }

  tree_expression * left_hand_side () { return m_lhs; }

  tree_expression * control_expr () { return m_expr; }

  tree_expression * maxproc_expr () { return m_maxproc; }

  tree_statement_list * body () { return m_body; }

  void accept (tree_walker& tw)
  {
    tw.visit_simple_for_command (*this);
  }

private:

  // FIXME: it would be better to get this info from FOR_TOK.
  bool m_parfor {false};

  token m_for_tok;

  token m_open_paren;

  // Expression to modify.
  tree_expression *m_lhs;

  token m_eq_tok;

  // Expression to evaluate.
  tree_expression *m_expr;

  token m_sep_tok;

  // Expression to tell how many processors should be used (only valid
  // if parallel is TRUE).
  tree_expression *m_maxproc {nullptr};

  token m_close_paren;

  // List of commands to execute.
  tree_statement_list *m_body;

  token m_end_tok;
};

class tree_complex_for_command : public tree_command
{
public:

  tree_complex_for_command (const token& for_tok, tree_argument_list *le, const token& eq_tok, tree_expression *re,
                            tree_statement_list *body, const token& end_tok)
    : m_for_tok (for_tok), m_lhs (le), m_eq_tok (eq_tok), m_expr (re), m_body (body), m_end_tok (end_tok)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_complex_for_command)

  ~tree_complex_for_command ();

  filepos beg_pos () const { return m_for_tok.beg_pos (); }
  filepos end_pos () const { return m_end_tok.end_pos (); }

  tree_argument_list * left_hand_side () { return m_lhs; }

  tree_expression * control_expr () { return m_expr; }

  tree_statement_list * body () { return m_body; }

  void accept (tree_walker& tw)
  {
    tw.visit_complex_for_command (*this);
  }

private:

  token m_for_tok;

  // Expression to modify.
  tree_argument_list *m_lhs;

  token m_eq_tok;

  // Expression to evaluate.
  tree_expression *m_expr;

  // List of commands to execute.
  tree_statement_list *m_body;

  token m_end_tok;
};

OCTAVE_END_NAMESPACE(octave)

#endif
