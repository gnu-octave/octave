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

#if ! defined (octave_pt_select_h)
#define octave_pt_select_h 1

#include "octave-config.h"

#include <list>

#include "comment-list.h"
#include "pt-cmd.h"
#include "pt-walk.h"
#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class comment_list;
class tree_expression;
class tree_statement_list;

// If.

class tree_if_clause : public tree
{
public:

  tree_if_clause (const token& tok, tree_expression *e, tree_statement_list *sl, int l = -1, int c = -1)
    : tree (l, c), m_tok (tok), m_expr (e), m_list (sl)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_if_clause)

  ~tree_if_clause ();

  token if_token () const { return m_tok; }

  bool is_else_clause () { return ! m_expr; }

  tree_expression * condition () { return m_expr; }

  tree_statement_list * commands () { return m_list; }

  comment_list leading_comments () const { return m_tok.leading_comments (); }

  void accept (tree_walker& tw)
  {
    tw.visit_if_clause (*this);
  }

private:

  token m_tok;

  // The condition to test.
  tree_expression *m_expr = nullptr;

  // The list of statements to evaluate if expr is true.
  tree_statement_list *m_list;
};

class tree_if_command_list : public std::list<tree_if_clause *>
{
public:

  tree_if_command_list () { }

  tree_if_command_list (tree_if_clause *t) { push_back (t); }

  OCTAVE_DISABLE_COPY_MOVE (tree_if_command_list)

  ~tree_if_command_list ()
  {
    while (! empty ())
      {
        auto p = begin ();
        delete *p;
        erase (p);
      }
  }

  token if_token () const
  {
    if (! empty ())
      {
        tree_if_clause *p = front ();
        return p->if_token ();
      }

    return token ();
  }

  void accept (tree_walker& tw)
  {
    tw.visit_if_command_list (*this);
  }
};

class tree_if_command : public tree_command
{
public:

  tree_if_command (const token& if_tok, const token& end_tok, int l = -1, int c = -1)
    : tree_command (l, c), m_if_tok (if_tok), m_end_tok (end_tok)
  { }

  tree_if_command (const token& if_tok, tree_if_command_list *lst, const token& end_tok, int l = -1, int c = -1)
    : tree_command (l, c), m_if_tok (if_tok), m_list (lst), m_end_tok (end_tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_if_command)

  ~tree_if_command ();

  tree_if_command_list * cmd_list () { return m_list; }

  comment_list leading_comments () const { return m_if_tok.leading_comments (); }

  void accept (tree_walker& tw)
  {
    tw.visit_if_command (*this);
  }

private:

  token m_if_tok;

  // List of if commands (if, elseif, elseif, ... else, endif)
  tree_if_command_list *m_list = nullptr;

  token m_end_tok;
};

// Switch.

class tree_switch_case : public tree
{
public:

  tree_switch_case (const token& tok, tree_statement_list *sl, int l = -1, int c = -1)
    : tree (l, c), m_tok (tok), m_list (sl)
  { }

  tree_switch_case (const token& tok, tree_expression *e, tree_statement_list *sl, int l = -1, int c = -1)
    : tree (l, c), m_tok (tok), m_label (e), m_list (sl)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_switch_case)

  ~tree_switch_case ();

  bool is_default_case () { return ! m_label; }

  tree_expression * case_label () { return m_label; }

  tree_statement_list * commands () { return m_list; }

  comment_list leading_comments () const { return m_tok.leading_comments (); }

  void accept (tree_walker& tw)
  {
    tw.visit_switch_case (*this);
  }

private:

  token m_tok;

  // The case label.
  tree_expression *m_label = nullptr;

  // The list of statements to evaluate if the label matches.
  tree_statement_list *m_list;
};

class tree_switch_case_list : public std::list<tree_switch_case *>
{
public:

  tree_switch_case_list () { }

  tree_switch_case_list (tree_switch_case *t) { push_back (t); }

  OCTAVE_DISABLE_COPY_MOVE (tree_switch_case_list)

  ~tree_switch_case_list ()
  {
    while (! empty ())
      {
        auto p = begin ();
        delete *p;
        erase (p);
      }
  }

  void accept (tree_walker& tw)
  {
    tw.visit_switch_case_list (*this);
  }
};

class tree_switch_command : public tree_command
{
public:

  tree_switch_command (const token& switch_tok, tree_expression *e, tree_switch_case_list *lst, const token& end_tok, int l = -1, int c = -1)
    : tree_command (l, c), m_switch_tok (switch_tok), m_expr (e), m_list (lst), m_end_tok (end_tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_switch_command)

  ~tree_switch_command ();

  tree_expression * switch_value () { return m_expr; }

  tree_switch_case_list * case_list () { return m_list; }

  comment_list leading_comments () const { return m_switch_tok.leading_comments (); }

  void accept (tree_walker& tw)
  {
    tw.visit_switch_command (*this);
  }

private:

  token m_switch_tok;

  // Value on which to switch.
  tree_expression *m_expr;

  // List of cases (case 1, case 2, ..., default)
  tree_switch_case_list *m_list;

  token m_end_tok;
};

OCTAVE_END_NAMESPACE(octave)

#endif
