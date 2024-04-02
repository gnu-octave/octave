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

#if ! defined (octave_pt_except_h)
#define octave_pt_except_h 1

#include "octave-config.h"

#include "pt-cmd.h"
#include "pt-id.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class comment_list;
class tree_statement_list;

// Simple exception handling.

class tree_try_catch_command : public tree_command
{
public:

  tree_try_catch_command (const token try_tok, tree_statement_list *tc, const token catch_tok, tree_identifier *id, tree_statement_list *cc, const token& end_tok, int l = -1, int c = -1)
    : tree_command (l, c), m_try_tok (try_tok), m_try_code (tc), m_catch_tok (catch_tok), m_expr_id (id), m_catch_code (cc), m_end_tok (end_tok)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_try_catch_command)

  ~tree_try_catch_command ();

  tree_identifier * identifier () { return m_expr_id; }

  tree_statement_list * body () { return m_try_code; }

  tree_statement_list * cleanup () { return m_catch_code; }

  void accept (tree_walker& tw)
  {
    tw.visit_try_catch_command (*this);
  }

private:

  token m_try_tok;

  // The first block of code to attempt to execute.
  tree_statement_list *m_try_code;

  token m_catch_tok;

  // Identifier to modify.
  tree_identifier *m_expr_id;

  // The code to execute if an error occurs in the first block.
  tree_statement_list *m_catch_code;

  token m_end_tok;
};

// Simple exception handling.

class tree_unwind_protect_command : public tree_command
{
public:

  tree_unwind_protect_command (const token& unwind_tok, tree_statement_list *tc, const token& cleanup_tok, tree_statement_list *cc, const token& end_tok, int l = -1, int c = -1)
    : tree_command (l, c), m_unwind_tok (unwind_tok), m_unwind_protect_code (tc), m_cleanup_tok (cleanup_tok), m_cleanup_code (cc), m_end_tok (end_tok)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_unwind_protect_command)

  ~tree_unwind_protect_command ();

  tree_statement_list * body () { return m_unwind_protect_code; }

  tree_statement_list * cleanup () { return m_cleanup_code; }

  void accept (tree_walker& tw)
  {
    tw.visit_unwind_protect_command (*this);
  }

private:

  token m_unwind_tok;

  // The first body of code to attempt to execute.
  tree_statement_list *m_unwind_protect_code;

  token m_cleanup_tok;

  // The body of code to execute no matter what happens in the first
  // body of code.
  tree_statement_list *m_cleanup_code;

  token m_end_tok;
};

OCTAVE_END_NAMESPACE(octave)

#endif
