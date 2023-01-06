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

#if ! defined (octave_pt_stmt_h)
#define octave_pt_stmt_h 1

#include "octave-config.h"

class octave_value_list;

#include <deque>

#include "base-list.h"
#include "bp-table.h"
#include "pt.h"
#include "pt-walk.h"

class event_manager;

OCTAVE_BEGIN_NAMESPACE(octave)

class comment_list;
class tree_command;
class tree_evaluator;
class tree_expression;

// A statement is either a command to execute or an expression to
// evaluate.

class tree_statement : public tree
{
public:

  tree_statement (void)
    : m_command (nullptr), m_expression (nullptr),
      m_comment_list (nullptr) { }

  tree_statement (tree_command *c, comment_list *cl)
    : m_command (c), m_expression (nullptr), m_comment_list (cl) { }

  tree_statement (tree_expression *e, comment_list *cl)
    : m_command (nullptr), m_expression (e), m_comment_list (cl) { }

  // No copying!

  tree_statement (const tree_statement&) = delete;

  tree_statement& operator = (const tree_statement&) = delete;

  ~tree_statement (void);

  void set_print_flag (bool print_flag);

  bool print_result (void);

  bool is_command (void) const { return m_command != nullptr; }

  bool is_expression (void) const { return m_expression != nullptr; }

  void set_breakpoint (const std::string& condition);

  void delete_breakpoint (void);

  bool is_breakpoint (void) const;

  bool is_active_breakpoint (tree_evaluator& tw) const;

  std::string bp_cond () const;

  int line (void) const;
  int column (void) const;

  void set_location (int l, int c);

  void echo_code (const std::string& prefix);

  tree_command * command (void) { return m_command; }

  tree_expression * expression (void) { return m_expression; }

  comment_list * comment_text (void) { return m_comment_list; }

  bool is_null_statement (void) const
  {
    return ! (m_command || m_expression || m_comment_list);
  }

  bool is_end_of_fcn_or_script (void) const;

  bool is_end_of_file (void) const;

  // Allow modification of this statement.  Note that there is no
  // checking.  If you use these, are you sure you know what you are
  // doing?

  void set_command (tree_command *c) { m_command = c; }

  void set_expression (tree_expression *e) { m_expression = e; }

  void accept (tree_walker& tw)
  {
    tw.visit_statement (*this);
  }

private:

  // Only one of cmd or expr can be valid at once.

  // Command to execute.
  tree_command *m_command;

  // Expression to evaluate.
  tree_expression *m_expression;

  // Comment associated with this statement.
  comment_list *m_comment_list;
};

// A list of statements to evaluate.

class tree_statement_list : public base_list<tree_statement *>
{
public:

  tree_statement_list (void)
    : m_function_body (false), m_anon_function_body (false),
      m_script_body (false) { }

  tree_statement_list (tree_statement *s)
    : m_function_body (false), m_anon_function_body (false),
      m_script_body (false) { append (s); }

  // No copying!

  tree_statement_list (const tree_statement_list&) = delete;

  tree_statement_list& operator = (const tree_statement_list&) = delete;

  ~tree_statement_list (void)
  {
    while (! empty ())
      {
        auto p = begin ();
        delete *p;
        erase (p);
      }
  }

  void mark_as_function_body (void) { m_function_body = true; }

  void mark_as_anon_function_body (void) { m_anon_function_body = true; }

  void mark_as_script_body (void) { m_script_body = true; }

  bool is_function_body (void) const { return m_function_body; }

  bool is_anon_function_body (void) const { return m_anon_function_body; }

  bool is_script_body (void) const { return m_script_body; }

  int set_breakpoint (int line, const std::string& condition);

  void delete_breakpoint (int line);

  octave_value_list list_breakpoints (void);

  std::list<bp_type> breakpoints_and_conds (void);

  bp_table::bp_lines add_breakpoint (event_manager& evmgr,
                                     const std::string& file,
                                     const bp_table::bp_lines& lines,
                                     const std::string& condition);

  bp_table::bp_lines remove_all_breakpoints (event_manager& evmgr,
      const std::string& file);

  void accept (tree_walker& tw)
  {
    tw.visit_statement_list (*this);
  }

private:

  // Does this list of statements make up the body of a function?
  bool m_function_body;

  // Does this list of statements make up the body of a function?
  bool m_anon_function_body;

  // Does this list of statements make up the body of a script?
  bool m_script_body;
};

OCTAVE_END_NAMESPACE(octave)

#endif
