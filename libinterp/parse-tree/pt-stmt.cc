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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <typeinfo>

#include "quit.h"

#include "bp-table.h"
#include "comment-list.h"
#include "event-manager.h"
#include "input.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pager.h"
#include "pt-bp.h"
#include "pt-cmd.h"
#include "pt-eval.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-jump.h"
#include "pt-pr-code.h"
#include "pt-stmt.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// A list of commands to be executed.

tree_statement::~tree_statement (void)
{
  delete m_command;
  delete m_expression;
  delete m_comment_list;
}

void
tree_statement::set_print_flag (bool print_flag)
{
  if (m_expression)
    m_expression->set_print_flag (print_flag);
}

bool
tree_statement::print_result (void)
{
  return m_expression && m_expression->print_result ();
}

void
tree_statement::set_breakpoint (const std::string& condition)
{
  if (m_command)
    m_command->set_breakpoint (condition);
  else if (m_expression)
    m_expression->set_breakpoint (condition);
}

void
tree_statement::delete_breakpoint (void)
{
  if (m_command)
    m_command->delete_breakpoint ();
  else if (m_expression)
    m_expression->delete_breakpoint ();
}

bool
tree_statement::is_breakpoint (void) const
{
  return m_command ? m_command->is_breakpoint ()
         : (m_expression ? m_expression->is_breakpoint ()
            : false);
}

bool
tree_statement::is_active_breakpoint (tree_evaluator& tw) const
{
  return m_command ? m_command->is_active_breakpoint (tw)
         : (m_expression ? m_expression->is_active_breakpoint (tw)
            : false);
}

std::string
tree_statement::bp_cond () const
{
  return (m_command
          ? m_command->bp_cond ()
          : (m_expression ? m_expression->bp_cond () : "0"));
}

int
tree_statement::line (void) const
{
  return (m_command
          ? m_command->line ()
          : (m_expression ? m_expression->line () : -1));
}

int
tree_statement::column (void) const
{
  return (m_command
          ? m_command->column ()
          : (m_expression ? m_expression->column () : -1));
}

void
tree_statement::set_location (int l, int c)
{
  if (m_command)
    m_command->set_location (l, c);
  else if (m_expression)
    m_expression->set_location (l, c);
}

void
tree_statement::echo_code (const std::string& prefix)
{
  tree_print_code tpc (octave_stdout, prefix);

  accept (tpc);
}

bool
tree_statement::is_end_of_fcn_or_script (void) const
{
  bool retval = false;

  if (m_command)
    {
      tree_no_op_command *no_op_cmd
        = dynamic_cast<tree_no_op_command *> (m_command);

      if (no_op_cmd)
        retval = no_op_cmd->is_end_of_fcn_or_script ();
    }

  return retval;
}

bool
tree_statement::is_end_of_file (void) const
{
  bool retval = false;

  if (m_command)
    {
      tree_no_op_command *no_op_cmd
        = dynamic_cast<tree_no_op_command *> (m_command);

      if (no_op_cmd)
        retval = no_op_cmd->is_end_of_file ();
    }

  return retval;
}

// Create a "breakpoint" tree-walker, and get it to "walk" this
// statement list
// (FIXME: What does that do???)

int
tree_statement_list::set_breakpoint (int line, const std::string& condition)
{
  tree_breakpoint tbp (line, tree_breakpoint::set, condition);
  accept (tbp);

  return tbp.get_line ();
}

void
tree_statement_list::delete_breakpoint (int line)
{
  if (line < 0)
    {
      octave_value_list bp_lst = list_breakpoints ();

      int len = bp_lst.length ();

      for (int i = 0; i < len; i++)
        {
          tree_breakpoint tbp (i, tree_breakpoint::clear);
          accept (tbp);
        }
    }
  else
    {
      tree_breakpoint tbp (line, tree_breakpoint::clear);
      accept (tbp);
    }
}

octave_value_list
tree_statement_list::list_breakpoints (void)
{
  tree_breakpoint tbp (0, tree_breakpoint::list);
  accept (tbp);

  return tbp.get_list ();
}

// Get list of pairs (breakpoint line, breakpoint condition)
std::list<bp_type>
tree_statement_list::breakpoints_and_conds (void)
{
  tree_breakpoint tbp (0, tree_breakpoint::list);
  accept (tbp);

  std::list<bp_type> retval;
  octave_value_list lines = tbp.get_list ();
  octave_value_list conds = tbp.get_cond_list ();

  for (int i = 0; i < lines.length (); i++)
    {
      retval.push_back (bp_type (lines(i).double_value (),
                                 conds(i).string_value ()));
    }

  return retval;
}

// Add breakpoints to  file  at multiple lines (the second arguments
// of  line), to stop only if  condition  is true.
// Updates GUI via  event_manager::update_breakpoint.
// FIXME: COME BACK TO ME.

bp_table::bp_lines
tree_statement_list::add_breakpoint (event_manager& evmgr,
                                     const std::string& file,
                                     const bp_table::bp_lines& lines,
                                     const std::string& condition)
{
  bp_table::bp_lines retval;

  for (const auto& lineno : lines)
    {
      int line = set_breakpoint (lineno, condition);

      if (line)
        {
          if (! file.empty ())
            evmgr.update_breakpoint (true, file, line, condition);

          retval.insert (line);
        }
    }

  return retval;
}

bp_table::bp_lines
tree_statement_list::remove_all_breakpoints (event_manager& evmgr,
    const std::string& file)
{
  bp_table::bp_lines retval;

  octave_value_list bkpts = list_breakpoints ();

  for (int i = 0; i < bkpts.length (); i++)
    {
      int lineno = bkpts(i).int_value ();

      delete_breakpoint (lineno);

      retval.insert (lineno);

      if (! file.empty ())
        evmgr.update_breakpoint (false, file, lineno);
    }

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
