////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2001-2023 The Octave Project Developers
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

#include "ov-usr-fcn.h"
#include "pager.h"
#include "pt-all.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void
tree_breakpoint::visit_while_command (tree_while_command& cmd)
{
  if (cmd.line () >= m_line)
    take_action (cmd);

  if (! m_found)
    {
      tree_statement_list *lst = cmd.body ();

      if (lst)
        lst->accept (*this);
    }
}

void
tree_breakpoint::visit_do_until_command (tree_do_until_command& cmd)
{
  if (! m_found)
    {
      tree_statement_list *lst = cmd.body ();

      if (lst)
        lst->accept (*this);

      if (! m_found)
        {
          if (cmd.line () >= m_line)
            take_action (cmd);
        }
    }
}

void
tree_breakpoint::visit_argument_list (tree_argument_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_arguments_block (tree_arguments_block&)
{
  // FIXME
}

void
tree_breakpoint::visit_args_block_attribute_list (tree_args_block_attribute_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_args_block_validation_list (tree_args_block_validation_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_arg_validation (tree_arg_validation&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_arg_size_spec (tree_arg_size_spec&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_arg_validation_fcns (tree_arg_validation_fcns&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_binary_expression (tree_binary_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_break_command (tree_break_command& cmd)
{
  if (cmd.line () >= m_line)
    take_action (cmd);
}

void
tree_breakpoint::visit_colon_expression (tree_colon_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_continue_command (tree_continue_command& cmd)
{
  if (cmd.line () >= m_line)
    take_action (cmd);
}

void
tree_breakpoint::visit_decl_command (tree_decl_command& cmd)
{
  if (cmd.line () >= m_line)
    take_action (cmd);
}

void
tree_breakpoint::visit_decl_init_list (tree_decl_init_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_decl_elt (tree_decl_elt&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_simple_for_command (tree_simple_for_command& cmd)
{
  if (cmd.line () >= m_line)
    take_action (cmd);

  if (! m_found)
    {
      tree_statement_list *lst = cmd.body ();

      if (lst)
        lst->accept (*this);
    }
}

void
tree_breakpoint::visit_complex_for_command (tree_complex_for_command& cmd)
{
  if (cmd.line () >= m_line)
    take_action (cmd);

  if (! m_found)
    {
      tree_statement_list *lst = cmd.body ();

      if (lst)
        lst->accept (*this);
    }
}

void
tree_breakpoint::visit_octave_user_function_header (octave_user_function&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_octave_user_function_trailer (octave_user_function&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_identifier (tree_identifier&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_if_clause (tree_if_clause&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_if_command_list (tree_if_command_list& lst)
{
  for (tree_if_clause *t : lst)
    {
      if (t->line () >= m_line)
        take_action (*t);

      if (! m_found)
        {
          tree_statement_list *stmt_lst = t->commands ();

          if (stmt_lst)
            stmt_lst->accept (*this);
        }

      if (m_found)
        break;
    }
}

void
tree_breakpoint::visit_index_expression (tree_index_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_matrix (tree_matrix&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_cell (tree_cell&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_multi_assignment (tree_multi_assignment&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_no_op_command (tree_no_op_command& cmd)
{
  if (cmd.is_end_of_fcn_or_script () && cmd.line () >= m_line)
    take_action (cmd);
}

void
tree_breakpoint::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_constant (tree_constant&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_fcn_handle (tree_fcn_handle&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_parameter_list (tree_parameter_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_postfix_expression (tree_postfix_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_prefix_expression (tree_prefix_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_return_command (tree_return_command& cmd)
{
  if (cmd.line () >= m_line)
    take_action (cmd);
}

void
tree_breakpoint::visit_simple_assignment (tree_simple_assignment&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_statement (tree_statement& stmt)
{
  if (stmt.is_command ())
    {
      tree_command *cmd = stmt.command ();

      cmd->accept (*this);
    }
  else
    {
      if (stmt.line () >= m_line)
        take_action (stmt);
    }
}

// Called by
//   tree_statement_list::set_breakpoint (int line, std::string& condition)
// with <lst> consisting of a user function in which to set a breakpoint.
void
tree_breakpoint::visit_statement_list (tree_statement_list& lst)
{
  for (tree_statement *elt : lst)
    {
      if (elt)
        {
          elt->accept (*this);

          if (m_found)
            break;
        }
    }
}

void
tree_breakpoint::visit_switch_case (tree_switch_case&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_switch_case_list (tree_switch_case_list& lst)
{
  for (tree_switch_case *t : lst)
    {
      if (t->line () >= m_line)
        take_action (*t);

      if (! m_found)
        {
          tree_statement_list *stmt_lst = t->commands ();

          if (stmt_lst)
            stmt_lst->accept (*this);
        }

      if (m_found)
        break;
    }
}

void
tree_breakpoint::visit_switch_command (tree_switch_command& cmd)
{
  if (cmd.line () >= m_line)
    take_action (cmd);

  if (! m_found)
    {
      tree_switch_case_list *lst = cmd.case_list ();

      if (lst)
        lst->accept (*this);
    }
}

void
tree_breakpoint::visit_try_catch_command (tree_try_catch_command& cmd)
{
  tree_statement_list *try_code = cmd.body ();

  if (try_code)
    try_code->accept (*this);

  if (! m_found)
    {
      tree_statement_list *catch_code = cmd.cleanup ();

      if (catch_code)
        catch_code->accept (*this);
    }
}

void
tree_breakpoint::visit_unwind_protect_command (tree_unwind_protect_command& cmd)
{
  tree_statement_list *body = cmd.body ();

  if (body)
    body->accept (*this);

  if (! m_found)
    {
      tree_statement_list *cleanup = cmd.cleanup ();

      if (cleanup)
        cleanup->accept (*this);
    }
}

void
tree_breakpoint::take_action (tree& tr)
{
  if (m_action == set)
    {
      tr.set_breakpoint (m_condition);
      m_line = tr.line ();
      m_found = true;
    }
  else if (m_action == clear)
    {
      if (tr.is_breakpoint ())
        {
          tr.delete_breakpoint ();
          m_found = true;
        }
    }
  else if (m_action == list)
    {
      if (tr.is_breakpoint ())
        {
          m_bp_list.append (octave_value (tr.line ()));
          m_bp_cond_list.append (octave_value (tr.bp_cond ()));
        }
    }
  else
    panic_impossible ();
}

void
tree_breakpoint::take_action (tree_statement& stmt)
{
  int lineno = stmt.line ();

  if (m_action == set)
    {
      stmt.set_breakpoint (m_condition);
      m_line = lineno;
      m_found = true;
    }
  else if (m_action == clear)
    {
      if (stmt.is_breakpoint ())
        {
          stmt.delete_breakpoint ();
          m_found = true;
        }
    }
  else if (m_action == list)
    {
      if (stmt.is_breakpoint ())
        {
          m_bp_list.append (octave_value (lineno));
          m_bp_cond_list.append (octave_value (stmt.bp_cond ()));
        }
    }
  else
    panic_impossible ();
}

OCTAVE_END_NAMESPACE(octave)
