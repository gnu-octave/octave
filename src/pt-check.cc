/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__) && ! defined (NO_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "input.h"
#include "ov-usr-fcn.h"
#include "pt-all.h"

void
tree_checker::visit_argument_list (tree_argument_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_expression *elt = lst (p);

      lst.next (p);

      if (elt)
	{
	  if (do_lvalue_check && ! elt->lvalue_ok ())
	    gripe ("invalid lvalue in multiple assignment", elt->line ());
	}
    }
}

void
tree_checker::visit_binary_expression (tree_binary_expression& expr)
{
  tree_expression *op1 = expr.lhs ();

  if (op1)
    op1->accept (*this);

  tree_expression *op2 = expr.rhs ();

  if (op2)
    op2->accept (*this);
}

void
tree_checker::visit_break_expression (tree_break_expression&)
{
}

void
tree_checker::visit_colon_expression (tree_colon_expression& expr)
{
  tree_expression *op1 = expr.base ();

  if (op1)
    op1->accept (*this);

  tree_expression *op3 = expr.increment ();

  if (op3)
    op3->accept (*this);

  tree_expression *op2 = expr.limit ();

  if (op2)
    op2->accept (*this);
}

void
tree_checker::visit_continue_expression (tree_continue_expression&)
{
}

void
tree_checker::visit_decl_command (tree_decl_command& cmd)
{
  tree_decl_init_list *init_list = cmd.initializer_list ();

  if (init_list)
    init_list->accept (*this);
}

void
tree_checker::visit_decl_elt (tree_decl_elt& cmd)
{
  tree_identifier *id = cmd.ident ();

  if (id)
    id->accept (*this);

  tree_expression *expr = cmd.expression ();

  if (expr)
    expr->accept (*this);
}

void
tree_checker::visit_decl_init_list (tree_decl_init_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_decl_elt *elt = lst (p);

      lst.next (p);

      if (elt)
	elt->accept (*this);
    }
}

void
tree_checker::visit_simple_for_command (tree_simple_for_command& cmd)
{
  tree_expression *lhs = cmd.left_hand_side ();

  if (lhs)
    {
      if (! lhs->lvalue_ok ())
	gripe ("invalid lvalue in for command", cmd.line ());
    }

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_complex_for_command (tree_complex_for_command& cmd)
{
  tree_argument_list *lhs = cmd.left_hand_side ();

  if (lhs)
    {
      int len = lhs->length ();

      if (len == 0 || len > 2)
	gripe ("invalid number of output arguments in for command",
	       cmd.line ());

      do_lvalue_check = true;

      lhs->accept (*this);

      do_lvalue_check = false;
    }

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_octave_user_function (octave_user_function& fcn)
{
  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    cmd_list->accept (*this);
}

void
tree_checker::visit_identifier (tree_identifier& /* id */)
{
}

void
tree_checker::visit_if_clause (tree_if_clause& cmd)
{
  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.commands ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_if_command (tree_if_command& cmd)
{
  tree_if_command_list *list = cmd.cmd_list ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_if_command_list (tree_if_command_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_if_clause *elt = lst (p);

      if (elt)
	elt->accept (*this);

      lst.next (p);
    }
}

void
tree_checker::visit_index_expression (tree_index_expression& expr)
{
  tree_expression *e = expr.expression ();

  if (e)
    e->accept (*this);

  SLList<tree_argument_list *> lst = expr.arg_lists ();

  Pix p = lst.first ();

  while (p)
    {
      tree_argument_list *elt = lst (p);

      if (elt)
	elt->accept (*this);

      lst.next (p);
    }
}

void
tree_checker::visit_matrix (tree_matrix& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_argument_list *elt = lst (p);

      lst.next (p);

      if (elt)
	elt->accept (*this);
    }
}

void
tree_checker::visit_multi_assignment (tree_multi_assignment& expr)
{
  tree_argument_list *lhs = expr.left_hand_side ();

  if (lhs)
    {
      do_lvalue_check = true;

      lhs->accept (*this);

      do_lvalue_check = false;
    }

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);
}

void
tree_checker::visit_no_op_command (tree_no_op_command& /* cmd */)
{
}

void
tree_checker::visit_constant (tree_constant& /* val */)
{
}

void
tree_checker::visit_parameter_list (tree_parameter_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_identifier *elt = lst (p);

      lst.next (p);

      if (elt)
	elt->accept (*this);
    }
}

void
tree_checker::visit_plot_command (tree_plot_command& cmd)
{
  plot_limits *range = cmd.limits ();

  if (range)
    range->accept (*this);

  subplot_list *plot_list = cmd.subplots ();

  if (plot_list)
    plot_list->accept (*this);
}

void
tree_checker::visit_plot_limits (plot_limits& cmd)
{
  plot_range *x_range = cmd.x_limits ();

  if (x_range)
    x_range->accept (*this);

  plot_range *y_range = cmd.y_limits ();

  if (y_range)
    y_range->accept (*this);

  plot_range *z_range = cmd.z_limits ();

  if (z_range)
    z_range->accept (*this);
}

void
tree_checker::visit_plot_range (plot_range& cmd)
{
  tree_expression *lower = cmd.lower_bound ();

  if (lower)
    lower->accept (*this);

  tree_expression *upper = cmd.upper_bound ();

  if (upper)
    upper->accept (*this);
}

void
tree_checker::visit_postfix_expression (tree_postfix_expression& expr)
{
  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);
}

void
tree_checker::visit_prefix_expression (tree_prefix_expression& expr)
{
  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);
}

void
tree_checker::visit_return_expression (tree_return_expression&)
{
}

void
tree_checker::visit_return_list (tree_return_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_index_expression *elt = lst (p);

      lst.next (p);

      if (elt)
	elt->accept (*this);
    }
}

void
tree_checker::visit_simple_assignment (tree_simple_assignment& expr)
{
  tree_expression *lhs = expr.left_hand_side ();

  if (lhs)
    {
      if (! lhs->lvalue_ok ())
	gripe ("invalid lvalue in assignment", expr.line ());
    }

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);
}

void
tree_checker::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();

  if (cmd)
    cmd->accept (*this);
  else
    {
      tree_expression *expr = stmt.expression ();

      if (expr)
	expr->accept (*this);
    }
}

void
tree_checker::visit_statement_list (tree_statement_list& lst)
{
  for (Pix p = lst.first (); p != 0; lst.next (p))
    {
      tree_statement *elt = lst (p);

      if (elt)
	elt->accept (*this);
    }
}

void
tree_checker::visit_subplot (subplot& cmd)
{
  tree_expression *sp_plot_data = cmd.plot_data ();

  if (sp_plot_data)
    sp_plot_data->accept (*this);

  subplot_using *sp_using_clause = cmd.using_clause ();

  if (sp_using_clause)
    sp_using_clause->accept (*this);

  tree_expression *sp_title_clause = cmd.title_clause ();

  if (sp_title_clause)
    sp_title_clause->accept (*this);

  subplot_style *sp_style_clause = cmd.style_clause ();

  if (sp_style_clause)
    sp_style_clause->accept (*this);
}

void
tree_checker::visit_subplot_list (subplot_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      subplot *elt = lst (p);

      lst.next (p);

      if (elt)
	elt->accept (*this);
    }
}

void
tree_checker::visit_subplot_style (subplot_style& cmd)
{
  tree_expression *sp_linetype = cmd.linetype ();

  if (sp_linetype)
    sp_linetype->accept (*this);

  tree_expression *sp_pointtype = cmd.pointtype ();

  if (sp_pointtype)
    sp_pointtype->accept (*this);
}

void
tree_checker::visit_subplot_using (subplot_using& cmd)
{
  int qual_count = cmd.qualifier_count ();

  if (qual_count > 0)
    {
      tree_expression **x = cmd.qualifiers ();

      for (int i = 0; i < qual_count; i++)
	{
	  if (x[i])
	    x[i]->accept (*this);
	}
    }
  else
    {
      tree_expression *scanf_fmt = cmd.scanf_format ();

      if (scanf_fmt)
	scanf_fmt->accept (*this);
    }
}

void
tree_checker::visit_switch_case (tree_switch_case& cs)
{
  tree_expression *label = cs.case_label ();

  if (label)
    label->accept (*this);

  tree_statement_list *list = cs.commands ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_switch_case_list (tree_switch_case_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_switch_case *elt = lst (p);

      if (elt)
	elt->accept (*this);

      lst.next (p);
    }
}

void
tree_checker::visit_switch_command (tree_switch_command& cmd)
{
  tree_expression *expr = cmd.switch_value ();

  if (expr)
    expr->accept (*this);

  tree_switch_case_list *list = cmd.case_list ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_try_catch_command (tree_try_catch_command& cmd)
{
  tree_statement_list *try_code = cmd.body ();

  if (try_code)
    try_code->accept (*this);

  tree_statement_list *catch_code = cmd.cleanup ();

  if (catch_code)
    catch_code->accept (*this);
}

void
tree_checker::visit_unwind_protect_command
  (tree_unwind_protect_command& cmd)
{
  tree_statement_list *unwind_protect_code = cmd.body ();

  if (unwind_protect_code)
    unwind_protect_code->accept (*this);

  tree_statement_list *cleanup_code = cmd.cleanup ();

  if (cleanup_code)
    cleanup_code->accept (*this);
}

void
tree_checker::visit_while_command (tree_while_command& cmd)
{
  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void
tree_checker::gripe (const std::string& msg, int line)
{
  if (curr_fcn_file_name.empty ())
    error ("%s", msg.c_str ());
  else
    error ("%s: %d: %s", curr_fcn_file_name.c_str (), line, msg.c_str ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
