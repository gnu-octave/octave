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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>

#include "error.h"
#include "ov-usr-fcn.h"
#include "pr-output.h"
#include "pt-arg-list.h"
#include "pt-assign.h"
#include "pt-base.h"
#include "pt-binop.h"
#include "pt-cmd.h"
#include "pt-colon.h"
#include "pt-const.h"
#include "pt-decl.h"
#include "pt-except.h"
#include "pt-exp.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-indir.h"
#include "pt-jump.h"
#include "pt-loop.h"
#include "pt-mat.h"
#include "pt-misc.h"
#include "pt-plot.h"
#include "pt-pr-code.h"
#include "pt-select.h"
#include "pt-stmt.h"
#include "pt-unop.h"
#include "pt-pr-code.h"
#include "pt-walk.h"

void
tree_print_code::visit_argument_list (tree_argument_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_expression *elt = lst (p);

      lst.next (p);

      if (elt)
	{
	  elt->accept (*this);

	  if (p)
	    os << ", ";
	}
    }
}

void
tree_print_code::visit_binary_expression (tree_binary_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *op1 = expr.lhs ();

  if (op1)
    op1->accept (*this);

  os << " " << expr.oper () << " ";

  tree_expression *op2 = expr.rhs ();

  if (op2)
    op2->accept (*this);

  print_parens (expr, ")");
}

void
tree_print_code::visit_break_command (tree_break_command&)
{
  indent ();

  os << "break";
}

void
tree_print_code::visit_colon_expression (tree_colon_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *op1 = expr.base ();

  if (op1)
    op1->accept (*this);

  // Stupid syntax.

  tree_expression *op3 = expr.increment ();

  if (op3)
    {
      os << ":";
      op3->accept (*this);
    }

  tree_expression *op2 = expr.limit ();

  if (op2)
    {
      os << ":";
      op2->accept (*this);
    }

  print_parens (expr, ")");
}

void
tree_print_code::visit_continue_command (tree_continue_command&)
{
  indent ();

  os << "continue";
}

void
tree_print_code::visit_decl_command (tree_decl_command& cmd)
{
  indent ();

  os << cmd.name () << " ";

  tree_decl_init_list *init_list = cmd.initializer_list ();

  if (init_list)
    init_list->accept (*this);
}

void
tree_print_code::visit_decl_elt (tree_decl_elt& cmd)
{
  tree_identifier *id = cmd.ident ();

  if (id)
    id->accept (*this);

  tree_expression *expr = cmd.expression ();

  if (expr)
    {
      os << " = ";

      expr->accept (*this);
    }
}

void
tree_print_code::visit_decl_init_list (tree_decl_init_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_decl_elt *elt = lst (p);

      lst.next (p);

      if (elt)
	{
	  elt->accept (*this);

	  if (p)
	    os << ", ";
	}
    }
}

void
tree_print_code::visit_simple_for_command (tree_simple_for_command& cmd)
{
  indent ();

  os << "for ";

  tree_expression *lhs = cmd.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  os << " = ";

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  newline ();

  tree_statement_list *list = cmd.body ();

  if (list)
    {
      increment_indent_level ();
      list->accept (*this);
      decrement_indent_level ();
    }

  indent ();

  os << "endfor";
}

void
tree_print_code::visit_complex_for_command (tree_complex_for_command& cmd)
{
  indent ();

  os << "for [";

  tree_argument_list *lhs = cmd.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  os << "] = ";

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  newline ();

  tree_statement_list *list = cmd.body ();

  if (list)
    {
      increment_indent_level ();
      list->accept (*this);
      decrement_indent_level ();
    }

  indent ();

  os << "endfor";
}

void
tree_print_code::visit_octave_user_function (octave_user_function& fcn)
{
  reset ();

  visit_octave_user_function_header (fcn);

  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    {
      increment_indent_level ();
      cmd_list->accept (*this);
      decrement_indent_level ();
    }

  visit_octave_user_function_trailer (fcn);
}

void
tree_print_code::visit_octave_user_function_header (octave_user_function& fcn)
{
  indent ();

  os << "function ";

  tree_parameter_list *ret_list = fcn.return_list ();

  if (ret_list)
    {
      bool takes_var_return = fcn.takes_var_return ();

      int len = ret_list->length ();

      if (len > 1 || takes_var_return)
	os << "[";

      ret_list->accept (*this);

      if (takes_var_return)
	{
	  if (len > 0)
	    os << ", ";

	  os << "...";
	}

      if (len > 1 || takes_var_return)
	os << "]";

      os << " = ";
    }

  string fcn_name = fcn.function_name ();

  os << (fcn_name.empty () ? string ("(empty)") : fcn_name) << " ";

  tree_parameter_list *param_list = fcn.parameter_list ();

  if (param_list)
    {
      bool takes_varargs = fcn.takes_varargs ();

      int len = param_list->length ();

      if (len > 0 || takes_varargs)
	os << "(";

      param_list->accept (*this);

      if (takes_varargs)
	{
	  if (len > 0)
	    os << ", ";

	  os << "...";
	}

      if (len > 0 || takes_varargs)
	{
	  os << ")";
	  newline ();
	}
    }
  else
    {
      os << "()";
      newline ();
    }
}

void
tree_print_code::visit_octave_user_function_trailer (octave_user_function&)
{
  indent ();

  os << "endfunction";

  newline ();
}

void
tree_print_code::visit_identifier (tree_identifier& id)
{
  indent ();

  print_parens (id, "(");

  string nm = id.name ();
  os << (nm.empty () ? string ("(empty)") : nm);

  print_parens (id, ")");
}

void
tree_print_code::visit_if_clause (tree_if_clause& cmd)
{
  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  newline ();

  increment_indent_level ();

  tree_statement_list *list = cmd.commands ();

  if (list)
    {
      list->accept (*this);

      decrement_indent_level ();
    }
}

void
tree_print_code::visit_if_command (tree_if_command& cmd)
{
  indent ();

  os << "if ";

  tree_if_command_list *list = cmd.cmd_list ();

  if (list)
    list->accept (*this);

  indent ();

  os << "endif";
}

void
tree_print_code::visit_if_command_list (tree_if_command_list& lst)
{
  Pix p = lst.first ();

  bool first_elt = true;

  while (p)
    {
      tree_if_clause *elt = lst (p);

      if (elt)
	{
	  if (! first_elt)
	    {
	      indent ();

	      if (elt->is_else_clause ())
		os << "else";
	      else
		os << "elseif ";
	    }

	  elt->accept (*this);
	}

      first_elt = false;
      lst.next (p);
    }
}

void
tree_print_code::visit_index_expression (tree_index_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *e= expr.expression ();

  if (e)
    e->accept (*this);

  tree_argument_list *list = expr.arg_list ();

  if (list)
    {
      os << " (";
      list->accept (*this);
      os << ")";
    }

  print_parens (expr, ")");
}

void
tree_print_code::visit_indirect_ref (tree_indirect_ref& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *e = expr.expression ();

  if (e)
    e->accept (*this);

  os << "." << expr.elt_name ();

  print_parens (expr, ")");
}

void
tree_print_code::visit_matrix (tree_matrix& lst)
{
  indent ();

  print_parens (lst, "(");

  os << "[";

  Pix p = lst.first ();

  while (p)
    {
      tree_argument_list *elt = lst (p);

      lst.next (p);

      if (elt)
	{
	  elt->accept (*this);

	  if (p)
	    os << "; ";
	}
    }

  os << "]";

  print_parens (lst, ")");
}

void
tree_print_code::visit_multi_assignment (tree_multi_assignment& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_argument_list *lhs = expr.left_hand_side ();

  if (lhs)
    {
      int len = lhs->length ();

      if (len > 1)
	os << "[";

      lhs->accept (*this);

      if (len > 1)
	os << "]";
    }

  os << " = ";

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);

  print_parens (expr, ")");
}

void
tree_print_code::visit_no_op_command (tree_no_op_command& cmd)
{
  indent ();

  os << cmd.original_command ();
}

void
tree_print_code::visit_oct_obj (tree_oct_obj&)
{
  ::error ("visit_oct_obj: internal error");
}

void
tree_print_code::visit_constant (tree_constant& val)
{
  indent ();

  print_parens (val, "(");

  val.print_raw (os, true, print_original_text);

  print_parens (val, ")");
}

void
tree_print_code::visit_parameter_list (tree_parameter_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_identifier *elt = lst (p);

      lst.next (p);

      if (elt)
	{
	  elt->accept (*this);

	  if (p)
	    os << ", ";
	}
    }
}

void
tree_print_code::visit_plot_command (tree_plot_command& cmd)
{
  indent ();

  int ndim = cmd.num_dimensions ();

  switch (ndim)
    {
    case 1:
      os << "replot";
      break;

    case 2:
      os << "gplot";
      break;

    case 3:
      os << "gsplot";
      break;

    default:
      os << "<unkown plot command>";
      break;
    }

  plot_limits *range = cmd.limits ();

  if (range)
    range->accept (*this);

  subplot_list *plot_list = cmd.subplots ();

  if (plot_list)
    plot_list->accept (*this);
}

void
tree_print_code::visit_plot_limits (plot_limits& cmd)
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
tree_print_code::visit_plot_range (plot_range& cmd)
{
  os << " [";

  tree_expression *lower = cmd.lower_bound ();

  if (lower)
    lower->accept (*this);

  os << ":";

  tree_expression *upper = cmd.upper_bound ();

  if (upper)
    upper->accept (*this);

  os << "]";
}

void
tree_print_code::visit_postfix_expression (tree_postfix_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);

  os << expr.oper ();

  print_parens (expr, ")");
}

void
tree_print_code::visit_prefix_expression (tree_prefix_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  os << expr.oper ();

  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);

  print_parens (expr, ")");
}

void
tree_print_code::visit_return_command (tree_return_command&)
{
  indent ();

  os << "return";
}

void
tree_print_code::visit_return_list (tree_return_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_index_expression *elt = lst (p);

      lst.next (p);

      if (elt)
	{
	  elt->accept (*this);

	  if (p)
	    os << ", ";
	}
    }
}

void
tree_print_code::visit_simple_assignment (tree_simple_assignment& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *lhs = expr.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  os << " " << expr.oper () << " ";

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);

  print_parens (expr, ")");
}

void
tree_print_code::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();

  if (cmd)
    {
      cmd->accept (*this);

      if (! stmt.print_result ())
	os << ";";

      newline ();
    }
  else
    {
      tree_expression *expr = stmt.expression ();

      if (expr)
	{
	  expr->accept (*this);

	  if (! stmt.print_result ())
	    os << ";";

	  newline ();
	}
    }
}

void
tree_print_code::visit_statement_list (tree_statement_list& lst)
{
  for (Pix p = lst.first (); p != 0; lst.next (p))
    {
      tree_statement *elt = lst (p);

      if (elt)
	elt->accept (*this);
    }
}

void
tree_print_code::visit_subplot (subplot& cmd)
{
  tree_expression *sp_plot_data = cmd.plot_data ();

  if (sp_plot_data)
    {
      os << " ";

      sp_plot_data->accept (*this);
    }

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
tree_print_code::visit_subplot_list (subplot_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      subplot *elt = lst (p);

      lst.next (p);

      if (elt)
	{
	  elt->accept (*this);

	  if (p)
	    os << ",";
	}
    }
}

void
tree_print_code::visit_subplot_style (subplot_style& cmd)
{
  os << " with " << cmd.style ();

  tree_expression *sp_linetype = cmd.linetype ();

  if (sp_linetype)
    {
      os << " ";

      sp_linetype->accept (*this);
    }

  tree_expression *sp_pointtype = cmd.pointtype ();

  if (sp_pointtype)
    {
      os << " ";

      sp_pointtype->accept (*this);
    }
}

void
tree_print_code::visit_subplot_using (subplot_using& cmd)
{
  os << " using ";

  int qual_count = cmd.qualifier_count ();

  if (qual_count > 0)
    {
      tree_expression **x = cmd.qualifiers ();

      for (int i = 0; i < qual_count; i++)
	{
	  if (i > 0)
	    os << ":";

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
tree_print_code::visit_switch_case (tree_switch_case& cs)
{
  indent ();

  if (cs.is_default_case ())
    os << "otherwise";
  else
    os << "case ";

  tree_expression *label = cs.case_label ();

  if (label)
    label->accept (*this);

  newline ();

  increment_indent_level ();

  tree_statement_list *list = cs.commands ();

  if (list)
    {
      list->accept (*this);

      decrement_indent_level ();
    }
}

void
tree_print_code::visit_switch_case_list (tree_switch_case_list& lst)
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
tree_print_code::visit_switch_command (tree_switch_command& cmd)
{
  indent ();

  os << "switch ";

  tree_expression *expr = cmd.switch_value ();

  if (expr)
    expr->accept (*this);

  newline ();

  increment_indent_level ();

  tree_switch_case_list *list = cmd.case_list ();

  if (list)
    list->accept (*this);

  indent ();

  os << "endswitch";
}

void
tree_print_code::visit_try_catch_command (tree_try_catch_command& cmd)
{
  indent ();

  os << "try_catch";

  newline ();

  tree_statement_list *try_code = cmd.body ();

  if (try_code)
    {
      increment_indent_level ();
      try_code->accept (*this);
      decrement_indent_level ();
    }

  indent ();

  os << "catch";

  newline ();

  tree_statement_list *catch_code = cmd.cleanup ();

  if (catch_code)
    {
      increment_indent_level ();
      catch_code->accept (*this);
      decrement_indent_level ();
    }

  indent ();

  os << "end_try_catch";
}

void
tree_print_code::visit_unwind_protect_command
  (tree_unwind_protect_command& cmd)
{
  indent ();

  os << "unwind_protect";

  newline ();

  tree_statement_list *unwind_protect_code = cmd.body ();

  if (unwind_protect_code)
    {
      increment_indent_level ();
      unwind_protect_code->accept (*this);
      decrement_indent_level ();
    }

  indent ();

  os << "cleanup_code";

  newline ();

  tree_statement_list *cleanup_code = cmd.cleanup ();

  if (cleanup_code)
    {
      increment_indent_level ();
      cleanup_code->accept (*this);
      decrement_indent_level ();
    }

  indent ();

  os << "end_unwind_protect";
}

void
tree_print_code::visit_while_command (tree_while_command& cmd)
{
  indent ();

  os << "while ";

  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  newline ();

  tree_statement_list *list = cmd.body ();

  if (list)
    {
      increment_indent_level ();
      list->accept (*this);
      decrement_indent_level ();
    }

  indent ();

  os << "endwhile";
}

// Current indentation.
int tree_print_code::curr_print_indent_level = 0;

// Nonzero means we are at the beginning of a line.
bool tree_print_code::beginning_of_line = true;

// Each print_code() function should call this before printing
// anything.
//
// This doesn't need to be fast, but isn't there a better way?

void
tree_print_code::indent (void)
{
  assert (curr_print_indent_level >= 0);
 
  if (beginning_of_line)
    {
      os << prefix;

      for (int i = 0; i < curr_print_indent_level; i++)
	os << " ";

      beginning_of_line = false;
    }
}

// All print_code() functions should use this to print new lines.

void
tree_print_code::newline (void)
{
  os << "\n";

  beginning_of_line = true;
}

// For ressetting print_code state.

void
tree_print_code::reset (void)
{
  beginning_of_line = true;
  curr_print_indent_level = 0;
}

void
tree_print_code::print_parens (const tree_expression& expr, const char *txt)
{
  int n = expr.paren_count ();

  for (int i = 0; i < n; i++)
    os << txt;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
