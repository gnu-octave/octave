/*

Copyright (C) 1996 John W. Eaton

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
#include "input.h"
#include "pr-output.h"
#include "pt-pr-code.h"

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

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

  tree_expression *op1 = expr.lhs ();

  if (op1)
    op1->accept (*this);

  os << " " << expr.oper () << " ";

  tree_expression *op2 = expr.rhs ();

  if (op2)
    op2->accept (*this);

  if (in_parens)
    os << ")";
}

void
tree_print_code::visit_break_command (tree_break_command&)
{
  indent ();

  os << "break";
}

void
tree_print_code::visit_builtin (tree_builtin& fcn)
{
  os << fcn.name ()
     << " can't be printed because it is a built-in function\n";
}

void
tree_print_code::visit_colon_expression (tree_colon_expression& expr)
{
  indent ();

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

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

  if (in_parens)
    os << ")";
}

void
tree_print_code::visit_continue_command (tree_continue_command&)
{
  indent ();

  os << "continue";
}

void
tree_print_code::visit_for_command (tree_for_command& cmd)
{
  indent ();

  os << "for ";

  tree_index_expression *id = cmd.ident ();

  if (id)
    id->accept (*this);

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
tree_print_code::visit_function (tree_function& fcn)
{
  reset ();

  visit_function_header (fcn);

  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    {
      increment_indent_level ();
      cmd_list->accept (*this);
      decrement_indent_level ();
    }

  visit_function_trailer (fcn);
}

void
tree_print_code::visit_function_header (tree_function& fcn)
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
tree_print_code::visit_function_trailer (tree_function&)
{
  indent ();

  os << "endfunction";

  newline ();
}

void
tree_print_code::visit_global (tree_global& cmd)
{
  tree_identifier *id = cmd.ident ();

  if (id)
    id->accept (*this);

  tree_simple_assignment_expression *ass_expr = cmd.assign_expr ();

  if (ass_expr)
    ass_expr->accept (*this);
}

void
tree_print_code::visit_global_command (tree_global_command& cmd)
{
  indent ();

  os << "global ";

  tree_global_init_list *init_list = cmd.initializer_list ();

  if (init_list)
    init_list->accept (*this);
}

void
tree_print_code::visit_global_init_list (tree_global_init_list& lst)
{
  Pix p = lst.first ();

  while (p)
    {
      tree_global *elt = lst (p);

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
tree_print_code::visit_identifier (tree_identifier& id)
{
  indent ();

  bool in_parens = id.is_in_parens ();

  if (in_parens)
    os << "(";

  string nm = id.name ();
  os << (nm.empty () ? string ("(empty)") : nm);

  if (in_parens)
    os << ")";
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

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

  tree_indirect_ref *id = expr.ident ();

  if (id)
    id->accept (*this);

  tree_argument_list *list = expr.arg_list ();

  if (list)
    {
      os << " (";
      list->accept (*this);
      os << ")";
    }

  if (in_parens)
    os << ")";
}

void
tree_print_code::visit_indirect_ref (tree_indirect_ref& expr)
{
  indent ();

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

  // The name of the indirect ref includes the sub-elements.

  string nm = expr.name ();
  os << (nm.empty () ? string ("(empty)") : nm);

  if (in_parens)
    os << ")";
}

void
tree_print_code::visit_matrix (tree_matrix& lst)
{
  indent ();

  bool in_parens = lst.is_in_parens ();

  if (in_parens)
    os << "(";

  os << "[";

  Pix p = lst.first ();

  while (p)
    {
      tree_matrix_row *elt = lst (p);

      lst.next (p);

      if (elt)
	{
	  elt->accept (*this);

	  if (p)
	    os << "; ";
	}
    }

  os << "]";

  if (in_parens)
    os << ")";
}

void
tree_print_code::visit_matrix_row (tree_matrix_row& lst)
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
tree_print_code::visit_multi_assignment_expression
  (tree_multi_assignment_expression& expr)
{
  indent ();

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

  tree_return_list *lhs = expr.left_hand_side ();

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

  tree_multi_val_ret *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);

  if (in_parens)
    os << ")";
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

  bool in_parens = val.is_in_parens ();

  if (in_parens)
    os << "(";

  if (val.is_real_scalar ())
    {
      string orig_text = val.original_text ();

      if (orig_text.empty ())
	octave_print_internal (os, val.double_value (), 1);
      else
	os << orig_text;
    }
  else if (val.is_real_matrix ())
    {
      octave_print_internal (os, val.matrix_value (), 1);
    }
  else if (val.is_complex_scalar ())
    {
      Complex cs = val.complex_value ();

      double re = cs.real ();
      double im = cs.imag ();

      // If we have the original text and a pure imaginary, just
      // print the original text, because this must be a constant
      // that was parsed as part of a function.

      string orig_text = val.original_text ();

      if (! orig_text.empty () && re == 0.0 && im > 0.0)
	os << orig_text;
      else
	octave_print_internal (os, cs, 1);
    }
  else if (val.is_complex_matrix ())
    {
      octave_print_internal (os, val.complex_matrix_value (), 1);
    }
  else if (val.is_char_matrix ())
    {
      octave_print_internal (os, val.char_matrix_value (), 1);
    }
  else if (val.is_string ())
    {
      octave_print_internal (os, val.all_strings (), 1, 1);
    }
  else if (val.is_range ())
    {
      octave_print_internal (os, val.range_value (), 1);
    }
  else if (val.is_magic_colon ())
    {
      os << ":";
    }
  else if (val.is_all_va_args ())
    {
      os << "all_va_args";
    }
  else
    {
      panic_impossible ();
    }

  if (in_parens)
    os << ")";
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

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

  tree_identifier *id = expr.ident ();

  if (id)
    id->accept (*this);

  os << expr.oper ();

  if (in_parens)
    os << ")";
}

void
tree_print_code::visit_prefix_expression (tree_prefix_expression& expr)
{
  indent ();

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

  os << expr.oper ();

  tree_identifier *id = expr.ident ();

  if (id)
    id->accept (*this);

  if (in_parens)
    os << ")";
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
tree_print_code::visit_simple_assignment_expression
  (tree_simple_assignment_expression& expr)
{
  indent ();

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

  if (! expr.is_ans_assign ())
    {
      tree_indirect_ref *lhs = expr.left_hand_side ();

      if (lhs)
	lhs->accept (*this);

      tree_argument_list *index = expr.lhs_index ();

      if (index)
	{
	  os << " (";
	  index->accept (*this);
	  os << ")";
	}

      os << " = ";
    }

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);

  if (in_parens)
    os << ")";
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
tree_print_code::visit_unary_expression (tree_unary_expression& expr)
{
  indent ();

  bool in_parens = expr.is_in_parens ();

  if (in_parens)
    os << "(";

  tree_expression *op = expr.operand ();

  if (expr.is_prefix_op ())
    {
      os << expr.oper ();

      if (op)
	op->accept (*this);
    }
  else
    {
      if (op)
	op->accept (*this);

      os << expr.oper ();
    }

  if (in_parens)
    os << ")";
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
      os.form ("%s%*s", Vps4.c_str (), curr_print_indent_level, "");
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
