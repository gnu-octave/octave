// tree-cmd.cc                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined (__GNUG__)
#pragma implementation
#endif

#include <iostream.h>

// Nonzero means we're breaking out of a loop.
int breaking = 0;

// Nonzero means we're jumping to the end of a loop.
int continuing = 0;

// Nonzero means we're returning from a function.  Global because it
// is also needed in tree-expr.cc.
int returning = 0;

#include "user-prefs.h"
#include "variables.h"
#include "symtab.h"
#include "error.h"
#include "gripes.h"
#include "tree-base.h"
#include "tree-expr.h"
#include "tree-cmd.h"
#include "tree-misc.h"
#include "tree-const.h"

// Decide if it's time to quit a for or while loop.
static int
quit_loop_now (void)
{
// Maybe handle `continue N' someday...

  if (continuing)
    continuing--;

  int quit = (returning || breaking || continuing);

  if (breaking)
    breaking--;

  return quit;
}

// But first, some extra functions used by the tree classes.

// We seem to have no use for this now.  Maybe it will be needed at
// some future date, so here it is.
#if 0

// Convert a linked list of trees to a vector of pointers to trees.

static tree **
list_to_vector (tree *list, int& len)
{
  len = list->length () + 1;

  tree **args = new tree * [len];

// args[0] may eventually hold something useful, like the function
// name.
  tree *tmp_list = list;
  for (int k = 1; k < len; k++)
    {
      args[k] = tmp_list;
      tmp_list = tmp_list->next_elem ();
    }
  return args;
}
#endif

// Global.

tree_global_command::~tree_global_command (void)
{
  delete init_list;
}

void
tree_global_command::eval (void)
{
  if (init_list)
    init_list->eval ();

  if (error_state > 0)
    ::error ("evaluating global command near line %d, column %d",
	     line (), column ());
}

void
tree_global_command::print_code (ostream& os)
{
  print_code_indent (os);

  os << "global ";

  if (init_list)
    init_list->print_code (os);
}

// While.

tree_while_command::~tree_while_command (void)
{
  delete expr;
  delete list;
}

void
tree_while_command::eval (void)
{
  if (error_state)
    return;

  for (;;)
    {
      int expr_value = 0;
      if (! expr)
	return;
      tree_constant t1 = expr->eval (0);

      if (error_state)
	{
	  eval_error ();
	  return;
	}

      if (t1.rows () == 0 || t1.columns () == 0)
	{
	  int flag = user_pref.propagate_empty_matrices;
	  if (flag < 0)
	    warning ("while: empty matrix used in conditional");
	  else if (flag == 0)
	    {
	      ::error ("while: empty matrix used in conditional");
	      return;
	    }
	  t1 = tree_constant (0.0);
	}
      else if (! t1.is_scalar_type ())
	{
	  tree_constant t2 = t1.all ();
	  t1 = t2.all ();
	}

      tree_constant_rep::constant_type t = t1.const_type ();
      if (t == tree_constant_rep::scalar_constant)
	expr_value = (int) t1.double_value ();
      else if (t == tree_constant_rep::complex_scalar_constant)
	expr_value = t1.complex_value () != 0.0;
      else
	panic_impossible ();

      if (expr_value)
	{
	  if (list)
	    {
	      list->eval (1);
	      if (error_state)
		{
		  eval_error ();
		  return;
		}
	    }

	  if (quit_loop_now ())
	    break;
	}
      else
	break;
    }
}

void
tree_while_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating while command near line %d, column %d",
	     line (), column ());
}

void
tree_while_command::print_code (ostream& os)
{
  print_code_indent (os);

  os << "while ";

  if (expr)
    expr->print_code (os);

  print_code_new_line (os);

  if (list)
    {
      increment_indent_level ();
      list->print_code (os);
      decrement_indent_level ();
    }

  print_code_indent (os);

  os << "endwhile";
}

// For.

tree_for_command::~tree_for_command (void)
{
  delete id;
  delete expr;
  delete list;
}

void
tree_for_command::eval (void)
{
  if (error_state || ! expr)
    return;

  tree_constant tmp_expr = expr->eval (0);

  if (error_state || tmp_expr.is_undefined ())
    {
      eval_error ();
      return;
    }

  tree_constant_rep::constant_type expr_type = tmp_expr.const_type ();
  switch (expr_type)
    {
    case tree_constant_rep::complex_scalar_constant:
    case tree_constant_rep::scalar_constant:
      {
	tree_constant *rhs = new tree_constant (tmp_expr);
	int quit = 0;
	do_for_loop_once (rhs, quit);
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
    case tree_constant_rep::matrix_constant:
      {
	Matrix m_tmp;
	ComplexMatrix cm_tmp;
	int nr;
	int steps;
	if (expr_type == tree_constant_rep::matrix_constant)
	  {
	    m_tmp = tmp_expr.matrix_value ();
	    nr = m_tmp.rows ();
	    steps = m_tmp.columns ();
	  }
	else
	  {
	    cm_tmp = tmp_expr.complex_matrix_value ();
	    nr = cm_tmp.rows ();
	    steps = cm_tmp.columns ();
	  }

	for (int i = 0; i < steps; i++)
	  {
	    tree_constant *rhs;

	    if (nr == 1)
	      {
		if (expr_type == tree_constant_rep::matrix_constant)
		  rhs = new tree_constant (m_tmp (0, i));
		else
		  rhs = new tree_constant (cm_tmp (0, i));
	      }
	    else
	      {
		if (expr_type == tree_constant_rep::matrix_constant)
		  rhs = new tree_constant (m_tmp.extract (0, i, nr-1, i));
		else
		  rhs = new tree_constant (cm_tmp.extract (0, i, nr-1, i));
	      }

	    int quit = 0;
	    do_for_loop_once (rhs, quit);
	    if (quit)
	      break;
	  }
      }
      break;
    case tree_constant_rep::string_constant:
      gripe_string_invalid ();
      break;
    case tree_constant_rep::range_constant:
      {
	Range rng = tmp_expr.range_value ();

	int steps = rng.nelem ();
	double b = rng.base ();
	double increment = rng.inc ();

	for (int i = 0; i < steps; i++)
	  {
	    double tmp_val = b + i * increment;

	    tree_constant *rhs = new tree_constant (tmp_val);

	    int quit = 0;
	    do_for_loop_once (rhs, quit);
	    if (quit)
	      break;
	  }
      }
      break;
    default:
      panic_impossible ();
      break;
    }
}

void
tree_for_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating for command near line %d, column %d",
	     line (), column ());
}

void
tree_for_command::do_for_loop_once (tree_constant *rhs, int& quit)
{
  quit = 0;

  tree_simple_assignment_expression tmp_ass (id, rhs, 1);

  tmp_ass.eval (0);

  if (error_state)
    {
      eval_error ();
      return;
    }

  if (list)
    {
      list->eval (1);
      if (error_state)
	{
	  eval_error ();
	  quit = 1;
	  return;
	}
    }

  quit = quit_loop_now ();
}

void
tree_for_command::print_code (ostream& os)
{
  print_code_indent (os);

  os << "for ";

  if (id)
    id->print_code (os);

  os << " = ";

  if (expr)
    expr->print_code (os);

  print_code_new_line (os);

  if (list)
    {
      increment_indent_level ();
      list->print_code (os);
      decrement_indent_level ();
    }

  print_code_indent (os);

  os << "endfor";
}

// If.

tree_if_command::~tree_if_command (void)
{
  delete list;
}

void
tree_if_command::eval (void)
{
  if (list)
    list->eval ();

  if (error_state > 0)
    ::error ("evaluating if command near line %d, column %d",
	     line (), column ());
}

void
tree_if_command::print_code (ostream& os)
{
  print_code_indent (os);

  os << "if ";

  if (list)
    list->print_code (os);

  print_code_indent (os);

  os << "endif";
}

// Break.

void
tree_break_command::eval (void)
{
  if (! error_state)
    breaking = 1;
}

void
tree_break_command::print_code (ostream& os)
{
  print_code_indent (os);

  os << "break";
}

// Continue.

void
tree_continue_command::eval (void)
{
  if (! error_state)
    continuing = 1;
}

void
tree_continue_command::print_code (ostream& os)
{
  print_code_indent (os);

  os << "continue";
}

// Return.

void
tree_return_command::eval (void)
{
  if (! error_state)
    returning = 1;
}

void
tree_return_command::print_code (ostream& os)
{
  print_code_indent (os);

  os << "return";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
