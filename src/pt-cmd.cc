// Tree class.                                          -*- C++ -*-
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

// For NULL.
#include <stdio.h>

#include "user-prefs.h"
#include "variables.h"
#include "symtab.h"
#include "error.h"
#include "gripes.h"
#include "tree.h"
#include "tree-cmd.h"
#include "tree-const.h"

// Nonzero means we're breaking out of a loop.
static int breaking = 0;

// Nonzero means we're jumping to the end of a loop.
static int continuing = 0;

// Nonzero means we're returning from a function.  Global because it
// is also needed in tree-expr.cc.
int returning = 0;

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
/*
 * Convert a linked list of trees to a vector of pointers to trees.
 */
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

/*
 * A command or two to be executed.
 */
tree_command_list::tree_command_list (void)
{
  command = NULL_TREE;
  print_flag = 1;
  next = (tree_command_list *) NULL;
}

tree_command_list::tree_command_list (tree *t)
{
  command = t;
  print_flag = 1;
  next = (tree_command_list *) NULL;
}

tree_command_list::~tree_command_list (void)
{
  delete command;
  delete next;
}

void
tree_command_list::set_print_flag (int flag)
{
  print_flag = flag;
}

tree_command_list *
tree_command_list::chain (tree *t)
{
  tree_command_list *tmp = new tree_command_list (t);
  tmp->next = this;
  return tmp;
}

tree_command_list *
tree_command_list::reverse (void)
{
  tree_command_list *list = this;
  tree_command_list *next;
  tree_command_list *prev = (tree_command_list *) NULL;

  while (list != (tree_command_list *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

tree_constant
tree_command_list::eval (int print)
{
  int pf;
  tree_constant retval;

  if (error_state)
    return retval;

  tree_command_list *list;
  for (list = this; list != (tree_command_list *) NULL; list = list->next)
    {
      if (print == 0)
	pf = 0;
      else
	pf = list->print_flag;

      tree *cmd = list->command;
      if (cmd == NULL_TREE)
	retval = tree_constant ();
      else
	{
	  retval = cmd->eval (pf);

	  if (error_state)
	    return tree_constant ();

	  if (breaking || continuing)
	    break;

	  if (returning)
	    break;
	}
    }
  return retval;
}

/*
 * Global.
 */
tree_global_command::tree_global_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  sr = (symbol_record *) NULL;
  rhs = (tree_expression *) NULL;
  next = (tree_global_command *) NULL;
}

tree_global_command::tree_global_command (symbol_record *s,
					  int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  sr = s;
  rhs = (tree_expression *) NULL;
  next = (tree_global_command *) NULL;
}

tree_global_command::tree_global_command (symbol_record *s,
					  tree_expression *e,
					  int l = -1, int c = -1) 
{
  line_num = l;
  column_num = c;
  sr = s;
  rhs = e;
  next = (tree_global_command *) NULL;
}

tree_global_command::~tree_global_command (void)
{
  delete next;
}

tree_global_command *
tree_global_command::chain (symbol_record *s, int l = -1, int c = -1)
{
  tree_global_command *tmp = new tree_global_command (s, l, c);
  tmp->next = this;
  return tmp;
}

tree_global_command *
tree_global_command::chain (symbol_record *s, tree_expression *e,
			    int l = -1, int c = -1)
{
  tree_global_command *tmp = new tree_global_command (s, e, l, c);
  tmp->next = this;
  return tmp;
}

tree_global_command *
tree_global_command::reverse (void)
{
  tree_global_command *list = this;
  tree_global_command *next;
  tree_global_command *prev = (tree_global_command *) NULL;

  while (list != (tree_global_command *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

tree_constant
tree_global_command::eval (int print)
{
  tree_constant retval;

  link_to_global_variable (sr);

  if (rhs != NULL_TREE)
    {
      tree_identifier *id = new tree_identifier (sr);
      tree_constant tmp_rhs = rhs->eval (0);
      if (error_state)
	{
	  delete id;
	  eval_error ();
	  return retval;
	}
      else
	{
	  tree_constant *tmp_val = new tree_constant (tmp_rhs);

	  tree_simple_assignment_expression tmp_ass (id, tmp_val);

	  tmp_ass.eval (0);

	  delete id; // XXX FIXME XXX

	  if (error_state)
	    {
	      eval_error ();
	      return retval;
	    }
	}
    }

  if (next != (tree_global_command *) NULL)
    next->eval (print);

  return retval;
}

void
tree_global_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating global command near line %d, column %d",
	     line (), column ());
}

/*
 * While.
 */
tree_while_command::tree_while_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = (tree_expression *) NULL;
  list = NULL_TREE;
}

tree_while_command::tree_while_command (tree_expression *e,
					int l = -1, int c = -1) 
{
  line_num = l;
  column_num = c;
  expr = e;
  list = NULL_TREE;
}

tree_while_command::tree_while_command (tree_expression *e, tree *lst,
					int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = e;
  list = lst;
}

tree_while_command::~tree_while_command (void)
{
  delete expr;
  delete list;
}

tree_constant
tree_while_command::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  for (;;)
    {
      int expr_value = 0;
      if (expr == (tree_expression *) NULL)
	return tree_constant ();
      tree_constant t1 = expr->eval (0);

      if (error_state)
	{
	  eval_error ();
	  return tree_constant ();
	}

      if (t1.rows () == 0 || t1.columns () == 0)
	{
	  int flag = user_pref.propagate_empty_matrices;
	  if (flag < 0)
	    warning ("while: empty matrix used in conditional");
	  else if (flag == 0)
	    {
	      ::error ("while: empty matrix used in conditional");
	      return tree_constant ();
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
	  if (list != NULL_TREE)
	    {
	      retval = list->eval (1);
	      if (error_state)
		{
		  eval_error ();
		  return tree_constant ();
		}
	    }

	  if (quit_loop_now ())
	    break;
	}
      else
	break;
    }
  return retval;
}

void
tree_while_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating while command near line %d, column %d",
	     line (), column ());
}

/*
 * For.
 */
tree_for_command::tree_for_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  id = (tree_index_expression *) NULL;
  expr = (tree_expression *) NULL;
  list = NULL_TREE;
}

tree_for_command::tree_for_command (tree_index_expression *ident,
				    tree_expression *e, tree *lst,
				    int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  id = ident;
  expr = e;
  list = lst;
}

tree_for_command::~tree_for_command (void)
{
  delete id;
  delete expr;
  delete list;
}

tree_constant
tree_for_command::eval (int print)
{
  tree_constant retval;

  if (error_state || expr == (tree_expression *) NULL)
    return retval;

  tree_constant tmp_expr = expr->eval (0);

  if (error_state || tmp_expr.is_undefined ())
    {
      eval_error ();
      return retval;
    }

  tree_constant_rep::constant_type expr_type = tmp_expr.const_type ();
  switch (expr_type)
    {
    case tree_constant_rep::complex_scalar_constant:
    case tree_constant_rep::scalar_constant:
      {
	tree_constant *rhs = new tree_constant (tmp_expr);
	int quit = 0;
	retval = do_for_loop_once (rhs, quit);
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
	    retval = do_for_loop_once (rhs, quit);
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
	    retval = do_for_loop_once (rhs, quit);
	    if (quit)
	      break;
	  }
      }
      break;
    default:
      panic_impossible ();
      break;
    }
  
  return retval;
}

void
tree_for_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating for command near line %d, column %d",
	     line (), column ());
}

tree_constant
tree_for_command::do_for_loop_once (tree_constant *rhs, int& quit)
{
  tree_constant retval;

  quit = 0;

  tree_simple_assignment_expression tmp_ass (id, rhs);

  tmp_ass.eval (0);

  if (error_state)
    {
      eval_error ();
      return tree_constant ();
    }

  if (list != NULL_TREE)
    {
      retval = list->eval (1);
      if (error_state)
	{
	  eval_error ();
	  quit = 1;
	  return tree_constant ();
	}
    }

  quit = quit_loop_now ();

  return retval;
}

/*
 * If.
 */
tree_if_command::tree_if_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = (tree_expression *) NULL;
  list = NULL_TREE;
  next = (tree_if_command *) NULL;
}

tree_if_command::tree_if_command (tree *lst, int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = (tree_expression *) NULL;
  list = lst;
  next = (tree_if_command *) NULL;
}

tree_if_command::tree_if_command (tree_expression *e, tree *lst,
				  int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = e;
  list = lst;
  next = (tree_if_command *) NULL;
}

tree_if_command::~tree_if_command (void)
{
  delete expr;
  delete list;
  delete next;
}

tree_if_command *
tree_if_command::chain (tree *lst, int l = -1, int c = -1)
{
  tree_if_command *tmp = new tree_if_command (lst, l, c);
  tmp->next = this;
  return tmp;
}

tree_if_command *
tree_if_command::chain (tree_expression *e, tree *lst, int l = -1, int c = -1)
{
  tree_if_command *tmp = new tree_if_command (e, lst, l, c);
  tmp->next = this;
  return tmp;
}

tree_if_command *
tree_if_command::reverse (void)
{
  tree_if_command *list = this;
  tree_if_command *next;
  tree_if_command *prev = (tree_if_command *) NULL;

  while (list != (tree_if_command *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

tree_constant
tree_if_command::eval (int print)
{
  int expr_value = 0;
  tree_constant retval;

  if (error_state)
    return retval;

  tree_if_command *lst;
  for (lst = this; lst != (tree_if_command *) NULL; lst = lst->next)
    {
      if (lst->expr != (tree_expression *) NULL)
	{
	  tree_expression *tmp = lst->expr;
	  if (tmp == (tree_expression *) NULL)
	    return tree_constant ();
	  tree_constant t1 = tmp->eval (0);
	  if (error_state || t1.is_undefined ())
	    {
	      lst->eval_error ();
	      break;
	    }

	  if (t1.rows () == 0 || t1.columns () == 0)
	    {
	      int flag = user_pref.propagate_empty_matrices;
	      if (flag < 0)
		warning ("if: empty matrix used in conditional");
	      else if (flag == 0)
		{
		  ::error ("if: empty matrix used in conditional");
		  lst->eval_error ();
		  return tree_constant ();
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
	      if (lst->list != NULL_TREE)
		retval = lst->list->eval (1);
	      else
		::error ("if: empty command list");

	      if (error_state)
		lst->eval_error ();

	      break;
	    }
	}
      else
	{
	  if (lst->list != NULL_TREE)
	    retval = lst->list->eval (1);
	  else
	    ::error ("if: empty command list");

	  if (error_state)
	    lst->eval_error ();

	  break;
	}
    }

  return retval;
}

void
tree_if_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating if command near line %d, column %d",
	     line (), column ());
}

/*
 * Break.  Is this overkill, or what?
 */
tree_break_command::tree_break_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
}

tree_break_command::~tree_break_command (void)
{
}

tree_constant
tree_break_command::eval (int print)
{
  if (! error_state)
    breaking = 1;
  return tree_constant ();
}

/*
 * Continue.
 */
tree_continue_command::tree_continue_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
}

tree_continue_command::~tree_continue_command (void)
{
}

tree_constant
tree_continue_command::eval (int print)
{
  if (! error_state)
    continuing = 1;
  return tree_constant ();
}

/*
 * Return.
 */
tree_return_command::tree_return_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
}

tree_return_command::~tree_return_command (void)
{
}

tree_constant
tree_return_command::eval (int print)
{
  if (! error_state)
    returning = 1;
  return tree_constant ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
