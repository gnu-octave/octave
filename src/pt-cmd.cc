// tree-cmd.cc                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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
#include <config.h>
#endif

#include <iostream.h>

// Nonzero means we're breaking out of a loop or function body.
int breaking = 0;

// Nonzero means we're jumping to the end of a loop.
int continuing = 0;

// Nonzero means we're returning from a function.  Global because it
// is also needed in tree-expr.cc.
int returning = 0;

#include "user-prefs.h"
#include "variables.h"
#include "oct-map.h"
#include "symtab.h"
#include "error.h"
#include "gripes.h"
#include "tree-base.h"
#include "tree-expr.h"
#include "tree-cmd.h"
#include "tree-misc.h"
#include "tree-const.h"
#include "unwind-prot.h"

// Decide if it's time to quit a for or while loop.
static inline int
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
	      ::error ("empty matrix used in while condition near line\
 %d, column %d", line (), column ()); 
	      return;
	    }
	  t1 = tree_constant (0.0);
	}
      else if (! t1.is_scalar_type ())
	{
	  tree_constant t2 = t1.all ();
	  t1 = t2.all ();
	}

      if (t1.is_real_scalar ())
	expr_value = (int) t1.double_value ();
      else if (t1.is_complex_scalar ())
	expr_value = t1.complex_value () != 0.0;
      else
	{
	  ::error ("invalid type used in while condition near line %d,\
 column %d", line (), column ());
	  return;
	}

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
  delete id_list;
  delete expr;
  delete list;
}

inline void
tree_for_command::do_for_loop_once (tree_return_list *lst,
				    const Octave_object& rhs, int& quit)
{
  quit = 0;

  tree_oct_obj *tmp = new tree_oct_obj (rhs);
  tree_multi_assignment_expression tmp_ass (lst, tmp, 1);
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

inline void
tree_for_command::do_for_loop_once (tree_index_expression *idx_expr,
				    const tree_constant& rhs, int& quit)
{
  quit = 0;

  tree_constant *tmp = new tree_constant (rhs);
  tree_simple_assignment_expression tmp_ass (idx_expr, tmp, 1);
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

inline void
tree_for_command::do_for_loop_once (tree_identifier *ident,
				    tree_constant& rhs, int& quit)
{
  quit = 0;

  ident->assign (rhs);

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

#define DO_LOOP(val) \
  do \
    { \
      if (ident) \
	for (int i = 0; i < steps; i++) \
	  { \
	    tree_constant rhs (val); \
	    int quit = 0; \
	    do_for_loop_once (ident, rhs, quit); \
	    if (quit) \
	      break; \
	  } \
      else if (id_list) \
	for (int i = 0; i < steps; i++) \
	  { \
	    Octave_object rhs (val); \
	    int quit = 0; \
	    do_for_loop_once (id_list, rhs, quit); \
	    if (quit) \
	      break; \
	  } \
      else \
	for (int i = 0; i < steps; i++) \
	  { \
	    tree_constant rhs (val); \
	    int quit = 0; \
	    do_for_loop_once (tmp_id, rhs, quit); \
	    if (quit) \
	      break; \
	  } \
    } \
  while (0)

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

  tree_index_expression *tmp_id = id;
  if (id_list && id_list->length () == 1)
    tmp_id = id_list->front ();

  tree_identifier *ident = 0;
  if (tmp_id && ! tmp_id->arg_list ())
    {
      tree_indirect_ref *idr = tmp_id->ident ();
      if (idr->is_identifier_only ())
	ident = idr->ident ();
    }

  if (id_list && ! ident && ! tmp_expr->is_map ())
    {
      error ("in statement `for [X, Y] = VAL', VAL must be a structure");
      return;
    }

  if (tmp_expr.is_scalar_type ())
    {
      int quit = 0;
      if (ident)
	do_for_loop_once (ident, tmp_expr, quit);
      else if (id_list)
	{
	  Octave_object rhs (tmp_expr);
	  do_for_loop_once (id_list, rhs, quit);
	}
      else
	do_for_loop_once (tmp_id, tmp_expr, quit);
    }
  else if (tmp_expr.is_matrix_type ())
    {
      Matrix m_tmp;
      ComplexMatrix cm_tmp;
      int nr;
      int steps;
      if (tmp_expr.is_real_matrix ())
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

      if (tmp_expr.is_real_matrix ())
	{
	  if (nr == 1)
	    DO_LOOP(m_tmp (0, i));
	  else
	    DO_LOOP(m_tmp.extract (0, i, nr-1, i));
	}
      else
	{
	  if (nr == 1)
	    DO_LOOP(cm_tmp (0, i));
	  else
	    DO_LOOP(cm_tmp.extract (0, i, nr-1, i));
	}
    }
  else if (tmp_expr.is_string ())
    {
      gripe_string_invalid ();
    }
  else if (tmp_expr.is_range ())
    {
      Range rng = tmp_expr.range_value ();

      int steps = rng.nelem ();
      double b = rng.base ();
      double increment = rng.inc ();

      if (ident)
	{
	  for (int i = 0; i < steps; i++)
	    {
	      double tmp_val = b + i * increment;

	      tree_constant rhs (tmp_val);

	      int quit = 0;
	      do_for_loop_once (ident, rhs, quit);

	      if (quit)
		break;
	    }
	}
      else if (id_list)
	{
	  for (int i = 0; i < steps; i++)
	    {
	      double tmp_val = b + i * increment;

	      Octave_object rhs (tmp_val);

	      int quit = 0;
	      do_for_loop_once (id_list, rhs, quit);

	      if (quit)
		break;
	    }
	}
      else
	{
	  for (int i = 0; i < steps; i++)
	    {
	      double tmp_val = b + i * increment;

	      tree_constant rhs (tmp_val);

	      int quit = 0;
	      do_for_loop_once (tmp_id, rhs, quit);

	      if (quit)
		break;
	    }
	}
    }
  else if (tmp_expr.is_map ())
    {
      if (ident)
	{
	  Octave_map tmp_val (tmp_expr.map_value ());

	  for (Pix p = tmp_val.first (); p != 0; tmp_val.next (p))
	    {
	      tree_constant rhs (tmp_val.contents (p));

	      int quit;
	      do_for_loop_once (ident, rhs, quit);

	      if (quit)
		break;
	    }
	}
      else if (id_list)
	{
	  // Cycle through structure elements.  First element of
	  // id_list is set to value and the second is set to the name
	  // of the structure element.

	  Octave_map tmp_val (tmp_expr.map_value ());

	  for (Pix p = tmp_val.first (); p != 0; tmp_val.next (p))
	    {
	      Octave_object tmp;
	      tmp (1) = tmp_val.key (p);
	      tmp (0) = tmp_val.contents (p);

	      int quit;
	      do_for_loop_once (id_list, tmp, quit);

	      if (quit)
		break;
	    }
	}
      else
	{
	  Octave_map tmp_val (tmp_expr.map_value ());

	  for (Pix p = tmp_val.first (); p != 0; tmp_val.next (p))
	    {
	      tree_constant rhs = tmp_val.contents (p);

	      int quit;
	      do_for_loop_once (tmp_id, rhs, quit);

	      if (quit)
		break;
	    }
	}
    }
  else
    {
      ::error ("invalid type in for loop expression near line %d, column %d",
	       line (), column ());
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

// Simple exception handling.

tree_unwind_protect_command::~tree_unwind_protect_command (void)
{
  delete unwind_protect_code;
  delete cleanup_code;
}

static void
do_unwind_protect_cleanup_code (void *ptr)
{
  tree_statement_list *list = (tree_statement_list *) ptr;

  // We want to run the cleanup code without error_state being set,
  // but we need to restore its value, so that any errors encountered
  // in the first part of the unwind_protect are not completely
  // ignored.

  unwind_protect_int (error_state);

  error_state = 0;

  if (list)
    list->eval (1);

  // We don't want to ignore errors that occur in the cleanup code, so
  // if an error is encountered there, leave error_state alone.
  // Otherwise, set it back to what it was before.

  if (error_state)
    discard_unwind_protect ();
  else
    run_unwind_protect ();
}

void
tree_unwind_protect_command::eval (void)
{
  add_unwind_protect (do_unwind_protect_cleanup_code, cleanup_code);

  if (unwind_protect_code)
    unwind_protect_code->eval (1);

  run_unwind_protect ();
}

void
tree_unwind_protect_command::print_code (ostream& os)
{
  print_code_indent (os);

  os << "unwind_protect";

  print_code_new_line (os);

  if (unwind_protect_code)
    {
      increment_indent_level ();
      unwind_protect_code->print_code (os);
      decrement_indent_level ();
    }

  print_code_indent (os);

  os << "cleanup_code";

  print_code_new_line (os);

  if (cleanup_code)
    {
      increment_indent_level ();
      cleanup_code->print_code (os);
      decrement_indent_level ();
    }

  print_code_indent (os);

  os << "end_unwind_protect";
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
