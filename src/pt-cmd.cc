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

// Nonzero means we're breaking out of a loop or function body.
int breaking = 0;

// Nonzero means we're jumping to the end of a loop.
int continuing = 0;

// Nonzero means we're returning from a function.  Global because it
// is also needed in tree-expr.cc.
int returning = 0;

#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "symtab.h"
#include "pt-cmd.h"
#include "ov.h"
#include "pt-exp.h"
#include "pt-fvc.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "pt-walk.h"
#include "unwind-prot.h"
#include "variables.h"

// Decide if it's time to quit a for or while loop.
static inline bool
quit_loop_now (void)
{
  // Maybe handle `continue N' someday...

  if (continuing)
    continuing--;

  bool quit = (returning || breaking || continuing);

  if (breaking)
    breaking--;

  return quit;
}

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
tree_global_command::accept (tree_walker& tw)
{
  tw.visit_global_command (*this);
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

  if (! expr)
    panic_impossible ();

  for (;;)
    {
      if (expr->is_logically_true ("while"))
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
tree_while_command::accept (tree_walker& tw)
{
  tw.visit_while_command (*this);
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
				    const octave_value_list& rhs, bool& quit)
{
  quit = false;

  tree_oct_obj *tmp = new tree_oct_obj (rhs);
  tree_multi_assignment_expression tmp_ass (lst, tmp, 1);
  tmp_ass.eval (false);

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
	  quit = true;
	  return;
	}
    }

  quit = quit_loop_now ();
}

inline void
tree_for_command::do_for_loop_once (tree_index_expression *idx_expr,
				    const octave_value& rhs, bool& quit)
{
  quit = false;

  octave_value *tmp = new octave_value (rhs);
  tree_simple_assignment_expression tmp_ass (idx_expr, tmp, true);
  tmp_ass.eval (false);

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
	  quit = true;
	  return;
	}
    }

  quit = quit_loop_now ();
}

inline void
tree_for_command::do_for_loop_once (tree_identifier *ident,
				    octave_value& rhs, bool& quit)
{
  quit = false;

  octave_variable_reference tmp (ident);

  if (error_state)
    {
      eval_error ();
      return;
    }

  tmp.assign (rhs);

  if (list)
    {
      list->eval (1);
      if (error_state)
	{
	  eval_error ();
	  quit = true;
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
	    octave_value rhs (val); \
	    bool quit = false; \
	    do_for_loop_once (ident, rhs, quit); \
	    if (quit) \
	      break; \
	  } \
      else if (id_list) \
	for (int i = 0; i < steps; i++) \
	  { \
	    octave_value_list rhs (val); \
	    bool quit = false; \
	    do_for_loop_once (id_list, rhs, quit); \
	    if (quit) \
	      break; \
	  } \
      else \
	for (int i = 0; i < steps; i++) \
	  { \
	    octave_value rhs (val); \
	    bool quit = false; \
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

  octave_value tmp_expr = expr->eval (false);

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

  if (id_list && ! ident && ! tmp_expr.is_map ())
    {
      error ("in statement `for [X, Y] = VAL', VAL must be a structure");
      return;
    }

  if (tmp_expr.is_scalar_type ())
    {
      bool quit = false;
      if (ident)
	do_for_loop_once (ident, tmp_expr, quit);
      else if (id_list)
	{
	  octave_value_list rhs (tmp_expr);
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

	      octave_value rhs (tmp_val);

	      bool quit = false;
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

	      octave_value_list rhs (tmp_val);

	      bool quit = false;
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

	      octave_value rhs (tmp_val);

	      bool quit = false;
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
	      octave_value rhs (tmp_val.contents (p));

	      bool quit = false;
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
	      octave_value_list tmp;
	      tmp (1) = tmp_val.key (p);
	      tmp (0) = tmp_val.contents (p);

	      bool quit = false;
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
	      octave_value rhs = tmp_val.contents (p);

	      bool quit = false;
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
tree_for_command::accept (tree_walker& tw)
{
  tw.visit_for_command (*this);
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
tree_if_command::accept (tree_walker& tw)
{
  tw.visit_if_command (*this);
}

// Switch.

tree_switch_command::~tree_switch_command (void)
{
  delete expr;
  delete list;
}

void
tree_switch_command::eval (void)
{
  if (expr)
    {
      octave_value val = expr->eval (0);

      if (! error_state)
	{
	  if (list)
	    list->eval (val);

	  if (error_state)
	    eval_error ();
	}
      else
	eval_error ();
    }
  else
    ::error ("missing value in switch command near line %d, column %d",
	     line (), column ());
}

void
tree_switch_command::eval_error (void)
{
  ::error ("evaluating switch command near line %d, column %d",
	   line (), column ());
}

void
tree_switch_command::accept (tree_walker& tw)
{
  tw.visit_switch_command (*this);
}

// Simple exception handling.

tree_try_catch_command::~tree_try_catch_command (void)
{
  delete try_code;
  delete catch_code;
}

static void
do_catch_code (void *ptr)
{
  tree_statement_list *list = (tree_statement_list *) ptr;

  // Set up for letting the user print any messages from errors that
  // occurred in the body of the try_catch statement.

  buffer_error_messages = 0;
  bind_global_error_variable ();
  add_unwind_protect (clear_global_error_variable, 0);

  // Similarly, if we have seen a return or break statement, allow all
  // the catch code to run before returning or handling the break.
  // We don't have to worry about continue statements because they can
  // only occur in loops.

  unwind_protect_int (returning);
  returning = 0;

  unwind_protect_int (breaking);
  breaking = 0;

  if (list)
    list->eval (true);

  // This is the one for breaking.  (The unwind_protects are popped
  // off the stack in the reverse of the order they are pushed on).

  // XXX FIXME XXX -- inside a try-catch, should break work like
  // a return, or just jump to the end of the try_catch block?
  // The following code makes it just jump to the end of the block.

  run_unwind_protect ();
  if (breaking)
    breaking--;

  // This is the one for returning.

  if (returning)
    discard_unwind_protect ();
  else
    run_unwind_protect ();

  run_unwind_protect ();
}

void
tree_try_catch_command::eval (void)
{
  begin_unwind_frame ("tree_try_catch::eval");

  add_unwind_protect (do_catch_code, catch_code);

  if (catch_code)
    {
      unwind_protect_int (buffer_error_messages);
      buffer_error_messages = 1;
    }

  if (try_code)
    try_code->eval (true);

  if (catch_code && error_state)
    {
      error_state = 0;
      run_unwind_frame ("tree_try_catch::eval");
    }
  else
    {
      error_state = 0;
      discard_unwind_frame ("tree_try_catch::eval");
    }
}

void
tree_try_catch_command::accept (tree_walker& tw)
{
  tw.visit_try_catch_command (*this);
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

  // Similarly, if we have seen a return or break statement, allow all
  // the cleanup code to run before returning or handling the break.
  // We don't have to worry about continue statements because they can
  // only occur in loops.

  unwind_protect_int (returning);
  returning = 0;

  unwind_protect_int (breaking);
  breaking = 0;

  if (list)
    list->eval (true);

  // This is the one for breaking.  (The unwind_protects are popped
  // off the stack in the reverse of the order they are pushed on).

  // XXX FIXME XXX -- inside an unwind_protect, should break work like
  // a return, or just jump to the end of the unwind_protect block?
  // The following code makes it just jump to the end of the block.

  run_unwind_protect ();
  if (breaking)
    breaking--;

  // This is the one for returning.

  if (returning)
    discard_unwind_protect ();
  else
    run_unwind_protect ();

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
    unwind_protect_code->eval (true);

  run_unwind_protect ();
}

void
tree_unwind_protect_command::accept (tree_walker& tw)
{
  tw.visit_unwind_protect_command (*this);
}

// No-op.

void
tree_no_op_command::accept (tree_walker& tw)
{
  tw.visit_no_op_command (*this);
}

// Break.

void
tree_break_command::eval (void)
{
  if (! error_state)
    breaking = 1;
}

void
tree_break_command::accept (tree_walker& tw)
{
  tw.visit_break_command (*this);
}

// Continue.

void
tree_continue_command::eval (void)
{
  if (! error_state)
    continuing = 1;
}

void
tree_continue_command::accept (tree_walker& tw)
{
  tw.visit_continue_command (*this);
}

// Return.

void
tree_return_command::eval (void)
{
  if (! error_state)
    returning = 1;
}

void
tree_return_command::accept (tree_walker& tw)
{
  tw.visit_return_command (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
