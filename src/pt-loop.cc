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

#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-jump.h"
#include "pt-loop.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "unwind-prot.h"

// TRUE means we are evaluating some kind of looping construct.
bool evaluating_looping_command = false;

// Decide if it's time to quit a for or while loop.
static inline bool
quit_loop_now (void)
{
  // Maybe handle `continue N' someday...

  if (tree_continue_command::continuing)
    tree_continue_command::continuing--;

  bool quit = (error_state
	       || tree_return_command::returning
	       || tree_break_command::breaking
	       || tree_continue_command::continuing);

  if (tree_break_command::breaking)
    tree_break_command::breaking--;

  return quit;
}

// While.

tree_while_command::~tree_while_command (void)
{
  delete expr;
  delete list;
  delete lead_comm;
  delete trail_comm;
}

void
tree_while_command::eval (void)
{
  if (error_state)
    return;

  unwind_protect::begin_frame ("while_command::eval");

  unwind_protect_bool (evaluating_looping_command);

  evaluating_looping_command = true;

  if (! expr)
    panic_impossible ();

  for (;;)
    {
      if (expr->is_logically_true ("while"))
	{
	  if (list)
	    {
	      list->eval ();

	      if (error_state)
		{
		  eval_error ();
		  goto cleanup;
		}
	    }

	  if (quit_loop_now ())
	    break;
	}
      else
	break;
    }

 cleanup:
  unwind_protect::run_frame ("while_command::eval");
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

// Do-Until

void
tree_do_until_command::eval (void)
{
  if (error_state)
    return;

  unwind_protect::begin_frame ("do_until_command::eval");

  unwind_protect_bool (evaluating_looping_command);

  evaluating_looping_command = true;

  if (! expr)
    panic_impossible ();

  for (;;)
    {
      MAYBE_DO_BREAKPOINT;

      if (list)
	{
	  list->eval ();

	  if (error_state)
	    {
	      eval_error ();
	      goto cleanup;
	    }
	}

      if (quit_loop_now () || expr->is_logically_true ("do-until"))
	break;
    }

 cleanup:
  unwind_protect::run_frame ("do_until_command::eval");
}

void
tree_do_until_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating do-until command near line %d, column %d",
	     line (), column ());
}

void
tree_do_until_command::accept (tree_walker& tw)
{
  tw.visit_do_until_command (*this);
}

// For.

tree_simple_for_command::~tree_simple_for_command (void)
{
  delete expr;
  delete list;
  delete lead_comm;
  delete trail_comm;
}

inline void
tree_simple_for_command::do_for_loop_once (octave_lvalue& ult,
					   const octave_value& rhs,
					   bool& quit)
{
  quit = false;

  ult.assign (octave_value::op_asn_eq, rhs);

  if (! error_state)
    {
      if (list)
	{
	  list->eval ();

	  if (error_state)
	    eval_error ();
	}
    }
  else
    eval_error ();

  quit = quit_loop_now ();
}

#define DO_LOOP(arg) \
  do \
    { \
      for (int i = 0; i < steps; i++) \
	{ \
	  MAYBE_DO_BREAKPOINT; \
 \
	  octave_value val (arg); \
 \
	  bool quit = false; \
 \
	  do_for_loop_once (ult, val, quit); \
 \
	  if (quit) \
	    break; \
	} \
    } \
  while (0)

void
tree_simple_for_command::eval (void)
{
  if (error_state)
    return;

  unwind_protect::begin_frame ("simple_for_command::eval");

  unwind_protect_bool (evaluating_looping_command);

  evaluating_looping_command = true;

  octave_value rhs = expr->rvalue ();

  if (error_state || rhs.is_undefined ())
    {
      eval_error ();
      goto cleanup;
    }

  {
    octave_lvalue ult = lhs->lvalue ();

    if (error_state)
      {
	eval_error ();
	goto cleanup;
      }

    if (rhs.is_range ())
      {
	Range rng = rhs.range_value ();

	int steps = rng.nelem ();
	double b = rng.base ();
	double increment = rng.inc ();

	for (int i = 0; i < steps; i++)
	  {
	    MAYBE_DO_BREAKPOINT;

	    double tmp_val = b + i * increment;

	    octave_value val (tmp_val);

	    bool quit = false;

	    do_for_loop_once (ult, val, quit);

	    if (quit)
	      break;
	  }
      }
    else if (rhs.is_scalar_type ())
      {
	bool quit = false;

	MAYBE_DO_BREAKPOINT;

	do_for_loop_once (ult, rhs, quit);
      }
    else if (rhs.is_string ())
      {
	charMatrix chm_tmp = rhs.char_matrix_value ();
	int nr = chm_tmp.rows ();
	int steps = chm_tmp.columns ();

	if (error_state)
	  goto cleanup;

	if (nr == 1)
	  DO_LOOP (chm_tmp (0, i));
	else
	  {
	    for (int i = 0; i < steps; i++)
	      {
		MAYBE_DO_BREAKPOINT;

		octave_value val (chm_tmp.extract (0, i, nr-1, i), true);

		bool quit = false;

		do_for_loop_once (ult, val, quit);

		if (quit)
		  break;
	      }
	  }
      }
    else if (rhs.is_matrix_type ())
      {
	Matrix m_tmp;
	ComplexMatrix cm_tmp;

	int nr;
	int steps;

	if (rhs.is_real_matrix ())
	  {
	    m_tmp = rhs.matrix_value ();
	    nr = m_tmp.rows ();
	    steps = m_tmp.columns ();
	  }
	else
	  {
	    cm_tmp = rhs.complex_matrix_value ();
	    nr = cm_tmp.rows ();
	    steps = cm_tmp.columns ();
	  }

	if (error_state)
	  goto cleanup;

	if (rhs.is_real_matrix ())
	  {
	    if (nr == 1)
	      DO_LOOP (m_tmp (0, i));
	    else
	      DO_LOOP (m_tmp.extract (0, i, nr-1, i));
	  }
	else
	  {
	    if (nr == 1)
	      DO_LOOP (cm_tmp (0, i));
	    else
	      DO_LOOP (cm_tmp.extract (0, i, nr-1, i));
	  }
      }
    else if (rhs.is_map ())
      {
	Octave_map tmp_val (rhs.map_value ());

	for (Pix p = tmp_val.first (); p != 0; tmp_val.next (p))
	  {
	    MAYBE_DO_BREAKPOINT;

	    octave_value val = tmp_val.contents (p);

	    bool quit = false;

	    do_for_loop_once (ult, val, quit);

	    if (quit)
	      break;
	  }
      }
    else
      {
	::error ("invalid type in for loop expression near line %d, column %d",
		 line (), column ());
      }
  }

 cleanup:
  unwind_protect::run_frame ("simple_for_command::eval");
}

void
tree_simple_for_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating for command near line %d, column %d",
	     line (), column ());
}

void
tree_simple_for_command::accept (tree_walker& tw)
{
  tw.visit_simple_for_command (*this);
}

tree_complex_for_command::~tree_complex_for_command (void)
{
  delete expr;
  delete list;
  delete lead_comm;
  delete trail_comm;
}

void
tree_complex_for_command::do_for_loop_once (octave_lvalue &val_ref,
					    octave_lvalue &key_ref,
					    const octave_value& val,
					    const octave_value& key,
					    bool& quit)
{
  quit = false;

  val_ref.assign (octave_value::op_asn_eq, val);
  key_ref.assign (octave_value::op_asn_eq, key);

  if (! error_state)
    {
      if (list)
	{
	  list->eval ();

	  if (error_state)
	    eval_error ();
	}
    }
  else
    eval_error ();

  quit = quit_loop_now ();
}

void
tree_complex_for_command::eval (void)
{
  if (error_state)
    return;

  unwind_protect::begin_frame ("complex_for_command::eval");

  unwind_protect_bool (evaluating_looping_command);

  evaluating_looping_command = true;

  octave_value rhs = expr->rvalue ();

  if (error_state || rhs.is_undefined ())
    {
      eval_error ();
      goto cleanup;
    }

  if (rhs.is_map ())
    {
      // Cycle through structure elements.  First element of id_list
      // is set to value and the second is set to the name of the
      // structure element.

      Pix p = lhs->first ();
      tree_expression *elt = lhs->operator () (p);
      octave_lvalue val_ref = elt->lvalue ();

      lhs->next (p);
      elt = lhs->operator () (p);
      octave_lvalue key_ref = elt->lvalue ();

      Octave_map tmp_val (rhs.map_value ());

      for (p = tmp_val.first (); p != 0; tmp_val.next (p))
	{
	  octave_value key = tmp_val.key (p);
	  octave_value val = tmp_val.contents (p);

	  MAYBE_DO_BREAKPOINT;

	  bool quit = false;

	  do_for_loop_once (key_ref, val_ref, key, val, quit);

	  if (quit)
	    break;
	}
    }
  else
    error ("in statement `for [X, Y] = VAL', VAL must be a structure");

 cleanup:
  unwind_protect::run_frame ("complex_for_command::eval");
}

void
tree_complex_for_command::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating for command near line %d, column %d",
	     line (), column ());
}

void
tree_complex_for_command::accept (tree_walker& tw)
{
  tw.visit_complex_for_command (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
