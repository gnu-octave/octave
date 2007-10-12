/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "quit.h"

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
  OCTAVE_QUIT;

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
  ::error ("evaluating while command near line %d, column %d",
	   line (), column ());
}

tree_command *
tree_while_command::dup (symbol_table *sym_tab)
{
  return new tree_while_command (expr ? expr->dup (sym_tab) : 0,
				 list ? list->dup (sym_tab) : 0,
				 lead_comm ? lead_comm->dup () : 0,
				 trail_comm ? trail_comm->dup (): 0,
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
  ::error ("evaluating do-until command near line %d, column %d",
	   line (), column ());
}

tree_command *
tree_do_until_command::dup (symbol_table *sym_tab)
{
  return new tree_do_until_command (expr ? expr->dup (sym_tab) : 0,
				    list ? list->dup (sym_tab) : 0,
				    lead_comm ? lead_comm->dup () : 0,
				    trail_comm ? trail_comm->dup (): 0,
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

#define DO_ND_LOOP(MTYPE, TYPE, CONV, ARG) \
  do \
    { \
      dim_vector dv = ARG.dims (); \
 \
      bool quit = false; \
 \
      TYPE *atmp = ARG.fortran_vec (); \
 \
      octave_idx_type steps = dv(1); \
 \
      octave_idx_type nrows = dv(0); \
 \
      int ndims = dv.length (); \
      if (ndims > 2) \
        { \
          for (int i = 2; i < ndims; i++) \
            steps *= dv(i); \
          dv(1) = steps; \
          dv.resize (2); \
        } \
 \
      if (steps > 0) \
	{ \
          if (nrows == 0) \
            { \
	      MTYPE tarray (dim_vector (0, 1)); \
 \
	      octave_value val (tarray); \
 \
	      for (octave_idx_type i = 0; i < steps; i++) \
		{ \
		  MAYBE_DO_BREAKPOINT; \
 \
	          do_for_loop_once (ult, val, quit); \
 \
	          if (quit) \
	            break; \
	       } \
            } \
          else if (nrows == 1) \
            { \
	      for (octave_idx_type i = 0; i < steps; i++) \
		{ \
		  MAYBE_DO_BREAKPOINT; \
 \
		  octave_value val (CONV (*atmp++)); \
 \
	          do_for_loop_once (ult, val, quit); \
 \
	          if (quit) \
	            break; \
	       } \
            } \
          else \
            { \
              if (ndims > 2) \
                ARG = ARG.reshape (dv); \
 \
              MTYPE tmp (dim_vector (nrows, 1)); \
 \
              TYPE *ftmp = tmp.fortran_vec (); \
 \
              for (octave_idx_type i = 0; i < steps; i++) \
	        { \
	          MAYBE_DO_BREAKPOINT; \
 \
 	          for (int j = 0; j < nrows; j++) \
	            ftmp[j] = *atmp++;  \
 \
                  octave_value val (tmp); \
 \
                  do_for_loop_once (ult, val, quit); \
                  quit = (i == steps - 1 ? true : quit); \
 \
	          if (quit) \
	            break; \
	        } \
	    } \
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

	octave_idx_type steps = rng.nelem ();
	double b = rng.base ();
	double increment = rng.inc ();
	bool quit = false;
	double tmp_val = b;

	for (octave_idx_type i = 0; i < steps; i++, tmp_val += increment)
	  {
	    MAYBE_DO_BREAKPOINT;

	    octave_value val (tmp_val);

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
	octave_idx_type nr = chm_tmp.rows ();
	octave_idx_type steps = chm_tmp.columns ();
	bool quit = false;

	if (error_state)
	  goto cleanup;

	if (nr == 1)
	  {
	    for (octave_idx_type i = 0; i < steps; i++)
	      {
		MAYBE_DO_BREAKPOINT;

		octave_value val (chm_tmp.xelem (0, i));

		do_for_loop_once (ult, val, quit);

		if (quit)
		  break;
	      }
	  }
	else
	  {
	    for (octave_idx_type i = 0; i < steps; i++)
	      {
		MAYBE_DO_BREAKPOINT;

		octave_value val (chm_tmp.extract (0, i, nr-1, i), true);

		do_for_loop_once (ult, val, quit);

		if (quit)
		  break;
	      }
	  }
      }
    else if (rhs.is_matrix_type ())
      {
	if (rhs.is_real_type ())
	  {
	    NDArray m_tmp = rhs.array_value ();

	    if (error_state)
	      goto cleanup;

	    DO_ND_LOOP (NDArray, double, , m_tmp);
	  }
	else
	  {
	    ComplexNDArray cm_tmp = rhs.complex_array_value ();

	    if (error_state)
	      goto cleanup;

	    DO_ND_LOOP (ComplexNDArray, Complex, , cm_tmp);
	  }
      }
    else if (rhs.is_map ())
      {
	Octave_map tmp_val (rhs.map_value ());

	bool quit = false;

	for (Octave_map::iterator p = tmp_val.begin ();
	     p != tmp_val.end ();
	     p++)
	  {
	    MAYBE_DO_BREAKPOINT;

	    Cell val_lst = tmp_val.contents (p);

	    octave_value val
	      = (val_lst.length () == 1) ? val_lst(0) : octave_value (val_lst);

	    do_for_loop_once (ult, val, quit);

	    if (quit)
	      break;
	  }
      }
    else if (rhs.is_cell ())
      {
	Cell c_tmp = rhs.cell_value ();

	DO_ND_LOOP (Cell, octave_value, Cell, c_tmp);
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
  ::error ("evaluating for command near line %d, column %d",
	   line (), column ());
}

tree_command *
tree_simple_for_command::dup (symbol_table *sym_tab)
{
  return new tree_simple_for_command (lhs ? lhs->dup (sym_tab) : 0,
				      expr ? expr->dup (sym_tab) : 0,
				      list ? list->dup (sym_tab) : 0,
				      lead_comm ? lead_comm->dup () : 0,
				      trail_comm ? trail_comm->dup () : 0,
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

      tree_argument_list::iterator p = lhs->begin ();
      tree_expression *elt = *p++;
      octave_lvalue val_ref = elt->lvalue ();
      elt = *p;
      octave_lvalue key_ref = elt->lvalue ();

      Octave_map tmp_val (rhs.map_value ());

      for (Octave_map::iterator q = tmp_val.begin (); q != tmp_val.end (); q++)
	{
	  octave_value key = tmp_val.key (q);

	  Cell val_lst = tmp_val.contents (q);

	  octave_idx_type n = tmp_val.numel ();

	  octave_value val = (n == 1) ? val_lst(0) : octave_value (val_lst);

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
  ::error ("evaluating for command near line %d, column %d",
	   line (), column ());
}

tree_command *
tree_complex_for_command::dup (symbol_table *sym_tab)
{
  return new tree_complex_for_command (lhs ? lhs->dup (sym_tab) : 0,
				      expr ? expr->dup (sym_tab) : 0,
				      list ? list->dup (sym_tab) : 0,
				      lead_comm ? lead_comm->dup () : 0,
				      trail_comm ? trail_comm->dup () : 0,
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
