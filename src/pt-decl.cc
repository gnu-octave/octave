/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2006, 2007 John W. Eaton

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

#include "defun.h"
#include "error.h"
#include "pt-cmd.h"
#include "ov.h"
#include "oct-lvalue.h"
#include "pt-bp.h"
#include "pt-decl.h"
#include "pt-exp.h"
#include "pt-id.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"

// Declarations (global, static, etc.).

tree_decl_elt::~tree_decl_elt (void)
{
  delete id;
  delete expr;
}

bool
tree_decl_elt::eval (void)
{
  bool retval = false;

  if (id && expr)
    {
      octave_lvalue ult = id->lvalue ();

      octave_value init_val = expr->rvalue ();

      if (! error_state)
	{
	  ult.assign (octave_value::op_asn_eq, init_val);

	  retval = true;
	}
    }

  return retval;
}

tree_decl_elt *
tree_decl_elt::dup (symbol_table::scope_id scope,
		    symbol_table::context_id context)
{
  return new tree_decl_elt (id ? id->dup (scope, context) : 0,
			    expr ? expr->dup (scope, context) : 0);
}

void
tree_decl_elt::accept (tree_walker& tw)
{
  tw.visit_decl_elt (*this);
}

// Initializer lists for declaration statements.

void
tree_decl_init_list::eval (tree_decl_elt::eval_fcn f)
{
  for (iterator p = begin (); p != end (); p++)
    {
      tree_decl_elt *elt = *p;

      f (*elt);

      if (error_state)
	break;
    }
}

tree_decl_init_list *
tree_decl_init_list::dup (symbol_table::scope_id scope,
			  symbol_table::context_id context)
{
  tree_decl_init_list *new_dil = new tree_decl_init_list ();

  for (iterator p = begin (); p != end (); p++)
    {
      tree_decl_elt *elt = *p;

      new_dil->append (elt ? elt->dup (scope, context) : 0);
    }
  
  return new_dil;
}

void
tree_decl_init_list::accept (tree_walker& tw)
{
  tw.visit_decl_init_list (*this);
}

// Base class for declaration commands (global, static).

tree_decl_command::~tree_decl_command (void)
{
  delete init_list;
}

void
tree_decl_command::accept (tree_walker& tw)
{
  tw.visit_decl_command (*this);
}

// Global.

void
tree_global_command::do_init (tree_decl_elt& elt)
{
  tree_identifier *id = elt.ident ();

  if (id)
    {
      id->mark_global ();

      if (! error_state)
	{
	  octave_lvalue ult = id->lvalue ();

	  if (ult.is_undefined ())
	    {
	      tree_expression *expr = elt.expression ();

	      octave_value init_val;

	      if (expr)
		init_val = expr->rvalue ();
	      else
		init_val = Matrix ();

	      ult.assign (octave_value::op_asn_eq, init_val);
	    }
	}
    }
}

void
tree_global_command::eval (void)
{
  MAYBE_DO_BREAKPOINT;

  if (init_list)
    init_list->eval (do_init);
}

tree_command *
tree_global_command::dup (symbol_table::scope_id scope,
			  symbol_table::context_id context)
{
  return
    new tree_global_command (init_list ? init_list->dup (scope, context) : 0,
			     line (), column ());
}

// Static.

void
tree_static_command::do_init (tree_decl_elt& elt)
{
  tree_identifier *id = elt.ident ();

  if (id)
    {
      id->mark_as_static ();

      octave_lvalue ult = id->lvalue ();

      if (ult.is_undefined ())
	{
	  tree_expression *expr = elt.expression ();

	  octave_value init_val;

	  if (expr)
	    init_val = expr->rvalue ();
	  else
	    init_val = Matrix ();

	  ult.assign (octave_value::op_asn_eq, init_val);
	}
    }
}

void
tree_static_command::eval (void)
{
  MAYBE_DO_BREAKPOINT;

  // Static variables only need to be marked and initialized once.

  if (init_list)
    init_list->eval (do_init);
}

tree_command *
tree_static_command::dup (symbol_table::scope_id scope,
			  symbol_table::context_id context)
{
  return
    new tree_static_command (init_list ? init_list->dup (scope, context) : 0,
			     line (), column ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
