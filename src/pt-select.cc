/*

Copyright (C) 1996, 1997, 2000, 2002, 2004, 2005, 2006, 2007
              John W. Eaton

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

#include "error.h"
#include "oct-obj.h"
#include "ov.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-select.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "Cell.h"
#include "ov-typeinfo.h"

// If clauses.

tree_if_clause::~tree_if_clause (void)
{
  delete expr;
  delete list;
  delete lead_comm;
}

int
tree_if_clause::eval (void)
{
  if (is_else_clause () || expr->is_logically_true ("if"))
    {
      if (list)
	list->eval ();

      return 1;
    }

  return 0;
}

tree_if_clause *
tree_if_clause::dup (symbol_table::scope_id scope,
		     symbol_table::context_id context)
{
  return new tree_if_clause (expr ? expr->dup (scope, context) : 0,
			     list ? list->dup (scope, context) : 0,
			     lead_comm ? lead_comm->dup () : 0);
}

void
tree_if_clause::accept (tree_walker& tw)
{
  tw.visit_if_clause (*this);
}

// List of if commands.

void
tree_if_command_list::eval (void)
{
  for (iterator p = begin (); p != end (); p++)
    {
      tree_if_clause *t = *p;

      if (t->eval () || error_state)
	break;
    }
}

tree_if_command_list *
tree_if_command_list::dup (symbol_table::scope_id scope,
			   symbol_table::context_id context)
{
  tree_if_command_list *new_icl = new tree_if_command_list ();

  for (iterator p = begin (); p != end (); p++)
    {
      tree_if_clause *elt = *p;

      new_icl->append (elt ? elt->dup (scope, context) : 0);
    }

  return new_icl;
}

void
tree_if_command_list::accept (tree_walker& tw)
{
  tw.visit_if_command_list (*this);
}

// If.

tree_if_command::~tree_if_command (void)
{
  delete list;
  delete lead_comm;
  delete trail_comm;
}

void
tree_if_command::eval (void)
{
  if (list)
    list->eval ();

  if (error_state)
    ::error ("evaluating if command near line %d, column %d",
	     line (), column ());
}

tree_command *
tree_if_command::dup (symbol_table::scope_id scope,
		      symbol_table::context_id context)
{
  return new tree_if_command (list ? list->dup (scope, context) : 0,
			      lead_comm ? lead_comm->dup () : 0,
			      trail_comm ? trail_comm->dup () : 0,
			      line (), column ());
}

void
tree_if_command::accept (tree_walker& tw)
{
  tw.visit_if_command (*this);
}

// Switch cases.

tree_switch_case::~tree_switch_case (void)
{
  delete label;
  delete list;
  delete lead_comm;
}


// Compare two octave values, returning true if equal, false if not
// FIXME --- should be member or friend of octave_value class.

static bool
equal (const octave_value& val, const octave_value& test)
{
  bool retval = false;

  // If there is no op_eq for these types, we can't compare values.

  if (val.rows () == test.rows () && val.columns () == test.columns ())
    {
      octave_value tmp = do_binary_op (octave_value::op_eq, val, test);

      if (! error_state && tmp.is_defined ())
	retval = tmp.is_true ();
    }

  return retval;
}

bool
tree_switch_case::label_matches (const octave_value& val)
{
  octave_value label_value = label->rvalue ();

  if (! error_state && label_value.is_defined() )
    {
      if (label_value.is_cell ())
	{
	  Cell cell (label_value.cell_value ());

	  for (octave_idx_type i = 0; i < cell.rows (); i++)
	    {
	      for (octave_idx_type j = 0; j < cell.columns (); j++)
		{
		  bool match = equal (val, cell(i,j));

		  if (error_state)
		    {
		      eval_error ();
		      return false;
		    }
		  else if (match)
		    return true;
		}
	    }
	}
      else
	{
	  bool match = equal (val, label_value);

	  if (error_state)
	    {
	      eval_error ();
	      return false;
	    }
	  else
	    return match;
	}
    }
  else
    eval_error ();

  return false;
}

int
tree_switch_case::eval (const octave_value& val)
{
  int retval = 0;

  if (is_default_case () || label_matches (val))
    {
      if (list)
	list->eval ();

      retval = 1;
    }

  return retval;
}

void
tree_switch_case::eval_error (void)
{
  ::error ("evaluating switch case label");
}

tree_switch_case *
tree_switch_case::dup (symbol_table::scope_id scope,
		       symbol_table::context_id context)
{
  return new tree_switch_case (label ? label->dup (scope, context) : 0,
			       list ? list->dup (scope, context) : 0,
			       lead_comm ? lead_comm->dup () : 0);
}

void
tree_switch_case::accept (tree_walker& tw)
{
  tw.visit_switch_case (*this);
}

// List of switch cases.

void
tree_switch_case_list::eval (const octave_value& val)
{
  for (iterator p = begin (); p != end (); p++)
    {
      tree_switch_case *t = *p;

      if (t->eval (val) || error_state)
	break;
    }
}

tree_switch_case_list *
tree_switch_case_list::dup (symbol_table::scope_id scope,
			    symbol_table::context_id context)
{
  tree_switch_case_list *new_scl = new tree_switch_case_list ();

  for (iterator p = begin (); p != end (); p++)
    {
      tree_switch_case *elt = *p;

      new_scl->append (elt ? elt->dup (scope, context) : 0);
    }
  
  return new_scl;
}

void
tree_switch_case_list::accept (tree_walker& tw)
{
  tw.visit_switch_case_list (*this);
}

// Switch.

tree_switch_command::~tree_switch_command (void)
{
  delete expr;
  delete list;
  delete lead_comm;
  delete trail_comm;
}

void
tree_switch_command::eval (void)
{
  if (expr)
    {
      octave_value val = expr->rvalue ();

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

tree_command *
tree_switch_command::dup (symbol_table::scope_id scope,
			  symbol_table::context_id context)
{
  return new tree_switch_command (expr ? expr->dup (scope, context) : 0,
				  list ? list->dup (scope, context) : 0,
				  lead_comm ? lead_comm->dup () : 0,
				  trail_comm ? trail_comm->dup () : 0,
				  line (), column ());
}

void
tree_switch_command::accept (tree_walker& tw)
{
  tw.visit_switch_command (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
