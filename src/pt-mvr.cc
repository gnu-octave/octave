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
#include "oct-obj.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-exp.h"
#include "pt-fvc.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "pt-walk.h"

// But first, some extra functions used by the tree classes.

// Make sure that all arguments have values.

static bool
all_args_defined (const octave_value_list& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (args(i).is_undefined ())
      return false;

  return true;
}

// Used internally.

octave_value
tree_oct_obj::eval (bool /* print */)
{
  return values(0);
}

octave_value_list
tree_oct_obj::eval (bool /* print */, int /* nargout */,
		    const octave_value_list& /* args */)
{
  return values;
}

void
tree_oct_obj::accept (tree_walker& tw)
{
  tw.visit_oct_obj (*this);
}

// Index expressions.

tree_index_expression::tree_index_expression
  (tree_identifier *i, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	id = new tree_indirect_ref (i);
	list = 0;
      }

tree_index_expression::tree_index_expression
  (tree_identifier *i, tree_argument_list *lst, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	id = new tree_indirect_ref (i);
	list = lst;
      }

tree_index_expression::~tree_index_expression (void)
{
  delete id;
  delete list;
}

string
tree_index_expression::name (void)
{
  return id->name ();
}

void
tree_index_expression::mark_for_possible_ans_assign (void)
{
  if (id)
    id->mark_for_possible_ans_assign ();
}

octave_value
tree_index_expression::eval (bool print)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (list)
    {
      // Extract the arguments into a simple vector.  Don't pass null
      // args.

      octave_value_list args = list->convert_to_const_vector ();

      if (error_state)
	eval_error ();
      else
	{
	  if (error_state)
	    eval_error ();
	  else
	    {
	      if (all_args_defined (args))
		{
		  octave_value_list tmp = id->eval (print, 1, args);

		  if (error_state)
		    eval_error ();
		  else if (tmp.length () > 0)
		    retval = tmp(0);
		}
	      else
		{
		  ::error ("undefined arguments found in index expression");
		  eval_error ();
		}
	    }
	}
    }
  else
    {
      retval = id->eval (print);

      if (error_state)
	eval_error ();
    }

  return retval;
}

octave_value_list
tree_index_expression::eval (bool print, int nargout,
			     const octave_value_list& /* args */)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  if (list)
    {
      // Extract the arguments into a simple vector.  Don't pass null
      // args.

      octave_value_list tmp_args = list->convert_to_const_vector ();

      if (error_state)
	eval_error ();
      else
	{
	  if (error_state)
	    eval_error ();
	  else
	    {
	      if (all_args_defined (tmp_args))
		{
		  retval = id->eval (print, nargout, tmp_args);

		  if (error_state)
		    eval_error ();
		}
	      else
		{
		  ::error ("undefined arguments found in index expression");
		  eval_error ();
		}
	    }
	}
    }
  else
    {
      octave_value_list tmp_args;

      retval = id->eval (print, nargout, tmp_args);

      if (error_state)
	eval_error ();
    }

  return retval;
}

void
tree_index_expression::eval_error (void)
{
  if (error_state > 0)
    {
      int l = line ();
      int c = column ();
      char *fmt;
      if (l != -1 && c != -1)
	{
	  if (list)
	    fmt = "evaluating index expression near line %d, column %d";
	  else
	    fmt = "evaluating expression near line %d, column %d";

	  ::error (fmt, l, c);
	}
      else
	{
	  if (list)
	    ::error ("evaluating index expression");
	  else
	    ::error ("evaluating expression");
	}
    }
}

void
tree_index_expression::accept (tree_walker& tw)
{
  tw.visit_index_expression (*this);
}

// Multi-valued assignmnt expressions.

tree_multi_assignment_expression::~tree_multi_assignment_expression (void)
{
  if (! preserve)
    delete lhs;

  delete rhs;
}

octave_value
tree_multi_assignment_expression::eval (bool print)
{
  octave_value retval;

  if (error_state)
    return retval;

  octave_value_list tmp_args;
  octave_value_list result = eval (print, 1, tmp_args);

  if (result.length () > 0)
    retval = result(0);

  return retval;
}

octave_value_list
tree_multi_assignment_expression::eval (bool print, int nargout,
					const octave_value_list& /* args */)
{
  assert (etype == tree_expression::multi_assignment);

  if (error_state || ! rhs)
    return octave_value_list ();

  nargout = lhs->length ();
  octave_value_list tmp_args;
  octave_value_list results = rhs->eval (0, nargout, tmp_args);

  if (error_state)
    eval_error ();

  int ma_line = line ();
  int ma_column = column ();

  if (results.length () > 0)
    {
      int i = 0;

      bool pad_after = false;

      for (Pix p = lhs->first (); p != 0; lhs->next (p))
	{
	  tree_index_expression *lhs_expr = lhs->operator () (p);

	  if (i < nargout)
	    {
	      // XXX FIXME? XXX -- this is apparently the way Matlab
	      // works, but maybe we should have the option of
	      // skipping the assignment instead.

	      tree_constant *tmp = 0;
	      if (results(i).is_undefined ())
		{
		  error ("element number %d undefined in return list", i+1);
		  eval_error ();
		  break;
		}
	      else
		tmp = new tree_constant (results(i));

	      tree_simple_assignment_expression tmp_expr
		(lhs_expr, tmp, 1, 0, ma_line, ma_column);

	      results(i) = tmp_expr.eval (false); // May change

	      if (error_state)
		break;

	      if (print && pad_after)
		octave_stdout << "\n";

	      if (print)
		results(i).print_with_name (lhs_expr->name (), 0);

	      pad_after = true;

	      i++;
	    }
	  else
	    {
	      tree_simple_assignment_expression tmp_expr
		(lhs_expr, 0, 1, 0, ma_line, ma_column);

	      tmp_expr.eval (false);
	    }
	}

      if (print && pad_after)
	octave_stdout << "\n";
    }

  return results;
}

void
tree_multi_assignment_expression::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating assignment expression near line %d, column %d",
	     line (), column ());
}

void
tree_multi_assignment_expression::accept (tree_walker& tw)
{
  tw.visit_multi_assignment_expression (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
