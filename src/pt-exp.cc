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

#include <iostream.h>
#include <strstream.h>

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "oct-obj.h"
#include "oct-var-ref.h"
#include "pager.h"
#include "ov.h"
#include "pt-exp.h"
#include "pt-id.h"
#include "pt-indir.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "pt-pr-code.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"

// Nonzero means we're returning from a function.
extern int returning;

// Nonzero means we're breaking out of a loop or function body.
extern int breaking;

// TRUE means print the right hand side of an assignment instead of
// the left.
static bool Vprint_rhs_assign_val;

// Prefix expressions.

octave_value
tree_prefix_expression::eval (bool)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op)
    {
      if (etype == unot || etype == uminus)
	{
	  octave_value val = op->eval ();

	  if (! error_state)
	    {
	      if (etype == unot)
		retval = val.not ();
	      else
		retval = val.uminus ();
	    }
	}
      else if (etype == increment || etype == decrement)
	{
	  octave_variable_reference ref = op->reference ();

	  if (! error_state)
	    {
	      if (etype == increment)
		ref.increment ();
	      else
		ref.decrement ();

	      retval = ref.value ();
	    }
	}
      else
	error ("prefix operator %d not implemented", etype);
    }

  return retval;
}

string
tree_prefix_expression::oper (void) const
{
  string retval = "<unknown>";

  switch (etype)
    {
    case unot:
      retval = "!";
      break;

    case uminus:
      retval = "-";
      break;

    case increment:
      retval = "++";
      break;

    case decrement:
      retval = "--";
      break;

    default:
      break;
    }

  return retval;
}

void
tree_prefix_expression::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating prefix operator `%s' near line %d, column %d",
	     oper () . c_str (), line (), column ());
}

void
tree_prefix_expression::accept (tree_walker& tw)
{
  tw.visit_prefix_expression (*this);
}

// Postfix expressions.

octave_value
tree_postfix_expression::eval (bool)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op)
    {
      if (etype == transpose || etype == hermitian)
	{
	  octave_value val = op->eval ();

	  if (! error_state)
	    {
	      if (etype == transpose)
		retval = val.transpose ();
	      else
		retval = val.hermitian ();
	    }
	}
      else if (etype == increment || etype == decrement)
	{
	  octave_variable_reference ref = op->reference ();

	  if (! error_state)
	    {
	      retval = ref.value ();

	      if (etype == increment)
		ref.increment ();
	      else
		ref.decrement ();
	    }
	}
      else
	error ("postfix operator %d not implemented", etype);
    }

  return retval;
}

string
tree_postfix_expression::oper (void) const
{
  string retval = "<unknown>";

  switch (etype)
    {
    case transpose:
      retval = ".'";
      break;

    case hermitian:
      retval = "'";
      break;

    case increment:
      retval = "++";
      break;

    case decrement:
      retval = "--";
      break;

    default:
      break;
    }

  return retval;
}

void
tree_postfix_expression::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating postfix operator `%s' near line %d, column %d",
	     oper () . c_str (), line (), column ());
}

void
tree_postfix_expression::accept (tree_walker& tw)
{
  tw.visit_postfix_expression (*this);
}

// Binary expressions.
 
octave_value
tree_binary_expression::eval (bool /* print */)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op_lhs)
    {
      octave_value a = op_lhs->eval ();

      if (error_state)
	eval_error ();
      else if (a.is_defined () && op_rhs)
	{
	  octave_value b = op_rhs->eval ();

	  if (error_state)
	    eval_error ();
	  else if (b.is_defined ())
	    {
	      retval = ::do_binary_op (etype, a, b);

	      if (error_state)
		{
		  retval = octave_value ();
		  eval_error ();
		}
	    }
	  else
	    eval_error ();
	}
      else
	eval_error ();
    }
  else
    eval_error ();

  return retval;
}

string
tree_binary_expression::oper (void) const
{
  return octave_value::binary_op_as_string (etype);
}

void
tree_binary_expression::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating binary operator `%s' near line %d, column %d",
	     oper () . c_str (), line (), column ());
}

void
tree_binary_expression::accept (tree_walker& tw)
{
  tw.visit_binary_expression (*this);
}

// Boolean expressions.
 
octave_value
tree_boolean_expression::eval (bool /* print */)
{
  octave_value retval;

  if (error_state)
    return retval;

  bool result = false;

  if (op_lhs)
    {
      octave_value a = op_lhs->eval ();

      if (error_state)
	eval_error ();
      else
	{
	  bool a_true = a.is_true ();

	  if (error_state)
	    eval_error ();
	  else
	    {
	      if (a_true)
		{
		  if (etype == bool_or)
		    {
		      result = true;
		      goto done;
		    }
		}
	      else
		{
		  if (etype == bool_and)
		    goto done;
		}

	      if (op_rhs)
		{
		  octave_value b = op_rhs->eval ();

		  if (error_state)
		    eval_error ();
		  else
		    {
		      result = b.is_true ();

		      if (error_state)
			eval_error ();
		    }
		}
	      else
		eval_error ();

	    done:

	      if (! error_state)
		retval = octave_value (static_cast<double> (result));
	    }
	}
    }
  else
    eval_error ();

  return retval;
}

string
tree_boolean_expression::oper (void) const
{
  string retval = "<unknown>";

  switch (etype)
    {
    case bool_and:
      retval = "&&";
      break;

    case bool_or:
      retval = "||";
      break;

    default:
      break;
    }

  return retval;
}

// Simple assignment expressions.

tree_simple_assignment_expression::tree_simple_assignment_expression
  (tree_identifier *i, tree_expression *r, bool plhs, bool ans_assign,
   int l, int c, octave_value::assign_op t)
    : tree_expression (l, c), lhs_idx_expr (0),
      lhs (new tree_indirect_ref (i)), index (0), rhs (r),
      preserve (plhs), ans_ass (ans_assign), etype (t) { }

tree_simple_assignment_expression::tree_simple_assignment_expression
  (tree_index_expression *idx_expr, tree_expression *r, bool plhs,
   bool ans_assign, int l, int c, octave_value::assign_op t)
    : tree_expression (l, c), lhs_idx_expr (idx_expr),
      lhs (idx_expr->ident ()), index (idx_expr->arg_list ()), rhs (r),
      preserve (plhs), ans_ass (ans_assign), etype (t) { }

tree_simple_assignment_expression::~tree_simple_assignment_expression (void)
{
  if (! preserve)
    {
      if (lhs_idx_expr)
	delete lhs_idx_expr;
      else
	delete lhs;
    }

  delete rhs;
}

bool
tree_simple_assignment_expression::left_hand_side_is_identifier_only (void)
{
  return lhs->is_identifier_only ();
}

tree_identifier *
tree_simple_assignment_expression::left_hand_side_id (void)
{
  return lhs->ident ();
}

// ??? FIXME ??? -- should octave_value::assign return the right thing
// for us to return?

octave_value
tree_simple_assignment_expression::eval (bool print)
{
  octave_value rhs_val;

  if (error_state)
    return rhs_val;

  if (rhs)
    {
      octave_value lhs_val;

      rhs_val = rhs->eval ();

      if (! error_state)
	{
	  if (rhs_val.is_undefined ())
	    {
	      error ("value on right hand side of assignment is undefined");
	      eval_error ();
	    }
	  else
	    {
	      octave_variable_reference ult = lhs->reference ();

	      if (error_state)
		eval_error ();
	      else
		{
		  if (index)
		    {
		      // Extract the arguments into a simple vector.

		      octave_value_list args
			= index->convert_to_const_vector ();

		      if (! error_state)
			{
			  int nargin = args.length ();

			  if (nargin > 0)
			    {
			      ult.index (args);

			      ult.assign (etype, rhs_val);

			      if (error_state)
				eval_error ();
			      else if (! Vprint_rhs_assign_val)
				lhs_val = ult.value ();
			    }
			  else
			    error ("??? invalid index list ???");
			}
		      else
			eval_error ();
		    }
		  else
		    {
		      ult.assign (etype, rhs_val);

		      if (error_state)
			eval_error ();
		      else if (! Vprint_rhs_assign_val)
			lhs_val = ult.value ();
		    }
		}
	    }
	}
      else
	eval_error ();

      if (! error_state && print)
	{
	  if (Vprint_rhs_assign_val)
	    {
	      ostrstream buf;

	      buf << lhs->name ();

	      if (index)
		{
		  buf << " (";
		  tree_print_code tpc (buf);
		  index->accept (tpc);
		  buf << ")";
		}

	      buf << ends;

	      const char *tag = buf.str ();

	      rhs_val.print_with_name (octave_stdout, tag);

	      delete [] tag;
	    }
	  else
	    lhs_val.print_with_name (octave_stdout, lhs->name ());
	}
    }

  return rhs_val;
}

void
tree_simple_assignment_expression::eval_error (void)
{
  if (error_state > 0)
    {
      int l = line ();
      int c = column ();

      if (l != -1 && c != -1)
	::error ("evaluating assignment expression near line %d, column %d",
		 l, c);
    }
}

string
tree_simple_assignment_expression::oper (void) const
{
  return octave_value::assign_op_as_string (etype);
}

void
tree_simple_assignment_expression::accept (tree_walker& tw)
{
  tw.visit_simple_assignment_expression (*this);
}

// Colon expressions.

tree_colon_expression *
tree_colon_expression::chain (tree_expression *t)
{
  tree_colon_expression *retval = 0;
  if (! op_base || op_increment)
    ::error ("invalid colon expression");
  else
    {
      // Stupid syntax:
      //
      // base : limit
      // base : increment : limit

      op_increment = op_limit;
      op_limit = t;

      retval = this;
    }
  return retval;
}

octave_value
tree_colon_expression::eval (bool /* print */)
{
  octave_value retval;

  if (error_state || ! op_base || ! op_limit)
    return retval;

  octave_value tmp = op_base->eval ();

  if (tmp.is_undefined ())
    {
      eval_error ("invalid null value in colon expression");
      return retval;
    }

  double base = tmp.double_value ();

  if (error_state)
    {
      error ("colon expression elements must be scalars");
      eval_error ("evaluating colon expression");
      return retval;
    }

  tmp = op_limit->eval ();

  if (tmp.is_undefined ())
    {
      eval_error ("invalid null value in colon expression");
      return retval;
    }

  double limit = tmp.double_value ();

  if (error_state)
    {
      error ("colon expression elements must be scalars");
      eval_error ("evaluating colon expression");
      return retval;
    }

  double inc = 1.0;

  if (op_increment)
    {
      tmp = op_increment->eval ();

      if (tmp.is_undefined ())
	{
	  eval_error ("invalid null value in colon expression");
	  return retval;
	}

      inc = tmp.double_value ();

      if (error_state)
	{
	  error ("colon expression elements must be scalars");
	  eval_error ("evaluating colon expression");
	  return retval;
	}
    }

  retval = octave_value (base, limit, inc);

  if (error_state)
    {
      if (error_state)
	eval_error ("evaluating colon expression");
      return octave_value ();
    }

  return retval;
}

void
tree_colon_expression::eval_error (const char *s)
{
  if (error_state > 0)
    ::error ("%s near line %d column %d", s, line (), column ());
}

void
tree_colon_expression::accept (tree_walker& tw)
{
  tw.visit_colon_expression (*this);
}

static int
print_rhs_assign_val (void)
{
  Vprint_rhs_assign_val = check_preference ("print_rhs_assign_val");

  return 0;
}

void
symbols_of_pt_exp (void)
{
  DEFVAR (print_rhs_assign_val, 0.0, 0, print_rhs_assign_val,
    "if TRUE, print the right hand side of assignments instead of the left");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
