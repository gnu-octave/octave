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
#include <strstream.h>

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "oct-obj.h"
#include "pager.h"
#include "ov.h"
#include "pt-exp.h"
#include "pt-fvc.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "pt-pr-code.h"
#include "pt-walk.h"
#include "utils.h"

// Nonzero means we're returning from a function.
extern int returning;

// Nonzero means we're breaking out of a loop or function body.
extern int breaking;

// Prefix expressions.

tree_prefix_expression::~tree_prefix_expression (void)
{
  delete id;
}

octave_value
tree_prefix_expression::eval (bool print)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (id)
    {
      switch (etype)
	{
	case increment:
	  id->increment ();
	  break;

	case decrement:
	  id->decrement ();
	  break;

	default:
	  error ("prefix operator %d not implemented", etype);
	  break;
	}


      if (error_state)
	eval_error ();
      else
	{
	  retval = id->eval (print);
	  if (error_state)
	    {
	      retval = octave_value ();
	      if (error_state)
		eval_error ();
	    }
	}
    }
  return retval;
}

char *
tree_prefix_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case increment:
      op = "++";
      break;

    case decrement:
      op = "--";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

void
tree_prefix_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op = oper ();

      ::error ("evaluating prefix operator `%s' near line %d, column %d",
	       op, line (), column ());
    }
}

void
tree_prefix_expression::accept (tree_walker& tw)
{
  tw.visit_prefix_expression (*this);
}

// Postfix expressions.

tree_postfix_expression::~tree_postfix_expression (void)
{
  delete id;
}

octave_value
tree_postfix_expression::eval (bool print)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (id)
    {
      retval = id->eval (print);

      switch (etype)
	{
	case increment:
	  id->increment ();
	  break;

	case decrement:
	  id->decrement ();
	  break;

	default:
	  error ("postfix operator %d not implemented", etype);
	  break;
	}

      if (error_state)
	{
	  retval = octave_value ();
	  if (error_state)
	    eval_error ();
	}
    }
  return retval;
}

char *
tree_postfix_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case increment:
      op = "++";
      break;

    case decrement:
      op = "--";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

void
tree_postfix_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op = oper ();

      ::error ("evaluating postfix operator `%s' near line %d, column %d",
	       op, line (), column ());
    }
}

void
tree_postfix_expression::accept (tree_walker& tw)
{
  tw.visit_postfix_expression (*this);
}

// Unary expressions.

octave_value
tree_unary_expression::eval (bool /* print */)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op)
    {
      octave_value u = op->eval (false);

      if (error_state)
	eval_error ();
      else if (u.is_defined ())
	{
	  switch (etype)
	    {
	    case not:
	      retval = u.not ();
	      break;

	    case uminus:
	      retval = u.uminus ();
	      break;

	    case transpose:
	      retval = u.transpose ();
	      break;

	    case hermitian:
	      retval = u.hermitian ();
	      break;

	    default:
	      ::error ("unary operator %d not implemented", etype);
	      break;
	    }

	  if (error_state)
	    {
	      retval = octave_value ();
	      eval_error ();
	    }
	}
    }

  return retval;
}

char *
tree_unary_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case not:
      op = "!";
      break;

    case uminus:
      op = "-";
      break;

    case transpose:
      op = ".'";
      break;

    case hermitian:
      op = "'";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

void
tree_unary_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op = oper ();

      ::error ("evaluating unary operator `%s' near line %d, column %d",
	       op, line (), column ());
    }
}

void
tree_unary_expression::accept (tree_walker& tw)
{
  tw.visit_unary_expression (*this);
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
      octave_value a = op_lhs->eval (false);

      if (error_state)
	eval_error ();
      else if (a.is_defined () && op_rhs)
	{
	  octave_value b = op_rhs->eval (false);

	  if (error_state)
	    eval_error ();
	  else if (b.is_defined ())
	    {
	      octave_value::binary_op op = octave_value::unknown_binary_op;

	      switch (etype)
		{
		case add:
		  op = octave_value::add;
		  break;

		case subtract:
		  op = octave_value::sub;
		  break;

		case multiply:
		  op = octave_value::mul;
		  break;

		case el_mul:
		  op = octave_value::el_mul;
		  break;

		case divide:
		  op = octave_value::div;
		  break;

		case el_div:
		  op = octave_value::el_div;
		  break;

		case leftdiv:
		  op = octave_value::ldiv;
		  break;

		case el_leftdiv:
		  op = octave_value::el_ldiv;
		  break;

		case power:
		  op = octave_value::pow;
		  break;

		case elem_pow:
		  op = octave_value::el_pow;
		  break;

		case cmp_lt:
		  op = octave_value::lt;
		  break;

		case cmp_le:
		  op = octave_value::le;
		  break;

		case cmp_eq:
		  op = octave_value::eq;
		  break;

		case cmp_ge:
		  op = octave_value::ge;
		  break;

		case cmp_gt:
		  op = octave_value::gt;
		  break;

		case cmp_ne:
		  op = octave_value::ne;
		  break;

		case and:
		  op = octave_value::el_and;
		  break;

		case or:
		  op = octave_value::el_or;
		  break;

		default:
		  ::error ("binary operator %d not implemented", etype);
		  break;
		}

	      if (! error_state)
		retval = ::do_binary_op (op, a, b);
	      else
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

char *
tree_binary_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case add:
      op = "+";
      break;

    case subtract:
      op = "-";
      break;

    case multiply:
      op = "*";
      break;

    case el_mul:
      op = ".*";
      break;

    case divide:
      op = "/";
      break;

    case el_div:
      op = "./";
      break;

    case leftdiv:
      op = "\\";
      break;

    case el_leftdiv:
      op = ".\\";
      break;

    case power:
      op = "^";
      break;

    case elem_pow:
      op = ".^";
      break;

    case cmp_lt:
      op = "<";
      break;

    case cmp_le:
      op = "<=";
      break;

    case cmp_eq:
      op = "==";
      break;

    case cmp_ge:
      op = ">=";
      break;

    case cmp_gt:
      op = ">";
      break;

    case cmp_ne:
      op = "!=";
      break;

    case and:
      op = "&";
      break;

    case or:
      op = "|";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

void
tree_binary_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op = oper ();

      ::error ("evaluating binary operator `%s' near line %d, column %d",
	       op, line (), column ());
    }
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
      octave_value a = op_lhs->eval (false);

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
		  if (etype == or)
		    {
		      result = true;
		      goto done;
		    }
		}
	      else
		{
		  if (etype == and)
		    goto done;
		}

	      if (op_rhs)
		{
		  octave_value b = op_rhs->eval (false);

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
		retval = octave_value ((double) result);
	    }
	}
    }
  else
    eval_error ();

  return retval;
}

char *
tree_boolean_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case and:
      op = "&&";
      break;

    case or:
      op = "||";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

// Simple assignment expressions.

tree_simple_assignment_expression::tree_simple_assignment_expression
  (tree_identifier *i, tree_expression *r, bool plhs, bool ans_assign,
   int l, int c)
    : tree_expression (l, c)
      {
	init (plhs, ans_assign);
	lhs = new tree_indirect_ref (i);
	rhs = r;
      }

tree_simple_assignment_expression::tree_simple_assignment_expression
  (tree_index_expression *idx_expr, tree_expression *r, bool plhs,
   bool ans_assign, int l, int c)
    : tree_expression (l, c)
      {
	init (plhs, ans_assign);
	lhs_idx_expr = idx_expr; // cache this -- we may need to delete it.
	lhs = idx_expr->ident ();
	index = idx_expr->arg_list ();
	rhs = r;
      }

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

// ??? FIXME ??? -- should this return the value of the RHS instead?

// ??? FIXME ??? -- should octave_variable_reference::assign return
// the right thing for us to return?

octave_value
tree_simple_assignment_expression::eval (bool print)
{
  assert (etype == tree_expression::assignment);

  octave_value retval;

  if (error_state)
    return retval;

  if (rhs)
    {
      octave_value rhs_val = rhs->eval (false);
      if (error_state)
	{
	  eval_error ();
	}
      else if (rhs_val.is_undefined ())
	{
	  error ("value on right hand side of assignment is undefined");
	  eval_error ();
	}
      else
	{
	  octave_variable_reference ult (lhs);

	  if (error_state)
	    eval_error ();
	  else
	    {
	      if (index)
		{
		  // Extract the arguments into a simple vector.

		  octave_value_list args = index->convert_to_const_vector ();

		  if (error_state)
		    eval_error ();
		  else
		    {
		      int nargin = args.length ();

		      if (nargin > 0)
			{
			  ult.assign (args, rhs_val);

			  if (error_state)
			    eval_error ();
			  else
			    retval = ult.value ();
			}
		      else
			error ("??? invalid index list ???");
		    }
		}
	      else
		{
		  ult.assign (rhs_val);
		  retval = ult.value ();
		}
	    }
	}
    }

  if (! error_state && print && retval.is_defined ())
    retval.print_with_name (lhs->name ());

  return retval;
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

  octave_value tmp = op_base->eval (false);

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

  tmp = op_limit->eval (false);

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
      tmp = op_increment->eval (false);

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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
