// pt-exp.cc                                          -*- C++ -*-
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
#include "pt-const.h"
#include "pt-exp.h"
#include "pt-fvc.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "user-prefs.h"
#include "utils.h"

// Nonzero means we're returning from a function.
extern int returning;

// Nonzero means we're breaking out of a loop or function body.
extern int breaking;

// But first, some extra functions used by the tree classes.

static void
print_constant (tree_constant& tc, char *name, int print_padding = 1)
{
  int pad_after = 0;
  if (user_pref.print_answer_id_name)
    {
      if (print_as_scalar (tc) || print_as_structure (tc))
	{
	  ostrstream output_buf;
	  output_buf << name << " = " << ends;
	  maybe_page_output (output_buf);
	}
      else
	{
	  pad_after = 1;
	  ostrstream output_buf;
	  output_buf << name << " =\n\n" << ends;
	  maybe_page_output (output_buf);
	}
    }

  tc.eval (1);

  if (print_padding && pad_after)
    {
      ostrstream output_buf;
      output_buf << "\n" << ends;
      maybe_page_output (output_buf);
    }
}

// Prefix expressions.

tree_prefix_expression::~tree_prefix_expression (void)
{
  delete id;
}

tree_constant
tree_prefix_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (id)
    {
      id->bump_value (etype);
      if (error_state)
	eval_error ();
      else
	{
	  retval = id->eval (print);
	  if (error_state)
	    {
	      retval = tree_constant ();
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
    case tree_expression::increment:
      op = "++";
      break;

    case tree_expression::decrement:
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
tree_prefix_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  os << oper ();

  if (id)
    id->print_code (os);

  if (in_parens)
    os << ")";
}

// Postfix expressions.

tree_postfix_expression::~tree_postfix_expression (void)
{
  delete id;
}

tree_constant
tree_postfix_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (id)
    {
      retval = id->eval (print);
      id->bump_value (etype);
      if (error_state)
	{
	  retval = tree_constant ();
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
    case tree_expression::increment:
      op = "++";
      break;

    case tree_expression::decrement:
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
tree_postfix_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (id)
    id->print_code (os);

  os << oper ();

  if (in_parens)
    os << ")";
}

// Unary expressions.

tree_constant
tree_unary_expression::eval (int /* print */)
{
  if (error_state)
    return tree_constant ();

  tree_constant retval;

  switch (etype)
    {
    case tree_expression::not:
    case tree_expression::uminus:
    case tree_expression::hermitian:
    case tree_expression::transpose:
      if (op)
	{
	  tree_constant u = op->eval (0);
	  if (error_state)
	    eval_error ();
	  else if (u.is_defined ())
	    {
	      retval = do_unary_op (u, etype);
	      if (error_state)
		{
		  retval = tree_constant ();
		  if (error_state)
		    eval_error ();
		}
	    }
	}
      break;

    default:
      ::error ("unary operator %d not implemented", etype);
      break;
    }

  return retval;
}

char *
tree_unary_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case tree_expression::not:
      op = "!";
      break;

    case tree_expression::uminus:
      op = "-";
      break;

    case tree_expression::hermitian:
      op = "'";
      break;

    case tree_expression::transpose:
      op = ".'";
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
tree_unary_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  switch (etype)
    {
    case tree_expression::not:
    case tree_expression::uminus:
      os << oper ();
      if (op)
	op->print_code (os);
      break;

    case tree_expression::hermitian:
    case tree_expression::transpose:
      if (op)
	op->print_code (os);
      os << oper ();
      break;

    default:
      os << oper ();
      if (op)
	op->print_code (os);
      break;
    }

  if (in_parens)
    os << ")";
}

// Binary expressions.
 
tree_constant
tree_binary_expression::eval (int /* print */)
{
  if (error_state)
    return tree_constant ();

  tree_constant retval;

  switch (etype)
    {
    case tree_expression::add:
    case tree_expression::subtract:
    case tree_expression::multiply:
    case tree_expression::el_mul:
    case tree_expression::divide:
    case tree_expression::el_div:
    case tree_expression::leftdiv:
    case tree_expression::el_leftdiv:
    case tree_expression::power:
    case tree_expression::elem_pow:
    case tree_expression::cmp_lt:
    case tree_expression::cmp_le:
    case tree_expression::cmp_eq:
    case tree_expression::cmp_ge:
    case tree_expression::cmp_gt:
    case tree_expression::cmp_ne:
    case tree_expression::and:
    case tree_expression::or:
      if (op1)
	{
	  tree_constant a = op1->eval (0);
	  if (error_state)
	    eval_error ();
	  else if (a.is_defined () && op2)
	    {
	      tree_constant b = op2->eval (0);
	      if (error_state)
		eval_error ();
	      else if (b.is_defined ())
		{
		  retval = do_binary_op (a, b, etype);
		  if (error_state)
		    {
		      retval = tree_constant ();
		      if (error_state)
			eval_error ();
		    }
		}
	    }
	}
      break;

    case tree_expression::and_and:
    case tree_expression::or_or:
      {
	int result = 0;
	if (op1)
	  {
	    tree_constant a = op1->eval (0);
	    if (error_state)
	      {
		eval_error ();
		break;
	      }

	    int a_true = a.is_true ();
	    if (error_state)
	      {
		eval_error ();
		break;
	      }

	    if (a_true)
	      {
		if (etype == tree_expression::or_or)
		  {
		    result = 1;
		    goto done;
		  }
	      }
	    else
	      {
		if (etype == tree_expression::and_and)
		  {
		    result = 0;
		    goto done;
		  }
	      }

	    if (op2)
	      {
		tree_constant b = op2->eval (0);
		if (error_state)
		  {
		    eval_error ();
		    break;
		  }

		result = b.is_true ();
		if (error_state)
		  {
		    eval_error ();
		    break;
		  }
	      }
	  }
      done:
	retval = tree_constant ((double) result);
      }
      break;

    default:
      ::error ("binary operator %d not implemented", etype);
      break;
    }

  return retval;
}

char *
tree_binary_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case tree_expression::add:
      op = "+";
      break;

    case tree_expression::subtract:
      op = "-";
      break;

    case tree_expression::multiply:
      op = "*";
      break;

    case tree_expression::el_mul:
      op = ".*";
      break;

    case tree_expression::divide:
      op = "/";
      break;

    case tree_expression::el_div:
      op = "./";
      break;

    case tree_expression::leftdiv:
      op = "\\";
      break;

    case tree_expression::el_leftdiv:
      op = ".\\";
      break;

    case tree_expression::power:
      op = "^";
      break;

    case tree_expression::elem_pow:
      op = ".^";
      break;

    case tree_expression::cmp_lt:
      op = "<";
      break;

    case tree_expression::cmp_le:
      op = "<=";
      break;

    case tree_expression::cmp_eq:
      op = "==";
      break;

    case tree_expression::cmp_ge:
      op = ">=";
      break;

    case tree_expression::cmp_gt:
      op = ">";
      break;

    case tree_expression::cmp_ne:
      op = "!=";
      break;

    case tree_expression::and_and:
      op = "&&";
      break;

    case tree_expression::or_or:
      op = "||";
      break;

    case tree_expression::and:
      op = "&";
      break;

    case tree_expression::or:
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
tree_binary_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (op1)
    op1->print_code (os);

  os << " " << oper () << " ";

  if (op2)
    op2->print_code (os);

  if (in_parens)
    os << ")";
}

// Simple assignment expressions.

tree_simple_assignment_expression::tree_simple_assignment_expression
  (tree_identifier *i, tree_expression *r, int plhs, int ans_assign,
   int l, int c)
    : tree_expression (l, c)
      {
	init (plhs, ans_assign);
	lhs = new tree_indirect_ref (i);
	rhs = r;
      }

tree_simple_assignment_expression::tree_simple_assignment_expression
  (tree_index_expression *idx_expr, tree_expression *r, int plhs,
   int ans_assign, int l, int c)
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

int
tree_simple_assignment_expression::left_hand_side_is_identifier_only (void)
{
  return lhs->is_identifier_only ();
}

tree_identifier *
tree_simple_assignment_expression::left_hand_side_id (void)
{
  return lhs->ident ();
}

tree_constant
tree_simple_assignment_expression::eval (int print)
{
  assert (etype == tree_expression::assignment);

  tree_constant retval;

  if (error_state)
    return retval;

  if (rhs)
    {
      tree_constant rhs_val = rhs->eval (0);
      if (error_state)
	{
	  eval_error ();
	}
      else if (rhs_val.is_undefined ())
	{
	  error ("value on right hand side of assignment is undefined");
	  eval_error ();
	}
      else if (! index)
	{
	  retval = lhs->assign (rhs_val);
	  if (error_state)
	    eval_error ();
	}
      else
	{
	  // Extract the arguments into a simple vector.

	  Octave_object args = index->convert_to_const_vector ();

	  if (error_state)
	    eval_error ();
	  else
	    {
	      int nargin = args.length ();

	      if (error_state)
		eval_error ();
	      else if (nargin > 0)
		{
		  retval = lhs->assign (rhs_val, args);
		  if (error_state)
		    eval_error ();
		}
	    }
	}
    }

  if (! error_state && print && retval.is_defined ())
    print_constant (retval, lhs->name ());

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
tree_simple_assignment_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (! is_ans_assign ())
    {
      if (lhs)
	lhs->print_code (os);

      if (index)
	{
	  os << " (";
	  index->print_code (os);
	  os << ")";
	}

      os << " = ";
    }

  if (rhs)
    rhs->print_code (os);

  if (in_parens)
    os << ")";
}

// Colon expressions.

int
tree_colon_expression::is_range_constant (void) const
{
  int tmp = (op1 && op1->is_constant ()
	     && op2 && op2->is_constant ());

  return op3 ? (tmp && op3->is_constant ()) : tmp;
}

tree_colon_expression *
tree_colon_expression::chain (tree_expression *t)
{
  tree_colon_expression *retval = 0;
  if (! op1 || op3)
    ::error ("invalid colon expression");
  else
    {
      op3 = op2;	// Stupid syntax.
      op2 = t;

      retval = this;
    }
  return retval;
}

tree_constant
tree_colon_expression::eval (int /* print */)
{
  tree_constant retval;

  if (error_state || ! op1 || ! op2)
    return retval;

  tree_constant tmp = op1->eval (0);

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

  tmp = op2->eval (0);

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
  if (op3)
    {
      tmp = op3->eval (0);

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

  retval = tree_constant (base, limit, inc);

  if (error_state)
    {
      if (error_state)
	eval_error ("evaluating colon expression");
      return tree_constant ();
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
tree_colon_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (op1)
    op1->print_code (os);

  // Stupid syntax.

  if (op3)
    {
      os << ":";
      op3->print_code (os);
    }

  if (op2)
    {
      os << ":";
      op2->print_code (os);
    }

  if (in_parens)
    os << ")";
}


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
