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

octave_value_list
tree_prefix_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("prefix operator `%s': invalid number of output arguments",
	   oper () . c_str ());
  else
    retval = rvalue ();

  return retval;
}

octave_value
tree_prefix_expression::rvalue (void)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op)
    {
      if (etype == unot || etype == uminus)
	{
	  octave_value val = op->rvalue ();

	  if (! error_state)
	    {
	      if (val.is_defined ())
		{
		  if (etype == unot)
		    retval = val.not ();
		  else
		    retval = val.uminus ();
		}
	      else
		error ("argument to prefix operator `%s' undefined",
		       oper () . c_str ());
	    }
	}
      else if (etype == increment || etype == decrement)
	{
	  octave_variable_reference ref = op->lvalue ();

	  if (! error_state)
	    {
	      if (ref.is_defined ())
		{
		  if (etype == increment)
		    ref.increment ();
		  else
		    ref.decrement ();

		  retval = ref.value ();
		}
	      else
		error ("argument to prefix operator `%s' undefined",
		       oper () . c_str ());
	    }
	}
      else
	error ("prefix operator %d not implemented", etype);
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
tree_prefix_expression::accept (tree_walker& tw)
{
  tw.visit_prefix_expression (*this);
}

// Postfix expressions.

octave_value_list
tree_postfix_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("postfix operator `%s': invalid number of output arguments",
	   oper () . c_str ());
  else
    retval = rvalue ();

  return retval;
}

octave_value
tree_postfix_expression::rvalue (void)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op)
    {
      if (etype == transpose || etype == hermitian)
	{
	  octave_value val = op->rvalue ();

	  if (! error_state)
	    {
	      if (val.is_defined ())
		{
		  if (etype == transpose)
		    retval = val.transpose ();
		  else
		    retval = val.hermitian ();
		}
	      else
		error ("argument to postfix operator `%s' undefined",
		       oper () . c_str ());
	    }
	}
      else if (etype == increment || etype == decrement)
	{
	  octave_variable_reference ref = op->lvalue ();

	  if (! error_state)
	    {
	      if (ref.is_defined ())
		{
		  retval = ref.value ();

		  if (etype == increment)
		    ref.increment ();
		  else
		    ref.decrement ();
		}
	      else
		error ("argument to postfix operator `%s' undefined",
		       oper () . c_str ());
	    }
	}
      else
	error ("postfix operator %d not implemented", etype);
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
tree_postfix_expression::accept (tree_walker& tw)
{
  tw.visit_postfix_expression (*this);
}

// Binary expressions.
 
octave_value_list
tree_binary_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("binary operator `%s': invalid number of output arguments",
	   oper () . c_str ());
  else
    retval = rvalue ();

  return retval;
}

octave_value
tree_binary_expression::rvalue (void)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op_lhs)
    {
      octave_value a = op_lhs->rvalue ();

      if (error_state)
	eval_error ();
      else if (a.is_defined () && op_rhs)
	{
	  octave_value b = op_rhs->rvalue ();

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

void
tree_binary_expression::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating binary operator `%s' near line %d, column %d",
	     oper () . c_str (), line (), column ());
}

string
tree_binary_expression::oper (void) const
{
  return octave_value::binary_op_as_string (etype);
}

void
tree_binary_expression::accept (tree_walker& tw)
{
  tw.visit_binary_expression (*this);
}

// Boolean expressions.
 
octave_value_list
tree_boolean_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("binary operator `%s': invalid number of output arguments",
	   oper () . c_str ());
  else
    retval = rvalue ();

  return retval;
}

octave_value
tree_boolean_expression::rvalue (void)
{
  octave_value retval;

  if (error_state)
    return retval;

  bool result = false;

  if (op_lhs)
    {
      octave_value a = op_lhs->rvalue ();

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
		  octave_value b = op_rhs->rvalue ();

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

tree_simple_assignment::~tree_simple_assignment (void)
{
  if (! preserve)
    delete lhs;

  delete rhs;
}

octave_value_list
tree_simple_assignment::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for expression X = RHS");
  else
    retval = rvalue ();

  return retval;
}

octave_value
tree_simple_assignment::rvalue (void)
{
  octave_value rhs_val;

  if (error_state)
    return rhs_val;

  if (rhs)
    {
      octave_value_list tmp = rhs->rvalue ();

      if (! (error_state || tmp.empty ()))
	{
	  rhs_val = tmp(0);

	  if (rhs_val.is_undefined ())
	    {
	      error ("value on right hand side of assignment is undefined");
	      eval_error ();
	    }
	  else
	    {
	      octave_variable_reference ult = lhs->lvalue ();

	      if (error_state)
		eval_error ();
	      else
		{
		  ult.assign (etype, rhs_val);

		  if (error_state)
		    eval_error ();
		  else if (! Vprint_rhs_assign_val)
		    {
		      octave_value lhs_val = ult.value ();

		      if (! error_state && print_result ())
			{
			  if (Vprint_rhs_assign_val)
			    {
			      ostrstream buf;

			      tree_print_code tpc (buf);

			      lhs->accept (tpc);

			      buf << ends;

			      const char *tag = buf.str ();

			      rhs_val.print_with_name (octave_stdout, tag);

			      delete [] tag;
			    }
			  else
			    lhs_val.print_with_name (octave_stdout,
						     lhs->name ());
			}
		    }
		}
	    }
	}
      else
	eval_error ();
    }

  return rhs_val;
}

void
tree_simple_assignment::eval_error (void)
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
tree_simple_assignment::oper (void) const
{
  return octave_value::assign_op_as_string (etype);
}

void
tree_simple_assignment::accept (tree_walker& tw)
{
  tw.visit_simple_assignment (*this);
}

// Colon expressions.

tree_colon_expression *
tree_colon_expression::append (tree_expression *t)
{
  tree_colon_expression *retval = 0;

  if (op_base)
    {
      if (op_limit)
	{
	  if (op_increment)
	    ::error ("invalid colon expression");
	  else
	    {
	      // Stupid syntax:
	      //
	      // base : limit
	      // base : increment : limit

	      op_increment = op_limit;
	      op_limit = t;
	    }
	}
      else
	op_limit = t;

      retval = this;
    }
  else
    ::error ("invalid colon expression");

  return retval;
}

octave_value_list
tree_colon_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for colon expression");
  else
    retval = rvalue ();

  return retval;
}

octave_value
tree_colon_expression::rvalue (void)
{
  octave_value retval;

  if (error_state || ! op_base || ! op_limit)
    return retval;

  octave_value tmp = op_base->rvalue ();

  if (tmp.is_undefined ())
    {
      eval_error ("invalid null value in colon expression");
      return retval;
    }

  double xbase = tmp.double_value ();

  if (error_state)
    {
      eval_error ("colon expression elements must be scalars");
      return retval;
    }

  tmp = op_limit->rvalue ();

  if (tmp.is_undefined ())
    {
      eval_error ("invalid null value in colon expression");
      return retval;
    }

  double xlimit = tmp.double_value ();

  if (error_state)
    {
      eval_error ("colon expression elements must be scalars");
      return retval;
    }

  double xinc = 1.0;

  if (op_increment)
    {
      tmp = op_increment->rvalue ();

      if (tmp.is_undefined ())
	{
	  eval_error ("invalid null value in colon expression");
	  return retval;
	}

      xinc = tmp.double_value ();

      if (error_state)
	{
	  eval_error ("colon expression elements must be scalars");
	  return retval;
	}
    }

  retval = octave_value (xbase, xlimit, xinc);

  if (error_state)
    {
      if (error_state)
	eval_error ();

      return octave_value ();
    }

  return retval;
}

void
tree_colon_expression::eval_error (const string& s)
{
  if (error_state > 0)
    {
      if (! s.empty ())
	::error ("%s", s.c_str ());

      ::error ("evaluating colon expression near line %d column %d",
	       line (), column ());
    }
}

void
tree_colon_expression::accept (tree_walker& tw)
{
  tw.visit_colon_expression (*this);
}

tree_index_expression::~tree_index_expression (void)
{
  delete expr;
  delete list;
}

octave_value_list
tree_index_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  octave_value tmp = expr->rvalue ();

  if (! error_state)
    {
      octave_value_list args;

      if (list)
	args = list->convert_to_const_vector ();

      if (! error_state)
	{
	  if (! args.empty ())
	    args.stash_name_tags (arg_nm);

	  // XXX FIXME XXX -- is this the right thing to do?
	  if (tmp.is_constant ())
	    retval = tmp.do_index_op (args);
	  else
	    retval = tmp.do_index_op (nargout, args);
	}
      else
	eval_error ();
    }
  else
    eval_error ();

  return retval;
}

octave_value
tree_index_expression::rvalue (void)
{
  octave_value retval;

  octave_value_list tmp = rvalue (1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

octave_variable_reference
tree_index_expression::lvalue (void)
{
  octave_variable_reference retval;

  if (! error_state)
    {
      retval = expr->lvalue ();

      if (! error_state)
	{
	  octave_value_list args;
	  
	  if (list)
	    args = list->convert_to_const_vector ();

	  retval.index (args);
	}
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

      if (l != -1 && c != -1)
	{
	  if (list)
	    ::error ("evaluating index expression near line %d, column %d",
		     l, c);
	  else
	    ::error ("evaluating expression near line %d, column %d", l, c);
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

tree_multi_assignment::~tree_multi_assignment (void)
{
  if (! preserve)
    delete lhs;

  delete rhs;
}

octave_value
tree_multi_assignment::rvalue (void)
{
  octave_value retval;

  octave_value_list tmp = rvalue (1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

octave_value_list
tree_multi_assignment::rvalue (int nargout)
{
  octave_value_list rhs_val;

  if (error_state)
    return rhs_val;

  if (rhs)
    {
      int n_out = lhs->length ();

      rhs_val = rhs->rvalue (n_out);

      if (! (error_state || rhs_val.empty ()))
	{
	  if (rhs_val.empty ())
	    {
	      error ("value on right hand side of assignment is undefined");
	      eval_error ();
	    }
	  else
	    {
	      int k = 0;

	      int n = rhs_val.length ();

	      for (Pix p = lhs->first (); p != 0; lhs->next (p))
		{
		  tree_expression *lhs_elt = lhs->operator () (p);

		  if (lhs_elt)
		    {
		      octave_variable_reference ult = lhs_elt->lvalue ();

		      if (error_state)
			eval_error ();
		      else
			{
			  octave_value tmp = k < n
			    ? rhs_val(k++) : octave_value ();

			  if (tmp.is_defined ())
			    {
			      // XXX FIXME XXX -- handle other assignment ops.
			      ult.assign (octave_value::asn_eq, tmp);
			    }
			  else
			    error ("element number %d undefined in return list", k+1);

			  if (error_state)
			    eval_error ();
			  else if (! Vprint_rhs_assign_val)
			    {
			      octave_value lhs_val = ult.value ();

			      if (! error_state && print_result ())
				{
				  if (Vprint_rhs_assign_val)
				    {
				      ostrstream buf;

				      tree_print_code tpc (buf);

				      lhs_elt->accept (tpc);

				      buf << ends;

				      const char *tag = buf.str ();

				      tmp.print_with_name
					(octave_stdout, tag);

				      delete [] tag;
				    }
				  else
				    lhs_val.print_with_name (octave_stdout,
							     lhs_elt->name ());
				}
			    }
			}
		    }

		  if (error_state)
		    break;
		}
	    }
	}
      else
	eval_error ();
    }

  return rhs_val;
}

void
tree_multi_assignment::eval_error (void)
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
tree_multi_assignment::accept (tree_walker& tw)
{
  tw.visit_multi_assignment (*this);
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
