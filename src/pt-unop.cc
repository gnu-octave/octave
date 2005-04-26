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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-unop.h"
#include "pt-walk.h"

// Unary expressions.

std::string
tree_unary_expression::oper (void) const
{
  return octave_value::unary_op_as_string (etype);
}

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

  MAYBE_DO_BREAKPOINT;

  if (error_state)
    return retval;

  if (op)
    {
      if (etype == octave_value::op_incr || etype == octave_value::op_decr)
	{
	  op->rvalue ();

	  if (! error_state)
	    {
	      octave_lvalue ref = op->lvalue ();

	      if (! error_state && ref.is_defined ())
		{
		  ref.do_unary_op (etype);

		  retval = ref.value ();
		}
	      else
		eval_error ();
	    }
	  else
	    eval_error ();
	}
      else
	{
	  octave_value val = op->rvalue ();

	  if (! error_state && val.is_defined ())
	    {
	      retval = ::do_unary_op (etype, val);

	      if (error_state)
		{
		  retval = octave_value ();
		  eval_error ();
		}
	    }
	  else
	    eval_error ();
	}
    }
  else
    eval_error ();

  return retval;
}

void
tree_prefix_expression::eval_error (void)
{
  ::error ("evaluating prefix operator `%s' near line %d, column %d",
	   oper () . c_str (), line (), column ());
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

  MAYBE_DO_BREAKPOINT;

  if (error_state)
    return retval;

  if (op)
    {
      if (etype == octave_value::op_incr || etype == octave_value::op_decr)
	{
	  op->rvalue ();

	  if (! error_state)
	    {
	      octave_lvalue ref = op->lvalue ();

	      if (! error_state && ref.is_defined ())
		{
		  retval = ref.value ();

		  ref.do_unary_op (etype);
		}
	      else
		eval_error ();
	    }
	  else
	    eval_error ();
	}
      else
	{
	  octave_value val = op->rvalue ();

	  if (! error_state && val.is_defined ())
	    {
	      retval = ::do_unary_op (etype, val);

	      if (error_state)
		{
		  retval = octave_value ();
		  eval_error ();
		}
	    }
	  else
	    eval_error ();
	}
    }
  else
    eval_error ();

  return retval;
}

void
tree_postfix_expression::eval_error (void)
{
  ::error ("evaluating postfix operator `%s' near line %d, column %d",
	   oper () . c_str (), line (), column ());
}

void
tree_postfix_expression::accept (tree_walker& tw)
{
  tw.visit_postfix_expression (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
