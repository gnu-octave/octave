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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "oct-obj.h"
#include "ov.h"
#include "pt-binop.h"
#include "pt-bp.h"
#include "pt-walk.h"

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

  MAYBE_DO_BREAKPOINT;

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
  ::error ("evaluating binary operator `%s' near line %d, column %d",
	   oper () . c_str (), line (), column ());
}

std::string
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

  MAYBE_DO_BREAKPOINT;

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
		retval = octave_value (result);
	    }
	}
    }
  else
    eval_error ();

  return retval;
}

std::string
tree_boolean_expression::oper (void) const
{
  std::string retval = "<unknown>";

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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
