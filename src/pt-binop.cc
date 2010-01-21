/*

Copyright (C) 1996, 1997, 2000, 2001, 2002, 2004, 2005, 2006, 2007,
              2008, 2009 John W. Eaton

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
    retval = rvalue1 (nargout);

  return retval;
}

octave_value
tree_binary_expression::rvalue1 (int)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op_lhs)
    {
      octave_value a = op_lhs->rvalue1 ();

      if (! error_state && a.is_defined () && op_rhs)
	{
	  octave_value b = op_rhs->rvalue1 ();

	  if (! error_state && b.is_defined ())
	    {
	      retval = ::do_binary_op (etype, a, b);

	      if (error_state)
		retval = octave_value ();
	    }
	}
    }

  return retval;
}

std::string
tree_binary_expression::oper (void) const
{
  return octave_value::binary_op_as_string (etype);
}

tree_expression *
tree_binary_expression::dup (symbol_table::scope_id scope,
			     symbol_table::context_id context) const
{
  tree_binary_expression *new_be
    = new tree_binary_expression (op_lhs ? op_lhs->dup (scope, context) : 0,
				  op_rhs ? op_rhs->dup (scope, context) : 0,
				  line (), column (), etype);

  new_be->copy_base (*this);

  return new_be;
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
    retval = rvalue1 (nargout);

  return retval;
}

octave_value
tree_boolean_expression::rvalue1 (int)
{
  octave_value retval;

  if (error_state)
    return retval;

  bool result = false;

  if (op_lhs)
    {
      octave_value a = op_lhs->rvalue1 ();

      if (! error_state)
	{
	  bool a_true = a.is_true ();

	  if (! error_state)
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
		  octave_value b = op_rhs->rvalue1 ();

		  if (! error_state)
		    result = b.is_true ();
		}

	    done:

	      if (! error_state)
		retval = octave_value (result);
	    }
	}
    }

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

tree_expression *
tree_boolean_expression::dup (symbol_table::scope_id scope,
			      symbol_table::context_id context) const
{
  tree_boolean_expression *new_be
    = new tree_boolean_expression (op_lhs ? op_lhs->dup (scope, context) : 0,
				   op_rhs ? op_rhs->dup (scope, context) : 0,
				   line (), column (), etype);

  new_be->copy_base (*this);

  return new_be;
}
