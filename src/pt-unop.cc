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

#include "error.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-unop.h"
#include "pt-walk.h"

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
	  octave_lvalue ref = op->lvalue ();

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
	  octave_lvalue ref = op->lvalue ();

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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
