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
#include "pager.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-colon.h"
#include "pt-walk.h"

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

  MAYBE_DO_BREAKPOINT;
  
  if (error_state || ! op_base || ! op_limit)
    return retval;

  octave_value tmp = op_base->rvalue ();

  if (error_state || tmp.is_undefined ())
    {
      eval_error ("invalid value in colon expression");
      return retval;
    }

  double xbase = tmp.double_value ();

  if (error_state)
    {
      eval_error ("colon expression elements must be scalars");
      return retval;
    }

  tmp = op_limit->rvalue ();

  if (error_state || tmp.is_undefined ())
    {
      eval_error ("invalid value in colon expression");
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

      if (error_state || tmp.is_undefined ())
	{
	  eval_error ("invalid value in colon expression");
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
      retval = octave_value ();
      eval_error ();
    }

  return retval;
}

void
tree_colon_expression::eval_error (const std::string& s)
{
  if (! s.empty ())
    ::error ("%s", s.c_str ());

  ::error ("evaluating colon expression near line %d column %d",
	   line (), column ());
}

int
tree_colon_expression::line (void) const
{
  return (op_base ? op_base->line ()
	  : (op_increment ? op_increment->line ()
	     : (op_limit ? op_limit->line ()
		: -1)));
}

int
tree_colon_expression::column (void) const
{
  return (op_base ? op_base->column ()
	  : (op_increment ? op_increment->column ()
	     : (op_limit ? op_limit->column ()
		: -1)));
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
