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
#include "pt-arg-list.h"
#include "pt-idx.h"
#include "pt-walk.h"

// Index expressions.

tree_index_expression::tree_index_expression (tree_expression *e = 0,
					      tree_argument_list *lst = 0,
					      int l = -1, int c = -1)
  : tree_expression (l, c), expr (e), list (lst),
    arg_nm (lst ? lst->get_arg_names () : string_vector ()) { }

tree_index_expression::~tree_index_expression (void)
{
  delete expr;
  delete list;
}

// This is useful for printing the name of the variable in an indexed
// assignment.

std::string
tree_index_expression::name (void) const
{
  return expr->name ();
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
	    retval = tmp.do_multi_index_op (nargout, args);
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

octave_lvalue
tree_index_expression::lvalue (void)
{
  octave_lvalue retval;

  if (! error_state)
    {
      retval = expr->lvalue ();

      if (! error_state)
	{
	  octave_value_list args;
	  
	  if (list)
	    args = list->convert_to_const_vector ();

	  retval.set_index (args);
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
