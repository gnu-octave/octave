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
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pager.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-idx.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"

// Index expressions.

tree_index_expression::tree_index_expression (tree_expression *e,
					      tree_argument_list *lst,
					      int l, int c, type t)
  : tree_expression (l, c), expr (e), list (lst),
    itype (t), arg_nm (lst ? lst->get_arg_names () : string_vector ()) { }

tree_index_expression::tree_index_expression (tree_expression *e,
					      const std::string& n,
					      int l = -1, int c = -1)
  : tree_expression (l, c), expr (e), list (0), itype (dot),
    arg_nm (n) { }

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
  // ??? FIXME ???
  std::string xname = expr->name ();

  return (! dot || xname == "<unknown>") ? xname : xname + "." + arg_nm(0);
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
      if (itype == dot)
	{
	  MAYBE_DO_BREAKPOINT;

	  if (nargout > 1)
	    error ("invalid number of output arguments for structure reference");
	  else
	    {
	      octave_value_list tmp = expr->rvalue (nargout);

	      if (tmp.empty ())
		eval_error ();
	      else
		{
		  octave_value val = tmp(0).do_struct_elt_index_op (arg_nm(0));

		  if (print_result () && nargout == 0 && val.is_defined ())
		    {
		      // ??? FIXME ???

		      std::string xname = name ();

		      if (xname == "<unknown>")
			bind_ans (val, true);
		      else
			val.print_with_name (octave_stdout, xname);
		    }

		  retval = val;
		}
	    }
	}
      else if (itype == paren || itype == brace)
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
#if 0
      else
	panic_impossible ();
#endif
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
      if (itype == dot)
	{
	  octave_lvalue tmp = expr->lvalue ();

	  if (tmp.is_undefined () || ! tmp.is_map ())
	    tmp.define (Octave_map ());

	  retval = tmp.struct_elt_ref (arg_nm(0));
	}
      else if (itype == paren || itype == brace)
	{
	  retval = expr->lvalue ();

	  if (! error_state)
	    {
	      octave_value_list args;
	  
	      if (list)
		args = list->convert_to_const_vector ();

	      retval.set_index (args);
	    }
#if 0
	  else
	    panic_impossible ();
#endif
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

      const char *type_str;

      if (itype == dot)
	type_str = "structure reference operator";
      else if (list)
	type_str = "index expression";
      else
	type_str = "expression";

      if (l != -1 && c != -1)
	::error ("evaluating %s near line %d, column %d", type_str, l, c);
      else
	::error ("evaluating %s", type_str);
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
