// pt-mvr.cc                                          -*- C++ -*-
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

#include "error.h"
#include "oct-obj.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-exp.h"
#include "pt-fvc.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "user-prefs.h"

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

// Make sure that all arguments have values.

static int
all_args_defined (const Octave_object& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (args(i).is_undefined ())
      return 0;

  return 1;
}

// Used internally.

tree_constant
tree_oct_obj::eval (int /* print */)
{
  return values(0);
}

Octave_object
tree_oct_obj::eval (int /* print */, int /* nargout */,
		    const Octave_object& /* args */)
{
  return values;
}

// Index expressions.

tree_index_expression::tree_index_expression
  (tree_identifier *i, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	id = new tree_indirect_ref (i);
	list = 0;
      }

tree_index_expression::tree_index_expression
  (tree_identifier *i, tree_argument_list *lst, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	id = new tree_indirect_ref (i);
	list = lst;
      }

tree_index_expression::~tree_index_expression (void)
{
  delete id;
  delete list;
}

char *
tree_index_expression::name (void)
{
  return id->name ();
}

void
tree_index_expression::mark_for_possible_ans_assign (void)
{
  if (id)
    id->mark_for_possible_ans_assign ();
}

tree_constant
tree_index_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (list)
    {
      // Extract the arguments into a simple vector.  Don't pass null
      // args.

      Octave_object args = list->convert_to_const_vector ();

      if (error_state)
	eval_error ();
      else
	{
	  if (error_state)
	    eval_error ();
	  else
	    {
	      if (all_args_defined (args))
		{
		  Octave_object tmp = id->eval (print, 1, args);

		  if (error_state)
		    eval_error ();
		  else if (tmp.length () > 0)
		    retval = tmp(0);
		}
	      else
		{
		  ::error ("undefined arguments found in index expression");
		  eval_error ();
		}
	    }
	}
    }
  else
    {
      retval = id->eval (print);

      if (error_state)
	eval_error ();
    }

  return retval;
}

Octave_object
tree_index_expression::eval (int print, int nargout,
			     const Octave_object& /* args */)
{
  Octave_object retval;

  if (error_state)
    return retval;

  if (list)
    {
      // Extract the arguments into a simple vector.  Don't pass null
      // args.

      Octave_object tmp_args = list->convert_to_const_vector ();

      if (error_state)
	eval_error ();
      else
	{
	  if (error_state)
	    eval_error ();
	  else
	    {
	      if (all_args_defined (tmp_args))
		{
		  retval = id->eval (print, nargout, tmp_args);

		  if (error_state)
		    eval_error ();
		}
	      else
		{
		  ::error ("undefined arguments found in index expression");
		  eval_error ();
		}
	    }
	}
    }
  else
    {
      Octave_object tmp_args;

      retval = id->eval (print, nargout, tmp_args);

      if (error_state)
	eval_error ();
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
      char *fmt;
      if (l != -1 && c != -1)
	{
	  if (list)
	    fmt = "evaluating index expression near line %d, column %d";
	  else
	    fmt = "evaluating expression near line %d, column %d";

	  ::error (fmt, l, c);
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
tree_index_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (id)
    id->print_code (os);

  if (list)
    {
      os << " (";
      list->print_code (os);
      os << ")";
    }

  if (in_parens)
    os << ")";
}

// Multi-valued assignmnt expressions.

tree_multi_assignment_expression::~tree_multi_assignment_expression (void)
{
  if (! preserve)
    delete lhs;

  delete rhs;
}

tree_constant
tree_multi_assignment_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  Octave_object tmp_args;
  Octave_object result = eval (print, 1, tmp_args);

  if (result.length () > 0)
    retval = result(0);

  return retval;
}

Octave_object
tree_multi_assignment_expression::eval (int print, int nargout,
					const Octave_object& /* args */)
{
  assert (etype == tree_expression::multi_assignment);

  if (error_state || ! rhs)
    return Octave_object ();

  nargout = lhs->length ();
  Octave_object tmp_args;
  Octave_object results = rhs->eval (0, nargout, tmp_args);

  if (error_state)
    eval_error ();

  int ma_line = line ();
  int ma_column = column ();

  if (results.length () > 0)
    {
      int i = 0;
      int pad_after = 0;
      int last_was_scalar_type = 0;
      for (Pix p = lhs->first (); p != 0; lhs->next (p))
	{
	  tree_index_expression *lhs_expr = lhs->operator () (p);

	  if (i < nargout)
	    {
	      // XXX FIXME? XXX -- this is apparently the way Matlab
	      // works, but maybe we should have the option of
	      // skipping the assignment instead.

	      tree_constant *tmp = 0;
	      if (results(i).is_undefined ())
		{
		  error ("element number %d undefined in return list", i+1);
		  eval_error ();
		  break;
		}
	      else
		tmp = new tree_constant (results(i));

	      tree_simple_assignment_expression tmp_expr
		(lhs_expr, tmp, 1, 0, ma_line, ma_column);

	      results(i) = tmp_expr.eval (0); // May change

	      if (error_state)
		break;

	      if (print && pad_after)
		{
		  ostrstream output_buf;
		  output_buf << "\n" << ends;
		  maybe_page_output (output_buf);
		}

	      if (print)
		print_constant (results(i), lhs_expr->name (), 0);

	      pad_after++;
	      i++;
	    }
	  else
	    {
	      tree_simple_assignment_expression tmp_expr
		(lhs_expr, 0, 1, 0, ma_line, ma_column);

	      tmp_expr.eval (0);

	      if (error_state)
		break;

	      if (last_was_scalar_type && i == 1)
		pad_after = 0;

	      break;
	    }
	}

      if (print && pad_after)
	{
	  ostrstream output_buf;
	  output_buf << "\n" << ends;
	  maybe_page_output (output_buf);
	}
    }

  return results;
}

void
tree_multi_assignment_expression::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating assignment expression near line %d, column %d",
	     line (), column ());
}

void
tree_multi_assignment_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (lhs)
    {
      int len = lhs->length ();

      if (len > 1)
	os << "[";

      lhs->print_code (os);

      if (len > 1)
	os << "]";
    }

  os << " = ";

  if (rhs)
    rhs->print_code (os);

  if (in_parens)
    os << ")";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
