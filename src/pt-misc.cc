// tree-misc.cc                                          -*- C++ -*-
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

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <iostream.h>

#include "error.h"
#include "tree-base.h"
#include "tree-expr.h"
#include "tree-cmd.h"
#include "octave.h"
#include "tree-misc.h"
#include "tree-const.h"
#include "user-prefs.h"
#include "oct-obj.h"

// Nonzero means we're breaking out of a loop or function body.
extern int breaking;

// Nonzero means we're jumping to the end of a loop.
extern int continuing;

// Nonzero means we're returning from a function.
extern int returning;

// A list of commands to be executed.

tree_statement::~tree_statement (void)
{
  delete command;
  delete expression;
}

void
tree_statement::print_code (ostream& os)
{
  if (command)
    {
      command->print_code (os);

      if (! print_flag)
	os << ";";

      command->print_code_new_line (os);
    }
  else if (expression)
    {
      expression->print_code (os);

      if (! print_flag)
	os << ";";

      expression->print_code_new_line (os);
    }


}

tree_constant
tree_statement_list::eval (int print)
{
  int pf;
  tree_constant retval;

  if (error_state)
    return retval;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_statement *elt = this->operator () (p);

      if (print == 0)
	pf = 0;
      else
	pf = elt->print_flag;

      tree_command *cmd = elt->command;
      tree_expression *expr = elt->expression;

      if (cmd || expr)
	{
	  if (cmd)
	    cmd->eval ();
	  else
	    retval = expr->eval (pf);

	  if (error_state)
	    return tree_constant ();

	  if (breaking || continuing)
	    break;

	  if (returning)
	    break;
	}
      else
	retval = tree_constant ();
    }
  return retval;
}

Octave_object
tree_statement_list::eval (int print, int nargout)
{
  Octave_object retval;

  if (nargout > 1)
    {
      int pf;

      if (error_state)
	return retval;

      for (Pix p = first (); p != 0; next (p))
	{
	  tree_statement *elt = this->operator () (p);

	  if (print == 0)
	    pf = 0;
	  else
	    pf = elt->print_flag;

	  tree_command *cmd = elt->command;
	  tree_expression *expr = elt->expression;

	  if (cmd || expr)
	    {
	      if (cmd)
		cmd->eval ();
	      else
		{
		  if (expr->is_multi_val_ret_expression ())
		    {
		      Octave_object args;
		      tree_multi_val_ret *t = (tree_multi_val_ret *) expr;
		      retval = t->eval (pf, nargout, args);
		    }
		  else
		    retval = expr->eval (pf);
		}

	      if (error_state)
		return tree_constant ();

	      if (breaking || continuing)
		break;

	      if (returning)
		break;
	    }
	  else
	    retval = Octave_object ();
	}
      return retval;
    }
  else
    retval = eval (print);

  return retval;
}

void
tree_statement_list::print_code (ostream& os)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_statement *elt = this->operator () (p);

      if (elt)
	elt->print_code (os);
    }
}

Octave_object
tree_argument_list::convert_to_const_vector (void)
{
  int len = length ();

// XXX FIXME XXX -- would be nice to know in advance how largs args
// needs to be even when we have a list containing an all_va_args
// token.

  Octave_object args;
  args.resize (len);

  Pix p = first ();
  int j = 0;
  for (int k = 0; k < len; k++)
    {
      tree_expression *elt = this->operator () (p);
      if (elt)
	{
	  tree_constant tmp = elt->eval (0);
	  if (error_state)
	    {
	      ::error ("evaluating argument list element number %d", k);
	      args = Octave_object ();
	      break;
	    }
	  else
	    {
	      if (tmp.is_all_va_args ())
		{
		  if (curr_function)
		    {
		      Octave_object tva;
		      tva = curr_function->octave_all_va_args ();
		      int n = tva.length ();
		      for (int i = 0; i < n; i++)
			args(j++) = tva(i);
		    }
		  else
		    {
		      ::error ("all_va_args is only valid inside functions");
		      args = Octave_object ();
		      break;
		    }
		}
	      else
		args(j++) = tmp;
	    }
	  next (p);
	}
      else
	{
	  args(j++) = tree_constant ();
	  break;
	}
    }

  args.resize (j);

  return args;
}

void
tree_argument_list::print_code (ostream& os)
{
  Pix p = first ();

  while (p)
    {
      tree_expression *elt = this->operator () (p);

      next (p);

      if (elt)
	{
	  elt->print_code (os);

	  if (p)
	    os << ", ";
	}
    }
}

// Parameter lists.

void
tree_parameter_list::mark_as_formal_parameters (void)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);
      elt->mark_as_formal_parameter ();
    }
}

void
tree_parameter_list::initialize_undefined_elements (tree_constant& val)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);
      if (! elt->is_defined ())
	elt->assign (val);
    }
}

void
tree_parameter_list::define_from_arg_vector (const Octave_object& args)
{
  int nargin = args.length ();

  if (nargin <= 0)
    return;

  int expected_nargin = length ();

  Pix p = first ();

  for (int i = 0; i < expected_nargin; i++)
    {
      tree_identifier *elt = this->operator () (p);

      tree_constant *tmp = 0;

      if (i < nargin)
	{
	  if (args(i).is_defined () && args(i).is_magic_colon ())
	    {
	      ::error ("invalid use of colon in function argument list");
	      return;
	    }
	  tmp = new tree_constant (args(i));
	}

      elt->define (tmp);

      next (p);
    }
}

Octave_object
tree_parameter_list::convert_to_const_vector (tree_va_return_list *vr_list)
{
  int nout = length ();

  if (vr_list)
    nout += vr_list->length ();

  Octave_object retval;
  retval.resize (nout);

  int i = 0;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);

      if (elt->is_defined ())
	retval(i) = elt->eval (0);

      i++;
    }

  if (vr_list)
    {
      for (Pix p = vr_list->first (); p != 0; vr_list->next (p))
	{
	  retval(i) = vr_list->operator () (p);
	  i++;
	}
    }

  return retval;
}

int
tree_parameter_list::is_defined (void)
{
  int status = 1;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);

      if (! elt->is_defined ())
	{
	  status = 0;
	  break;
	}
    }

  return status;
}

void
tree_parameter_list::print_code (ostream& os)
{
  Pix p = first ();

  while (p)
    {
      tree_identifier *elt = this->operator () (p);

      next (p);

      if (elt)
	{
	  elt->print_code (os);

	  if (p)
	    os << ", ";
	}
    }
}

// Return lists.

void
tree_return_list::print_code (ostream& os)
{
  Pix p = first ();

  while (p)
    {
      tree_index_expression *elt = this->operator () (p);

      next (p);

      if (elt)
	{
	  elt->print_code (os);

	  if (p)
	    os << ", ";
	}
    }
}

// Global.

void
tree_global::eval (void)
{
  if (ident)
    {
      ident->link_to_global ();
    }
  else if (assign_expr)
    {
      tree_identifier *id = 0;
      if (assign_expr->left_hand_side_is_identifier_only ()
	  && (id = assign_expr->left_hand_side_id ()))
	{
	  id->link_to_global ();
	  assign_expr->eval (0);
	}
      else
	error ("global: unable to make individual structure elements global");
    }
}

void
tree_global::print_code (ostream& os)
{
  if (ident)
    ident->print_code (os);

  if (assign_expr)
    assign_expr->print_code (os);
}

// Global initializer lists.

void
tree_global_init_list::eval (void)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_global *t = this->operator () (p);
      t->eval ();
    }
}

void
tree_global_init_list::print_code (ostream& os)
{
  Pix p = first ();

  while (p)
    {
      tree_global *elt = this->operator () (p);

      next (p);

      if (elt)
	{
	  elt->print_code (os);

	  if (p)
	    os << ", ";
	}
    }
}

// If.

int
tree_if_clause::is_else_clause (void)
{
  return (! expr);
}

int
tree_if_clause::eval (void)
{
  if (expr)
    {
      tree_constant t1 = expr->eval (0);

      if (error_state || t1.is_undefined ())
	return 0;

      if (t1.rows () == 0 || t1.columns () == 0)
	{
	  int flag = user_pref.propagate_empty_matrices;
	  if (flag < 0)
	    warning ("if: empty matrix used in conditional");
	  else if (flag == 0)
	    {
	      ::error ("if: empty matrix used in conditional");
	      return 0;
	    }
	  t1 = tree_constant (0.0);
	}
      else if (! t1.is_scalar_type ())
	{
	  tree_constant t2 = t1.all ();
	  t1 = t2.all ();
	}

      int expr_value = 0;

      if (t1.is_real_scalar ())
	expr_value = (int) t1.double_value ();
      else if (t1.is_complex_scalar ())
	expr_value = t1.complex_value () != 0.0;
      else
	error ("if: all (all (cond)) is not a scalar");

      if (expr_value)
	{
	  if (list)
	    list->eval (1);

	  return 1;
	}
    }
  else
    {
      if (list)
	list->eval (1);

      return 1;
    }

  return 0;
}

void
tree_if_clause::print_code (ostream& os)
{
  if (expr)
    expr->print_code (os);

  print_code_new_line (os);
  increment_indent_level ();

  if (list)
    {
      list->print_code (os);

      decrement_indent_level ();
    }
}

// List of if commands.

void
tree_if_command_list::eval (void)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_if_clause *t = this->operator () (p);

      if (t->eval () || error_state)
	break;
    }
}

void
tree_if_command_list::print_code (ostream& os)
{
  Pix p = first ();

  int first_elt = 1;

  while (p)
    {
      tree_if_clause *elt = this->operator () (p);

      if (elt)
	{
	  if (! first_elt)
	    {
	      print_code_indent (os);

	      if (elt->is_else_clause ())
		os << "else";
	      else
		os << "elseif ";
	    }

	  elt->print_code (os);
	}

      first_elt = 0;
      next (p);
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
