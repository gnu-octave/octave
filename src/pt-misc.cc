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

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "error.h"
#include "input.h"
#include "oct-obj.h"
#include "pager.h"
#include "toplev.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-fcn.h"
#include "pt-fvc.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "pt-walk.h"
#include "pt-pr-code.h"
#include "ov.h"
#include "variables.h"

// Nonzero means we're breaking out of a loop or function body.
extern int breaking;

// Nonzero means we're jumping to the end of a loop.
extern int continuing;

// Nonzero means we're returning from a function.
extern int returning;

// A list of commands to be executed.

tree_statement::~tree_statement (void)
{
  delete cmd;
  delete expr;
}

int
tree_statement::line (void)
{
  return cmd ? cmd->line () : (expr ? expr->line () : -1);
}

int
tree_statement::column (void)
{
  return cmd ? cmd->column () : (expr ? expr->column () : -1);
}

void
tree_statement::maybe_echo_code (bool in_function_body)
{
  if (in_function_body
      && (Vecho_executing_commands & ECHO_FUNCTIONS))
    {
      tree_print_code tpc (octave_stdout, Vps4);

      accept (tpc);
    }
}

void
tree_statement::accept (tree_walker& tw)
{
  tw.visit_statement (*this);
}

octave_value
tree_statement_list::eval (bool print)
{
  bool pf;
  octave_value retval;

  if (error_state)
    return retval;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_statement *elt = this->operator () (p);

      if (! print)
	pf = false;
      else
	pf = elt->print_flag;

      tree_command *cmd = elt->command ();
      tree_expression *expr = elt->expression ();

      if (cmd || expr)
	{
	  elt->maybe_echo_code (function_body);

	  if (cmd)
	    cmd->eval ();
	  else
	    retval = expr->eval (pf);

	  if (error_state)
	    return octave_value ();

	  if (breaking || continuing)
	    break;

	  if (returning)
	    break;
	}
      else
	retval = octave_value ();
    }
  return retval;
}

octave_value_list
tree_statement_list::eval (bool print, int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    {
      bool pf;

      if (error_state)
	return retval;

      for (Pix p = first (); p != 0; next (p))
	{
	  tree_statement *elt = this->operator () (p);

	  if (! print)
	    pf = false;
	  else
	    pf = elt->print_flag;

	  tree_command *cmd = elt->command ();
	  tree_expression *expr = elt->expression ();

	  if (cmd || expr)
	    {
	      elt->maybe_echo_code (function_body);

	      if (cmd)
		cmd->eval ();
	      else
		{
		  if (expr->is_multi_val_ret_expression ())
		    {
		      octave_value_list args;
		      tree_multi_val_ret *t;
		      t = static_cast<tree_multi_val_ret *> (expr);
		      retval = t->eval (pf, nargout, args);
		    }
		  else
		    retval = expr->eval (pf);
		}

	      if (error_state)
		return octave_value ();

	      if (breaking || continuing)
		break;

	      if (returning)
		break;
	    }
	  else
	    retval = octave_value_list ();
	}
      return retval;
    }
  else
    retval = eval (print);

  return retval;
}

void
tree_statement_list::accept (tree_walker& tw)
{
  tw.visit_statement_list (*this);
}

octave_value_list
tree_argument_list::convert_to_const_vector (void)
{
  int len = length ();

  // XXX FIXME XXX -- would be nice to know in advance how largs args
  // needs to be even when we have a list containing an all_va_args
  // token.

  octave_value_list args;
  args.resize (len);

  Pix p = first ();
  int j = 0;
  for (int k = 0; k < len; k++)
    {
      tree_expression *elt = this->operator () (p);
      if (elt)
	{
	  octave_value tmp = elt->eval (false);
	  if (error_state)
	    {
	      ::error ("evaluating argument list element number %d", k);
	      args = octave_value_list ();
	      break;
	    }
	  else
	    {
	      if (tmp.is_all_va_args ())
		{
		  if (curr_function)
		    {
		      octave_value_list tva;
		      tva = curr_function->octave_all_va_args ();
		      int n = tva.length ();
		      for (int i = 0; i < n; i++)
			args(j++) = tva(i);
		    }
		  else
		    {
		      ::error ("all_va_args is only valid inside functions");
		      args = octave_value_list ();
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
	  args(j++) = octave_value ();
	  break;
	}
    }

  args.resize (j);

  return args;
}

void
tree_argument_list::accept (tree_walker& tw)
{
  tw.visit_argument_list (*this);
}

// Parameter lists.

tree_parameter_list::~tree_parameter_list (void)
{
  while (! empty ())
    {
      tree_identifier *t = remove_front ();
      delete t;
    }
}

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
tree_parameter_list::initialize_undefined_elements (octave_value& val)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);
      if (! elt->is_defined ())
	{
	  octave_variable_reference tmp (elt);
	  tmp.assign (val);
	}
    }
}

void
tree_parameter_list::define_from_arg_vector (const octave_value_list& args)
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
	  tmp = new tree_constant (args (i));
	}

      elt->define (tmp);

      next (p);
    }
}

octave_value_list
tree_parameter_list::convert_to_const_vector (tree_va_return_list *vr_list)
{
  int nout = length ();

  if (vr_list)
    nout += vr_list->length ();

  octave_value_list retval;
  retval.resize (nout);

  int i = 0;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);

      if (elt->is_defined ())
	retval(i) = elt->eval (false);

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

bool
tree_parameter_list::is_defined (void)
{
  bool status = true;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);

      if (! elt->is_defined ())
	{
	  status = false;
	  break;
	}
    }

  return status;
}

void
tree_parameter_list::accept (tree_walker& tw)
{
  tw.visit_parameter_list (*this);
}

// Return lists.

tree_return_list::~tree_return_list (void)
{
  while (! empty ())
    {
      tree_index_expression *t = remove_front ();
      delete t;
    }
}

void
tree_return_list::accept (tree_walker& tw)
{
  tw.visit_return_list (*this);
}

// Declarations (global, static, etc.).

tree_decl_elt::~tree_decl_elt (void)
{
  delete id;
  delete ass_expr;
}

void
tree_decl_elt::accept (tree_walker& tw)
{
  tw.visit_decl_elt (*this);
}

// Initializer lists for declaration statements.

void
tree_decl_init_list::eval (tree_decl_elt::eval_fcn f, bool skip_init)
{
  for (Pix p = first (); p != 0; next (p))
    {
      f (*(this->operator () (p)), skip_init);

      if (error_state)
	break;
    }
}

void
tree_decl_init_list::accept (tree_walker& tw)
{
  tw.visit_decl_init_list (*this);
}

// If.

tree_if_clause::~tree_if_clause (void)
{
  delete expr;
  delete list;
}

int
tree_if_clause::eval (void)
{
  if (is_else_clause () || expr->is_logically_true ("if"))
    {
      if (list)
	list->eval (true);

      return 1;
    }

  return 0;
}

void
tree_if_clause::accept (tree_walker& tw)
{
  tw.visit_if_clause (*this);
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
tree_if_command_list::accept (tree_walker& tw)
{
  tw.visit_if_command_list (*this);
}

// Switch.

tree_switch_case::~tree_switch_case (void)
{
  delete label;
  delete list;
}

bool
tree_switch_case::label_matches (const octave_value& val)
{
  bool retval = false;

  octave_value label_value = label->eval (false);

  if (! error_state)
    {
      if (label_value.is_defined ())
	{
	  octave_value tmp = do_binary_op (octave_value::eq,
					   val, label_value);

	  if (! error_state)
	    {
	      if (tmp.is_defined ())
		retval = tmp.is_true ();
	      else
		eval_error ();
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

int
tree_switch_case::eval (const octave_value& val)
{
  int retval = 0;

  if (is_default_case () || label_matches (val))
    {
      if (list)
	list->eval (true);

      retval = 1;
    }

  return retval;
}

void
tree_switch_case::eval_error (void)
{
  ::error ("evaluating switch case label");
}

void
tree_switch_case::accept (tree_walker& tw)
{
  tw.visit_switch_case (*this);
}

// List of switch cases.

void
tree_switch_case_list::eval (const octave_value& val)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_switch_case *t = this->operator () (p);

      if (t->eval (val) || error_state)
	break;
    }
}

void
tree_switch_case_list::accept (tree_walker& tw)
{
  tw.visit_switch_case_list (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
