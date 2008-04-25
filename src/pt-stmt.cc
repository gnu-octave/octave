/*

Copyright (C) 1996, 1997, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
              2006, 2007 John W. Eaton

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

#include "quit.h"

#include "defun.h"
#include "error.h"
#include "ov.h"
#include "oct-lvalue.h"
#include "input.h"
#include "pager.h"
#include "pt-bp.h"
#include "pt-cmd.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-jump.h"
#include "pt-pr-code.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// A list of commands to be executed.

tree_statement::~tree_statement (void)
{
  delete cmd;
  delete expr;
  delete comm;
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
tree_statement::maybe_echo_code (bool in_function_or_script_body)
{
  if (in_function_or_script_body
      && (Vecho_executing_commands & ECHO_FUNCTIONS))
    {
      tree_print_code tpc (octave_stdout, VPS4);

      accept (tpc);
    }
}

octave_value_list
tree_statement::eval (bool silent, int nargout,
		      bool in_function_or_script_body)
{
  octave_value_list retval;

  bool pf = silent ? false : print_flag;

  if (cmd || expr)
    {
      if (in_function_or_script_body)
	octave_call_stack::set_statement (this);

      maybe_echo_code (in_function_or_script_body);

      try
	{
	  if (cmd)
	    cmd->eval ();
	  else
	    {
	      expr->set_print_flag (pf);

	      // FIXME -- maybe all of this should be packaged in
	      // one virtual function that returns a flag saying whether
	      // or not the expression will take care of binding ans and
	      // printing the result.

	      // FIXME -- it seems that we should just have to
	      // call expr->rvalue () and that should take care of
	      // everything, binding ans as necessary?

	      bool do_bind_ans = false;

	      if (expr->is_identifier ())
		{
		  tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

		  do_bind_ans = (! id->is_variable ());
		}
	      else
		do_bind_ans = (! expr->is_assignment_expression ());

	      retval = expr->rvalue (nargout);

	      if (do_bind_ans && ! (error_state || retval.empty ()))
		bind_ans (retval(0), pf);
	    }
	}
      catch (octave_execution_exception)
	{
	  octave_exception_state = octave_no_exception;
	  error ("caught execution error in library function");
	}
    }

  return retval;
}

tree_statement *
tree_statement::dup (symbol_table::scope_id scope)
{
  tree_statement *new_stmt = new tree_statement ();

  new_stmt->cmd = cmd ? cmd->dup (scope) : 0;

  new_stmt->expr = expr ? expr->dup (scope) : 0;

  new_stmt->comm = comm ? comm->dup () : 0;

  new_stmt->print_flag = print_flag;

  return new_stmt;
}

void
tree_statement::accept (tree_walker& tw)
{
  tw.visit_statement (*this);
}

octave_value_list
tree_statement_list::eval (bool silent, int nargout)
{
  octave_value_list retval;

  static octave_value_list empty_list;

  if (error_state)
    return retval;

  iterator p = begin ();

  if (p != end ())
    {
      while (true)
	{
	  tree_statement *elt = *p++;

	  if (elt)
	    {
	      OCTAVE_QUIT;

	      retval = elt->eval (silent, nargout,
				  function_body || script_body);

	      if (error_state)
		break;

	      if (tree_break_command::breaking
		  || tree_continue_command::continuing)
		break;

	      if (tree_return_command::returning)
		break;

	      if (p == end ())
		break;
	      else
		{
		  // Clear preivous values before next statement is
		  // evaluated so that we aren't holding an extra
		  // reference to a value that may be used next.  For
		  // example, in code like this:
		  //
		  //   X = rand (N);  ## refcount for X should be 1
		  //                  ## after this statement
		  //
		  //   X(idx) = val;  ## no extra copy of X should be
		  //                  ## needed, but we will be faked
		  //                  ## out if retval is not cleared
		  //                  ## between statements here

		  retval = empty_list;
		}
	    }
	  else
	    error ("invalid statement found in statement list!");
	}
    }

  return retval;
}

int
tree_statement_list::set_breakpoint (int line)
{
  tree_breakpoint tbp (line, tree_breakpoint::set);
  accept (tbp);
  
  return tbp.get_line ();
}

void
tree_statement_list::delete_breakpoint (int line)
{
  if (line < 0)
    {
      octave_value_list bp_lst = list_breakpoints ();

      int len = bp_lst.length ();

      for (int i = 0; i < len; i++)
	{
	  tree_breakpoint tbp (i, tree_breakpoint::clear);
	  accept (tbp);
	}
    }
  else
    {
      tree_breakpoint tbp (line, tree_breakpoint::clear); 
      accept (tbp);
    }
}

octave_value_list
tree_statement_list::list_breakpoints (void)
{
  tree_breakpoint tbp (0, tree_breakpoint::list);
  accept (tbp);

  return tbp.get_list ();
}

tree_statement_list *
tree_statement_list::dup (symbol_table::scope_id scope)
{
  tree_statement_list *new_list = new tree_statement_list ();

  new_list->function_body = function_body;

  for (iterator p = begin (); p != end (); p++)
    {
      tree_statement *elt = *p;

      new_list->append (elt ? elt->dup (scope) : 0);
    }

  return new_list;
}

void
tree_statement_list::accept (tree_walker& tw)
{
  tw.visit_statement_list (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
