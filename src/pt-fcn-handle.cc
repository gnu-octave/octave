/*

Copyright (C) 2003, 2004, 2005, 2006, 2007 John W. Eaton

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

#include <iostream>

#include "error.h"
#include "oct-obj.h"
#include "ov-fcn-handle.h"
#include "pt-fcn-handle.h"
#include "pager.h"
#include "pt-walk.h"
#include "variables.h"

void
tree_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax,
			bool pr_orig_text)
{
  print_raw (os, pr_as_read_syntax, pr_orig_text);
}

void
tree_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax,
			    bool pr_orig_text) 
{
  os << ((pr_as_read_syntax || pr_orig_text) ? "@" : "") << nm;
}

octave_value
tree_fcn_handle::rvalue (void)
{
  MAYBE_DO_BREAKPOINT;

  return make_fcn_handle (nm);
}


octave_value_list
tree_fcn_handle::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for function handle expression");
  else
    retval = rvalue ();

  return retval;
}

tree_expression *
tree_fcn_handle::dup (symbol_table::scope_id,
		      symbol_table::context_id)
{
  tree_fcn_handle *new_fh = new tree_fcn_handle (nm, line (), column ());

  new_fh->copy_base (*this);

  return new_fh;
}

void
tree_fcn_handle::accept (tree_walker& tw)
{
  tw.visit_fcn_handle (*this);
}

octave_value
tree_anon_fcn_handle::rvalue (void)
{
  MAYBE_DO_BREAKPOINT;

  tree_parameter_list *param_list = parameter_list ();
  tree_parameter_list *ret_list = return_list ();
  tree_statement_list *cmd_list = body ();
  symbol_table::scope_id this_scope = scope ();

  symbol_table::scope_id new_scope = symbol_table::dup_scope (this_scope);

  if (new_scope > 0)
    symbol_table::inherit (new_scope, symbol_table::current_scope (),
			   symbol_table::current_context ());

  octave_user_function *uf
    = new octave_user_function (new_scope,
				param_list ? param_list->dup (new_scope, 0) : 0,
				ret_list ? ret_list->dup (new_scope, 0) : 0,
				cmd_list ? cmd_list->dup (new_scope, 0) : 0);

  octave_function *curr_fcn = octave_call_stack::current ();

  if (curr_fcn)
    {
      uf->stash_parent_fcn_name (curr_fcn->name ());
      uf->stash_parent_fcn_scope (curr_fcn->scope ());
    }

  uf->mark_as_inline_function ();

  octave_value ov_fcn (uf);

  octave_value fh (new octave_fcn_handle (ov_fcn, "@<anonymous>"));

  return fh;
}

octave_value_list
tree_anon_fcn_handle::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for anonymous function handle expression");
  else
    retval = rvalue ();

  return retval;
}

tree_expression *
tree_anon_fcn_handle::dup (symbol_table::scope_id parent_scope,
			   symbol_table::context_id parent_context)
{
  tree_parameter_list *param_list = parameter_list ();
  tree_parameter_list *ret_list = return_list ();
  tree_statement_list *cmd_list = body ();
  symbol_table::scope_id this_scope = scope ();

  symbol_table::scope_id new_scope = symbol_table::dup_scope (this_scope);

  if (new_scope > 0)
    symbol_table::inherit (new_scope, parent_scope, parent_context);

  tree_anon_fcn_handle *new_afh = new
    tree_anon_fcn_handle (param_list ? param_list->dup (new_scope, 0) : 0,
			  ret_list ? ret_list->dup (new_scope, 0) : 0,
			  cmd_list ? cmd_list->dup (new_scope, 0) : 0,
			  new_scope, line (), column ());

  new_afh->copy_base (*this);

  return new_afh;
}

void
tree_anon_fcn_handle::accept (tree_walker& tw)
{
  tw.visit_anon_fcn_handle (*this);
}



/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
