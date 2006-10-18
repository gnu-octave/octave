/*

Copyright (C) 2003 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

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
tree_fcn_handle::dup (symbol_table *sym_tab)
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

tree_anon_fcn_handle::~tree_anon_fcn_handle (void)
{
  delete param_list;
  delete cmd_list;
  delete ret_list;
  delete sym_tab;
}

octave_value
tree_anon_fcn_handle::rvalue (void)
{
  MAYBE_DO_BREAKPOINT;

  symbol_table *new_sym_tab = sym_tab ? sym_tab->dup () : 0;

  if (new_sym_tab)
    new_sym_tab->inherit (curr_sym_tab);

  tree_parameter_list *new_param_list
    = param_list ? param_list->dup (new_sym_tab) : 0;

  tree_statement_list *new_cmd_list
    = cmd_list ? cmd_list->dup (new_sym_tab) : 0;

  tree_parameter_list *new_ret_list
    = ret_list ? ret_list->dup (new_sym_tab) : 0;

  octave_user_function *uf
    = new octave_user_function (new_param_list, new_ret_list,
				new_cmd_list, new_sym_tab);

  octave_value fcn (uf);

  octave_value fh (new octave_fcn_handle (fcn, "@<anonymous>"));

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
tree_anon_fcn_handle::dup (symbol_table *st)
{
  symbol_table *new_sym_tab = sym_tab ? sym_tab->dup () : 0;

  if (new_sym_tab)
    new_sym_tab->inherit (st);

  tree_anon_fcn_handle *new_afh
    = new tree_anon_fcn_handle (param_list ? param_list->dup (new_sym_tab) : 0,
				ret_list ? ret_list->dup (new_sym_tab) : 0,
				cmd_list ? cmd_list->dup (new_sym_tab) : 0,
				new_sym_tab, line (), column ());

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
