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

#if !defined (octave_tree_fcn_handle_h)
#define octave_fcn_handle_h 1

#include <iostream>
#include <string>

#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-misc.h"
#include "pt-stmt.h"

class octave_value_list;

class tree_walker;

#include "ov.h"

class
tree_fcn_handle : public tree_expression
{
public:

  tree_fcn_handle (int l = -1, int c = -1)
    : tree_expression (l, c), nm () { }

  tree_fcn_handle (const std::string& n, int l = -1, int c = -1)
    : tree_expression (l, c), nm (n) { }

  ~tree_fcn_handle (void) { }

  bool has_magic_end (void) const { return false; }

  void print (std::ostream& os, bool pr_as_read_syntax = false,
	      bool pr_orig_txt = true);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false,
		  bool pr_orig_txt = true);

  std::string name (void) const { return nm; }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  tree_expression *dup (symbol_table *sym_tab);

  void accept (tree_walker& tw);

private:

  // The name of this function handle.
  std::string nm;

  // No copying!

  tree_fcn_handle (const tree_fcn_handle&);

  tree_fcn_handle& operator = (const tree_fcn_handle&);
};

class
tree_anon_fcn_handle : public tree_expression
{
public:

  tree_anon_fcn_handle (int l = -1, int c = -1)
    : tree_expression (l, c), param_list (0), cmd_list (0),
      ret_list (0), sym_tab (0) { }

  tree_anon_fcn_handle (tree_parameter_list *p, tree_parameter_list *r,
			tree_statement_list *cl, symbol_table *st,
			int l = -1, int c = -1)
    : tree_expression (l, c), param_list (p), cmd_list (cl),
      ret_list (r), sym_tab (st) { }

  ~tree_anon_fcn_handle (void);

  bool has_magic_end (void) const { return false; }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  tree_parameter_list *parameter_list (void) { return param_list; }

  tree_statement_list *body (void) { return cmd_list; }

  tree_expression *dup (symbol_table *sym_tab);

  void accept (tree_walker& tw);

private:

  // The parameter list.
  tree_parameter_list *param_list;

  // The statement that makes up the body of the function.
  tree_statement_list *cmd_list;

  // The list of return values.
  tree_parameter_list *ret_list;

  // The symbol table.
  symbol_table *sym_tab;

  // No copying!

  tree_anon_fcn_handle (const tree_anon_fcn_handle&);

  tree_anon_fcn_handle& operator = (const tree_anon_fcn_handle&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
