/*

Copyright (C) 2003 John W. Eaton

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
#include "ov-usr-fcn.h"

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
    : tree_expression (l, c), fcn () { }

  tree_anon_fcn_handle (tree_parameter_list *pl, tree_parameter_list *rl,
			tree_statement_list *cl, symbol_table *st,
			int l = -1, int c = -1)
    : tree_expression (l, c), fcn (pl, rl, cl, st) { }

  ~tree_anon_fcn_handle (void) { }

  bool has_magic_end (void) const { return false; }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  tree_parameter_list *parameter_list (void) { return fcn.parameter_list (); }

  tree_statement_list *body (void) { return fcn.body (); }

  tree_expression *dup (symbol_table *sym_tab);

  void accept (tree_walker& tw);

private:

  // The function.
  octave_user_function fcn;

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
