/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_tree_walker_h)
#define octave_tree_walker_h 1

class tree_anon_fcn_handle;
class tree_argument_list;
class tree_binary_expression;
class tree_break_command;
class tree_colon_expression;
class tree_continue_command;
class tree_global_command;
class tree_static_command;
class tree_decl_elt;
class tree_decl_init_list;
class tree_simple_for_command;
class tree_complex_for_command;
class octave_user_script;
class octave_user_function;
class tree_function_def;
class tree_identifier;
class tree_if_clause;
class tree_if_command;
class tree_if_command_list;
class tree_switch_case;
class tree_switch_case_list;
class tree_switch_command;
class tree_index_expression;
class tree_matrix;
class tree_cell;
class tree_multi_assignment;
class tree_no_op_command;
class tree_constant;
class tree_fcn_handle;
class tree_parameter_list;
class tree_postfix_expression;
class tree_prefix_expression;
class tree_return_command;
class tree_return_list;
class tree_simple_assignment;
class tree_statement;
class tree_statement_list;
class tree_try_catch_command;
class tree_unwind_protect_command;
class tree_while_command;
class tree_do_until_command;

class
tree_walker
{
public:

  virtual void
  visit_anon_fcn_handle (tree_anon_fcn_handle&) = 0;

  virtual void
  visit_argument_list (tree_argument_list&) = 0;

  virtual void
  visit_binary_expression (tree_binary_expression&) = 0;

  virtual void
  visit_break_command (tree_break_command&) = 0;

  virtual void
  visit_colon_expression (tree_colon_expression&) = 0;

  virtual void
  visit_continue_command (tree_continue_command&) = 0;

  virtual void
  visit_global_command (tree_global_command&) = 0;

  virtual void
  visit_static_command (tree_static_command&) = 0;

  virtual void
  visit_decl_elt (tree_decl_elt&) = 0;

  virtual void
  visit_decl_init_list (tree_decl_init_list&) = 0;

  virtual void
  visit_simple_for_command (tree_simple_for_command&) = 0;

  virtual void
  visit_complex_for_command (tree_complex_for_command&) = 0;

  virtual void
  visit_octave_user_script (octave_user_script&) = 0;

  virtual void
  visit_octave_user_function (octave_user_function&) = 0;

  virtual void
  visit_function_def (tree_function_def&) = 0;

  virtual void
  visit_identifier (tree_identifier&) = 0;

  virtual void
  visit_if_clause (tree_if_clause&) = 0;

  virtual void
  visit_if_command (tree_if_command&) = 0;

  virtual void
  visit_if_command_list (tree_if_command_list&) = 0;

  virtual void
  visit_switch_case (tree_switch_case&) = 0;

  virtual void
  visit_switch_case_list (tree_switch_case_list&) = 0;

  virtual void
  visit_switch_command (tree_switch_command&) = 0;

  virtual void
  visit_index_expression (tree_index_expression&) = 0;

  virtual void
  visit_matrix (tree_matrix&) = 0;

  virtual void
  visit_cell (tree_cell&) = 0;

  virtual void
  visit_multi_assignment (tree_multi_assignment&) = 0;

  virtual void
  visit_no_op_command (tree_no_op_command&) = 0;

  virtual void
  visit_constant (tree_constant&) = 0;

  virtual void
  visit_fcn_handle (tree_fcn_handle&) = 0;

  virtual void
  visit_parameter_list (tree_parameter_list&) = 0;

  virtual void
  visit_postfix_expression (tree_postfix_expression&) = 0;

  virtual void
  visit_prefix_expression (tree_prefix_expression&) = 0;

  virtual void
  visit_return_command (tree_return_command&) = 0;

  virtual void
  visit_return_list (tree_return_list&) = 0;

  virtual void
  visit_simple_assignment (tree_simple_assignment&) = 0;

  virtual void
  visit_statement (tree_statement&) = 0;

  virtual void
  visit_statement_list (tree_statement_list&) = 0;

  virtual void
  visit_try_catch_command (tree_try_catch_command&) = 0;

  virtual void
  visit_unwind_protect_command (tree_unwind_protect_command&) = 0;

  virtual void
  visit_while_command (tree_while_command&) = 0;

  virtual void
  visit_do_until_command (tree_do_until_command&) = 0;

protected:

  tree_walker (void) { }

  virtual ~tree_walker (void) { }

private:

  // No copying!

  tree_walker (const tree_walker&);

  tree_walker& operator = (const tree_walker&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
