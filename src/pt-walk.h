/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_tree_walker_h)
#define octave_tree_walker_h 1

#include "pt-cmd.h"
#include "pt-const.h"
#include "pt-exp.h"
#include "pt-fcn.h"
#include "pt-fvc.h"
#include "pt-mat.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "pt-plot.h"

class
tree_walker
{
public:

  virtual void
  visit_argument_list (tree_argument_list&) = 0;

  virtual void
  visit_binary_expression (tree_binary_expression&) = 0;

  virtual void
  visit_break_command (tree_break_command&) = 0;

  virtual void
  visit_builtin (tree_builtin&) = 0;

  virtual void
  visit_colon_expression (tree_colon_expression&) = 0;

  virtual void
  visit_continue_command (tree_continue_command&) = 0;

  virtual void
  visit_for_command (tree_for_command&) = 0;

  virtual void
  visit_function (tree_function&) = 0;

  virtual void
  visit_global (tree_global&) = 0;

  virtual void
  visit_global_command (tree_global_command&) = 0;

  virtual void
  visit_global_init_list (tree_global_init_list&) = 0;

  virtual void
  visit_identifier (tree_identifier&) = 0;

  virtual void
  visit_if_clause (tree_if_clause&) = 0;

  virtual void
  visit_if_command (tree_if_command&) = 0;

  virtual void
  visit_if_command_list (tree_if_command_list&) = 0;

  virtual void
  visit_index_expression (tree_index_expression&) = 0;

  virtual void
  visit_indirect_ref (tree_indirect_ref&) = 0;

  virtual void
  visit_matrix (tree_matrix&) = 0;

  virtual void
  visit_matrix_row (tree_matrix_row&) = 0;

  virtual void
  visit_multi_assignment_expression (tree_multi_assignment_expression&) = 0;

  virtual void
  visit_oct_obj (tree_oct_obj&) = 0;

  virtual void
  visit_octave_value (octave_value&) = 0;

  virtual void
  visit_parameter_list (tree_parameter_list&) = 0;

  virtual void
  visit_plot_command (tree_plot_command&) = 0;

  virtual void
  visit_plot_limits (plot_limits&) = 0;

  virtual void
  visit_plot_range (plot_range&) = 0;

  virtual void
  visit_postfix_expression (tree_postfix_expression&) = 0;

  virtual void
  visit_prefix_expression (tree_prefix_expression&) = 0;

  virtual void
  visit_return_command (tree_return_command&) = 0;

  virtual void
  visit_return_list (tree_return_list&) = 0;

  virtual void
  visit_simple_assignment_expression (tree_simple_assignment_expression&) = 0;

  virtual void
  visit_statement (tree_statement&) = 0;

  virtual void
  visit_statement_list (tree_statement_list&) = 0;

  virtual void
  visit_subplot (subplot&) = 0;

  virtual void
  visit_subplot_list (subplot_list&) = 0;

  virtual void
  visit_subplot_style (subplot_style&) = 0;

  virtual void
  visit_subplot_using (subplot_using&) = 0;

  virtual void
  visit_try_catch_command (tree_try_catch_command&) = 0;

  virtual void
  visit_unary_expression (tree_unary_expression&) = 0;

  virtual void
  visit_unwind_protect_command (tree_unwind_protect_command&) = 0;

  virtual void
  visit_while_command (tree_while_command&) = 0;

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
