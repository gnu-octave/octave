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

#if !defined (octave_tree_print_code_h)
#define octave_tree_print_code_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <string>

#include "comment-list.h"
#include "pt-walk.h"

class tree_expression;

// How to print the code that the parse trees represent.

class
tree_print_code : public tree_walker
{
public:

  tree_print_code (std::ostream& os_arg,
		   const std::string& pfx = std::string (),
		   bool pr_orig_txt = true)
    : os (os_arg), prefix (pfx), print_original_text (pr_orig_txt),
      curr_print_indent_level (0), beginning_of_line (true) { }

  ~tree_print_code (void) { }

  void visit_argument_list (tree_argument_list&);

  void visit_binary_expression (tree_binary_expression&);

  void visit_break_expression (tree_break_expression&);

  void visit_colon_expression (tree_colon_expression&);

  void visit_continue_expression (tree_continue_expression&);

  void visit_decl_command (tree_decl_command&);

  void visit_decl_elt (tree_decl_elt&);

  void visit_decl_init_list (tree_decl_init_list&);

  void visit_simple_for_command (tree_simple_for_command&);

  void visit_complex_for_command (tree_complex_for_command&);

  void visit_octave_user_function (octave_user_function&);

  void visit_octave_user_function_header (octave_user_function&);

  void visit_octave_user_function_trailer (octave_user_function&);

  void visit_identifier (tree_identifier&);

  void visit_if_clause (tree_if_clause&);

  void visit_if_command (tree_if_command&);

  void visit_if_command_list (tree_if_command_list&);

  void visit_index_expression (tree_index_expression&);

  void visit_matrix (tree_matrix&);

  void visit_cell (tree_cell&);

  void visit_multi_assignment (tree_multi_assignment&);

  void visit_no_op_command (tree_no_op_command&);

  void visit_constant (tree_constant&);

  void visit_parameter_list (tree_parameter_list&);

  void visit_plot_command (tree_plot_command&);

  void visit_plot_limits (plot_limits&);

  void visit_plot_range (plot_range&);

  void visit_postfix_expression (tree_postfix_expression&);

  void visit_prefix_expression (tree_prefix_expression&);

  void visit_return_expression (tree_return_expression&);

  void visit_return_list (tree_return_list&);

  void visit_simple_assignment (tree_simple_assignment&);

  void visit_statement (tree_statement&);

  void visit_statement_list (tree_statement_list&);

  void visit_subplot (subplot&);

  void visit_subplot_axes (subplot_axes&);

  void visit_subplot_list (subplot_list&);

  void visit_subplot_style (subplot_style&);

  void visit_subplot_using (subplot_using&);

  void visit_switch_case (tree_switch_case&);

  void visit_switch_case_list (tree_switch_case_list&);

  void visit_switch_command (tree_switch_command&);

  void visit_try_catch_command (tree_try_catch_command&);

  void visit_unwind_protect_command (tree_unwind_protect_command&);

  void visit_while_command (tree_while_command&);

  void visit_do_until_command (tree_do_until_command&);

private:

  std::ostream& os;

  std::string prefix;

  bool print_original_text;

  // Current indentation.
  int curr_print_indent_level;

  // TRUE means we are at the beginning of a line.
  bool beginning_of_line;

  void reset_indent_level (void) { curr_print_indent_level = 0; }

  void increment_indent_level (void) { curr_print_indent_level += 2; }

  void decrement_indent_level (void) { curr_print_indent_level -= 2; }

  void newline (void);

  void indent (void);

  void reset (void);

  void print_parens (const tree_expression& expr, const char *txt);

  void print_comment_list (octave_comment_list *comment_list);

  void print_comment_elt (const octave_comment_elt& comment_elt);

  void print_indented_comment (octave_comment_list *comment_list);

  // Must create with an output stream!

  tree_print_code (void);

  // No copying!

  tree_print_code (const tree_print_code&);

  tree_print_code& operator = (const tree_print_code&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
