////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_pt_pr_code_h)
#define octave_pt_pr_code_h 1

#include "octave-config.h"

#include <stack>
#include <string>

#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class comment_elt;
class comment_list;
class tree_decl_command;
class tree_expression;

// How to print the code that the parse trees represent.

class tree_print_code : public tree_walker
{
public:

  tree_print_code (std::ostream& os_arg,
                   const std::string& pfx = "",
                   bool pr_orig_txt = true)
    : m_os (os_arg), m_prefix (pfx), m_nesting (),
      m_print_original_text (pr_orig_txt),
      m_curr_print_indent_level (0), m_beginning_of_line (true),
      m_suppress_newlines (0)
  {
    // For "none".
    m_nesting.push ('n');
  }

  // No copying!

  tree_print_code (const tree_print_code&) = delete;

  tree_print_code& operator = (const tree_print_code&) = delete;

  ~tree_print_code (void) = default;

  void visit_anon_fcn_handle (tree_anon_fcn_handle&);

  void visit_argument_list (tree_argument_list&);

  void visit_arguments_block (tree_arguments_block&);

  void visit_args_block_attribute_list (tree_args_block_attribute_list&);

  void visit_args_block_validation_list (tree_args_block_validation_list&);

  void visit_arg_validation (tree_arg_validation&);

  void visit_arg_size_spec (tree_arg_size_spec&);

  void visit_arg_validation_fcns (tree_arg_validation_fcns&);

  void visit_binary_expression (tree_binary_expression&);

  void visit_break_command (tree_break_command&);

  void visit_colon_expression (tree_colon_expression&);

  void visit_continue_command (tree_continue_command&);

  void visit_decl_command (tree_decl_command&);

  void visit_decl_init_list (tree_decl_init_list&);

  void visit_decl_elt (tree_decl_elt&);

  void visit_simple_for_command (tree_simple_for_command&);

  void visit_complex_for_command (tree_complex_for_command&);

  void visit_spmd_command (tree_spmd_command&);

  void visit_octave_user_script (octave_user_script&);

  void visit_octave_user_function (octave_user_function&);

  void visit_octave_user_function_header (octave_user_function&);

  void visit_octave_user_function_trailer (octave_user_function&);

  void visit_function_def (tree_function_def&);

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

  void visit_fcn_handle (tree_fcn_handle&);

  void visit_parameter_list (tree_parameter_list&);

  void visit_postfix_expression (tree_postfix_expression&);

  void visit_prefix_expression (tree_prefix_expression&);

  void visit_return_command (tree_return_command&);

  void visit_simple_assignment (tree_simple_assignment&);

  void visit_statement (tree_statement&);

  void visit_statement_list (tree_statement_list&);

  void visit_switch_case (tree_switch_case&);

  void visit_switch_command (tree_switch_command&);

  void visit_try_catch_command (tree_try_catch_command&);

  void visit_unwind_protect_command (tree_unwind_protect_command&);

  void visit_while_command (tree_while_command&);

  void visit_do_until_command (tree_do_until_command&);

  void visit_superclass_ref (tree_superclass_ref&);

  void visit_metaclass_query (tree_metaclass_query&);

  void print_fcn_handle_body (tree_expression *);

private:

  std::ostream& m_os;

  std::string m_prefix;

  std::stack<char> m_nesting;

  bool m_print_original_text;

  // Current indentation.
  int m_curr_print_indent_level;

  // TRUE means we are at the beginning of a line.
  bool m_beginning_of_line;

  // Nonzero means we are not printing newlines and indenting.
  int m_suppress_newlines;

  void reset_indent_level (void) { m_curr_print_indent_level = 0; }

  void increment_indent_level (void) { m_curr_print_indent_level += 2; }

  void decrement_indent_level (void) { m_curr_print_indent_level -= 2; }

  void newline (const char *alt_txt = ", ");

  void indent (void);

  void reset (void);

  void print_parens (const tree_expression& expr, const char *txt);

  void print_comment_list (comment_list *comment_list);

  void print_comment_elt (const comment_elt& comment_elt);

  void print_indented_comment (comment_list *comment_list);

  // Must create with an output stream!

  tree_print_code (void);
};

OCTAVE_END_NAMESPACE(octave)

#endif
