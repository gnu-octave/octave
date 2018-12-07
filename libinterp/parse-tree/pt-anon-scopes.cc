/*

Copyright (C) 1996-2018 John W. Eaton
Copyright (C) 2015-2018 Olaf Till

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "pt-all.h"
#include "pt-anon-scopes.h"

// TODO: make sure that if(f->scope()) is checked if necessary

namespace octave
{
  tree_anon_scopes::tree_anon_scopes (octave_user_function *f)
    : scopes (), merged_tables ()
  {
    if (f)
      {
        if (! f->is_anonymous_function ())
          panic_impossible ();

        // Collect the scope of the outer anonymous function.

        stash_scope_if_valid (f->scope ());

        // Further walk the tree to find nested definitions of further
        // anonymous functions.

        tree_statement_list *cmd_list = f->body ();

        if (cmd_list)
          cmd_list->accept (*this);

        // Collect symbol records of all collected scopes.

        merge_tables ();
      }
  }

  void
  tree_anon_scopes::visit_anon_fcn_handle (tree_anon_fcn_handle& afh)
  {
    // Collect the scope of this anonymous function.

    stash_scope_if_valid (afh.scope ());

    // Further walk the tree to find nested definitions of further
    // anonymous functions.

    tree_expression *e = afh.expression ();

    if (e)
      e->accept (*this);
  }

  // The rest of visit_... methods is only for walking the tree. Many of
  // them, in particular all methods for commands, are not applicable to
  // anonymous functions. Only parts of the tree are walked which could
  // contain further (nested) anonymous function definitions (so
  // e.g. identifiers and left hand sides of assignments are ignored).

  void
  tree_anon_scopes::visit_argument_list (tree_argument_list& lst)
  {
    tree_argument_list::iterator p = lst.begin ();

    while (p != lst.end ())
      {
        tree_expression *elt = *p++;

        if (elt)
          {
            elt->accept (*this);
          }
      }
  }

  void
  tree_anon_scopes::visit_binary_expression (tree_binary_expression& expr)
  {
    tree_expression *op1 = expr.lhs ();

    if (op1)
      op1->accept (*this);

    tree_expression *op2 = expr.rhs ();

    if (op2)
      op2->accept (*this);
  }

  void
  tree_anon_scopes::visit_break_command (tree_break_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_colon_expression (tree_colon_expression& expr)
  {
    tree_expression *op1 = expr.base ();

    if (op1)
      op1->accept (*this);

    tree_expression *op3 = expr.increment ();

    if (op3)
      op3->accept (*this);

    tree_expression *op2 = expr.limit ();

    if (op2)
      op2->accept (*this);
  }

  void
  tree_anon_scopes::visit_continue_command (tree_continue_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_decl_command (tree_decl_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_decl_elt (tree_decl_elt&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_decl_init_list (tree_decl_init_list&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_simple_for_command (tree_simple_for_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_complex_for_command (tree_complex_for_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_octave_user_script (octave_user_script&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_octave_user_function (octave_user_function&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_function_def (tree_function_def&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_identifier (tree_identifier& /* id */)
  {
  }

  void
  tree_anon_scopes::visit_if_clause (tree_if_clause&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_if_command (tree_if_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_if_command_list (tree_if_command_list&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_switch_case (tree_switch_case&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_switch_case_list (tree_switch_case_list&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_switch_command (tree_switch_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_index_expression (tree_index_expression& expr)
  {
    tree_expression *e = expr.expression ();

    if (e)
      e->accept (*this);

    std::list<tree_argument_list *> lst = expr.arg_lists ();

    std::list<tree_argument_list *>::iterator p = lst.begin ();

    while (p != lst.end ())
      {
        tree_argument_list *elt = *p++;

        if (elt)
          elt->accept (*this);
      }
  }

  void
  tree_anon_scopes::visit_matrix (tree_matrix& lst)
  {
    tree_matrix::iterator p = lst.begin ();

    while (p != lst.end ())
      {
        tree_argument_list *elt = *p++;

        if (elt)
          elt->accept (*this);
      }
  }

  void
  tree_anon_scopes::visit_cell (tree_cell& lst)
  {
    tree_matrix::iterator p = lst.begin ();

    while (p != lst.end ())
      {
        tree_argument_list *elt = *p++;

        if (elt)
          elt->accept (*this);
      }
  }

  void
  tree_anon_scopes::visit_multi_assignment (tree_multi_assignment& expr)
  {
    tree_expression *rhs = expr.right_hand_side ();

    if (rhs)
      rhs->accept (*this);
  }

  void
  tree_anon_scopes::visit_no_op_command (tree_no_op_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_constant (tree_constant& /* val */)
  {
  }

  void
  tree_anon_scopes::visit_fcn_handle (tree_fcn_handle& /* fh */)
  {
  }

  void
  tree_anon_scopes::visit_funcall (tree_funcall& /* fc */)
  {
  }

  void
  tree_anon_scopes::visit_parameter_list (tree_parameter_list&)
  {
    // In visit_anon_fcn_handle we only accept/visit the body of
    // anonymous function definitions, not the parameter list.

    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_postfix_expression (tree_postfix_expression& expr)
  {
    tree_expression *e = expr.operand ();

    if (e)
      e->accept (*this);
  }

  void
  tree_anon_scopes::visit_prefix_expression (tree_prefix_expression& expr)
  {
    tree_expression *e = expr.operand ();

    if (e)
      e->accept (*this);
  }

  void
  tree_anon_scopes::visit_return_command (tree_return_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_return_list (tree_return_list& lst)
  {
    tree_return_list::iterator p = lst.begin ();

    while (p != lst.end ())
      {
        tree_index_expression *elt = *p++;

        if (elt)
          elt->accept (*this);
      }
  }

  void
  tree_anon_scopes::visit_simple_assignment (tree_simple_assignment& expr)
  {
    tree_expression *rhs = expr.right_hand_side ();

    if (rhs)
      rhs->accept (*this);
  }

  void
  tree_anon_scopes::visit_statement (tree_statement& stmt)
  {
    tree_command *cmd = stmt.command ();

    if (cmd)
      panic_impossible ();
    else
      {
        tree_expression *expr = stmt.expression ();

        if (expr)
          expr->accept (*this);
      }
  }

  void
  tree_anon_scopes::visit_statement_list (tree_statement_list& lst)
  {
    for (auto& p : lst)
      {
        tree_statement *elt = p;

        if (elt)
          elt->accept (*this);
      }
  }

  void
  tree_anon_scopes::visit_try_catch_command (tree_try_catch_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_unwind_protect_command (tree_unwind_protect_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_while_command (tree_while_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::visit_do_until_command (tree_do_until_command&)
  {
    panic_impossible ();
  }

  void
  tree_anon_scopes::merge_tables (void)
  {
    for (const auto& sc : scopes)
      {
        symrec_list vars = sc.all_variables ();

        for (const auto& symrec : vars)
          merged_tables[symrec.name ()] = symrec;
      }
  }
}

