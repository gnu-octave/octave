////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "pt-all.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void tree_walker::visit_anon_fcn_handle (tree_anon_fcn_handle& afh)
{
  tree_parameter_list *parameter_list = afh.parameter_list ();

  if (parameter_list)
    parameter_list->accept (*this);

  tree_expression *expression = afh.expression ();

  if (expression)
    expression->accept (*this);
}

void tree_walker::visit_argument_list (tree_argument_list& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_expression *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_arguments_block (tree_arguments_block& blk)
{
  tree_args_block_attribute_list *attribute_list = blk.attribute_list ();

  if (attribute_list)
    attribute_list->accept (*this);

  tree_args_block_validation_list *validation_list = blk.validation_list ();

  if (validation_list)
    validation_list->accept (*this);
}

void tree_walker::visit_args_block_attribute_list (tree_args_block_attribute_list& lst)
{
  tree_identifier *attribute = lst.attribute ();

  if (attribute)
    attribute->accept (*this);
}

void tree_walker::visit_args_block_validation_list (tree_args_block_validation_list& lst)
{
  for (auto *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_arg_validation (tree_arg_validation& val)
{
  tree_expression *arg_name = val.identifier_expression ();

  if (arg_name)
    arg_name->accept (*this);

  tree_arg_size_spec *size_spec = val.size_spec ();

  if (size_spec)
    size_spec->accept (*this);

  tree_identifier *class_name = val.class_name ();

  if (class_name)
    class_name->accept (*this);

  tree_arg_validation_fcns *validation_fcns = val.validation_fcns ();

  if (validation_fcns)
    validation_fcns->accept (*this);

  tree_expression *default_value = val.initializer_expression ();

  if (default_value)
    default_value->accept (*this);
}

void tree_walker::visit_arg_size_spec (tree_arg_size_spec& spec)
{
  tree_argument_list *size_args = spec.size_args ();

  if (size_args)
    size_args->accept (*this);
}

void tree_walker::visit_arg_validation_fcns (tree_arg_validation_fcns& spec)
{
  tree_argument_list *fcn_args = spec.fcn_args ();

  if (fcn_args)
    fcn_args->accept (*this);
}

void tree_walker::visit_binary_expression (tree_binary_expression& expr)
{
  tree_expression *op1 = expr.lhs ();

  if (op1)
    op1->accept (*this);

  tree_expression *op2 = expr.rhs ();

  if (op2)
    op2->accept (*this);
}

void tree_walker::visit_boolean_expression (tree_boolean_expression& expr)
{
  visit_binary_expression (expr);
}

void tree_walker::visit_compound_binary_expression (tree_compound_binary_expression& expr)
{
  visit_binary_expression (expr);
}

void tree_walker::visit_break_command (tree_break_command&)
{
  // Nothing to do.
}

void tree_walker::visit_colon_expression (tree_colon_expression& expr)
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

void tree_walker::visit_continue_command (tree_continue_command&)
{
  // Nothing to do.
}

void tree_walker::visit_decl_command (tree_decl_command& cmd)
{
  tree_decl_init_list *init_list = cmd.initializer_list ();

  if (init_list)
    init_list->accept (*this);
}

void tree_walker::visit_decl_elt (tree_decl_elt& cmd)
{
  tree_identifier *id = cmd.ident ();

  if (id)
    id->accept (*this);

  tree_expression *expr = cmd.expression ();

  if (expr)
    expr->accept (*this);
}

void tree_walker::visit_decl_init_list (tree_decl_init_list& lst)
{
  // FIXME: tree_decl_elt is not derived from tree, so should it
  // really have an accept method?

  for (tree_decl_elt *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_simple_for_command (tree_simple_for_command& cmd)
{
  tree_expression *lhs = cmd.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  tree_expression *maxproc = cmd.maxproc_expr ();

  if (maxproc)
    maxproc->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void tree_walker::visit_complex_for_command (tree_complex_for_command& cmd)
{
  tree_argument_list *lhs = cmd.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void tree_walker::visit_spmd_command (tree_spmd_command& cmd)
{
  tree_statement_list *body = cmd.body ();

  if (body)
    body->accept (*this);
}

void tree_walker::visit_octave_user_script (octave_user_script& fcn)
{
  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    cmd_list->accept (*this);
}

void tree_walker::visit_octave_user_function (octave_user_function& fcn)
{
  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    cmd_list->accept (*this);
}

void tree_walker::visit_function_def (tree_function_def& fdef)
{
  octave_value fcn = fdef.function ();

  octave_function *f = fcn.function_value ();

  if (f)
    f->accept (*this);
}

void tree_walker::visit_identifier (tree_identifier&)
{
  // Nothing to do.
}

void tree_walker::visit_if_clause (tree_if_clause& cmd)
{
  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.commands ();

  if (list)
    list->accept (*this);
}

void tree_walker::visit_if_command (tree_if_command& cmd)
{
  tree_if_command_list *list = cmd.cmd_list ();

  if (list)
    list->accept (*this);
}

void tree_walker::visit_if_command_list (tree_if_command_list& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_if_clause *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_switch_case (tree_switch_case& cs)
{
  tree_expression *label = cs.case_label ();

  if (label)
    label->accept (*this);

  tree_statement_list *list = cs.commands ();

  if (list)
    list->accept (*this);
}

void tree_walker::visit_switch_case_list (tree_switch_case_list& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_switch_case *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_switch_command (tree_switch_command& cmd)
{
  tree_expression *expr = cmd.switch_value ();

  if (expr)
    expr->accept (*this);

  tree_switch_case_list *list = cmd.case_list ();

  if (list)
    list->accept (*this);
}

void tree_walker::visit_index_expression (tree_index_expression& expr)
{
  tree_expression *e = expr.expression ();

  if (e)
    e->accept (*this);

  std::list<tree_argument_list *> arg_lists = expr.arg_lists ();
  std::list<string_vector> arg_names = expr.arg_names ();
  std::list<tree_expression *> dyn_fields = expr.dyn_fields ();

  auto p_arg_lists = arg_lists.begin ();
  auto p_arg_names = arg_names.begin ();
  auto p_dyn_fields = dyn_fields.begin ();

  std::string type_tags = expr.type_tags ();
  int n = type_tags.length ();

  for (int i = 0; i < n; i++)
    {
      switch (type_tags[i])
        {
        case '(':
        case '{':
          {
            tree_argument_list *l = *p_arg_lists;
            if (l)
              l->accept (*this);
          }
          break;

        case '.':
          {
            std::string fn = (*p_arg_names)(0);
            if (fn.empty ())
              {
                tree_expression *df = *p_dyn_fields;
                if (df)
                  df->accept (*this);
              }
          }
          break;

        default:
          panic_impossible ();
        }

      p_arg_lists++;
      p_arg_names++;
      p_dyn_fields++;
    }
}

void tree_walker::visit_matrix (tree_matrix& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_argument_list *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_cell (tree_cell& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_argument_list *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_multi_assignment (tree_multi_assignment& expr)
{
  tree_argument_list *lhs = expr.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);
}

void tree_walker::visit_no_op_command (tree_no_op_command&)
{
  // Nothing to do.
}

void tree_walker::visit_constant (tree_constant&)
{
  // Nothing to do.
}

void tree_walker::visit_fcn_handle (tree_fcn_handle&)
{
  // Nothing to do.
}

void tree_walker::visit_parameter_list (tree_parameter_list& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_decl_elt *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_postfix_expression (tree_postfix_expression& expr)
{
  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);
}

void tree_walker::visit_prefix_expression (tree_prefix_expression& expr)
{
  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);
}

void tree_walker::visit_return_command (tree_return_command&)
{
  // Nothing to do.
}

void tree_walker::visit_simple_assignment (tree_simple_assignment& expr)
{
  tree_expression *lhs = expr.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);
}

void tree_walker::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();

  if (cmd)
    cmd->accept (*this);
  else
    {
      tree_expression *expr = stmt.expression ();

      if (expr)
        expr->accept (*this);
    }
}

void tree_walker::visit_statement_list (tree_statement_list& lst)
{
  for (tree_statement *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_try_catch_command (tree_try_catch_command& cmd)
{
  tree_statement_list *try_code = cmd.body ();

  if (try_code)
    try_code->accept (*this);

  tree_identifier *expr_id = cmd.identifier ();

  if (expr_id)
    expr_id->accept (*this);

  tree_statement_list *catch_code = cmd.cleanup ();

  if (catch_code)
    catch_code->accept (*this);
}

void tree_walker::visit_unwind_protect_command (tree_unwind_protect_command& cmd)
{
  tree_statement_list *unwind_protect_code = cmd.body ();

  if (unwind_protect_code)
    unwind_protect_code->accept (*this);

  tree_statement_list *cleanup_code = cmd.cleanup ();

  if (cleanup_code)
    cleanup_code->accept (*this);
}

void tree_walker::visit_while_command (tree_while_command& cmd)
{
  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void tree_walker::visit_do_until_command (tree_do_until_command& cmd)
{
  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);

  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);
}

void tree_walker::visit_superclass_ref (tree_superclass_ref&)
{
  // Nothing to do.
}

void tree_walker::visit_metaclass_query (tree_metaclass_query&)
{
  // Nothing to do.
}

void tree_walker::visit_classdef_attribute (tree_classdef_attribute& attr)
{
  tree_identifier *id = attr.ident ();

  if (id)
    id->accept (*this);

  tree_expression *expr = attr.expression ();

  if (expr)
    expr->accept (*this);
}

void tree_walker::visit_classdef_attribute_list (tree_classdef_attribute_list& lst)
{
  for (auto *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_classdef_superclass (tree_classdef_superclass&)
{
  // Nothing to do.
}

void tree_walker::visit_classdef_superclass_list (tree_classdef_superclass_list& lst)
{
  for (auto *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_classdef_property (tree_classdef_property&)
{
  // FIXME: Should we operate on the tree_arg_validation object or the
  // identifier and expression parts separately?
}

void tree_walker::visit_classdef_property_list (tree_classdef_property_list& lst)
{
  for (auto *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_classdef_properties_block (tree_classdef_properties_block& blk)
{
  tree_classdef_property_list *property_list = blk.element_list ();

  if (property_list)
    property_list->accept (*this);
}

void tree_walker::visit_classdef_methods_list (tree_classdef_methods_list& lst)
{
  for (auto ov_meth : lst)
    {
      octave_user_function *meth = ov_meth.user_function_value ();

      if (meth)
        meth->accept (*this);
    }
}

void tree_walker::visit_classdef_methods_block (tree_classdef_methods_block& blk)
{
  tree_classdef_methods_list *methods_list = blk.element_list ();

  if (methods_list)
    methods_list->accept (*this);
}

void tree_walker::visit_classdef_event (tree_classdef_event&)
{
  // Nothing to do.
}

void tree_walker::visit_classdef_events_list (tree_classdef_events_list& lst)
{
  for (auto *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_classdef_events_block (tree_classdef_events_block& blk)
{
  tree_classdef_events_list *events_list = blk.element_list ();

  if (events_list)
    events_list->accept (*this);
}

void tree_walker::visit_classdef_enum (tree_classdef_enum&)
{
  // Nothing to do.
}

void tree_walker::visit_classdef_enum_list (tree_classdef_enum_list& lst)
{
  for (auto *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_classdef_enum_block (tree_classdef_enum_block& blk)
{
  tree_classdef_enum_list *enum_list = blk.element_list ();

  if (enum_list)
    enum_list->accept (*this);
}

void tree_walker::visit_classdef_body (tree_classdef_body& body)
{
  for (auto *elt : body.properties_list ())
    {
      if (elt)
        elt->accept (*this);
    }

  for (auto *elt : body.methods_list ())
    {
      if (elt)
        elt->accept (*this);
    }


  for (auto *elt : body.events_list ())
    {
      if (elt)
        elt->accept (*this);
    }

  for (auto *elt : body.enum_list ())
    {
      if (elt)
        elt->accept (*this);
    }
}

void tree_walker::visit_classdef (tree_classdef& cdef)
{
  tree_classdef_attribute_list *attribute_list = cdef.attribute_list ();

  if (attribute_list)
    attribute_list->accept (*this);

  tree_identifier *ident = cdef.ident ();

  if (ident)
    ident->accept (*this);

  tree_classdef_superclass_list *superclass_list = cdef.superclass_list ();

  if (superclass_list)
    superclass_list->accept (*this);

  tree_classdef_body *body = cdef.body ();

  if (body)
    body->accept (*this);
}

OCTAVE_END_NAMESPACE(octave)
