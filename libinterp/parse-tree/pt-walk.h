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

#if ! defined (octave_pt_walk_h)
#define octave_pt_walk_h 1

#include "octave-config.h"

class octave_user_script;
class octave_user_function;

OCTAVE_BEGIN_NAMESPACE(octave)

// No separate visitor needed
// Base classes only, so no need to include them.
//
//  class tree_array_list
//  class tree_unary_expression
//  class tree_black_hole

class tree_anon_fcn_handle;
class tree_arg_size_spec;
class tree_arg_validation;
class tree_arg_validation_fcns;
class tree_args_block_attribute_list;
class tree_args_block_validation_list;
class tree_argument_list;
class tree_arguments_block;
class tree_binary_expression;
class tree_boolean_expression;
class tree_compound_binary_expression;
class tree_break_command;
class tree_colon_expression;
class tree_continue_command;
class tree_decl_command;
class tree_decl_init_list;
class tree_decl_elt;
class tree_simple_for_command;
class tree_complex_for_command;
class tree_spmd_command;
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
class tree_simple_assignment;
class tree_statement;
class tree_statement_list;
class tree_try_catch_command;
class tree_unwind_protect_command;
class tree_while_command;
class tree_do_until_command;

class tree_superclass_ref;
class tree_metaclass_query;
class tree_classdef_attribute;
class tree_classdef_attribute_list;
class tree_classdef_superclass;
class tree_classdef_superclass_list;
class tree_classdef_property;
class tree_classdef_property_list;
class tree_classdef_properties_block;
class tree_classdef_methods_list;
class tree_classdef_methods_block;
class tree_classdef_event;
class tree_classdef_events_list;
class tree_classdef_events_block;
class tree_classdef_enum;
class tree_classdef_enum_list;
class tree_classdef_enum_block;
class tree_classdef_body;
class tree_classdef;

class tree_walker
{
protected:

  tree_walker (void) { }

  virtual ~tree_walker (void) = default;

public:

  // No copying!

  tree_walker (const tree_walker&) = delete;

  tree_walker& operator = (const tree_walker&) = delete;

  virtual void visit_anon_fcn_handle (tree_anon_fcn_handle&);

  virtual void visit_argument_list (tree_argument_list&);

  virtual void visit_arguments_block (tree_arguments_block&);

  virtual void visit_args_block_attribute_list (tree_args_block_attribute_list&);

  virtual void visit_args_block_validation_list (tree_args_block_validation_list&);

  virtual void visit_arg_validation (tree_arg_validation&);

  virtual void visit_arg_size_spec (tree_arg_size_spec&);

  virtual void visit_arg_validation_fcns (tree_arg_validation_fcns&);

  virtual void visit_binary_expression (tree_binary_expression&);

  virtual void visit_boolean_expression (tree_boolean_expression&);

  virtual void visit_compound_binary_expression (tree_compound_binary_expression&);

  virtual void visit_break_command (tree_break_command&);

  virtual void visit_colon_expression (tree_colon_expression&);

  virtual void visit_continue_command (tree_continue_command&);

  virtual void visit_decl_command (tree_decl_command&);

  virtual void visit_decl_elt (tree_decl_elt&);

  virtual void visit_decl_init_list (tree_decl_init_list&);

  virtual void visit_simple_for_command (tree_simple_for_command&);

  virtual void visit_complex_for_command (tree_complex_for_command&);

  virtual void visit_spmd_command (tree_spmd_command&);

  virtual void visit_octave_user_script (octave_user_script&);

  virtual void visit_octave_user_function (octave_user_function&);

  virtual void visit_function_def (tree_function_def&);

  virtual void visit_identifier (tree_identifier&);

  virtual void visit_if_clause (tree_if_clause&);

  virtual void visit_if_command (tree_if_command&);

  virtual void visit_if_command_list (tree_if_command_list&);

  virtual void visit_switch_case (tree_switch_case&);

  virtual void visit_switch_case_list (tree_switch_case_list&);

  virtual void visit_switch_command (tree_switch_command&);

  virtual void visit_index_expression (tree_index_expression&);

  virtual void visit_matrix (tree_matrix&);

  virtual void visit_cell (tree_cell&);

  virtual void visit_multi_assignment (tree_multi_assignment&);

  virtual void visit_no_op_command (tree_no_op_command&);

  virtual void visit_constant (tree_constant&);

  virtual void visit_fcn_handle (tree_fcn_handle&);

  virtual void visit_parameter_list (tree_parameter_list&);

  virtual void visit_postfix_expression (tree_postfix_expression&);

  virtual void visit_prefix_expression (tree_prefix_expression&);

  virtual void visit_return_command (tree_return_command&);

  virtual void visit_simple_assignment (tree_simple_assignment&);

  virtual void visit_statement (tree_statement&);

  virtual void visit_statement_list (tree_statement_list&);

  virtual void visit_try_catch_command (tree_try_catch_command&);

  virtual void visit_unwind_protect_command (tree_unwind_protect_command&);

  virtual void visit_while_command (tree_while_command&);

  virtual void visit_do_until_command (tree_do_until_command&);

  virtual void visit_superclass_ref (tree_superclass_ref&);

  virtual void visit_metaclass_query (tree_metaclass_query&);

  virtual void visit_classdef_attribute (tree_classdef_attribute&);

  virtual void visit_classdef_attribute_list (tree_classdef_attribute_list&);

  virtual void visit_classdef_superclass (tree_classdef_superclass&);

  virtual void visit_classdef_superclass_list (tree_classdef_superclass_list&);

  virtual void visit_classdef_property (tree_classdef_property&);

  virtual void visit_classdef_property_list (tree_classdef_property_list&);

  virtual void visit_classdef_properties_block (tree_classdef_properties_block&);

  virtual void visit_classdef_methods_list (tree_classdef_methods_list&);

  virtual void visit_classdef_methods_block (tree_classdef_methods_block&);

  virtual void visit_classdef_event (tree_classdef_event&);

  virtual void visit_classdef_events_list (tree_classdef_events_list&);

  virtual void visit_classdef_events_block (tree_classdef_events_block&);

  virtual void visit_classdef_enum (tree_classdef_enum&);

  virtual void visit_classdef_enum_list (tree_classdef_enum_list&);

  virtual void visit_classdef_enum_block (tree_classdef_enum_block&);

  virtual void visit_classdef_body (tree_classdef_body&);

  virtual void visit_classdef (tree_classdef&);
};

OCTAVE_END_NAMESPACE(octave)

#endif
