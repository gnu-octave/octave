////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023 The Octave Project Developers
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

#if ! defined (octave_pt_bytecode_walk_h)
#define octave_pt_bytecode_walk_h 1

#include <map>

#include "octave-config.h"
#include "pt-walk.h"
#include "error.h"

#include "pt-bytecode-vm.h"

class octave_user_script;
class octave_user_function;

#define ERROR_NOT_IMPLEMENTED \
{ error("Not implemented %s:%d", __FILE__, __LINE__); }

namespace octave
{
  void compile_user_function (octave_user_code &ufn, bool do_print);
  void compile_nested_user_function (octave_user_function &ufn, bool do_print, std::vector<octave_user_function *> v_parent_fns);
  void compile_anon_user_function (octave_user_code &ufn, bool do_print, stack_frame::local_vars_map &locals);

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
  //class tree_simple_index_expression;
  class tree_statement;
  //class tree_statement_cmd;
  //class tree_statement_expression;
  //class tree_statement_null;
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

  struct id_being_indexed
  {
    int slot;
    int idx;
    int nargs;
    int type;
  };

  class bytecode_walker : public tree_walker
  {
  public:

    enum class nesting_statement
      {
        INVALID,
        FOR_LOOP,
        ONE_OV_ON_STACK,
      };

    bytecode_walker (void) { }

    virtual ~bytecode_walker (void) = default;

    // The bytecode will be put in this container
    bytecode m_code;
    // The bytecode need its own scope object that will
    // be written back to the octave_user_function object
    symbol_scope m_scope;

    bool m_varargout = false;
    bool m_is_script = false;
    bool m_is_anon = false;
    int m_n_nested_fn = 0;
    std::vector<octave_user_function*> m_v_parent_fns; // Parent functions for nested functions

    std::vector<std::vector<int>> m_continue_target;
    std::vector<std::vector<int>> m_need_break_target;
    std::vector<int> m_loop_target;
    std::vector<bool> m_all_paths_terminated;
    std::vector<int> m_nargout;
    std::vector<std::vector<int>> m_need_unwind_target;

    std::vector<nesting_statement> m_nesting_statement;

    // For "end" in indexing expression we need to know what variable is
    // being indexed.
    std::vector<id_being_indexed> m_indexed_id;

    int m_depth = 0;
    int m_offset_n_locals = -1;
    int m_n_locals = 0;
    int m_n_nested_loops = 0;

    // Counter to choose different alternative op-codes in a try to help branch prediction
    int m_cnt_alts_cst = 0;
    int m_cnt_alts_mul = 0;
    int m_cnt_alts_add = 0;
    int m_cnt_alts_div = 0;

    // Simple way to keep down amount of temporary slots made to store results
    int m_n_multi_assign = 0;

    // Need to keep track of ignored outputs with the '~'
    bool m_pending_ignore_outputs = false;
    int m_ignored_of_total = 0;
    std::vector<int> m_v_ignored;
    int m_ignored_ip_start = 0;

    // The values that the locals of an anonymous function are supposed
    // to be set to. The field is set before calling the first accept()
    // for anonymous functions.
    stack_frame::local_vars_map *m_anon_local_values = nullptr;

    // TODO: Kludge alert. Mirror the behaviour in ov_classdef::subsref
    // where under certain conditions a magic number nargout of -1 is
    // expected to  maybe return a cs-list. "-1" in this context 
    // does not have the same meaning as in the VM, where it means
    // a varargout with only one return symbol 'varargout'.
    //
    // We need to track "unknown nargout" for this.
    int m_unknown_nargout = 0;

    //
    bool m_is_folding = false;
    std::vector<tree*> m_v_trees_to_fold;
    std::vector<int> m_v_offset_of_folds;
    int m_n_folds = 0;

    std::map<std::string, int> m_map_locals_to_slot;

    std::map<std::string, bool> m_map_id_is_global;
    std::map<std::string, bool> m_map_id_is_persistent;

    static std::map<std::string, octave_base_value::unary_mapper_t> m_name_to_unary_func;

    int add_id_to_table (std::string name);

    int n_on_stack_due_to_stmt ();

    void emit_return ();

    void emit_alt (int &cntr, std::vector<INSTR> alts);

    void emit_load_2_cst (tree_expression *lhs, tree_expression *rhs);

    void maybe_emit_anon_maybe_ignore_outputs ();
    void maybe_emit_bind_ans_and_disp (tree_expression &expr, const std::string maybe_cmd_name = "");
    void maybe_emit_disp_id (tree_expression &expr, const std::string &name, const std::string maybe_cmd_name = "" );
    void maybe_emit_push_and_disp_id (tree_expression &expr, const std::string &name, const std::string maybe_cmd_name = "");
    void emit_disp_obj (tree_expression &expr);

    int get_slot (std::string name)
    {
      auto it = m_map_locals_to_slot.find (name);
      if (it == m_map_locals_to_slot.end ())
        error ("VM internal error: Slot %s does not exist", name.c_str ());
      return it->second;
    }

    void ctor_unary_map ();

    // No copying!

    bytecode_walker (const bytecode_walker&) = delete;

    bytecode_walker& operator = (const bytecode_walker&) = delete;

    void visit_anon_fcn_handle (tree_anon_fcn_handle&);

    void visit_argument_list (tree_argument_list&) ERROR_NOT_IMPLEMENTED

    void visit_arguments_block (tree_arguments_block&) ERROR_NOT_IMPLEMENTED

    void visit_args_block_attribute_list (tree_args_block_attribute_list&)
      ERROR_NOT_IMPLEMENTED

    void visit_args_block_validation_list (tree_args_block_validation_list&)
      ERROR_NOT_IMPLEMENTED

    void visit_arg_validation (tree_arg_validation&) ERROR_NOT_IMPLEMENTED

    void visit_arg_size_spec (tree_arg_size_spec&) ERROR_NOT_IMPLEMENTED

    void visit_arg_validation_fcns (tree_arg_validation_fcns&)
      ERROR_NOT_IMPLEMENTED

    void visit_binary_expression (tree_binary_expression&);

    void visit_boolean_expression (tree_boolean_expression&);

    void visit_compound_binary_expression (tree_compound_binary_expression&);

    void visit_break_command (tree_break_command&);

    void visit_colon_expression (tree_colon_expression&);

    void visit_continue_command (tree_continue_command&);

    void visit_decl_command (tree_decl_command&);

    void visit_decl_elt (tree_decl_elt&) ERROR_NOT_IMPLEMENTED

    void visit_decl_init_list (tree_decl_init_list&) ERROR_NOT_IMPLEMENTED

    void visit_simple_for_command (tree_simple_for_command&);

    void visit_complex_for_command (tree_complex_for_command&);

    void visit_spmd_command (tree_spmd_command&) ERROR_NOT_IMPLEMENTED

    void visit_octave_user_script (octave_user_script&);

    void visit_octave_user_function (octave_user_function&);

    void visit_function_def (tree_function_def&);

    void visit_identifier (tree_identifier&);

    void visit_if_clause (tree_if_clause&) ERROR_NOT_IMPLEMENTED

    void visit_if_command (tree_if_command&);

    void visit_if_command_list (tree_if_command_list&) ERROR_NOT_IMPLEMENTED

    void visit_switch_case (tree_switch_case&) ERROR_NOT_IMPLEMENTED

    void visit_switch_case_list (tree_switch_case_list&) ERROR_NOT_IMPLEMENTED

    void visit_switch_command (tree_switch_command&);

    // Helper functions
    void
    emit_args_for_visit_index_expression (tree_argument_list *arg_list,
                                          tree_expression *lhs_root);

    void
    emit_fields_for_visit_index_expression (string_vector &field_names,
                                            tree_expression *dyn_expr,
                                            tree_expression *lhs_root,
                                            bool *struct_is_id_dot_id);

    void simple_visit_index_expression (tree_index_expression&);
    void eval_visit_index_expression (tree_index_expression&);

    void visit_index_expression (tree_index_expression&);

    //void visit_simple_index_expression (tree_simple_index_expression&);

    void visit_matrix (tree_matrix&);

    void visit_cell (tree_cell&);

    void visit_multi_assignment (tree_multi_assignment&);

    void visit_no_op_command (tree_no_op_command&);

    void visit_constant (tree_constant&);

    void visit_fcn_handle (tree_fcn_handle&);

    void visit_parameter_list (tree_parameter_list&) ERROR_NOT_IMPLEMENTED

    void visit_postfix_expression (tree_postfix_expression&);

    void visit_prefix_expression (tree_prefix_expression&);

    void visit_return_command (tree_return_command&);

    void visit_simple_assignment (tree_simple_assignment&);

    void visit_statement (tree_statement&);

    void visit_statement_list (tree_statement_list&);

    void visit_try_catch_command (tree_try_catch_command&);

    void emit_unwind_protect_code (tree_statement_list *body,
                                   tree_statement_list *cleanup_code,
                                   tree_expression *body_expr = nullptr,
                                   tree_expression *cleanup_expr = nullptr,
                                   std::vector<int> cleanup_instructions = {});

    struct emit_unwind_protect_data
    {
      int m_idx_unwind;
      bool m_break_stack_populated;
      std::vector<int> m_v_need_breaks_initial;
      int m_n_need_break;
      int m_n_need_cleanup;
    };

    emit_unwind_protect_data emit_unwind_protect_code_start ();
    void emit_unwind_protect_code_before_cleanup (emit_unwind_protect_data &data);
    void emit_unwind_protect_code_end (emit_unwind_protect_data &data);

    void visit_unwind_protect_command (tree_unwind_protect_command&);

    void visit_while_command (tree_while_command&);

    void visit_do_until_command (tree_do_until_command&);

    void visit_superclass_ref (tree_superclass_ref&) ERROR_NOT_IMPLEMENTED

    void visit_metaclass_query (tree_metaclass_query&) ERROR_NOT_IMPLEMENTED

    void visit_classdef_attribute (tree_classdef_attribute&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_attribute_list (tree_classdef_attribute_list&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_superclass (tree_classdef_superclass&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_superclass_list (tree_classdef_superclass_list&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_property (tree_classdef_property&) ERROR_NOT_IMPLEMENTED

    void visit_classdef_property_list (tree_classdef_property_list&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_properties_block (tree_classdef_properties_block&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_methods_list (tree_classdef_methods_list&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_methods_block (tree_classdef_methods_block&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_event (tree_classdef_event&) ERROR_NOT_IMPLEMENTED

    void visit_classdef_events_list (tree_classdef_events_list&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_events_block (tree_classdef_events_block&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_enum (tree_classdef_enum&) ERROR_NOT_IMPLEMENTED

    void visit_classdef_enum_list (tree_classdef_enum_list&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_enum_block (tree_classdef_enum_block&)
      ERROR_NOT_IMPLEMENTED

    void visit_classdef_body (tree_classdef_body&) ERROR_NOT_IMPLEMENTED

    void visit_classdef (tree_classdef&) ERROR_NOT_IMPLEMENTED
  };
}

#endif
