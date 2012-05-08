/*

Copyright (C) 2012 Max Brister <max@2bass.com>

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

#if !defined (octave_tree_jit_h)
#define octave_tree_jit_h 1

#include <map>
#include <stdexcept>
#include <vector>

#include "pt-walk.h"

class jit_fail_exception : public std::exception {};

// LLVM forward declares
namespace llvm
{
  class Value;
  class Module;
  class FunctionPassManager;
  class ExecutionEngine;
  class Function;
  class BasicBlock;
  class LLVMContext;
}

class tree;

class
OCTINTERP_API
tree_jit : private tree_walker
{
public:
  tree_jit (void);

  ~tree_jit (void);

  bool execute (tree& tee);
 private:
  typedef void (*jit_function)(bool*, double*);

  class function_info
  {
  public:
    function_info (void);
    function_info (jit_function fn, const std::vector<std::string>& args,
                   const std::vector<bool>& args_used);

    bool execute ();
  private:
    jit_function function;
    std::vector<std::string> arguments;
    
    // is the argument used? or is it just declared?
    std::vector<bool> argument_used;
  };

  struct variable_info
  {
    llvm::Value *defined;
    llvm::Value *value;
    bool use;
  };

  function_info *compile (tree& tee);

  variable_info find (const std::string &name, bool use);

  void do_assign (variable_info vinfo, llvm::Value *value);

  void emit_print (const std::string& vname, llvm::Value *value);

  // tree_walker
  void visit_anon_fcn_handle (tree_anon_fcn_handle&);

  void visit_argument_list (tree_argument_list&);

  void visit_binary_expression (tree_binary_expression&);

  void visit_break_command (tree_break_command&);

  void visit_colon_expression (tree_colon_expression&);

  void visit_continue_command (tree_continue_command&);

  void visit_global_command (tree_global_command&);

  void visit_persistent_command (tree_persistent_command&);

  void visit_decl_elt (tree_decl_elt&);

  void visit_decl_init_list (tree_decl_init_list&);

  void visit_simple_for_command (tree_simple_for_command&);

  void visit_complex_for_command (tree_complex_for_command&);

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

  void visit_return_list (tree_return_list&);

  void visit_simple_assignment (tree_simple_assignment&);

  void visit_statement (tree_statement&);

  void visit_statement_list (tree_statement_list&);

  void visit_switch_case (tree_switch_case&);

  void visit_switch_case_list (tree_switch_case_list&);

  void visit_switch_command (tree_switch_command&);

  void visit_try_catch_command (tree_try_catch_command&);

  void do_unwind_protect_cleanup_code (tree_statement_list *list);

  void visit_unwind_protect_command (tree_unwind_protect_command&);

  void visit_while_command (tree_while_command&);

  void visit_do_until_command (tree_do_until_command&);

  void fail (void);

  typedef std::map<std::string, variable_info> var_map;
  typedef var_map::iterator var_map_iterator;
  typedef std::map<tree*, function_info*> finfo_map;
  typedef finfo_map::iterator finfo_map_iterator;

  std::vector<llvm::Value*> value_stack;
  var_map variables;
  finfo_map compiled_functions;

  llvm::LLVMContext &context;
  llvm::Module *module;
  llvm::FunctionPassManager *pass_manager;
  llvm::ExecutionEngine *engine;
  llvm::BasicBlock *entry_block;

  llvm::Function *print_double;
};

#endif
