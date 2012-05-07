/*

Copyright (C) 2009-2012 John W. Eaton

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

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "pt-jit.h"

#include <typeinfo>

#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/BasicBlock.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/PassManager.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_os_ostream.h>

#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pt-all.h"

using namespace llvm;

//FIXME: Move into tree_jit
static IRBuilder<> builder (getGlobalContext ());

extern "C" void
octave_print_double (const char *name, double value)
{
  // FIXME: We should avoid allocating a new octave_scalar each time
  octave_value ov (value);
  ov.print_with_name (octave_stdout, name);
}

tree_jit::tree_jit (void) : context (getGlobalContext ()), engine (0)
{
  InitializeNativeTarget ();
  InitializeNativeTargetAsmPrinter ();
  module = new Module ("octave", context);
}

tree_jit::~tree_jit (void)
{
  delete module;
}

bool
tree_jit::execute (tree& tee)
{
  if (!engine)
    {
      engine = ExecutionEngine::createJIT (module);

      // initialize pass manager
      pass_manager = new FunctionPassManager (module);
      pass_manager->add (new TargetData(*engine->getTargetData ()));
      pass_manager->add (createBasicAliasAnalysisPass ());
      pass_manager->add (createPromoteMemoryToRegisterPass ());
      pass_manager->add (createInstructionCombiningPass ());
      pass_manager->add (createReassociatePass ());
      pass_manager->add (createGVNPass ());
      pass_manager->add (createCFGSimplificationPass ());
      pass_manager->doInitialization ();

      // create external functions
      Type *vtype = Type::getVoidTy (context);
      std::vector<Type*> pd_args (2);
      pd_args[0] = Type::getInt8PtrTy (context);
      pd_args[1] = Type::getDoubleTy (context);
      FunctionType *print_double_ty = FunctionType::get (vtype, pd_args, false);
      print_double = Function::Create (print_double_ty,
                                       Function::ExternalLinkage,
                                       "octave_print_double", module);
      engine->addGlobalMapping (print_double,
                                reinterpret_cast<void*>(&octave_print_double));
    }

  if (!engine)
    // sometimes this fails during early initialization
    return false;

  // find function
  function_info *finfo;
  finfo_map_iterator iter = compiled_functions.find (&tee);

  if (iter == compiled_functions.end ())
    finfo = compile (tee);
  else
    finfo = iter->second;

  return finfo->execute ();
}

tree_jit::function_info*
tree_jit::compile (tree& tee)
{
  value_stack.clear ();
  variables.clear ();
  
  // setup function
  std::vector<Type*> args (2);
  args[0] = Type::getInt1PtrTy (context);
  args[1] = Type::getDoublePtrTy (context);
  FunctionType *ft = FunctionType::get (Type::getVoidTy (context), args, false);
  Function *compiling = Function::Create (ft, Function::ExternalLinkage,
                                          "test_fn", module);

  entry_block = BasicBlock::Create (context, "entry", compiling);
  BasicBlock *body = BasicBlock::Create (context, "body",
                                         compiling);
  builder.SetInsertPoint (body);

  // convert tree to LLVM IR
  try
    {
      tee.accept (*this);
    }
  catch (const jit_fail_exception&)
    {
      //FIXME: cleanup
      return  compiled_functions[&tee] = new function_info ();
    }

  // copy input arguments
  builder.SetInsertPoint (entry_block);

  Function::arg_iterator arg_iter = compiling->arg_begin ();
  Value *arg_defined = arg_iter;
  Value *arg_value = ++arg_iter;

  arg_defined->setName ("arg_defined");
  arg_value->setName ("arg_value");

  size_t idx = 0;
  std::vector<std::string> arg_names;
  std::vector<bool> arg_used;
  for (var_map_iterator iter = variables.begin (); iter != variables.end ();
       ++iter, ++idx)
    {
      arg_names.push_back (iter->first);
      arg_used.push_back (iter->second.use);

      Value *gep_defined = builder.CreateConstInBoundsGEP1_32 (arg_defined, idx);
      Value *defined = builder.CreateLoad (gep_defined);
      builder.CreateStore (defined, iter->second.defined);

      Value *gep_value = builder.CreateConstInBoundsGEP1_32 (arg_value, idx);
      Value *value = builder.CreateLoad (gep_value);
      builder.CreateStore (value, iter->second.value);
    }
  builder.CreateBr (body);

  // copy output arguments
  BasicBlock *cleanup = BasicBlock::Create (context, "cleanup", compiling);
  builder.SetInsertPoint (body);
  builder.CreateBr (cleanup);
  builder.SetInsertPoint (cleanup);

  idx = 0;
  for (var_map_iterator iter = variables.begin (); iter != variables.end ();
       ++iter, ++idx)
    {
      Value *gep_defined = builder.CreateConstInBoundsGEP1_32 (arg_defined, idx);
      Value *defined = builder.CreateLoad (iter->second.defined);
      builder.CreateStore (defined, gep_defined);

      Value *gep_value = builder.CreateConstInBoundsGEP1_32 (arg_value, idx);
      Value *value = builder.CreateLoad (iter->second.value, iter->first);
      builder.CreateStore (value, gep_value);
    }

  builder.CreateRetVoid ();

  // print what we compiled (for debugging)
  // we leave this in for now, as other people might want to view the ir created
  // should be removed eventually though
  const bool debug_print_ir = false;
  if (debug_print_ir)
    {
      raw_os_ostream os (std::cout);
      std::cout << "Compiling --------------------\n";
      tree_print_code tpc (std::cout);
      std::cout << typeid (tee).name () << std::endl;
      tee.accept (tpc);
      std::cout << "\n--------------------\n";

      std::cout << "llvm_ir\n";
      compiling->print (os);
      std::cout << "--------------------\n";
    }
  
  // compile code
  verifyFunction (*compiling);
  pass_manager->run (*compiling);

  if (debug_print_ir)
    {
      raw_os_ostream os (std::cout);
      std::cout << "optimized llvm_ir\n";
      compiling->print (os);
      std::cout << "--------------------\n";
    }

  jit_function fun =
    reinterpret_cast<jit_function> (engine->getPointerToFunction (compiling));

  return compiled_functions[&tee] = new function_info (fun, arg_names, arg_used);
}

tree_jit::variable_info
tree_jit::find (const std::string &name, bool use)
{
  var_map_iterator iter = variables.find (name);
  if (iter == variables.end ())
    {
      // we currently just assume everything is a double
      Type *dbl = Type::getDoubleTy (context);
      Type *bol = Type::getInt1Ty (context);
      IRBuilder<> tmpB (entry_block, entry_block->begin ());

      variable_info vinfo;
      vinfo.defined = tmpB.CreateAlloca (bol, 0);
      vinfo.value = tmpB.CreateAlloca (dbl, 0, name);
      vinfo.use = use;
      variables[name] = vinfo;
      return vinfo;
    }
  else
    {
      iter->second.use = iter->second.use || use;
      return iter->second;
    }
}

void
tree_jit::do_assign (variable_info vinfo, llvm::Value *value)
{
  // create assign expression
  Value *result = builder.CreateStore (value, vinfo.value);
  value_stack.push_back (result);

  // update defined for lhs
  Type *btype = Type::getInt1Ty (context);
  Value *btrue = ConstantInt::get (btype, APInt (1, 1));
  builder.CreateStore (btrue, vinfo.defined);
}

void
tree_jit::emit_print (const std::string& vname, llvm::Value *value)
{
  Value *pname = builder.CreateGlobalStringPtr (vname);
  builder.CreateCall2 (print_double, pname, value);
}

void
tree_jit::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  fail ();
}

void
tree_jit::visit_argument_list (tree_argument_list&)
{
  fail ();
}

void
tree_jit::visit_binary_expression (tree_binary_expression& be)
{
  tree_expression *lhs = be.lhs ();
  tree_expression *rhs = be.rhs ();
  if (lhs && rhs)
    {
      lhs->accept (*this);
      rhs->accept (*this);

      Value *lhsv = value_stack.back ();
      value_stack.pop_back ();

      Value *rhsv = value_stack.back ();
      value_stack.pop_back ();

      Value *result;
      switch (be.op_type ())
        {
        case octave_value::op_add:
          result = builder.CreateFAdd (lhsv, rhsv);
          break;
        case octave_value::op_sub:
          result = builder.CreateFSub (lhsv, rhsv);
          break;
        case octave_value::op_mul:
          result = builder.CreateFMul (lhsv, rhsv);
          break;
        case octave_value::op_div:
          result = builder.CreateFDiv (lhsv, rhsv);
          break;
        default:
          fail ();
        }

      value_stack.push_back (result);
    }
  else
    fail ();
}

void
tree_jit::visit_break_command (tree_break_command&)
{
  fail ();
}

void
tree_jit::visit_colon_expression (tree_colon_expression&)
{
  fail ();
}

void
tree_jit::visit_continue_command (tree_continue_command&)
{
  fail ();
}

void
tree_jit::visit_global_command (tree_global_command&)
{
  fail ();
}

void
tree_jit::visit_persistent_command (tree_persistent_command&)
{
  fail ();
}

void
tree_jit::visit_decl_elt (tree_decl_elt&)
{
  fail ();
}

void
tree_jit::visit_decl_init_list (tree_decl_init_list&)
{
  fail ();
}

void
tree_jit::visit_simple_for_command (tree_simple_for_command&)
{
  fail ();
}

void
tree_jit::visit_complex_for_command (tree_complex_for_command&)
{
  fail ();
}

void
tree_jit::visit_octave_user_script (octave_user_script&)
{
  fail ();
}

void
tree_jit::visit_octave_user_function (octave_user_function&)
{
  fail ();
}

void
tree_jit::visit_octave_user_function_header (octave_user_function&)
{
  fail ();
}

void
tree_jit::visit_octave_user_function_trailer (octave_user_function&)
{
  fail ();
}

void
tree_jit::visit_function_def (tree_function_def&)
{
  fail ();
}

void
tree_jit::visit_identifier (tree_identifier& ti)
{
  octave_value ov = ti.do_lookup ();
  if (ov.is_function ())
    fail ();

  std::string name = ti.name ();
  variable_info vinfo = find (ti.name (), true);

  // TODO check defined

  Value *load_value = builder.CreateLoad (vinfo.value, name);
  value_stack.push_back (load_value);
}

void
tree_jit::visit_if_clause (tree_if_clause&)
{
  fail ();
}

void
tree_jit::visit_if_command (tree_if_command&)
{
  fail ();
}

void
tree_jit::visit_if_command_list (tree_if_command_list&)
{
  fail ();
}

void
tree_jit::visit_index_expression (tree_index_expression&)
{
  fail ();
}

void
tree_jit::visit_matrix (tree_matrix&)
{
  fail ();
}

void
tree_jit::visit_cell (tree_cell&)
{
  fail ();
}

void
tree_jit::visit_multi_assignment (tree_multi_assignment&)
{
  fail ();
}

void
tree_jit::visit_no_op_command (tree_no_op_command&)
{
  fail ();
}

void
tree_jit::visit_constant (tree_constant& tc)
{
  octave_value v = tc.rvalue1 ();
  if (v.is_real_scalar () && v.is_double_type ())
    {
      double dv = v.double_value ();
      Value *lv = ConstantFP::get (context,  APFloat (dv));
      value_stack.push_back (lv);
    }
  else
    fail ();
}

void
tree_jit::visit_fcn_handle (tree_fcn_handle&)
{
  fail ();
}

void
tree_jit::visit_parameter_list (tree_parameter_list&)
{
  fail ();
}

void
tree_jit::visit_postfix_expression (tree_postfix_expression&)
{
  fail ();
}

void
tree_jit::visit_prefix_expression (tree_prefix_expression&)
{
  fail ();
}

void
tree_jit::visit_return_command (tree_return_command&)
{
  fail ();
}

void
tree_jit::visit_return_list (tree_return_list&)
{
  fail ();
}

void
tree_jit::visit_simple_assignment (tree_simple_assignment& tsa)
{
  // only support an identifier as lhs
  tree_identifier *lhs = dynamic_cast<tree_identifier*> (tsa.left_hand_side ());
  if (!lhs)
    fail ();

  variable_info lhsv = find (lhs->name (), false);
  
  // resolve rhs as normal
  tree_expression *rhs = tsa.right_hand_side ();
  rhs->accept (*this);

  Value *rhsv = value_stack.back ();
  value_stack.pop_back ();

  do_assign (lhsv, rhsv);

  if (tsa.print_result ())
    emit_print (lhs->name (), rhsv);
}

void
tree_jit::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();
  tree_expression *expr = stmt.expression ();

  if (cmd)
    cmd->accept (*this);
  else
    {
      // TODO deal with printing

      // stolen from tree_evaluator::visit_statement
      bool do_bind_ans = false;

      if (expr->is_identifier ())
        {
          tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

          do_bind_ans = (! id->is_variable ());
        }
      else
        do_bind_ans = (! expr->is_assignment_expression ());

      expr->accept (*this);

      if (do_bind_ans)
        {
          Value *rhs = value_stack.back ();
          value_stack.pop_back ();

          variable_info ans = find ("ans", false);
          do_assign (ans, rhs);
        }
      else if (expr->is_identifier () && expr->print_result ())
        {
          // FIXME: ugly hack, we need to come up with a way to pass
          // nargout to visit_identifier
          emit_print (expr->name (), value_stack.back ());
        }


      value_stack.pop_back ();
    }
}

void
tree_jit::visit_statement_list (tree_statement_list&)
{
  fail ();
}

void
tree_jit::visit_switch_case (tree_switch_case&)
{
  fail ();
}

void
tree_jit::visit_switch_case_list (tree_switch_case_list&)
{
  fail ();
}

void
tree_jit::visit_switch_command (tree_switch_command&)
{
  fail ();
}

void
tree_jit::visit_try_catch_command (tree_try_catch_command&)
{
  fail ();
}

void
tree_jit::visit_unwind_protect_command (tree_unwind_protect_command&)
{
  fail ();
}

void
tree_jit::visit_while_command (tree_while_command&)
{
  fail ();
}

void
tree_jit::visit_do_until_command (tree_do_until_command&)
{
  fail ();
}

void
tree_jit::fail (void)
{
  throw jit_fail_exception ();
}

tree_jit::function_info::function_info (void) : function (0)
{}

tree_jit::function_info::function_info (jit_function fun,
                                        const std::vector<std::string>& args,
                                        const std::vector<bool>& arg_used) :
  function (fun), arguments (args), argument_used (arg_used)
{}

bool tree_jit::function_info::execute ()
{
  if (! function)
    return false;

  // FIXME: we are doing hash lookups every time, this has got to be slow
  unwind_protect up;
  bool *args_defined = new bool[arguments.size ()];   // vector<bool> sucks
  up.add_delete (args_defined);

  std::vector<double>  args_values (arguments.size ());
  for (size_t i = 0; i < arguments.size (); ++i)
    {
      octave_value ov = symbol_table::varval (arguments[i]);

      if (argument_used[i])
        {
          if (! (ov.is_double_type () && ov.is_real_scalar ()))
            return false;

          args_defined[i] = ov.is_defined ();
          args_values[i] = ov.double_value ();
        }
      else
        args_defined[i] = false;
    }

  function (args_defined, &args_values[0]);

  for (size_t i = 0; i < arguments.size (); ++i)
    if (args_defined[i])
      symbol_table::varref (arguments[i]) = octave_value (args_values[i]);

  return true;
}
