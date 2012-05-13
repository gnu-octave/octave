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
#include <llvm/Analysis/CallGraph.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/ExecutionEngine/GenericValue.h>

#include "octave.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pt-all.h"

// FIXME: Remove eventually
// For now we leave this in so people tell when JIT actually happens
static const bool debug_print = false;

//FIXME: Move into tree_jit
static llvm::IRBuilder<> builder (llvm::getGlobalContext ());

// function that jit code calls
extern "C" void
octave_jit_print_any (const char *name, octave_base_value *obv)
{
  obv->print_with_name (octave_stdout, name, true);
}

extern "C" void
octave_jit_print_double (const char *name, double value)
{
  // FIXME: We should avoid allocating a new octave_scalar each time
  octave_value ov (value);
  ov.print_with_name (octave_stdout, name);
}

extern "C" octave_base_value*
octave_jit_binary_any_any (octave_value::binary_op op, octave_base_value *lhs,
                           octave_base_value *rhs)
{
  octave_value olhs (lhs, true);
  octave_value orhs (rhs, true);
  octave_value result = do_binary_op (op, olhs, orhs);
  octave_base_value *rep = result.internal_rep ();
  rep->grab ();
  return rep;
}

extern "C" void
octave_jit_assign_any_any_help (octave_base_value *lhs, octave_base_value *rhs)
{
  if (lhs != rhs)
    {
      rhs->grab ();
      lhs->release ();
    }
}

// -------------------- jit_type --------------------
llvm::Type *
jit_type::to_llvm_arg (void) const
{
  return llvm_type ? llvm_type->getPointerTo () : 0;
}

// -------------------- jit_function --------------------
void
jit_function::add_overload (const overload& func,
                            const std::vector<jit_type*>& args)
{
  if (args.size () >= overloads.size ())
    overloads.resize (args.size () + 1);

  Array<overload>& over = overloads[args.size ()];
  dim_vector dv (over.dims ());
  Array<octave_idx_type> idx = to_idx (args);
  bool must_resize = false;

  if (dv.length () != idx.numel ())
    {
      dv.resize (idx.numel ());
      must_resize = true;
    }

  for (octave_idx_type i = 0; i < dv.length (); ++i)
    if (dv(i) <= idx(i))
      {
        must_resize = true;
        dv(i) = idx(i) + 1;
      }

  if (must_resize)
    over.resize (dv);

  over(idx) = func;
}

const jit_function::overload&
jit_function::get_overload (const std::vector<jit_type*>& types) const
{
  // FIXME: We should search for the next best overload on failure
  static overload null_overload;
  if (types.size () >= overloads.size ())
    return null_overload;

  const Array<overload>& over = overloads[types.size ()];
  dim_vector dv (over.dims ());
  Array<octave_idx_type> idx = to_idx (types);
  for (octave_idx_type i = 0; i < dv.length (); ++i)
    if (idx(i) >= dv(i))
      return null_overload;

  return over(idx);
}

Array<octave_idx_type>
jit_function::to_idx (const std::vector<jit_type*>& types) const
{
  octave_idx_type numel = types.size ();
  if (numel == 1)
    numel = 2;

  Array<octave_idx_type> idx (dim_vector (1, numel));
  for (octave_idx_type i = 0; i < static_cast<octave_idx_type> (types.size ());
       ++i)
    idx(i) = types[i]->type_id ();

  if (types.size () == 1)
    {
      idx(1) = idx(0);
      idx(0) = 0;
    }

  return idx;
}

// -------------------- jit_typeinfo --------------------
jit_typeinfo::jit_typeinfo (llvm::Module *m, llvm::ExecutionEngine *e, llvm::Type *ov)
  : module (m), engine (e), next_id (0), ov_t (ov)
{
  // FIXME: We should be registering types like in octave_value_typeinfo
  llvm::LLVMContext &ctx = m->getContext ();

  // create types
  any = new_type ("any", true, 0, ov_t);
  scalar = new_type ("scalar", false, any, llvm::Type::getDoubleTy (ctx));

  // any with anything is an any op
  llvm::IRBuilder<> fn_builder (ctx);

  llvm::Type *binary_op_type
    = llvm::Type::getIntNTy (ctx, sizeof (octave_value::binary_op));
  std::vector<llvm::Type*> args (3);
  args[0] = binary_op_type;
  args[1] = args[2] = any->to_llvm ();
  llvm::FunctionType *any_binary_t = llvm::FunctionType::get (ov_t, args, false);
  llvm::Function *any_binary = llvm::Function::Create (any_binary_t,
                                                       llvm::Function::ExternalLinkage,
                                                       "octave_jit_binary_any_any",
                                                       module);
  engine->addGlobalMapping (any_binary,
                            reinterpret_cast<void*>(&octave_jit_binary_any_any));

  args.resize (2);
  args[0] = any->to_llvm ();
  args[1] = any->to_llvm ();

  binary_ops.resize (octave_value::num_binary_ops);
  for (int op = 0; op < octave_value::num_binary_ops; ++op)
    {
      llvm::FunctionType *ftype = llvm::FunctionType::get (ov_t, args, false);
      
      llvm::Twine fn_name ("octave_jit_binary_any_any_");
      fn_name = fn_name + llvm::Twine (op);
      llvm::Function *fn = llvm::Function::Create (ftype,
                                                   llvm::Function::ExternalLinkage,
                                                   fn_name, module);
      llvm::BasicBlock *block = llvm::BasicBlock::Create (ctx, "body", fn);
      fn_builder.SetInsertPoint (block);
      llvm::APInt op_int(sizeof (octave_value::binary_op), op,
                         std::numeric_limits<octave_value::binary_op>::is_signed);
      llvm::Value *op_as_llvm = llvm::ConstantInt::get (binary_op_type, op_int);
      llvm::Value *ret = fn_builder.CreateCall3 (any_binary,
                                                 op_as_llvm,
                                                 fn->arg_begin (),
                                                 ++fn->arg_begin ());
      fn_builder.CreateRet (ret);

      jit_function::overload overload (fn, true, any, any, any);
      for (octave_idx_type i = 0; i < next_id; ++i)
        binary_ops[op].add_overload (overload);
    }

  // assign any = any
  llvm::Type *tvoid = llvm::Type::getVoidTy (ctx);
  args.resize (2);
  args[0] = any->to_llvm ();
  args[1] = any->to_llvm ();
  llvm::FunctionType *ft = llvm::FunctionType::get (tvoid, args, false);
  llvm::Function *fn_help = llvm::Function::Create (ft, llvm::Function::ExternalLinkage,
                                                    "octave_jit_assign_any_any_help",
                                                    module);
  engine->addGlobalMapping (fn_help,
                            reinterpret_cast<void*>(&octave_jit_assign_any_any_help));

  args.resize (2);
  args[0] = any->to_llvm_arg ();
  args[1] = any->to_llvm ();
  ft = llvm::FunctionType::get (tvoid, args, false);
  llvm::Function *fn = llvm::Function::Create (ft, llvm::Function::ExternalLinkage,
                                               "octave_jit_assign_any_any",
                                               module);
  fn->addFnAttr (llvm::Attribute::AlwaysInline);
  llvm::BasicBlock *body = llvm::BasicBlock::Create (ctx, "body", fn);
  fn_builder.SetInsertPoint (body);
  llvm::Value *value = fn_builder.CreateLoad (fn->arg_begin ());
  fn_builder.CreateCall2 (fn_help, value, ++fn->arg_begin ());
  fn_builder.CreateStore (++fn->arg_begin (), fn->arg_begin ());
  fn_builder.CreateRetVoid ();
  llvm::verifyFunction (*fn);
  assign_fn.add_overload (fn, false, 0, any, any);

  // assign scalar = scalar
  args.resize (2);
  args[0] = scalar->to_llvm_arg ();
  args[1] = scalar->to_llvm ();
  ft = llvm::FunctionType::get (tvoid, args, false);
  fn = llvm::Function::Create (ft, llvm::Function::ExternalLinkage,
                               "octave_jit_assign_scalar_scalar", module);
  fn->addFnAttr (llvm::Attribute::AlwaysInline);
  body = llvm::BasicBlock::Create (ctx, "body", fn);
  fn_builder.SetInsertPoint (body);
  fn_builder.CreateStore (++fn->arg_begin (), fn->arg_begin ());
  fn_builder.CreateRetVoid ();
  llvm::verifyFunction (*fn);
  assign_fn.add_overload (fn, false, 0, scalar, scalar);

  // now for binary scalar operations
  // FIXME: Finish all operations
  add_binary_op (scalar, octave_value::op_add, llvm::Instruction::FAdd);
  add_binary_op (scalar, octave_value::op_sub, llvm::Instruction::FSub);
  add_binary_op (scalar, octave_value::op_mul, llvm::Instruction::FMul);
  add_binary_op (scalar, octave_value::op_el_mul, llvm::Instruction::FMul);

  // FIXME: Warn if rhs is zero
  add_binary_op (scalar, octave_value::op_div, llvm::Instruction::FDiv);
  add_binary_op (scalar, octave_value::op_el_div, llvm::Instruction::FDiv);

  // now for printing functions
  add_print (any, reinterpret_cast<void*> (&octave_jit_print_any));
  add_print (scalar, reinterpret_cast<void*> (&octave_jit_print_double));
}

void
jit_typeinfo::add_print (jit_type *ty, void *call)
{
  llvm::LLVMContext& ctx = llvm::getGlobalContext ();
  llvm::Type *tvoid = llvm::Type::getVoidTy (ctx);
  std::vector<llvm::Type *> args (2);
  args[0] = llvm::Type::getInt8PtrTy (ctx);
  args[1] = ty->to_llvm ();

  std::stringstream name;
  name << "octave_jit_print_" << ty->name ();

  llvm::FunctionType *print_ty = llvm::FunctionType::get (tvoid, args, false);
  llvm::Function *fn = llvm::Function::Create (print_ty,
                                               llvm::Function::ExternalLinkage,
                                               name.str (), module);
  engine->addGlobalMapping (fn, call);

  jit_function::overload ol (fn, false, 0, ty);
  print_fn.add_overload (ol);
}

void
jit_typeinfo::add_binary_op (jit_type *ty, int op, int llvm_op)
{
  llvm::LLVMContext& ctx = llvm::getGlobalContext ();
  std::vector<llvm::Type *> args (2, ty->to_llvm ());
  llvm::FunctionType *ft = llvm::FunctionType::get (ty->to_llvm (), args,
                                                    false);

  std::stringstream fname;
  octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
  fname << "octave_jit_" << octave_value::binary_op_as_string (ov_op)
        << "_" << ty->name ();
  
  llvm::Function *fn = llvm::Function::Create (ft,
                                               llvm::Function::ExternalLinkage,
                                               fname.str (),
                                               module);
  fn->addFnAttr (llvm::Attribute::AlwaysInline);
  llvm::BasicBlock *block = llvm::BasicBlock::Create (ctx, "body", fn);
  llvm::IRBuilder<> fn_builder (block);
  llvm::Instruction::BinaryOps temp
    = static_cast<llvm::Instruction::BinaryOps>(llvm_op);
  llvm::Value *ret = fn_builder.CreateBinOp (temp, fn->arg_begin (),
                                             ++fn->arg_begin ());
  fn_builder.CreateRet (ret);
  llvm::verifyFunction (*fn);

  jit_function::overload ol(fn, false, ty, ty, ty);
  binary_ops[op].add_overload (ol);
}

jit_type*
jit_typeinfo::type_of (const octave_value &ov) const
{
  if (ov.is_undefined () || ov.is_function ())
    return 0;

  if (ov.is_double_type () && ov.is_real_scalar ())
    return get_scalar ();

  return get_any ();
}

const jit_function&
jit_typeinfo::binary_op (int op) const
{
  return binary_ops[op];
}

const jit_function::overload&
jit_typeinfo::assign_op (jit_type *lhs, jit_type *rhs) const
{
  assert (lhs == rhs);
  return assign_fn.get_overload (lhs, rhs);
}

const jit_function::overload&
jit_typeinfo::print_value (jit_type *to_print) const
{
  return print_fn.get_overload (to_print);
}

void
jit_typeinfo::to_generic (jit_type *type, llvm::GenericValue& gv)
{
  // duplication here can probably be removed somehow
  if (type == any)
    to_generic (type, gv, octave_value ());
  else if (type == scalar)
    to_generic (type, gv, octave_value (0));
  else
    assert (false && "Type not supported yet");
}

void
jit_typeinfo::to_generic (jit_type *type, llvm::GenericValue& gv, octave_value ov)
{
  if (type == any)
    {
      octave_base_value *obv = ov.internal_rep ();
      obv->grab ();
      ov_out[ov_out_idx] = obv;
      gv.PointerVal = &ov_out[ov_out_idx++];
    }
  else
    {
      scalar_out[scalar_out_idx] = ov.double_value ();
      gv.PointerVal = &scalar_out[scalar_out_idx++];
    }
}

octave_value
jit_typeinfo::to_octave_value (jit_type *type, llvm::GenericValue& gv)
{
  if (type == any)
    {
      octave_base_value **ptr = reinterpret_cast<octave_base_value**>(gv.PointerVal);
      return octave_value (*ptr);
    }
  else if (type == scalar)
    {
      double *ptr = reinterpret_cast<double*>(gv.PointerVal);
      return octave_value (*ptr);
    }
  else
    assert (false && "Type not supported yet");
}

void
jit_typeinfo::reset_generic (size_t nargs)
{
  scalar_out_idx = 0;
  ov_out_idx = 0;

  if (scalar_out.size () < nargs)
    scalar_out.resize (nargs);

  if (ov_out.size () < nargs)
    ov_out.resize (nargs);
}

jit_type*
jit_typeinfo::new_type (const std::string& name, bool force_init,
                        jit_type *parent, llvm::Type *llvm_type)
{
  jit_type *ret = new jit_type (name, force_init, parent, llvm_type, next_id++);
  id_to_type.push_back (ret);
  return ret;
}

tree_jit::tree_jit (void) : context (llvm::getGlobalContext ()), engine (0)
{
  llvm::InitializeNativeTarget ();
  module = new llvm::Module ("octave", context);
  assert (module);
}

tree_jit::~tree_jit (void)
{
  for (compiled_map::iterator iter = compiled.begin (); iter != compiled.end ();
       ++iter)
    {
      function_list& flist = iter->second;
      for (function_list::iterator fiter = flist.begin (); fiter != flist.end ();
           ++fiter)
        delete *fiter;
    }

  delete tinfo;
}

bool
tree_jit::execute (tree& tee)
{
  // something funny happens during initialization with the engine
  bool need_init = false;
  if (! engine)
    {
      need_init = true;
      engine = llvm::ExecutionEngine::createJIT (module);
    }

  if (! engine)
    return false;

  if (need_init)
    {
      module_pass_manager = new llvm::PassManager ();
      module_pass_manager->add (llvm::createAlwaysInlinerPass ());

      pass_manager = new llvm::FunctionPassManager (module);
      pass_manager->add (new llvm::TargetData(*engine->getTargetData ()));
      pass_manager->add (llvm::createBasicAliasAnalysisPass ());
      pass_manager->add (llvm::createPromoteMemoryToRegisterPass ());
      pass_manager->add (llvm::createInstructionCombiningPass ());
      pass_manager->add (llvm::createReassociatePass ());
      pass_manager->add (llvm::createGVNPass ());
      pass_manager->add (llvm::createCFGSimplificationPass ());
      pass_manager->doInitialization ();

      llvm::Type *ov_t = llvm::StructType::create (context, "octave_base_value");
      ov_t = ov_t->getPointerTo ();

      tinfo = new jit_typeinfo (module, engine, ov_t);
    }

  function_list& fnlist = compiled[&tee];
  for (function_list::iterator iter = fnlist.begin (); iter != fnlist.end ();
       ++iter)
    {
      function_info& fi = **iter;
      if (fi.match ())
        return fi.execute ();
    }

  function_info *fi = new function_info (*this, tee);
  fnlist.push_back (fi);

  return fi->execute ();
}

void
tree_jit::type_infer::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  fail ();
}

void
tree_jit::type_infer::visit_argument_list (tree_argument_list&)
{
  fail ();
}

void
tree_jit::type_infer::visit_binary_expression (tree_binary_expression& be)
{
  if (is_lvalue)
    fail ();

  tree_expression *lhs = be.lhs ();
  lhs->accept (*this);
  jit_type *tlhs = type_stack.back ();
  type_stack.pop_back ();

  tree_expression *rhs = be.rhs ();
  rhs->accept (*this);
  jit_type *trhs = type_stack.back ();

  jit_type *result = tinfo->binary_op_result (be.op_type (), tlhs, trhs);
  if (! result)
    fail ();

  type_stack.push_back (result);
}

void
tree_jit::type_infer::visit_break_command (tree_break_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_colon_expression (tree_colon_expression&)
{
  fail ();
}

void
tree_jit::type_infer::visit_continue_command (tree_continue_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_global_command (tree_global_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_persistent_command (tree_persistent_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_decl_elt (tree_decl_elt&)
{
  fail ();
}

void
tree_jit::type_infer::visit_decl_init_list (tree_decl_init_list&)
{
  fail ();
}

void
tree_jit::type_infer::visit_simple_for_command (tree_simple_for_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_complex_for_command (tree_complex_for_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_octave_user_script (octave_user_script&)
{
  fail ();
}

void
tree_jit::type_infer::visit_octave_user_function (octave_user_function&)
{
  fail ();
}

void
tree_jit::type_infer::visit_octave_user_function_header (octave_user_function&)
{
  fail ();
}

void
tree_jit::type_infer::visit_octave_user_function_trailer (octave_user_function&)
{
  fail ();
}

void
tree_jit::type_infer::visit_function_def (tree_function_def&)
{
  fail ();
}

void
tree_jit::type_infer::visit_identifier (tree_identifier& ti)
{
  handle_identifier (ti.name (), ti.do_lookup ());
}

void
tree_jit::type_infer::visit_if_clause (tree_if_clause&)
{
  fail ();
}

void
tree_jit::type_infer::visit_if_command (tree_if_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_if_command_list (tree_if_command_list&)
{
  fail ();
}

void
tree_jit::type_infer::visit_index_expression (tree_index_expression&)
{
  fail ();
}

void
tree_jit::type_infer::visit_matrix (tree_matrix&)
{
  fail ();
}

void
tree_jit::type_infer::visit_cell (tree_cell&)
{
  fail ();
}

void
tree_jit::type_infer::visit_multi_assignment (tree_multi_assignment&)
{
  fail ();
}

void
tree_jit::type_infer::visit_no_op_command (tree_no_op_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_constant (tree_constant& tc)
{
  if (is_lvalue)
    fail ();

  octave_value v = tc.rvalue1 ();
  jit_type *type = tinfo->type_of (v);
  if (! type)
    fail ();

  type_stack.push_back (type);
}

void
tree_jit::type_infer::visit_fcn_handle (tree_fcn_handle&)
{
  fail ();
}

void
tree_jit::type_infer::visit_parameter_list (tree_parameter_list&)
{
  fail ();
}

void
tree_jit::type_infer::visit_postfix_expression (tree_postfix_expression&)
{
  fail ();
}

void
tree_jit::type_infer::visit_prefix_expression (tree_prefix_expression&)
{
  fail ();
}

void
tree_jit::type_infer::visit_return_command (tree_return_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_return_list (tree_return_list&)
{
  fail ();
}

void
tree_jit::type_infer::visit_simple_assignment (tree_simple_assignment& tsa)
{
  if (is_lvalue)
    fail ();

  // resolve rhs
  is_lvalue = false;
  tree_expression *rhs = tsa.right_hand_side ();
  rhs->accept (*this);

  jit_type *trhs = type_stack.back ();
  type_stack.pop_back ();

  // resolve lhs
  is_lvalue = true;
  rvalue_type = trhs;
  tree_expression *lhs = tsa.left_hand_side ();
  lhs->accept (*this);

  // we don't pop back here, as the resulting type should be the rhs type
  // which is equal to the lhs type anways
  jit_type *tlhs = type_stack.back ();
  if (tlhs != trhs)
    fail ();

  is_lvalue = false;
  rvalue_type = 0;
}

void
tree_jit::type_infer::visit_statement (tree_statement& stmt)
{
  if (is_lvalue)
    fail ();

  tree_command *cmd = stmt.command ();
  tree_expression *expr = stmt.expression ();

  if (cmd)
    cmd->accept (*this);
  else
    {
      // ok, this check for ans appears three times as cp
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
          is_lvalue = true;
          rvalue_type = type_stack.back ();
          type_stack.pop_back ();
          handle_identifier ("ans", symbol_table::varval ("ans"));

          if (rvalue_type != type_stack.back ())
            fail ();

          is_lvalue = false;
          rvalue_type = 0;
        }

      type_stack.pop_back ();
    }
}

void
tree_jit::type_infer::visit_statement_list (tree_statement_list&)
{
  fail ();
}

void
tree_jit::type_infer::visit_switch_case (tree_switch_case&)
{
  fail ();
}

void
tree_jit::type_infer::visit_switch_case_list (tree_switch_case_list&)
{
  fail ();
}

void
tree_jit::type_infer::visit_switch_command (tree_switch_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_try_catch_command (tree_try_catch_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_unwind_protect_command (tree_unwind_protect_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_while_command (tree_while_command&)
{
  fail ();
}

void
tree_jit::type_infer::visit_do_until_command (tree_do_until_command&)
{
  fail ();
}

void
tree_jit::type_infer::handle_identifier (const std::string& name, octave_value v)
{
  type_map::iterator iter = types.find (name);
  if (iter == types.end ())
    {
      jit_type *ty = tinfo->type_of (v);
      if (is_lvalue)
        {
          if (! ty)
            ty = rvalue_type;
        }
      else
        {
          if (! ty)
            fail ();

          argin.insert (name);
        }

      types[name] = ty;
      type_stack.push_back (ty);
    }
  else
    type_stack.push_back (iter->second);
}

tree_jit::code_generator::code_generator (jit_typeinfo *ti, llvm::Module *module,
                                          tree &tee,
                                          const std::set<std::string>& argin,
                                          const type_map& infered_types)
  : tinfo (ti), is_lvalue (false)
  
{
  // determine the function type through the type of all variables
  std::vector<llvm::Type *> arg_types (infered_types.size ());
  size_t idx = 0;
  type_map::const_iterator iter;
  for (iter = infered_types.begin (); iter != infered_types.end (); ++iter, ++idx)
    arg_types[idx] = iter->second->to_llvm_arg ();

  // now create the LLVM function from our determined types
  llvm::LLVMContext &ctx = llvm::getGlobalContext ();
  llvm::Type *tvoid = llvm::Type::getVoidTy (ctx);
  llvm::FunctionType *ft = llvm::FunctionType::get (tvoid, arg_types, false);
  function = llvm::Function::Create (ft, llvm::Function::ExternalLinkage,
                                     "foobar", module);

  // declare each argument and copy its initial value
  llvm::BasicBlock *body = llvm::BasicBlock::Create (ctx, "body", function);
  builder.SetInsertPoint (body);
  llvm::Function::arg_iterator arg_iter = function->arg_begin();
  for (iter = infered_types.begin (); iter != infered_types.end ();
       ++iter, ++arg_iter)
       
    {
      llvm::Type *vartype = iter->second->to_llvm ();
      llvm::Value *var = builder.CreateAlloca (vartype, 0, iter->first);
      variables[iter->first] = value (iter->second, var);

      if (iter->second->force_init () || argin.count (iter->first))
        {
          llvm::Value *loaded_arg = builder.CreateLoad (arg_iter);
          builder.CreateStore (loaded_arg, var);
        }
    }

  // generate body
  try
    {
      tee.accept (*this);
    }
  catch (const jit_fail_exception&)
    {
      function->eraseFromParent ();
      function = 0;
      return;
    }

  // copy computed values back into arguments
  arg_iter = function->arg_begin ();
  for (iter = infered_types.begin (); iter != infered_types.end ();
       ++iter, ++arg_iter)
    {
      llvm::Value *var = variables[iter->first].second;
      llvm::Value *loaded_var = builder.CreateLoad (var);
      builder.CreateStore (loaded_var, arg_iter);
    }
  builder.CreateRetVoid ();
}

void
tree_jit::code_generator::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  fail ();
}

void
tree_jit::code_generator::visit_argument_list (tree_argument_list&)
{
  fail ();
}

void
tree_jit::code_generator::visit_binary_expression (tree_binary_expression& be)
{
  tree_expression *lhs = be.lhs ();
  lhs->accept (*this);
  value lhsv = value_stack.back ();
  value_stack.pop_back ();

  tree_expression *rhs = be.rhs ();
  rhs->accept (*this);
  value rhsv = value_stack.back ();
  value_stack.pop_back ();

  const jit_function::overload& ol
    = tinfo->binary_op_overload (be.op_type (), lhsv.first, rhsv.first);

  if (! ol.function)
    fail ();

  llvm::Value *result = builder.CreateCall2 (ol.function, lhsv.second,
                                             rhsv.second);
  push_value (ol.result, result);
}

void
tree_jit::code_generator::visit_break_command (tree_break_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_colon_expression (tree_colon_expression&)
{
  fail ();
}

void
tree_jit::code_generator::visit_continue_command (tree_continue_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_global_command (tree_global_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_persistent_command (tree_persistent_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_decl_elt (tree_decl_elt&)
{
  fail ();
}

void
tree_jit::code_generator::visit_decl_init_list (tree_decl_init_list&)
{
  fail ();
}

void
tree_jit::code_generator::visit_simple_for_command (tree_simple_for_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_complex_for_command (tree_complex_for_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_octave_user_script (octave_user_script&)
{
  fail ();
}

void
tree_jit::code_generator::visit_octave_user_function (octave_user_function&)
{
  fail ();
}

void
tree_jit::code_generator::visit_octave_user_function_header (octave_user_function&)
{
  fail ();
}

void
tree_jit::code_generator::visit_octave_user_function_trailer (octave_user_function&)
{
  fail ();
}

void
tree_jit::code_generator::visit_function_def (tree_function_def&)
{
  fail ();
}

void
tree_jit::code_generator::visit_identifier (tree_identifier& ti)
{
  std::string name = ti.name ();
  value variable = variables[name];
  if (is_lvalue)
    value_stack.push_back (variable);
  else
    {
      llvm::Value *load = builder.CreateLoad (variable.second, name);
      push_value (variable.first, load);
    }
}

void
tree_jit::code_generator::visit_if_clause (tree_if_clause&)
{
  fail ();
}

void
tree_jit::code_generator::visit_if_command (tree_if_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_if_command_list (tree_if_command_list&)
{
  fail ();
}

void
tree_jit::code_generator::visit_index_expression (tree_index_expression&)
{
  fail ();
}

void
tree_jit::code_generator::visit_matrix (tree_matrix&)
{
  fail ();
}

void
tree_jit::code_generator::visit_cell (tree_cell&)
{
  fail ();
}

void
tree_jit::code_generator::visit_multi_assignment (tree_multi_assignment&)
{
  fail ();
}

void
tree_jit::code_generator::visit_no_op_command (tree_no_op_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_constant (tree_constant& tc)
{
  octave_value v = tc.rvalue1 ();
  if (v.is_real_scalar () && v.is_double_type ())
    {
      llvm::LLVMContext& ctx = llvm::getGlobalContext ();
      double dv = v.double_value ();
      llvm::Value *lv = llvm::ConstantFP::get (ctx, llvm::APFloat (dv));
      push_value (tinfo->get_scalar (), lv);
    }
  else
    fail ();
}

void
tree_jit::code_generator::visit_fcn_handle (tree_fcn_handle&)
{
  fail ();
}

void
tree_jit::code_generator::visit_parameter_list (tree_parameter_list&)
{
  fail ();
}

void
tree_jit::code_generator::visit_postfix_expression (tree_postfix_expression&)
{
  fail ();
}

void
tree_jit::code_generator::visit_prefix_expression (tree_prefix_expression&)
{
  fail ();
}

void
tree_jit::code_generator::visit_return_command (tree_return_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_return_list (tree_return_list&)
{
  fail ();
}

void
tree_jit::code_generator::visit_simple_assignment (tree_simple_assignment& tsa)
{
  if (is_lvalue)
    fail ();

  // resolve lhs
  is_lvalue = true;
  tree_expression *lhs = tsa.left_hand_side ();
  lhs->accept (*this);

  value lhsv = value_stack.back ();
  value_stack.pop_back ();

  // resolve rhs
  is_lvalue = false;
  tree_expression *rhs = tsa.right_hand_side ();
  rhs->accept (*this);

  value rhsv = value_stack.back ();
  value_stack.pop_back ();

  // do assign, then store rhs as the result
  jit_function::overload ol = tinfo->assign_op (lhsv.first, rhsv.first);
  builder.CreateCall2 (ol.function, lhsv.second, rhsv.second);

  if (tsa.print_result ())
    emit_print (lhs->name (), rhsv);

  value_stack.push_back (rhsv);
}

void
tree_jit::code_generator::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();
  tree_expression *expr = stmt.expression ();

  if (cmd)
    cmd->accept (*this);
  else
    {
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
          value rhs = value_stack.back ();
          value ans = variables["ans"];
          if (ans.first != rhs.first)
            fail ();

          builder.CreateStore (rhs.second, ans.second);

          if (expr->print_result ())
            emit_print ("ans", rhs);
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
tree_jit::code_generator::visit_statement_list (tree_statement_list&)
{
  fail ();
}

void
tree_jit::code_generator::visit_switch_case (tree_switch_case&)
{
  fail ();
}

void
tree_jit::code_generator::visit_switch_case_list (tree_switch_case_list&)
{
  fail ();
}

void
tree_jit::code_generator::visit_switch_command (tree_switch_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_try_catch_command (tree_try_catch_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_unwind_protect_command (tree_unwind_protect_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_while_command (tree_while_command&)
{
  fail ();
}

void
tree_jit::code_generator::visit_do_until_command (tree_do_until_command&)
{
  fail ();
}

void
tree_jit::code_generator::emit_print (const std::string& name, const value& v)
{
  const jit_function::overload& ol = tinfo->print_value (v.first);
  if (! ol.function)
    fail ();

  llvm::Value *str = builder.CreateGlobalStringPtr (name);
  builder.CreateCall2 (ol.function, str, v.second);
}

tree_jit::function_info::function_info (tree_jit& tjit, tree& tee) :
  tinfo (tjit.tinfo), engine (tjit.engine)
{
  type_infer infer(tjit.tinfo);

  try
    {
      tee.accept (infer);
    }
  catch (const jit_fail_exception&)
    {
      function = 0;
      return;
    }

  argin = infer.get_argin ();
  types = infer.get_types ();

  code_generator gen(tjit.tinfo, tjit.module, tee, argin, types);
  function = gen.get_function ();

  if (function)
    {
      llvm::verifyFunction (*function);
      tjit.module_pass_manager->run (*tjit.module);
      tjit.pass_manager->run (*function);

      if (debug_print)
        {
          std::cout << "Compiled:\n";
          std::cout << tee.str_print_code () << std::endl;

          std::cout << "Code:\n";

          llvm::raw_os_ostream os (std::cout);
          function->print (os);
        }
    }
}

bool
tree_jit::function_info::execute () const
{
  if (! function)
    return false;

  tinfo->reset_generic (types.size ());

  std::vector<llvm::GenericValue> args (types.size ());
  size_t idx;
  type_map::const_iterator iter;
  for (idx = 0, iter = types.begin (); iter != types.end (); ++iter, ++idx)
    {
      if (argin.count (iter->first))
        {
          octave_value ov = symbol_table::varval (iter->first);
          tinfo->to_generic (iter->second, args[idx], ov);
        }
      else
        tinfo->to_generic (iter->second, args[idx]);
    }

  engine->runFunction (function, args);

  for (idx = 0, iter = types.begin (); iter != types.end (); ++iter, ++idx)
    {
      octave_value result = tinfo->to_octave_value (iter->second, args[idx]);
      symbol_table::varref (iter->first) = result;
    }

  return true;
}

bool
tree_jit::function_info::match () const
{
  for (std::set<std::string>::iterator iter = argin.begin ();
       iter != argin.end (); ++iter)
    {
      jit_type *required_type = types.find (*iter)->second;
      octave_value val = symbol_table::varref (*iter);
      jit_type *current_type = tinfo->type_of (val);

      // FIXME: should be: ! required_type->is_parent (current_type)
      if (required_type != current_type)
        return false;
    }

  return true;
}
