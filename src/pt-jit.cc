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

#include "octave.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "ov-scalar.h"
#include "pt-all.h"

// FIXME: Remove eventually
// For now we leave this in so people tell when JIT actually happens
static const bool debug_print = false;

static llvm::IRBuilder<> builder (llvm::getGlobalContext ());

static llvm::LLVMContext& context = llvm::getGlobalContext ();

jit_typeinfo *jit_typeinfo::instance;

// thrown when we should give up on JIT and interpret
class jit_fail_exception : public std::runtime_error
{
public:
  jit_fail_exception (void) : std::runtime_error ("unknown"), mknown (false) {}
  jit_fail_exception (const std::string& reason) : std::runtime_error (reason),
                                                   mknown (true)
  {}

  bool known (void) const { return mknown; }
private:
  bool mknown;
};

static void
fail (void)
{
  throw jit_fail_exception ();
}

static void
fail (const std::string& reason)
{
  throw jit_fail_exception (reason);
}

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
  octave_value olhs (lhs);
  octave_value orhs (rhs);
  octave_value result = do_binary_op (op, olhs, orhs);
  octave_base_value *rep = result.internal_rep ();
  rep->grab ();
  return rep;
}

extern "C" void
octave_jit_release_any (octave_base_value *obv)
{
  obv->release ();
}

extern "C" octave_base_value *
octave_jit_grab_any (octave_base_value *obv)
{
  obv->grab ();
  return obv;
}

extern "C" double
octave_jit_cast_scalar_any (octave_base_value *obv)
{
  double ret = obv->double_value ();
  obv->release ();
  return ret;
}

extern "C" octave_base_value *
octave_jit_cast_any_scalar (double value)
{
  return new octave_scalar (value);
}

// -------------------- jit_range --------------------
std::ostream&
operator<< (std::ostream& os, const jit_range& rng)
{
  return os << "Range[" << rng.base << ", " << rng.limit << ", " << rng.inc
            << ", " << rng.nelem << "]";
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

  for (size_t i  =0; i < types.size (); ++i)
    if (! types[i])
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
void
jit_typeinfo::initialize (llvm::Module *m, llvm::ExecutionEngine *e)
{
  instance = new jit_typeinfo (m, e);
}

jit_typeinfo::jit_typeinfo (llvm::Module *m, llvm::ExecutionEngine *e)
  : module (m), engine (e), next_id (0)
{
  // FIXME: We should be registering types like in octave_value_typeinfo
  ov_t = llvm::StructType::create (context, "octave_base_value");
  ov_t = ov_t->getPointerTo ();

  llvm::Type *dbl = llvm::Type::getDoubleTy (context);
  llvm::Type *bool_t = llvm::Type::getInt1Ty (context);
  llvm::Type *string_t = llvm::Type::getInt8Ty (context);
  string_t = string_t->getPointerTo ();
  llvm::Type *index_t = 0;
  switch (sizeof(octave_idx_type))
    {
    case 4:
      index_t = llvm::Type::getInt32Ty (context);
      break;
    case 8:
      index_t = llvm::Type::getInt64Ty (context);
      break;
    default:
      assert (false && "Unrecognized index type size");
    }

  llvm::StructType *range_t = llvm::StructType::create (context, "range");
  std::vector<llvm::Type *> range_contents (4, dbl);
  range_contents[3] = index_t;
  range_t->setBody (range_contents);

  // create types
  any = new_type ("any", 0, ov_t);
  scalar = new_type ("scalar", any, dbl);
  range = new_type ("range", any, range_t);
  string = new_type ("string", any, string_t);
  boolean = new_type ("bool", any, bool_t);
  index = new_type ("index", any, index_t);

  casts.resize (next_id + 1);
  identities.resize (next_id + 1, 0);

  // any with anything is an any op
  llvm::Function *fn;
  llvm::Type *binary_op_type
    = llvm::Type::getIntNTy (context, sizeof (octave_value::binary_op));
  llvm::Function *any_binary = create_function ("octave_jit_binary_any_any",
                                                any->to_llvm (), binary_op_type,
                                                any->to_llvm (), any->to_llvm ());
  engine->addGlobalMapping (any_binary,
                            reinterpret_cast<void*>(&octave_jit_binary_any_any));

  binary_ops.resize (octave_value::num_binary_ops);
  for (size_t i = 0; i < octave_value::num_binary_ops; ++i)
    {
      octave_value::binary_op op = static_cast<octave_value::binary_op> (i);
      std::string op_name = octave_value::binary_op_as_string (op);
      binary_ops[i].stash_name ("binary" + op_name);
    }

  for (int op = 0; op < octave_value::num_binary_ops; ++op)
    {
      llvm::Twine fn_name ("octave_jit_binary_any_any_");
      fn_name = fn_name + llvm::Twine (op);
      fn = create_function (fn_name, any, any, any);
      llvm::BasicBlock *block = llvm::BasicBlock::Create (context, "body", fn);
      builder.SetInsertPoint (block);
      llvm::APInt op_int(sizeof (octave_value::binary_op), op,
                         std::numeric_limits<octave_value::binary_op>::is_signed);
      llvm::Value *op_as_llvm = llvm::ConstantInt::get (binary_op_type, op_int);
      llvm::Value *ret = builder.CreateCall3 (any_binary,
                                                 op_as_llvm,
                                                 fn->arg_begin (),
                                                 ++fn->arg_begin ());
      builder.CreateRet (ret);

      jit_function::overload overload (fn, true, any, any, any);
      for (octave_idx_type i = 0; i < next_id; ++i)
        binary_ops[op].add_overload (overload);
    }

  llvm::Type *void_t = llvm::Type::getVoidTy (context);

  // grab any
  fn = create_function ("octave_jit_grab_any", any, any);
                        
  engine->addGlobalMapping (fn, reinterpret_cast<void*>(&octave_jit_grab_any));
  grab_fn.add_overload (fn, false, any, any);
  grab_fn.stash_name ("grab");

  // grab scalar
  fn = create_identity (scalar);
  grab_fn.add_overload (fn, false, scalar, scalar);

  // grab index
  fn = create_identity (index);
  grab_fn.add_overload (fn, false, index, index);

  // release any
  fn = create_function ("octave_jit_release_any", void_t, any->to_llvm ());
  engine->addGlobalMapping (fn, reinterpret_cast<void*>(&octave_jit_release_any));
  release_fn.add_overload (fn, false, 0, any);
  release_fn.stash_name ("release");

  // release scalar
  fn = create_identity (scalar);
  release_fn.add_overload (fn, false, 0, scalar);

  // release index
  fn = create_identity (index);
  release_fn.add_overload (fn, false, 0, index);

  // now for binary scalar operations
  // FIXME: Finish all operations
  add_binary_op (scalar, octave_value::op_add, llvm::Instruction::FAdd);
  add_binary_op (scalar, octave_value::op_sub, llvm::Instruction::FSub);
  add_binary_op (scalar, octave_value::op_mul, llvm::Instruction::FMul);
  add_binary_op (scalar, octave_value::op_el_mul, llvm::Instruction::FMul);

  // FIXME: Warn if rhs is zero
  add_binary_op (scalar, octave_value::op_div, llvm::Instruction::FDiv);
  add_binary_op (scalar, octave_value::op_el_div, llvm::Instruction::FDiv);

  add_binary_fcmp (scalar, octave_value::op_lt, llvm::CmpInst::FCMP_ULT);
  add_binary_fcmp (scalar, octave_value::op_le, llvm::CmpInst::FCMP_ULE);
  add_binary_fcmp (scalar, octave_value::op_eq, llvm::CmpInst::FCMP_UEQ);
  add_binary_fcmp (scalar, octave_value::op_ge, llvm::CmpInst::FCMP_UGE);
  add_binary_fcmp (scalar, octave_value::op_gt, llvm::CmpInst::FCMP_UGT);
  add_binary_fcmp (scalar, octave_value::op_ne, llvm::CmpInst::FCMP_UNE);

  // now for binary index operators
  add_binary_op (index, octave_value::op_add, llvm::Instruction::Add);

  // now for printing functions
  print_fn.stash_name ("print");
  add_print (any, reinterpret_cast<void*> (&octave_jit_print_any));
  add_print (scalar, reinterpret_cast<void*> (&octave_jit_print_double));

  // initialize for loop
  for_init_fn.stash_name ("for_init");

  fn = create_function ("octave_jit_for_range_init", index, range);
  llvm::BasicBlock *body = llvm::BasicBlock::Create (context, "body", fn); 
  builder.SetInsertPoint (body);
  {
    llvm::Value *zero = llvm::ConstantInt::get (index_t, 0);
    builder.CreateRet (zero);
  }
  llvm::verifyFunction (*fn);
  for_init_fn.add_overload (fn, false, index, range);

  // bounds check for for loop
  for_check_fn.stash_name ("for_check");

  fn = create_function ("octave_jit_for_range_check", boolean, range, index);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::Value *nelem
      = builder.CreateExtractValue (fn->arg_begin (), 3);
    llvm::Value *idx = ++fn->arg_begin ();
    llvm::Value *ret = builder.CreateICmpULT (idx, nelem);
    builder.CreateRet (ret);
  }
  llvm::verifyFunction (*fn);
  for_check_fn.add_overload (fn, false, boolean, range, index);

  // index variabe for for loop
  for_index_fn.stash_name ("for_index");

  fn = create_function ("octave_jit_for_range_idx", scalar, range, index);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::Value *idx = ++fn->arg_begin ();
    llvm::Value *didx = builder.CreateUIToFP (idx, dbl);
    llvm::Value *rng = fn->arg_begin ();
    llvm::Value *base = builder.CreateExtractValue (rng, 0);
    llvm::Value *inc = builder.CreateExtractValue (rng, 2);

    llvm::Value *ret = builder.CreateFMul (didx, inc);
    ret = builder.CreateFAdd (base, ret);
    builder.CreateRet (ret);
  }
  llvm::verifyFunction (*fn);
  for_index_fn.add_overload (fn, false, scalar, range, index);

  // logically true
  // FIXME: Check for NaN
  fn = create_function ("octave_logically_true_scalar", boolean, scalar);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::Value *zero = llvm::ConstantFP::get (scalar->to_llvm (), 0);
    llvm::Value *ret = builder.CreateFCmpUNE (fn->arg_begin (), zero);
    builder.CreateRet (ret);
  }
  llvm::verifyFunction (*fn);
  logically_true.add_overload (fn, true, boolean, scalar);

  fn = create_function ("octave_logically_true_bool", boolean, boolean);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  builder.CreateRet (fn->arg_begin ());
  llvm::verifyFunction (*fn);
  logically_true.add_overload (fn, false, boolean, boolean);
  logically_true.stash_name ("logically_true");

  casts[any->type_id ()].stash_name ("(any)");
  casts[scalar->type_id ()].stash_name ("(scalar)");

  // cast any <- scalar
  fn = create_function ("octave_jit_cast_any_scalar", any, scalar);
  engine->addGlobalMapping (fn, reinterpret_cast<void*> (&octave_jit_cast_any_scalar));
  casts[any->type_id ()].add_overload (fn, false, any, scalar);

  // cast scalar <- any
  fn = create_function ("octave_jit_cast_scalar_any", scalar, any);
  engine->addGlobalMapping (fn, reinterpret_cast<void*> (&octave_jit_cast_scalar_any));
  casts[scalar->type_id ()].add_overload (fn, false, scalar, any);

  // cast any <- any
  fn = create_identity (any);
  casts[any->type_id ()].add_overload (fn, false, any, any);

  // cast scalar <- scalar
  fn = create_identity (scalar);
  casts[scalar->type_id ()].add_overload (fn, false, scalar, scalar);
}

void
jit_typeinfo::add_print (jit_type *ty, void *call)
{
  std::stringstream name;
  name << "octave_jit_print_" << ty->name ();

  llvm::Type *void_t = llvm::Type::getVoidTy (context);
  llvm::Function *fn = create_function (name.str (), void_t,
                                        llvm::Type::getInt8PtrTy (context),
                                        ty->to_llvm ());
  engine->addGlobalMapping (fn, call);

  jit_function::overload ol (fn, false, 0, string, ty);
  print_fn.add_overload (ol);
}

// FIXME: cp between add_binary_op, add_binary_icmp, and add_binary_fcmp
void
jit_typeinfo::add_binary_op (jit_type *ty, int op, int llvm_op)
{
  std::stringstream fname;
  octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
  fname << "octave_jit_" << octave_value::binary_op_as_string (ov_op)
        << "_" << ty->name ();

  llvm::Function *fn = create_function (fname.str (), ty, ty, ty);
  llvm::BasicBlock *block = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (block);
  llvm::Instruction::BinaryOps temp
    = static_cast<llvm::Instruction::BinaryOps>(llvm_op);
  llvm::Value *ret = builder.CreateBinOp (temp, fn->arg_begin (),
                                          ++fn->arg_begin ());
  builder.CreateRet (ret);
  llvm::verifyFunction (*fn);

  jit_function::overload ol(fn, false, ty, ty, ty);
  binary_ops[op].add_overload (ol);
}

void
jit_typeinfo::add_binary_icmp (jit_type *ty, int op, int llvm_op)
{
  std::stringstream fname;
  octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
  fname << "octave_jit" << octave_value::binary_op_as_string (ov_op)
        << "_" << ty->name ();

  llvm::Function *fn = create_function (fname.str (), boolean, ty, ty);
  llvm::BasicBlock *block = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (block);
  llvm::CmpInst::Predicate temp
    = static_cast<llvm::CmpInst::Predicate>(llvm_op);
  llvm::Value *ret = builder.CreateICmp (temp, fn->arg_begin (),
                                         ++fn->arg_begin ());
  builder.CreateRet (ret);
  llvm::verifyFunction (*fn);

  jit_function::overload ol (fn, false, boolean, ty, ty);
  binary_ops[op].add_overload (ol);
}

void
jit_typeinfo::add_binary_fcmp (jit_type *ty, int op, int llvm_op)
{
  std::stringstream fname;
  octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
  fname << "octave_jit" << octave_value::binary_op_as_string (ov_op)
        << "_" << ty->name ();

  llvm::Function *fn = create_function (fname.str (), boolean, ty, ty);
  llvm::BasicBlock *block = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (block);
  llvm::CmpInst::Predicate temp
    = static_cast<llvm::CmpInst::Predicate>(llvm_op);
  llvm::Value *ret = builder.CreateFCmp (temp, fn->arg_begin (),
                                         ++fn->arg_begin ());
  builder.CreateRet (ret);
  llvm::verifyFunction (*fn);

  jit_function::overload ol (fn, false, boolean, ty, ty);
  binary_ops[op].add_overload (ol);
}

llvm::Function *
jit_typeinfo::create_function (const llvm::Twine& name, llvm::Type *ret,
                               const std::vector<llvm::Type *>& args)
{
  llvm::FunctionType *ft = llvm::FunctionType::get (ret, args, false);
  llvm::Function *fn = llvm::Function::Create (ft,
                                               llvm::Function::ExternalLinkage,
                                               name, module);
  fn->addFnAttr (llvm::Attribute::AlwaysInline);
  return fn;
}

llvm::Function *
jit_typeinfo::create_identity (jit_type *type)
{
  size_t id = type->type_id ();
  if (id >= identities.size ())
    identities.resize (id + 1, 0);

  if (! identities[id])
    {
      llvm::Function *fn = create_function ("id", type, type);
      llvm::BasicBlock *body = llvm::BasicBlock::Create (context, "body", fn);
      builder.SetInsertPoint (body);
      builder.CreateRet (fn->arg_begin ());
      llvm::verifyFunction (*fn);
      identities[id] = fn;
    }

  return identities[id];
}

jit_type *
jit_typeinfo::do_type_of (const octave_value &ov) const
{
  if (ov.is_undefined () || ov.is_function ())
    return 0;

  if (ov.is_double_type () && ov.is_real_scalar ())
    return get_scalar ();

  if (ov.is_range ())
    return get_range ();

  return get_any ();
}

jit_type*
jit_typeinfo::new_type (const std::string& name, jit_type *parent,
                        llvm::Type *llvm_type)
{
  jit_type *ret = new jit_type (name, parent, llvm_type, next_id++);
  id_to_type.push_back (ret);
  return ret;
}

// -------------------- jit_use --------------------
jit_block *
jit_use::user_parent (void) const
{
  return usr->parent ();
}

// -------------------- jit_value --------------------
#define JIT_METH(clname)                                \
  void                                                  \
  jit_ ## clname::accept (jit_ir_walker& walker)        \
  {                                                     \
    walker.visit (*this);                               \
  }

JIT_VISIT_IR_NOTEMPLATE
#undef JIT_METH

// -------------------- jit_instruction --------------------
llvm::BasicBlock *
jit_instruction::parent_llvm (void) const
{
  return mparent->to_llvm ();
}

// -------------------- jit_block --------------------
jit_instruction *
jit_block::prepend (jit_instruction *instr)
{
  instructions.push_front (instr);
  instr->stash_parent (this);
  return instr;
}

jit_instruction *
jit_block::append (jit_instruction *instr)
{
  instructions.push_back (instr);
  instr->stash_parent (this);
  return instr;
}

jit_terminator *
jit_block::terminator (void) const
{
  if (instructions.empty ())
    return 0;

  jit_instruction *last = instructions.back ();
  return dynamic_cast<jit_terminator *> (last);
}

llvm::Value *
jit_block::pred_terminator_llvm (size_t idx) const
{
  jit_terminator *term = pred_terminator (idx);
  return term ? term->to_llvm () : 0;
}

void
jit_block::create_merge (llvm::Function *inside, size_t pred_idx)
{
  mpred_llvm.resize (pred_count ());

  jit_block *ipred = pred (pred_idx);
  if (! mpred_llvm[pred_idx] && ipred->pred_count () > 1)
    {
      llvm::BasicBlock *merge;
      merge = llvm::BasicBlock::Create (context, "phi_merge", inside,
                                        to_llvm ());
          
      // fix the predecessor jump if it has been created
      llvm::Value *term = pred_terminator_llvm (pred_idx);
      if (term)
        {
          llvm::TerminatorInst *branch = llvm::cast<llvm::TerminatorInst> (term);
          for (size_t i = 0; i < branch->getNumSuccessors (); ++i)
            {
              if (branch->getSuccessor (i) == to_llvm ())
                branch->setSuccessor (i, merge);
            }
        }

      llvm::IRBuilder<> temp (merge);
      temp.CreateBr (to_llvm ());
      mpred_llvm[pred_idx] = merge;
    }
}

size_t
jit_block::succ_count (void) const
{
  jit_terminator *term = terminator ();
  return term ? term->sucessor_count () : 0;
}

llvm::BasicBlock *
jit_block::to_llvm (void) const
{
  return llvm::cast<llvm::BasicBlock> (llvm_value);
}

// -------------------- jit_call --------------------
bool
jit_call::infer (void)
{
  // FIXME explain algorithm
  jit_type *current = type ();
  for (size_t i = 0; i < argument_count (); ++i)
    {
      jit_type *arg_type = argument_type (i);
      jit_type *todo = jit_typeinfo::difference (arg_type, already_infered[i]);
      if (todo)
        {
          already_infered[i] = todo;
          jit_type *fresult = mfunction.get_result (already_infered);
          current = jit_typeinfo::tunion (current, fresult);
          already_infered[i] = arg_type;
        }
    }

  if (current != type ())
    {
      stash_type (current);
      return true;
    }

  return false;
}

// -------------------- jit_convert --------------------
jit_convert::jit_convert (llvm::Module *module, tree &tee)
{
  jit_instruction::reset_ids ();

  jit_block *entry_block = new jit_block ("body");
  block = entry_block;
  blocks.push_back (block);

  toplevel_map tlevel (block);
  variables = &tlevel;
  final_block = new jit_block ("final");
  visit (tee);

  blocks.push_back (final_block);
  block->append (new jit_break (final_block));

  for (variable_map::iterator iter = variables->begin ();
       iter != variables->end (); ++iter)
    final_block->append (new jit_store_argument (iter->first, iter->second));

  // FIXME: Maybe we should remove dead code here?

  // initialize the worklist to instructions derived from constants
  for (std::list<jit_value *>::iterator iter = constants.begin ();
       iter != constants.end (); ++iter)
    append_users (*iter);

  // also get anything from jit_extract_argument, as these have constant types
  for (jit_block::iterator iter = entry_block->begin ();
       iter != entry_block->end (); ++iter)
    {
      jit_instruction *instr = *iter;
      if (jit_extract_argument *extract = dynamic_cast<jit_extract_argument *>(instr))
        {
          if (! extract->type ())
            fail (); // we depend on an unknown type
          append_users (extract);
        }
    }

  // FIXME: Describe algorithm here
  while (worklist.size ())
    {
      jit_instruction *next = worklist.front ();
      worklist.pop_front ();

      if (next->infer ())
        append_users (next);
    }

  if (debug_print)
    {
      std::cout << "-------------------- Compiling tree --------------------\n";
      std::cout << tee.str_print_code () << std::endl;
      std::cout << "-------------------- octave jit ir --------------------\n";
      for (std::list<jit_block *>::iterator iter = blocks.begin ();
           iter != blocks.end (); ++iter)
        (*iter)->print (std::cout, 0);
      std::cout << std::endl;
    }

  // for now just init arguments from entry, later we will have to do something
  // more interesting
  for (jit_block::iterator iter = entry_block->begin ();
       iter != entry_block->end (); ++iter)
    {
      if (jit_extract_argument *extract = dynamic_cast<jit_extract_argument *> (*iter))
        arguments.push_back (std::make_pair (extract->tag (), true));
    }

  convert_llvm to_llvm;
  function = to_llvm.convert (module, arguments, blocks, constants);

  if (debug_print)
    {
      std::cout << "-------------------- llvm ir --------------------";
      llvm::raw_os_ostream llvm_cout (std::cout);
      function->print (llvm_cout);
      std::cout << std::endl;
      llvm::verifyFunction (*function);
    }
}

void
jit_convert::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  fail ();
}

void
jit_convert::visit_argument_list (tree_argument_list&)
{
  fail ();
}

void
jit_convert::visit_binary_expression (tree_binary_expression& be)
{
  if (be.op_type () >= octave_value::num_binary_ops)
    // this is the case for bool_or and bool_and
    fail ();

  tree_expression *lhs = be.lhs ();
  jit_value *lhsv = visit (lhs);

  tree_expression *rhs = be.rhs ();
  jit_value *rhsv = visit (rhs);

  const jit_function& fn = jit_typeinfo::binary_op (be.op_type ());
  result = block->append (new jit_call (fn, lhsv, rhsv));
}

void
jit_convert::visit_break_command (tree_break_command&)
{
  fail ();
}

void
jit_convert::visit_colon_expression (tree_colon_expression&)
{
  fail ();
}

void
jit_convert::visit_continue_command (tree_continue_command&)
{
  fail ();
}

void
jit_convert::visit_global_command (tree_global_command&)
{
  fail ();
}

void
jit_convert::visit_persistent_command (tree_persistent_command&)
{
  fail ();
}

void
jit_convert::visit_decl_elt (tree_decl_elt&)
{
  fail ();
}

void
jit_convert::visit_decl_init_list (tree_decl_init_list&)
{
  fail ();
}

void
jit_convert::visit_simple_for_command (tree_simple_for_command& cmd)
{
  // how a for statement is compiled. Note we do an initial check
  // to see if the loop will run atleast once. This allows us to get
  // better type inference bounds on variables defined and used only
  // inside the for loop (e.g. the index variable)

  // prev_block: % pred = ?
  //  #control.0 = % compute_control (note this will just be a temp)
  //  #iter.0 = call for_init (#control.0) % Let type of control decide iter
  //                                       % initial value and type
  //  #temp.0 = call for_check (control.0, #iter.0)
  //  cond_break #temp.0, for_body, for_tail
  // for_body: % pred = for_init, for_cond
  //  idxvar.2 = phi | for_init -> idxvar.1
  //                 | for_body -> idxvar.3
  //  #iter.1 = phi | for_init -> #iter.0
  //                | for_body -> #iter.2
  //  idxvar.3 = call for_index (#control.0, #iter.1)
  //  % do loop body
  //  #iter.2 = #iter.1 + 1 % release is implicit in iter reuse
  //  #check = call for_check (#control.0, iter.2)
  //  cond_break #check for_body, for_tail
  // for_tail: % pred = prev_block, for_body
  //  #iter.3 = phi | prev_block -> #iter.0
  //                | for_body -> #iter.2
  //  idxvar.4 = phi | prev_block -> idxvar.0
  //                 | for_body -> idxvar.3
  //  call release (#iter.3)
  //  % rest of code

  // FIXME: one of these days we will introduce proper lvalues...
  tree_identifier *lhs = dynamic_cast<tree_identifier *>(cmd.left_hand_side ());
  if (! lhs)
    fail ();
  std::string lhs_name = lhs->name ();

  jit_block *body = new jit_block ("for_body");
  blocks.push_back (body);

  jit_block *tail = new jit_block ("for_tail");
  unwind_protect prot_tail;
  prot_tail.add_delete (tail); // incase we fail before adding tail to blocks

  // do control expression, iter init, and condition check in prev_block (block)
  jit_value *control = visit (cmd.control_expr ());
  jit_call *init_iter = new jit_call (jit_typeinfo::for_init, control);
  init_iter->stash_tag ("#iter");
  block->append (init_iter);
  jit_value *check = block->append (new jit_call (jit_typeinfo::for_check,
                                                  control, init_iter));
  block->append (new jit_cond_break (check, body, tail));

  // we need to do iter phi manually, for_map handles the rest
  jit_phi *iter_phi = new jit_phi (2);
  iter_phi->stash_tag ("#iter");
  iter_phi->stash_argument (1, init_iter);
  body->append (iter_phi);

  variable_map *merge_vars = variables;
  for_map body_vars (variables, body);
  variables = &body_vars;
  block = body;

  // first thing we do in the for loop is bind our index from our itertor
  jit_call *idx_rhs = new jit_call (jit_typeinfo::for_index, control, iter_phi);
  block->append (idx_rhs);
  idx_rhs->stash_tag (lhs_name);
  do_assign (lhs_name, idx_rhs, false);
  
  tree_statement_list *pt_body = cmd.body ();
  pt_body->accept (*this);

  // increment iterator, check conditional, and repeat
  const jit_function& add_fn = jit_typeinfo::binary_op (octave_value::op_add);
  jit_call *iter_inc = new jit_call (add_fn, iter_phi,
                                     get_const<jit_const_index> (1));
  iter_inc->stash_tag ("#iter");
  block->append (iter_inc);
  check = block->append (new jit_call (jit_typeinfo::for_check, control,
                                       iter_inc));
  block->append (new jit_cond_break (check, body, tail));
  iter_phi->stash_argument (0, iter_inc);
  body_vars.finish_phi (*variables);

  blocks.push_back (tail);
  prot_tail.discard ();
  block = tail;

  variables = merge_vars;
  merge (body_vars);
  iter_phi = new jit_phi (2);
  iter_phi->stash_tag ("#iter");
  iter_phi->stash_argument (0, iter_inc);
  iter_phi->stash_argument (1, init_iter);
  block->append (iter_phi);
  block->append (new jit_call (jit_typeinfo::release, iter_phi));
}

void
jit_convert::visit_complex_for_command (tree_complex_for_command&)
{
  fail ();
}

void
jit_convert::visit_octave_user_script (octave_user_script&)
{
  fail ();
}

void
jit_convert::visit_octave_user_function (octave_user_function&)
{
  fail ();
}

void
jit_convert::visit_octave_user_function_header (octave_user_function&)
{
  fail ();
}

void
jit_convert::visit_octave_user_function_trailer (octave_user_function&)
{
  fail ();
}

void
jit_convert::visit_function_def (tree_function_def&)
{
  fail ();
}

void
jit_convert::visit_identifier (tree_identifier& ti)
{
  const jit_function& fn = jit_typeinfo::grab ();
  jit_value *var = variables->get (ti.name ());
  result = block->append (new jit_call (fn, var));
}

void
jit_convert::visit_if_clause (tree_if_clause&)
{
  fail ();
}

void
jit_convert::visit_if_command (tree_if_command&)
{
  fail ();
}

void
jit_convert::visit_if_command_list (tree_if_command_list&)
{
  fail ();
}

void
jit_convert::visit_index_expression (tree_index_expression&)
{
  fail ();
}

void
jit_convert::visit_matrix (tree_matrix&)
{
  fail ();
}

void
jit_convert::visit_cell (tree_cell&)
{
  fail ();
}

void
jit_convert::visit_multi_assignment (tree_multi_assignment&)
{
  fail ();
}

void
jit_convert::visit_no_op_command (tree_no_op_command&)
{
  fail ();
}

void
jit_convert::visit_constant (tree_constant& tc)
{
  octave_value v = tc.rvalue1 ();
  if (v.is_real_scalar () && v.is_double_type ())
    {
      double dv = v.double_value ();
      result = get_const<jit_const_scalar> (dv);
    }
  else if (v.is_range ())
    {
      Range rv = v.range_value ();
      result = get_const<jit_const_range> (rv);
    }
  else
    fail ();
}

void
jit_convert::visit_fcn_handle (tree_fcn_handle&)
{
  fail ();
}

void
jit_convert::visit_parameter_list (tree_parameter_list&)
{
  fail ();
}

void
jit_convert::visit_postfix_expression (tree_postfix_expression&)
{
  fail ();
}

void
jit_convert::visit_prefix_expression (tree_prefix_expression&)
{
  fail ();
}

void
jit_convert::visit_return_command (tree_return_command&)
{
  fail ();
}

void
jit_convert::visit_return_list (tree_return_list&)
{
  fail ();
}

void
jit_convert::visit_simple_assignment (tree_simple_assignment& tsa)
{
  // resolve rhs
  tree_expression *rhs = tsa.right_hand_side ();
  jit_value *rhsv = visit (rhs);

  // resolve lhs
  tree_expression *lhs = tsa.left_hand_side ();
  if (! lhs->is_identifier ())
    fail ();

  std::string lhs_name = lhs->name ();
  do_assign (lhs_name, rhsv, tsa.print_result ());
  result = rhsv;

  if (jit_instruction *instr = dynamic_cast<jit_instruction *>(rhsv))
    instr->stash_tag (lhs_name);
}

void
jit_convert::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();
  tree_expression *expr = stmt.expression ();

  if (cmd)
    visit (cmd);
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

      jit_value *expr_result = visit (expr);

      if (do_bind_ans)
        do_assign ("ans", expr_result, expr->print_result ());
      else if (expr->is_identifier () && expr->print_result ())
        {
          // FIXME: ugly hack, we need to come up with a way to pass
          // nargout to visit_identifier
          const jit_function& fn = jit_typeinfo::print_value ();
          jit_const_string *name = get_const<jit_const_string> (expr->name ());
          block->append (new jit_call (fn, name, expr_result));
        }
    }
}

void
jit_convert::visit_statement_list (tree_statement_list& lst)
{
  for (tree_statement_list::iterator iter = lst.begin (); iter != lst.end();
       ++iter)
    {
      tree_statement *elt = *iter;
      // jwe: Can this ever be null?
      assert (elt);
      elt->accept (*this);
    }
}

void
jit_convert::visit_switch_case (tree_switch_case&)
{
  fail ();
}

void
jit_convert::visit_switch_case_list (tree_switch_case_list&)
{
  fail ();
}

void
jit_convert::visit_switch_command (tree_switch_command&)
{
  fail ();
}

void
jit_convert::visit_try_catch_command (tree_try_catch_command&)
{
  fail ();
}

void
jit_convert::visit_unwind_protect_command (tree_unwind_protect_command&)
{
  fail ();
}

void
jit_convert::visit_while_command (tree_while_command&)
{
  fail ();
}

void
jit_convert::visit_do_until_command (tree_do_until_command&)
{
  fail ();
}

void
jit_convert::do_assign (const std::string& lhs, jit_value *rhs, bool print)
{
  const jit_function& release = jit_typeinfo::release ();
  jit_value *current = variables->get (lhs);
  block->append (new jit_call (release, current));
  variables->set (lhs, rhs);

  if (print)
    {
      const jit_function& print_fn = jit_typeinfo::print_value ();
      jit_const_string *name = get_const<jit_const_string> (lhs);
      block->append (new jit_call (print_fn, name, rhs));
    }
}

jit_value *
jit_convert::visit (tree& tee)
{
  result = 0;
  tee.accept (*this);

  jit_value *ret = result;
  result = 0;
  return ret;
}

void
jit_convert::merge (const variable_map& ref)
{
  assert (variables->size () == ref.size ());
  variable_map::iterator viter = variables->begin ();
  variable_map::const_iterator riter = ref.begin ();
  for (; viter != variables->end (); ++viter, ++riter)
    {
      assert (viter->first == riter->first);
      if (viter->second != riter->second)
        {
          jit_phi *phi = new jit_phi (2);
          phi->stash_tag (viter->first);
          block->prepend (phi);
          phi->stash_argument (0, riter->second);
          phi->stash_argument (1, viter->second);
          viter->second = phi;
        }
    }
}

// -------------------- jit_convert::toplevel_map --------------------
jit_value *
jit_convert::toplevel_map::insert (const std::string& name, jit_value *pval)
{
  assert (pval == 0); // we have no parent

  jit_block *entry = block ();
  octave_value val = symbol_table::find (name);
  jit_type *type = jit_typeinfo::type_of (val);
  jit_instruction *ret = new jit_extract_argument (type, name);
  return vars[name] = entry->prepend (ret);
}

// -------------------- jit_convert::convert_llvm --------------------
llvm::Function *
jit_convert::convert_llvm::convert (llvm::Module *module,
                                    const std::vector<std::pair< std::string, bool> >& args,
                                    const std::list<jit_block *>& blocks,
                                    const std::list<jit_value *>& constants)
{
  jit_type *any = jit_typeinfo::get_any ();

  // argument is an array of octave_base_value*, or octave_base_value**
  llvm::Type *arg_type = any->to_llvm (); // this is octave_base_value*
  arg_type = arg_type->getPointerTo ();
  llvm::FunctionType *ft = llvm::FunctionType::get (llvm::Type::getVoidTy (context),
                                                    arg_type, false);
  function = llvm::Function::Create (ft, llvm::Function::ExternalLinkage,
                                     "foobar", module);

  try
    {
      llvm::BasicBlock *prelude = llvm::BasicBlock::Create (context, "prelude",
                                                            function);
      builder.SetInsertPoint (prelude);

      llvm::Value *arg = function->arg_begin ();
      for (size_t i = 0; i < args.size (); ++i)
        {
          llvm::Value *loaded_arg = builder.CreateConstInBoundsGEP1_32 (arg, i);
          arguments[args[i].first] = loaded_arg;
        }

      // we need to generate llvm values for constants, as these don't appear in
      // a block
      for (std::list<jit_value *>::const_iterator iter = constants.begin ();
           iter != constants.end (); ++iter)
        visit (*iter);

      std::list<jit_block *>::const_iterator biter;
      for (biter = blocks.begin (); biter != blocks.end (); ++biter)
        {
          jit_block *jblock = *biter;
          llvm::BasicBlock *block = llvm::BasicBlock::Create (context, jblock->name (),
                                                              function);
          jblock->stash_llvm (block);
        }

      jit_block *first = *blocks.begin ();
      builder.CreateBr (first->to_llvm ());

      // convert all instructions
      for (biter = blocks.begin (); biter != blocks.end (); ++biter)
        visit (*biter);

      // now finish phi nodes
      for (biter = blocks.begin (); biter != blocks.end (); ++biter)
        {
          jit_block& block = **biter;
          for (jit_block::iterator piter = block.begin ();
               piter != block.end () && dynamic_cast<jit_phi *> (*piter); ++piter)
            {
              // our phi nodes don't have to have the same incomming type,
              // so we do casts here
              jit_instruction *phi = *piter;
              jit_block *pblock = phi->parent ();
              llvm::PHINode *llvm_phi = llvm::cast<llvm::PHINode> (phi->to_llvm ());
              for (size_t i = 0; i < phi->argument_count (); ++i)
                {
                  llvm::BasicBlock *pred = pblock->pred_llvm (i);
                  if (phi->argument_type_llvm (i) == phi->type_llvm ())
                    {
                      llvm_phi->addIncoming (phi->argument_llvm (i), pred);
                    }
                  else
                    {
                      // add cast right before pred terminator
                      builder.SetInsertPoint (--pred->end ());

                      const jit_function::overload& ol
                        = jit_typeinfo::cast (phi->type (),
                                              phi->argument_type (i));
                      if (! ol.function)
                        {
                          std::stringstream ss;
                          ss << "No cast for phi(" << i << "): ";
                          phi->print (ss);
                          fail (ss.str ());
                        }

                      llvm::Value *casted;
                      casted = builder.CreateCall (ol.function,
                                                   phi->argument_llvm (i));
                      llvm_phi->addIncoming (casted, pred);
                    }
                }
            }
        }

      jit_block *last = blocks.back ();
      builder.SetInsertPoint (last->to_llvm ());
      builder.CreateRetVoid ();
    } catch (const jit_fail_exception& e)
    {
      function->eraseFromParent ();
      throw;
    }

  return function;
}

void
jit_convert::convert_llvm::visit (jit_const_string& cs)
{
  cs.stash_llvm (builder.CreateGlobalStringPtr (cs.value ()));
}

void
jit_convert::convert_llvm::visit (jit_const_scalar& cs)
{
  cs.stash_llvm (llvm::ConstantFP::get (cs.type_llvm (), cs.value ()));
}

void jit_convert::convert_llvm::visit (jit_const_index& ci)
{
  ci.stash_llvm (llvm::ConstantInt::get (ci.type_llvm (), ci.value ()));
}

void
jit_convert::convert_llvm::visit (jit_const_range& cr)
{
  llvm::StructType *stype = llvm::cast<llvm::StructType>(cr.type_llvm ());
  llvm::Type *dbl = jit_typeinfo::get_scalar_llvm ();
  llvm::Type *idx = jit_typeinfo::get_index_llvm ();
  const jit_range& rng = cr.value ();

  llvm::Constant *constants[4];
  constants[0] = llvm::ConstantFP::get (dbl, rng.base);
  constants[1] = llvm::ConstantFP::get (dbl, rng.limit);
  constants[2] = llvm::ConstantFP::get (dbl, rng.inc);
  constants[3] = llvm::ConstantInt::get (idx, rng.nelem);

  llvm::Value *as_llvm;
  as_llvm = llvm::ConstantStruct::get (stype,
                                       llvm::makeArrayRef (constants, 4));
  cr.stash_llvm (as_llvm);
}

void
jit_convert::convert_llvm::visit (jit_block& b)
{
  llvm::BasicBlock *block = b.to_llvm ();
  builder.SetInsertPoint (block);
  for (jit_block::iterator iter = b.begin (); iter != b.end (); ++iter)
    visit (*iter);
}

void
jit_convert::convert_llvm::visit (jit_break& b)
{
  b.stash_llvm (builder.CreateBr (b.sucessor_llvm ()));
}

void
jit_convert::convert_llvm::visit (jit_cond_break& cb)
{
  llvm::Value *cond = cb.cond_llvm ();
  llvm::Value *br;
  br = builder.CreateCondBr (cond, cb.sucessor_llvm (0), cb.sucessor_llvm (1));
  cb.stash_llvm (br);
}

void
jit_convert::convert_llvm::visit (jit_call& call)
{
  const jit_function::overload& ol = call.overload ();
  if (! ol.function)
    fail ("No overload for: " + call.print_string ());
  
  std::vector<llvm::Value *> args (call.argument_count ());
  for (size_t i = 0; i < call.argument_count (); ++i)
    args[i] = call.argument_llvm (i);

  call.stash_llvm (builder.CreateCall (ol.function, args, call.tag ()));
}

void
jit_convert::convert_llvm::visit (jit_extract_argument& extract)
{
  const jit_function::overload& ol = extract.overload ();
  if (! ol.function)
    fail ();

  llvm::Value *arg = arguments[extract.tag ()];
  assert (arg);
  arg = builder.CreateLoad (arg);
  extract.stash_llvm (builder.CreateCall (ol.function, arg, extract.tag ()));
}

void
jit_convert::convert_llvm::visit (jit_store_argument& store)
{
  llvm::Value *arg_value = store.result_llvm ();
  const jit_function::overload& ol = store.overload ();
  if (! ol.function)
    fail ();

  arg_value = builder.CreateCall (ol.function, arg_value);

  llvm::Value *arg = arguments[store.tag ()];
  store.stash_llvm (builder.CreateStore (arg_value, arg));
}

void
jit_convert::convert_llvm::visit (jit_phi& phi)
{
  // we might not have converted all incoming branches, so we don't
  // set incomming branches now
  llvm::PHINode *node = llvm::PHINode::Create (phi.type_llvm (),
                                               phi.argument_count (),
                                               phi.tag ());
  builder.Insert (node);
  phi.stash_llvm (node);

  jit_block *parent = phi.parent ();
  for (size_t i = 0; i < phi.argument_count (); ++i)
    if (phi.argument_type (i) != phi.type ())
      parent->create_merge (function, i);
}

// -------------------- tree_jit --------------------

tree_jit::tree_jit (void) : module (0), engine (0)
{
}

tree_jit::~tree_jit (void)
{}

bool
tree_jit::execute (tree_simple_for_command& cmd)
{
  if (! initialize ())
    return false;

  jit_info *info = cmd.get_info ();
  if (! info || ! info->match ())
    {
      delete info;
      info = new jit_info (*this, cmd);
      cmd.stash_info (info);
    }

  return info->execute ();
}

bool
tree_jit::initialize (void)
{
  if (engine)
    return true;

  if (! module)
    {
      llvm::InitializeNativeTarget ();
      module = new llvm::Module ("octave", context);
    }

  // sometimes this fails pre main
  engine = llvm::ExecutionEngine::createJIT (module);

  if (! engine)
    return false;

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

  jit_typeinfo::initialize (module, engine);

  return true;
}


void
tree_jit::optimize (llvm::Function *fn)
{
  module_pass_manager->run (*module);
  pass_manager->run (*fn);
}

// -------------------- jit_info --------------------
jit_info::jit_info (tree_jit& tjit, tree& tee)
  : engine (tjit.get_engine ())
{
  llvm::Function *fun = 0;
  try
    {
      jit_convert conv (tjit.get_module (), tee);
      fun = conv.get_function ();
      arguments = conv.get_arguments ();
      bounds = conv.get_bounds ();
    }
  catch (const jit_fail_exception& e)
    {
      if (debug_print && e.known ())
        std::cout << "jit fail: " << e.what () << std::endl;
    }

  if (! fun)
    {
      function = 0;
      return;
    }

  tjit.optimize (fun);

  if (debug_print)
    {
      std::cout << "-------------------- optimized llvm ir --------------------\n";
      llvm::raw_os_ostream llvm_cout (std::cout);
      fun->print (llvm_cout);
      std::cout << std::endl;
    }

  function = reinterpret_cast<jited_function>(engine->getPointerToFunction (fun));
}

bool
jit_info::execute (void) const
{
  if (! function)
    return false;

  std::vector<octave_base_value *> real_arguments (arguments.size ());
  for (size_t i = 0; i < arguments.size (); ++i)
    {
      if (arguments[i].second)
        {
          octave_value current = symbol_table::varval (arguments[i].first);
          octave_base_value *obv = current.internal_rep ();
          obv->grab ();
          real_arguments[i] = obv;
        }
    }

  function (&real_arguments[0]);

  for (size_t i = 0; i < arguments.size (); ++i)
    symbol_table::varref (arguments[i].first) = real_arguments[i];

  return true;
}

bool
jit_info::match (void) const
{
  if (! function)
    return true;

  for (size_t i = 0; i < bounds.size (); ++i)
    {
      const std::string& arg_name = bounds[i].second;
      octave_value value = symbol_table::find (arg_name);
      jit_type *type = jit_typeinfo::type_of (value);

      // FIXME: Check for a parent relationship
      if (type != bounds[i].first)
        return false;
    }

  return true;
}
