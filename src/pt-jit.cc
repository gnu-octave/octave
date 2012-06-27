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

#ifdef HAVE_LLVM

#include "pt-jit.h"

#include <typeinfo>

#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/BasicBlock.h>
#include <llvm/Intrinsics.h>
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
#include "ov-builtin.h"
#include "ov-scalar.h"
#include "pt-all.h"

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

static void fail (void) GCC_ATTR_NORETURN;
static void fail (const std::string&) GCC_ATTR_NORETURN;

static void
fail (void)
{
  throw jit_fail_exception ();
}

#ifdef OCTAVE_JIT_DEBUG
static void
fail (const std::string& reason)
{
  throw jit_fail_exception (reason);
}
#else
static void
fail (const std::string&)
{
  throw jit_fail_exception ();
}
#endif // OCTAVE_JIT_DEBUG

std::ostream& jit_print (std::ostream& os, jit_type *atype)
{
  if (! atype)
    return os << "null";
  return os << atype->name ();
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
  octave_value olhs (lhs, true);
  octave_value orhs (rhs, true);
  octave_value result = do_binary_op (op, olhs, orhs);
  octave_base_value *rep = result.internal_rep ();
  rep->grab ();
  return rep;
}

extern "C" octave_idx_type
octave_jit_compute_nelem (double base, double limit, double inc)
{
  Range rng = Range (base, limit, inc);
  return rng.nelem ();
}

extern "C" void
octave_jit_release_any (octave_base_value *obv)
{
  obv->release ();
}

extern "C" void
octave_jit_release_matrix (jit_matrix *m)
{
  delete m->array;
}

extern "C" octave_base_value *
octave_jit_grab_any (octave_base_value *obv)
{
  obv->grab ();
  return obv;
}

extern "C" void
octave_jit_grab_matrix (jit_matrix *result, jit_matrix *m)
{
  *result = *m->array;
}

extern "C" octave_base_value *
octave_jit_cast_any_matrix (jit_matrix *m)
{
  octave_value ret (*m->array);
  octave_base_value *rep = ret.internal_rep ();
  rep->grab ();

  return rep;
}

extern "C" void
octave_jit_cast_matrix_any (jit_matrix *ret, octave_base_value *obv)
{
  NDArray m = obv->array_value ();
  *ret = m;
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

extern "C" void
octave_jit_gripe_nan_to_logical_conversion (void)
{
  try
    {
      gripe_nan_to_logical_conversion ();
    }
  catch (const octave_execution_exception&)
    {
      gripe_library_execution_error ();
    }
}

extern "C" void
octave_jit_ginvalid_index (void)
{
  try
    {
      gripe_invalid_index ();
    }
  catch (const octave_execution_exception&)
    {
      gripe_library_execution_error ();
    }
}

extern "C" void
octave_jit_gindex_range (int nd, int dim, octave_idx_type iext,
                         octave_idx_type ext)
{
  try
    {
      gripe_index_out_of_range (nd, dim, iext, ext);
    }
  catch (const octave_execution_exception&)
    {
      gripe_library_execution_error ();
    }
}

extern "C" void
octave_jit_paren_subsasgn_impl (jit_matrix *mat, octave_idx_type index,
                                double value)
{
  NDArray *array = mat->array;
  if (array->nelem () < index)
    array->resize1 (index);

  double *data = array->fortran_vec ();
  data[index - 1] = value;

  mat->ref_count = array->jit_ref_count ();
  mat->slice_data = array->jit_slice_data () - 1;
  mat->dimensions = array->jit_dimensions ();
  mat->slice_len = array->nelem ();
}

extern "C" void
octave_jit_print_matrix (jit_matrix *m)
{
  std::cout << *m << std::endl;
}

static void
gripe_bad_result (void)
{
  error ("incorrect type information given to the JIT compiler");
}

// FIXME: Add support for multiple outputs
extern "C" octave_base_value *
octave_jit_call (octave_builtin::fcn fn, size_t nargin,
                 octave_base_value **argin, jit_type *result_type)
{
  octave_value_list ovl (nargin);
  for (size_t i = 0; i < nargin; ++i)
    ovl.xelem (i) = octave_value (argin[i]);

  ovl = fn (ovl, 1);

  // These type checks are not strictly required, but I'm guessing that
  // incorrect types will be entered on occasion. This will be very difficult to
  // debug unless we do the sanity check here.
  if (result_type)
    {
      if (ovl.length () != 1)
        {
          gripe_bad_result ();
          return 0;
        }

      octave_value& result = ovl.xelem (0);
      jit_type *jtype = jit_typeinfo::join (jit_typeinfo::type_of (result),
                                            result_type);
      if (jtype != result_type)
        {
          gripe_bad_result ();
          return 0;
        }

      octave_base_value *ret = result.internal_rep ();
      ret->grab ();
      return ret;
    }

  if (! (ovl.length () == 0
         || (ovl.length () == 1 && ovl.xelem (0).is_undefined ())))
    gripe_bad_result ();

  return 0;
}

// -------------------- jit_range --------------------
std::ostream&
operator<< (std::ostream& os, const jit_range& rng)
{
  return os << "Range[" << rng.base << ", " << rng.limit << ", " << rng.inc
            << ", " << rng.nelem << "]";
}

// -------------------- jit_matrix --------------------

std::ostream&
operator<< (std::ostream& os, const jit_matrix& mat)
{
  return os << "Matrix[" << mat.ref_count << ", " << mat.slice_data << ", "
            << mat.slice_len << ", " << mat.dimensions << ", "
            << mat.array << "]";
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
  llvm::Type *any_t = llvm::StructType::create (context, "octave_base_value");
  any_t = any_t->getPointerTo ();

  llvm::Type *scalar_t = llvm::Type::getDoubleTy (context);
  llvm::Type *bool_t = llvm::Type::getInt1Ty (context);
  llvm::Type *string_t = llvm::Type::getInt8Ty (context);
  string_t = string_t->getPointerTo ();
  llvm::Type *index_t = llvm::Type::getIntNTy (context,
                                               sizeof(octave_idx_type) * 8);

  llvm::StructType *range_t = llvm::StructType::create (context, "range");
  std::vector<llvm::Type *> range_contents (4, scalar_t);
  range_contents[3] = index_t;
  range_t->setBody (range_contents);

  llvm::Type *refcount_t = llvm::Type::getIntNTy (context, sizeof(int) * 8);
  llvm::Type *int_t = refcount_t;

  llvm::StructType *matrix_t = llvm::StructType::create (context, "matrix");
  llvm::Type *matrix_contents[5];
  matrix_contents[0] = refcount_t->getPointerTo ();
  matrix_contents[1] = scalar_t->getPointerTo ();
  matrix_contents[2] = index_t;
  matrix_contents[3] = index_t->getPointerTo ();
  matrix_contents[4] = string_t;
  matrix_t->setBody (llvm::makeArrayRef (matrix_contents, 5));

  // create types
  any = new_type ("any", 0, any_t);
  matrix = new_type ("matrix", any, matrix_t);
  scalar = new_type ("scalar", any, scalar_t);
  range = new_type ("range", any, range_t);
  string = new_type ("string", any, string_t);
  boolean = new_type ("bool", any, bool_t);
  index = new_type ("index", any, index_t);

  casts.resize (next_id + 1);
  identities.resize (next_id + 1, 0);

  // bind global variables
  lerror_state = new llvm::GlobalVariable (*module, bool_t, false,
                                           llvm::GlobalValue::ExternalLinkage,
                                           0, "error_state");
  engine->addGlobalMapping (lerror_state,
                            reinterpret_cast<void *> (&error_state));

  // any with anything is an any op
  llvm::Function *fn;
  llvm::Type *binary_op_type
    = llvm::Type::getIntNTy (context, sizeof (octave_value::binary_op));
  llvm::Function *any_binary = create_function ("octave_jit_binary_any_any",
                                                any_t, binary_op_type,
                                                any_t, any_t);
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
      binary_ops[op].add_overload (fn, true, any, any, any);
    }

  llvm::Type *void_t = llvm::Type::getVoidTy (context);

  // grab any
  fn = create_function ("octave_jit_grab_any", any, any);
  engine->addGlobalMapping (fn, reinterpret_cast<void*>(&octave_jit_grab_any));
  grab_fn.add_overload (fn, false, any, any);
  grab_fn.stash_name ("grab");

  // grab matrix
  llvm::Function *print_matrix = create_function ("octave_jit_print_matrix",
                                                  void_t,
                                                  matrix_t->getPointerTo ());
  engine->addGlobalMapping (print_matrix,
                            reinterpret_cast<void*>(&octave_jit_print_matrix));
  fn = create_function ("octave_jit_grab_matrix", void_t,
                        matrix_t->getPointerTo (), matrix_t->getPointerTo ());
  engine->addGlobalMapping (fn,
                            reinterpret_cast<void *> (&octave_jit_grab_matrix));
  grab_fn.add_overload (fn, false, matrix, matrix);

  // release any
  fn = create_function ("octave_jit_release_any", void_t, any_t);
  llvm::Function *release_any = fn;
  engine->addGlobalMapping (fn,
                            reinterpret_cast<void*>(&octave_jit_release_any));
  release_fn.add_overload (fn, false, 0, any);
  release_fn.stash_name ("release");

  // release matrix
  fn = create_function ("octave_jit_release_matrix", void_t,
                        matrix_t->getPointerTo ());
  engine->addGlobalMapping (fn,
                            reinterpret_cast<void *> (&octave_jit_release_matrix));
  release_fn.add_overload (fn, false, 0, matrix);

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

  add_binary_fcmp (scalar, octave_value::op_lt, llvm::CmpInst::FCMP_ULT);
  add_binary_fcmp (scalar, octave_value::op_le, llvm::CmpInst::FCMP_ULE);
  add_binary_fcmp (scalar, octave_value::op_eq, llvm::CmpInst::FCMP_UEQ);
  add_binary_fcmp (scalar, octave_value::op_ge, llvm::CmpInst::FCMP_UGE);
  add_binary_fcmp (scalar, octave_value::op_gt, llvm::CmpInst::FCMP_UGT);
  add_binary_fcmp (scalar, octave_value::op_ne, llvm::CmpInst::FCMP_UNE);

  llvm::Function *gripe_div0 = create_function ("gripe_divide_by_zero", void_t);
  engine->addGlobalMapping (gripe_div0,
                            reinterpret_cast<void *> (&gripe_divide_by_zero));

  // divide is annoying because it might error
  fn = create_function ("octave_jit_div_scalar_scalar", scalar, scalar, scalar);
  llvm::BasicBlock *body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::BasicBlock *warn_block = llvm::BasicBlock::Create (context, "warn",
                                                             fn);
    llvm::BasicBlock *normal_block = llvm::BasicBlock::Create (context,
                                                               "normal", fn);

    llvm::Value *zero = llvm::ConstantFP::get (scalar_t, 0);
    llvm::Value *check = builder.CreateFCmpUEQ (zero, ++fn->arg_begin ());
    builder.CreateCondBr (check, warn_block, normal_block);

    builder.SetInsertPoint (warn_block);
    builder.CreateCall (gripe_div0);
    builder.CreateBr (normal_block);

    builder.SetInsertPoint (normal_block);
    llvm::Value *ret = builder.CreateFDiv (fn->arg_begin (),
                                           ++fn->arg_begin ());
    builder.CreateRet (ret);

    jit_function::overload ol (fn, true, scalar, scalar, scalar);
    binary_ops[octave_value::op_div].add_overload (ol);
    binary_ops[octave_value::op_el_div].add_overload (ol);
  }
  llvm::verifyFunction (*fn);

  // ldiv is the same as div with the operators reversed
  llvm::Function *div = fn;
  fn = create_function ("octave_jit_ldiv_scalar_scalar", scalar, scalar,
                        scalar);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::Value *ret = builder.CreateCall2 (div, ++fn->arg_begin (),
                                            fn->arg_begin ());
    builder.CreateRet (ret);

    jit_function::overload ol (fn, true, scalar, scalar, scalar);
    binary_ops[octave_value::op_ldiv].add_overload (ol);
    binary_ops[octave_value::op_el_ldiv].add_overload (ol);
  }
  llvm::verifyFunction (*fn);

  // now for binary index operators
  add_binary_op (index, octave_value::op_add, llvm::Instruction::Add);

  // and binary bool operators
  add_binary_op (boolean, octave_value::op_el_or, llvm::Instruction::Or);
  add_binary_op (boolean, octave_value::op_el_and, llvm::Instruction::And);

  // now for printing functions
  print_fn.stash_name ("print");
  add_print (any, reinterpret_cast<void*> (&octave_jit_print_any));
  add_print (scalar, reinterpret_cast<void*> (&octave_jit_print_double));

  // initialize for loop
  for_init_fn.stash_name ("for_init");

  fn = create_function ("octave_jit_for_range_init", index, range);
  body = llvm::BasicBlock::Create (context, "body", fn);
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
    llvm::Value *didx = builder.CreateSIToFP (idx, scalar_t);
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
  logically_true_fn.stash_name ("logically_true");

  llvm::Function *gripe_nantl
    = create_function ("octave_jit_gripe_nan_to_logical_conversion", void_t);
  engine->addGlobalMapping (gripe_nantl, reinterpret_cast<void *> (&octave_jit_gripe_nan_to_logical_conversion));

  fn = create_function ("octave_jit_logically_true_scalar", boolean, scalar);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::BasicBlock *error_block = llvm::BasicBlock::Create (context, "error",
                                                              fn);
    llvm::BasicBlock *normal_block = llvm::BasicBlock::Create (context,
                                                               "normal", fn);

    llvm::Value *check = builder.CreateFCmpUNE (fn->arg_begin (),
                                                fn->arg_begin ());
    builder.CreateCondBr (check, error_block, normal_block);

    builder.SetInsertPoint (error_block);
    builder.CreateCall (gripe_nantl);
    builder.CreateBr (normal_block);
    builder.SetInsertPoint (normal_block);

    llvm::Value *zero = llvm::ConstantFP::get (scalar_t, 0);
    llvm::Value *ret = builder.CreateFCmpONE (fn->arg_begin (), zero);
    builder.CreateRet (ret);
  }
  llvm::verifyFunction (*fn);
  logically_true_fn.add_overload (fn, true, boolean, scalar);

  fn = create_function ("octave_logically_true_bool", boolean, boolean);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  builder.CreateRet (fn->arg_begin ());
  llvm::verifyFunction (*fn);
  logically_true_fn.add_overload (fn, false, boolean, boolean);

  // make_range
  // FIXME: May be benificial to implement all in LLVM
  make_range_fn.stash_name ("make_range");
  llvm::Function *compute_nelem
    = create_function ("octave_jit_compute_nelem", index, scalar, scalar,
                       scalar);
  engine->addGlobalMapping (compute_nelem,
                            reinterpret_cast<void*> (&octave_jit_compute_nelem));

  fn = create_function ("octave_jit_make_range", range, scalar, scalar, scalar);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::Function::arg_iterator args = fn->arg_begin ();
    llvm::Value *base = args;
    llvm::Value *limit = ++args;
    llvm::Value *inc = ++args;
    llvm::Value *nelem = builder.CreateCall3 (compute_nelem, base, limit, inc);

    llvm::Value *dzero = llvm::ConstantFP::get (scalar_t, 0);
    llvm::Value *izero = llvm::ConstantInt::get (index_t, 0);
    llvm::Value *rng = llvm::ConstantStruct::get (range_t, dzero, dzero, dzero,
                                                  izero, NULL);
    rng = builder.CreateInsertValue (rng, base, 0);
    rng = builder.CreateInsertValue (rng, limit, 1);
    rng = builder.CreateInsertValue (rng, inc, 2);
    rng = builder.CreateInsertValue (rng, nelem, 3);
    builder.CreateRet (rng);
  }
  llvm::verifyFunction (*fn);
  make_range_fn.add_overload (fn, false, range, scalar, scalar, scalar);

  // paren_subsref
  llvm::Function *ginvalid_index = create_function ("gipe_invalid_index",
                                                    void_t);
  engine->addGlobalMapping (ginvalid_index,
                            reinterpret_cast<void*> (&octave_jit_ginvalid_index));

  llvm::Function *gindex_range = create_function ("gripe_index_out_of_range",
                                                  void_t, int_t, int_t, index_t,
                                                  index_t);
  engine->addGlobalMapping (gindex_range,
                            reinterpret_cast<void*> (&octave_jit_gindex_range));

  fn = create_function ("()subsref", scalar, matrix, scalar);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::Value *one = llvm::ConstantInt::get (index_t, 1);
    llvm::Value *ione;
    if (index_t == int_t)
      ione = one;
    else
      ione = llvm::ConstantInt::get (int_t, 1);

    llvm::Value *undef = llvm::UndefValue::get (scalar_t);

    llvm::Function::arg_iterator args = fn->arg_begin ();
    llvm::Value *mat = args++;
    llvm::Value *idx = args;

    // convert index to scalar to integer, and check index >= 1
    llvm::Value *int_idx = builder.CreateFPToSI (idx, index_t);
    llvm::Value *check_idx = builder.CreateSIToFP (int_idx, scalar_t);
    llvm::Value *cond0 = builder.CreateFCmpUNE (idx, check_idx);
    llvm::Value *cond1 = builder.CreateICmpSLT (int_idx, one);
    llvm::Value *cond = builder.CreateOr (cond0, cond1);

    llvm::BasicBlock *done = llvm::BasicBlock::Create (context, "done", fn);

    llvm::BasicBlock *conv_error = llvm::BasicBlock::Create (context,
                                                             "conv_error", fn,
                                                             done);
    llvm::BasicBlock *normal = llvm::BasicBlock::Create (context, "normal", fn,
                                                         done);
    builder.CreateCondBr (cond, conv_error, normal);

    builder.SetInsertPoint (conv_error);
    builder.CreateCall (ginvalid_index);
    builder.CreateBr (done);

    builder.SetInsertPoint (normal);
    llvm::Value *len = builder.CreateExtractValue (mat,
                                                   llvm::ArrayRef<unsigned> (2));
    cond = builder.CreateICmpSGT (int_idx, len);


    llvm::BasicBlock *bounds_error = llvm::BasicBlock::Create (context,
                                                               "bounds_error",
                                                               fn, done);

    llvm::BasicBlock *success = llvm::BasicBlock::Create (context, "success",
                                                          fn, done);
    builder.CreateCondBr (cond, bounds_error, success);

    builder.SetInsertPoint (bounds_error);
    builder.CreateCall4 (gindex_range, ione, ione, int_idx, len);
    builder.CreateBr (done);

    builder.SetInsertPoint (success);
    llvm::Value *data = builder.CreateExtractValue (mat,
                                                    llvm::ArrayRef<unsigned> (1));
    llvm::Value *gep = builder.CreateInBoundsGEP (data, int_idx);
    llvm::Value *ret = builder.CreateLoad (gep);
    builder.CreateBr (done);

    builder.SetInsertPoint (done);

    llvm::PHINode *merge = llvm::PHINode::Create (scalar_t, 3);
    builder.Insert (merge);
    merge->addIncoming (undef, conv_error);
    merge->addIncoming (undef, bounds_error);
    merge->addIncoming (ret, success);
    builder.CreateRet (merge);
  }
  llvm::verifyFunction (*fn);
  paren_subsref_fn.add_overload (fn, true, scalar, matrix, scalar);

  // paren subsasgn
  paren_subsasgn_fn.stash_name ("()subsasgn");

  llvm::Function *resize_paren_subsasgn
    = create_function ("octave_jit_paren_subsasgn_impl", void_t,
                       matrix_t->getPointerTo (), index_t, scalar_t);
  engine->addGlobalMapping (resize_paren_subsasgn,
                            reinterpret_cast<void *> (&octave_jit_paren_subsasgn_impl));

  fn = create_function ("octave_jit_paren_subsasgn", matrix, matrix, scalar,
                        scalar);
  body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);
  {
    llvm::Value *one = llvm::ConstantInt::get (index_t, 1);

    llvm::Function::arg_iterator args = fn->arg_begin ();
    llvm::Value *mat = args++;
    llvm::Value *idx = args++;
    llvm::Value *value = args;

    llvm::Value *int_idx = builder.CreateFPToSI (idx, index_t);
    llvm::Value *check_idx = builder.CreateSIToFP (int_idx, scalar_t);
    llvm::Value *cond0 = builder.CreateFCmpUNE (idx, check_idx);
    llvm::Value *cond1 = builder.CreateICmpSLT (int_idx, one);
    llvm::Value *cond = builder.CreateOr (cond0, cond1);

    llvm::BasicBlock *done = llvm::BasicBlock::Create (context, "done", fn);

    llvm::BasicBlock *conv_error = llvm::BasicBlock::Create (context,
                                                             "conv_error", fn,
                                                             done);
    llvm::BasicBlock *normal = llvm::BasicBlock::Create (context, "normal", fn,
                                                         done);
    builder.CreateCondBr (cond, conv_error, normal);
    builder.SetInsertPoint (conv_error);
    builder.CreateCall (ginvalid_index);
    builder.CreateBr (done);

    builder.SetInsertPoint (normal);
    llvm::Value *len = builder.CreateExtractValue (mat,
                                                   llvm::ArrayRef<unsigned> (2));
    cond0 = builder.CreateICmpSGT (int_idx, len);

    llvm::Value *rcount = builder.CreateExtractValue (mat, 0);
    rcount = builder.CreateLoad (rcount);
    cond1 = builder.CreateICmpSGT (rcount, one);
    cond = builder.CreateOr (cond0, cond1);

    llvm::BasicBlock *bounds_error = llvm::BasicBlock::Create (context,
                                                               "bounds_error",
                                                               fn, done);

    llvm::BasicBlock *success = llvm::BasicBlock::Create (context, "success",
                                                          fn, done);
    builder.CreateCondBr (cond, bounds_error, success);

    // resize on out of bounds access
    builder.SetInsertPoint (bounds_error);
    llvm::Value *resize_result = builder.CreateAlloca (matrix_t);
    builder.CreateStore (mat, resize_result);
    builder.CreateCall3 (resize_paren_subsasgn, resize_result, int_idx, value);
    resize_result = builder.CreateLoad (resize_result);
    builder.CreateBr (done);

    builder.SetInsertPoint (success);
    llvm::Value *data = builder.CreateExtractValue (mat,
                                                    llvm::ArrayRef<unsigned> (1));
    llvm::Value *gep = builder.CreateInBoundsGEP (data, int_idx);
    builder.CreateStore (value, gep);
    builder.CreateBr (done);

    builder.SetInsertPoint (done);

    llvm::PHINode *merge = llvm::PHINode::Create (matrix_t, 3);
    builder.Insert (merge);
    merge->addIncoming (mat, conv_error);
    merge->addIncoming (resize_result, bounds_error);
    merge->addIncoming (mat, success);
    builder.CreateRet (merge);
  }
  llvm::verifyFunction (*fn);
  paren_subsasgn_fn.add_overload (fn, true, matrix, matrix, scalar, scalar);

  // paren_subsasgn

  casts[any->type_id ()].stash_name ("(any)");
  casts[scalar->type_id ()].stash_name ("(scalar)");

  // cast any <- matrix
  fn = create_function ("octave_jit_cast_any_matrix", any_t,
                        matrix_t->getPointerTo ());
  engine->addGlobalMapping (fn,
                            reinterpret_cast<void*> (&octave_jit_cast_any_matrix));
  casts[any->type_id ()].add_overload (fn, false, any, matrix);

  // cast matrix <- any
  fn = create_function ("octave_jit_cast_matrix_any", void_t,
                        matrix_t->getPointerTo (), any_t);
  engine->addGlobalMapping (fn,
                            reinterpret_cast<void*> (&octave_jit_cast_matrix_any));
  casts[matrix->type_id ()].add_overload (fn, false, matrix, any);

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


  // -------------------- builtin functions --------------------
  add_builtin ("sin");
  register_intrinsic ("sin", llvm::Intrinsic::sin, scalar, scalar);
  register_generic ("sin", matrix, matrix);

  add_builtin ("cos");
  register_intrinsic ("cos", llvm::Intrinsic::cos, scalar, scalar);
  register_generic ("cos", matrix, matrix);

  add_builtin ("exp");
  register_intrinsic ("exp", llvm::Intrinsic::cos, scalar, scalar);
  register_generic ("exp", matrix, matrix);

  casts.resize (next_id + 1);
  fn = create_identity (any);
  for (std::map<std::string, jit_type *>::iterator iter = builtins.begin ();
       iter != builtins.end (); ++iter)
    {
      jit_type *btype = iter->second;
      release_fn.add_overload (release_any, false, 0, btype);
      casts[any->type_id ()].add_overload (fn, false, any, btype);
      casts[btype->type_id ()].add_overload (fn, false, btype, any);
    }
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
jit_typeinfo::create_function (const llvm::Twine& name, jit_type *ret,
                               const std::vector<jit_type *>& args)
{
  llvm::Type *void_t = llvm::Type::getVoidTy (context);
  std::vector<llvm::Type *> llvm_args (args.size (), void_t);
  for (size_t i = 0; i < args.size (); ++i)
    if (args[i])
      llvm_args[i] = args[i]->to_llvm ();

  return create_function (name, ret ? ret->to_llvm () : void_t, llvm_args);
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

llvm::Value *
jit_typeinfo::do_insert_error_check (void)
{
  return builder.CreateLoad (lerror_state);
}

void
jit_typeinfo::add_builtin (const std::string& name)
{
  jit_type *btype = new_type (name, any, any->to_llvm ());
  builtins[name] = btype;

  octave_builtin *ov_builtin = find_builtin (name);
  if (ov_builtin)
    ov_builtin->stash_jit (*btype);
}

void
jit_typeinfo::register_intrinsic (const std::string& name, size_t iid,
                                  jit_type *result,
                                  const std::vector<jit_type *>& args)
{
  jit_type *builtin_type = builtins[name];
  size_t nargs = args.size ();
  llvm::SmallVector<llvm::Type *, 5> llvm_args (nargs);
  for (size_t i = 0; i < nargs; ++i)
    llvm_args[i] = args[i]->to_llvm ();

  llvm::Intrinsic::ID id = static_cast<llvm::Intrinsic::ID> (iid);
  llvm::Function *ifun = llvm::Intrinsic::getDeclaration (module, id,
                                                          llvm_args);
  std::stringstream fn_name;
  fn_name << "octave_jit_" << name;

  std::vector<jit_type *> args1 (nargs + 1);
  args1[0] = builtin_type;
  std::copy (args.begin (), args.end (), args1.begin () + 1);

  // The first argument will be the Octave function, but we already know that
  // the function call is the equivalent of the intrinsic, so we ignore it and
  // call the intrinsic with the remaining arguments.
  llvm::Function *fn = create_function (fn_name.str (), result, args1);
  llvm::BasicBlock *body = llvm::BasicBlock::Create (context, "body", fn);
  builder.SetInsertPoint (body);

  llvm::SmallVector<llvm::Value *, 5> fargs (nargs);
  llvm::Function::arg_iterator iter = fn->arg_begin ();
  ++iter;
  for (size_t i = 0; i < nargs; ++i, ++iter)
    fargs[i] = iter;

  llvm::Value *ret = builder.CreateCall (ifun, fargs);
  builder.CreateRet (ret);
  llvm::verifyFunction (*fn);

  paren_subsref_fn.add_overload (fn, false, result, args1);
}

octave_builtin *
jit_typeinfo::find_builtin (const std::string& name)
{
  // FIXME: Finalize what we want to store in octave_builtin, then add functions
  // to access these values in octave_value
  octave_value ov_builtin = symbol_table::find (name);
  return dynamic_cast<octave_builtin *> (ov_builtin.internal_rep ());
}

void
jit_typeinfo::register_generic (const std::string&, jit_type *,
                                const std::vector<jit_type *>&)
{
  // FIXME: Implement
}

jit_type *
jit_typeinfo::do_type_of (const octave_value &ov) const
{
  if (ov.is_function ())
    {
      // FIXME: This is ugly, we need to finalize how we want to to this, then
      // have octave_value fully support the needed functionality
      octave_builtin *builtin
        = dynamic_cast<octave_builtin *> (ov.internal_rep ());
      return builtin ? builtin->to_jit () : 0;
    }

  if (ov.is_range ())
    return get_range ();

  if (ov.is_double_type ())
    {
      if (ov.is_real_scalar ())
        return get_scalar ();

      if (ov.is_matrix_type ())
        return get_matrix ();
    }

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
  return muser->parent ();
}

// -------------------- jit_value --------------------
jit_value::~jit_value (void)
{}

jit_block *
jit_value::first_use_block (void)
{
  jit_use *use = first_use ();
  while (use)
    {
      if (! isa<jit_error_check> (use->user ()))
        return use->user_parent ();

      use = use->next ();
    }

  return 0;
}

void
jit_value::replace_with (jit_value *value)
{
  while (first_use ())
    {
      jit_instruction *user = first_use ()->user ();
      size_t idx = first_use ()->index ();
      user->stash_argument (idx, value);
    }
}

#define JIT_METH(clname)                                \
  void                                                  \
  jit_ ## clname::accept (jit_ir_walker& walker)        \
  {                                                     \
    walker.visit (*this);                               \
  }

JIT_VISIT_IR_NOTEMPLATE
#undef JIT_METH

std::ostream&
operator<< (std::ostream& os, const jit_value& value)
{
  return value.short_print (os);
}

std::ostream&
jit_print (std::ostream& os, jit_value *avalue)
{
  if (avalue)
    return avalue->print (os);
  return os << "NULL";
}

// -------------------- jit_instruction --------------------
void
jit_instruction::remove (void)
{
  if (mparent)
    mparent->remove (mlocation);
  resize_arguments (0);
}

llvm::BasicBlock *
jit_instruction::parent_llvm (void) const
{
  return mparent->to_llvm ();
}

std::ostream&
jit_instruction::short_print (std::ostream& os) const
{
  if (type ())
    jit_print (os, type ()) << ": ";
  return os << "#" << mid;
}

void
jit_instruction::do_construct_ssa (size_t start, size_t end)
{
  for (size_t i = start; i < end; ++i)
    {
      jit_value *arg = argument (i);
      jit_variable *var = dynamic_cast<jit_variable *> (arg);
      if (var && var->has_top ())
        stash_argument (i, var->top ());
    }
}

// -------------------- jit_block --------------------
void
jit_block::replace_with (jit_value *value)
{
  assert (isa<jit_block> (value));
  jit_block *block = static_cast<jit_block *> (value);

  jit_value::replace_with (block);

  while (ILIST_T::first_use ())
    {
      jit_phi_incomming *incomming = ILIST_T::first_use ();
      incomming->stash_value (block);
    }
}

void
jit_block::replace_in_phi (jit_block *ablock, jit_block *with)
{
  jit_phi_incomming *node = ILIST_T::first_use ();
  while (node)
    {
      jit_phi_incomming *prev = node;
      node = node->next ();

      if (prev->user_parent () == ablock)
        prev->stash_value (with);
    }
}

jit_block *
jit_block::maybe_merge ()
{
  if (successor_count () == 1 && successor (0) != this
      && (successor (0)->use_count () == 1 || instructions.size () == 1))
    {
      jit_block *to_merge = successor (0);
      merge (*to_merge);
      return to_merge;
    }

  return 0;
}

void
jit_block::merge (jit_block& block)
{
  // the merge block will contain a new terminator
  jit_terminator *old_term = terminator ();
  if (old_term)
    old_term->remove ();

  bool was_empty = end () == begin ();
  iterator merge_begin = end ();
  if (! was_empty)
    --merge_begin;

  instructions.splice (end (), block.instructions);
  if (was_empty)
    merge_begin = begin ();
  else
    ++merge_begin;

  // now merge_begin points to the start of the new instructions, we must
  // update their parent information
  for (iterator iter = merge_begin; iter != end (); ++iter)
    {
      jit_instruction *instr = *iter;
      instr->stash_parent (this, iter);
    }

  block.replace_with (this);
}

jit_instruction *
jit_block::prepend (jit_instruction *instr)
{
  instructions.push_front (instr);
  instr->stash_parent (this, instructions.begin ());
  return instr;
}

jit_instruction *
jit_block::prepend_after_phi (jit_instruction *instr)
{
  // FIXME: Make this O(1)
  for (iterator iter = begin (); iter != end (); ++iter)
    {
      jit_instruction *temp = *iter;
      if (! isa<jit_phi> (temp))
        {
          insert_before (iter, instr);
          return instr;
        }
    }

  return append (instr);
}

void
jit_block::internal_append (jit_instruction *instr)
{
  instructions.push_back (instr);
  instr->stash_parent (this, --instructions.end ());
}

jit_instruction *
jit_block::insert_before (iterator loc, jit_instruction *instr)
{
  iterator iloc = instructions.insert (loc, instr);
  instr->stash_parent (this, iloc);
  return instr;
}

jit_instruction *
jit_block::insert_after (iterator loc, jit_instruction *instr)
{
  ++loc;
  iterator iloc = instructions.insert (loc, instr);
  instr->stash_parent (this, iloc);
  return instr;
}

jit_terminator *
jit_block::terminator (void) const
{
  assert (this);
  if (instructions.empty ())
    return 0;

  jit_instruction *last = instructions.back ();
  return dynamic_cast<jit_terminator *> (last);
}

bool
jit_block::branch_alive (jit_block *asucc) const
{
  return terminator ()->alive (asucc);
}

jit_block *
jit_block::successor (size_t i) const
{
  jit_terminator *term = terminator ();
  return term->successor (i);
}

size_t
jit_block::successor_count (void) const
{
  jit_terminator *term = terminator ();
  return term ? term->successor_count () : 0;
}

llvm::BasicBlock *
jit_block::to_llvm (void) const
{
  return llvm::cast<llvm::BasicBlock> (llvm_value);
}

std::ostream&
jit_block::print_dom (std::ostream& os) const
{
  short_print (os);
  os << ":\n";
  os << "  mid: " << mid << std::endl;
  os << "  predecessors: ";
  for (jit_use *use = first_use (); use; use = use->next ())
    os << *use->user_parent () << " ";
  os << std::endl;

  os << "  successors: ";
  for (size_t i = 0; i < successor_count (); ++i)
    os << *successor (i) << " ";
  os << std::endl;

  os << "  idom: ";
  if (idom)
    os << *idom;
  else
    os << "NULL";
  os << std::endl;
  os << "  df: ";
  for (df_iterator iter = df_begin (); iter != df_end (); ++iter)
    os << **iter << " ";
  os << std::endl;

  os << "  dom_succ: ";
  for (size_t i = 0; i < dom_succ.size (); ++i)
    os << *dom_succ[i] << " ";

  return os << std::endl;
}

void
jit_block::compute_df (size_t avisit_count)
{
  if (visited (avisit_count))
    return;

  if (use_count () >= 2)
    {
      for (jit_use *use = first_use (); use; use = use->next ())
        {
          jit_block *runner = use->user_parent ();
          while (runner != idom)
            {
              runner->mdf.insert (this);
              runner = runner->idom;
            }
        }
    }

  for (size_t i = 0; i < successor_count (); ++i)
    successor (i)->compute_df (avisit_count);
}

bool
jit_block::update_idom (size_t avisit_count)
{
  if (visited (avisit_count) || ! use_count ())
    return false;

  bool changed = false;
  for (jit_use *use = first_use (); use; use = use->next ())
    {
      jit_block *pred = use->user_parent ();
      changed = pred->update_idom (avisit_count) || changed;
    }

  jit_use *use = first_use ();
  jit_block *new_idom = use->user_parent ();
  use = use->next ();

  for (; use; use = use->next ())
    {
      jit_block *pred = use->user_parent ();
      jit_block *pidom = pred->idom;
      if (pidom)
        new_idom = pidom->idom_intersect (new_idom);
    }

  if (idom != new_idom)
    {
      idom = new_idom;
      return true;
    }

  return changed;
}

void
jit_block::pop_all (void)
{
  for (iterator iter = begin (); iter != end (); ++iter)
    {
      jit_instruction *instr = *iter;
      instr->pop_variable ();
    }
}

jit_block *
jit_block::maybe_split (jit_convert& convert, jit_block *asuccessor)
{
  if (successor_count () > 1)
    {
      jit_terminator *term = terminator ();
      size_t idx = term->successor_index (asuccessor);
      jit_block *split = convert.create<jit_block> ("phi_split", mvisit_count);

      // try to place splits where they make sense
      if (id () < asuccessor->id ())
        convert.insert_before (asuccessor, split);
      else
        convert.insert_after (this, split);

      term->stash_argument (idx, split);
      jit_branch *br = split->append (convert.create<jit_branch> (asuccessor));
      replace_in_phi (asuccessor, split);

      if (alive ())
        {
          split->mark_alive ();
          br->infer ();
        }

      return split;
    }

  return this;
}

void
jit_block::create_dom_tree (size_t avisit_count)
{
  if (visited (avisit_count))
    return;

  if (idom != this)
    idom->dom_succ.push_back (this);

  for (size_t i = 0; i < successor_count (); ++i)
    successor (i)->create_dom_tree (avisit_count);
}

jit_block *
jit_block::idom_intersect (jit_block *b)
{
  jit_block *i = this;
  jit_block *j = b;

  while (i != j)
    {
      while (i->id () > j->id ())
        i = i->idom;

      while (j->id () > i->id ())
        j = j->idom;
    }

  return i;
}

// -------------------- jit_phi_incomming --------------------

jit_block *
jit_phi_incomming::user_parent (void) const
{ return muser->parent (); }

// -------------------- jit_phi --------------------
bool
jit_phi::prune (void)
{
  jit_block *p = parent ();
  size_t new_idx = 0;
  jit_value *unique = argument (1);

  for (size_t i = 0; i < argument_count (); ++i)
    {
      jit_block *inc = incomming (i);
      if (inc->branch_alive (p))
        {
          if (unique != argument (i))
            unique = 0;

          if (new_idx != i)
            {
              stash_argument (new_idx, argument (i));
              mincomming[new_idx].stash_value (inc);
            }

          ++new_idx;
        }
    }

  if (new_idx != argument_count ())
    {
      resize_arguments (new_idx);
      mincomming.resize (new_idx);
    }

  assert (argument_count () > 0);
  if (unique)
    {
      replace_with (unique);
      return true;
    }

  return false;
}

bool
jit_phi::infer (void)
{
  jit_block *p = parent ();
  if (! p->alive ())
    return false;

  jit_type *infered = 0;
  for (size_t i = 0; i < argument_count (); ++i)
    {
      jit_block *inc = incomming (i);
      if (inc->branch_alive (p))
        infered = jit_typeinfo::join (infered, argument_type (i));
    }

  if (infered != type ())
    {
      stash_type (infered);
      return true;
    }

  return false;
}

llvm::PHINode *
jit_phi::to_llvm (void) const
{
  return llvm::cast<llvm::PHINode> (jit_value::to_llvm ());
}

// -------------------- jit_terminator --------------------
size_t
jit_terminator::successor_index (const jit_block *asuccessor) const
{
  size_t scount = successor_count ();
  for (size_t i = 0; i < scount; ++i)
    if (successor (i) == asuccessor)
      return i;

  panic_impossible ();
}

bool
jit_terminator::infer (void)
{
  if (! parent ()->alive ())
    return false;

  bool changed = false;
  for (size_t i = 0; i < malive.size (); ++i)
    if (! malive[i] && check_alive (i))
      {
        changed = true;
        malive[i] = true;
        successor (i)->mark_alive ();
      }

  return changed;
}

llvm::TerminatorInst *
jit_terminator::to_llvm (void) const
{
  return llvm::cast<llvm::TerminatorInst> (jit_value::to_llvm ());
}

// -------------------- jit_call --------------------
bool
jit_call::infer (void)
{
  // FIXME: explain algorithm
  for (size_t i = 0; i < argument_count (); ++i)
    {
      already_infered[i] = argument_type (i);
      if (! already_infered[i])
        return false;
    }

  jit_type *infered = mfunction.get_result (already_infered);
  if (! infered && use_count ())
    {
      std::stringstream ss;
      ss << "Missing overload in type inference for ";
      print (ss, 0);
      fail (ss.str ());
    }

  if (infered != type ())
    {
      stash_type (infered);
      return true;
    }

  return false;
}

// -------------------- jit_convert --------------------
jit_convert::jit_convert (llvm::Module *module, tree &tee)
  : iterator_count (0), breaking (false)
{
  jit_instruction::reset_ids ();

  entry_block = create<jit_block> ("body");
  final_block = create<jit_block> ("final");
  append (entry_block);
  entry_block->mark_alive ();
  block = entry_block;
  visit (tee);

  // FIXME: Remove if we no longer only compile loops
  assert (! breaking);
  assert (breaks.empty ());
  assert (continues.empty ());

  block->append (create<jit_branch> (final_block));
  append (final_block);

  for (vmap_t::iterator iter = vmap.begin (); iter != vmap.end (); ++iter)
    {
      jit_variable *var = iter->second;
      const std::string& name = var->name ();
      if (name.size () && name[0] != '#')
        final_block->append (create<jit_store_argument> (var));
    }

  construct_ssa ();

  // initialize the worklist to instructions derived from constants
  for (std::list<jit_value *>::iterator iter = constants.begin ();
       iter != constants.end (); ++iter)
    append_users (*iter);

  // FIXME: Describe algorithm here
  while (worklist.size ())
    {
      jit_instruction *next = worklist.front ();
      worklist.pop_front ();
      next->stash_in_worklist (false);

      if (next->infer ())
        {
          // terminators need to be handles specially
          if (jit_terminator *term = dynamic_cast<jit_terminator *> (next))
            append_users_term (term);
          else
            append_users (next);
        }
    }

  remove_dead ();
  merge_blocks ();
  final_block->label ();
  place_releases ();
  simplify_phi ();

#ifdef OCTAVE_JIT_DEBUG
  final_block->label ();
  std::cout << "-------------------- Compiling tree --------------------\n";
  std::cout << tee.str_print_code () << std::endl;
  print_blocks ("octave jit ir");
#endif

  // for now just init arguments from entry, later we will have to do something
  // more interesting
  for (jit_block::iterator iter = entry_block->begin ();
       iter != entry_block->end (); ++iter)
    if (jit_extract_argument *extract
        = dynamic_cast<jit_extract_argument *> (*iter))
      arguments.push_back (std::make_pair (extract->name (), true));

  convert_llvm to_llvm (*this);
  function = to_llvm.convert (module, arguments, blocks, constants);

#ifdef OCTAVE_JIT_DEBUG
  std::cout << "-------------------- llvm ir --------------------";
  llvm::raw_os_ostream llvm_cout (std::cout);
  function->print (llvm_cout);
  std::cout << std::endl;
  llvm::verifyFunction (*function);
#endif
}

jit_convert::~jit_convert (void)
{
  for (std::list<jit_value *>::iterator iter = all_values.begin ();
       iter != all_values.end (); ++iter)
    delete *iter;
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
  // this is the case for bool_or and bool_and
  if (be.op_type () >= octave_value::num_binary_ops)
    fail ("Unsupported binary operator");

  tree_expression *lhs = be.lhs ();
  jit_value *lhsv = visit (lhs);

  tree_expression *rhs = be.rhs ();
  jit_value *rhsv = visit (rhs);

  const jit_function& fn = jit_typeinfo::binary_op (be.op_type ());
  result = create_checked (fn, lhsv, rhsv);
}

void
jit_convert::visit_break_command (tree_break_command&)
{
  breaks.push_back (block);
  breaking = true;
}

void
jit_convert::visit_colon_expression (tree_colon_expression& expr)
{
  // in the futher we need to add support for classes and deal with rvalues
  jit_value *base = visit (expr.base ());
  jit_value *limit = visit (expr.limit ());
  jit_value *increment;
  tree_expression *tinc = expr.increment ();

  if (tinc)
    increment = visit (tinc);
  else
    increment = create<jit_const_scalar> (1);

  result = block->append (create<jit_call> (jit_typeinfo::make_range, base,
                                            limit, increment));
}

void
jit_convert::visit_continue_command (tree_continue_command&)
{
  continues.push_back (block);
  breaking = true;
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
  // Note we do an initial check to see if the loop will run atleast once.
  // This allows us to get better type inference bounds on variables defined
  // and used only inside the for loop (e.g. the index variable)

  // If we are a nested for loop we need to store the previous breaks
  assert (! breaking);
  unwind_protect prot;
  prot.protect_var (breaks);
  prot.protect_var (continues);
  prot.protect_var (breaking);
  breaks.clear ();

  // we need a variable for our iterator, because it is used in multiple blocks
  std::stringstream ss;
  ss << "#iter" << iterator_count++;
  std::string iter_name = ss.str ();
  jit_variable *iterator = create<jit_variable> (iter_name);
  vmap[iter_name] = iterator;

  jit_block *body = create<jit_block> ("for_body");
  append (body);

  jit_block *tail = create<jit_block> ("for_tail");

  // do control expression, iter init, and condition check in prev_block (block)
  jit_value *control = visit (cmd.control_expr ());
  jit_call *init_iter = create<jit_call> (jit_typeinfo::for_init, control);
  block->append (init_iter);
  block->append (create<jit_assign> (iterator, init_iter));

  jit_value *check = block->append (create<jit_call> (jit_typeinfo::for_check,
                                                      control, iterator));
  block->append (create<jit_cond_branch> (check, body, tail));
  block = body;

  // compute the syntactical iterator
  jit_call *idx_rhs = create<jit_call> (jit_typeinfo::for_index, control,
                                        iterator);
  block->append (idx_rhs);
  do_assign (cmd.left_hand_side (), idx_rhs);

  // do loop
  tree_statement_list *pt_body = cmd.body ();
  pt_body->accept (*this);

  if (breaking && continues.empty ())
    {
      // WTF are you doing user? Every branch was a continue, why did you have
      // a loop??? Users are silly people...
      finish_breaks (tail, breaks);
      append (tail);
      block = tail;
      return;
    }

  // check our condition, continues jump to this block
  jit_block *check_block = create<jit_block> ("for_check");
  append (check_block);

  if (! breaking)
    block->append (create<jit_branch> (check_block));
  finish_breaks (check_block, continues);

  block = check_block;
  const jit_function& add_fn = jit_typeinfo::binary_op (octave_value::op_add);
  jit_value *one = create<jit_const_index> (1);
  jit_call *iter_inc = create<jit_call> (add_fn, iterator, one);
  block->append (iter_inc);
  block->append (create<jit_assign> (iterator, iter_inc));
  check = block->append (create<jit_call> (jit_typeinfo::for_check, control,
                                           iterator));
  block->append (create<jit_cond_branch> (check, body, tail));

  // breaks will go to our tail
  append (tail);
  finish_breaks (tail, breaks);
  block = tail;
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
  result = get_variable (ti.name ());
}

void
jit_convert::visit_if_clause (tree_if_clause&)
{
  fail ();
}

void
jit_convert::visit_if_command (tree_if_command& cmd)
{
  tree_if_command_list *lst = cmd.cmd_list ();
  assert (lst); // jwe: Can this be null?
  lst->accept (*this);
}

void
jit_convert::visit_if_command_list (tree_if_command_list& lst)
{
  tree_if_clause *last = lst.back ();
  size_t last_else = static_cast<size_t> (last->is_else_clause ());

  // entry_blocks represents the block you need to enter in order to execute
  // the condition check for the ith clause. For the else, it is simple the
  // else body. If there is no else body, then it is padded with the tail
  std::vector<jit_block *> entry_blocks (lst.size () + 1 - last_else);
  std::vector<jit_block *> branch_blocks (lst.size (), 0); // final blocks
  entry_blocks[0] = block;

  // we need to construct blocks first, because they have jumps to eachother
  tree_if_command_list::iterator iter = lst.begin ();
  ++iter;
  for (size_t i = 1; iter != lst.end (); ++iter, ++i)
    {
      tree_if_clause *tic = *iter;
      if (tic->is_else_clause ())
        entry_blocks[i] = create<jit_block> ("else");
      else
        entry_blocks[i] = create<jit_block> ("ifelse_cond");
    }

  jit_block *tail = create<jit_block> ("if_tail");
  if (! last_else)
    entry_blocks[entry_blocks.size () - 1] = tail;

  size_t num_incomming = 0; // number of incomming blocks to our tail
  iter = lst.begin ();
  for (size_t i = 0; iter != lst.end (); ++iter, ++i)
    {
      tree_if_clause *tic = *iter;
      block = entry_blocks[i];
      assert (block);

      if (i) // the first block is prev_block, so it has already been added
        append (entry_blocks[i]);

      if (! tic->is_else_clause ())
        {
          tree_expression *expr = tic->condition ();
          jit_value *cond = visit (expr);
          jit_call *check = create_checked (&jit_typeinfo::logically_true,
                                            cond);
          block->append (check);

          jit_block *body = create<jit_block> (i == 0 ? "if_body"
                                               : "ifelse_body");
          append (body);

          jit_instruction *br = create<jit_cond_branch> (check, body,
                                                        entry_blocks[i + 1]);
          block->append (br);
          block = body;
        }

      tree_statement_list *stmt_lst = tic->commands ();
      assert (stmt_lst); // jwe: Can this be null?
      stmt_lst->accept (*this);

      if (breaking)
        breaking = false;
      else
        {
          ++num_incomming;
          block->append (create<jit_branch> (tail));
        }
    }

  if (num_incomming || ! last_else)
    {
      append (tail);
      block = tail;
    }
  else
    // every branch broke, so we don't have a tail
    breaking = true;
}

void
jit_convert::visit_index_expression (tree_index_expression& exp)
{
  std::pair<jit_value *, jit_value *> res = resolve (exp);
  jit_value *object = res.first;
  jit_value *index = res.second;

  result = create_checked (jit_typeinfo::paren_subsref, object, index);
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
      result = create<jit_const_scalar> (dv);
    }
  else if (v.is_range ())
    {
      Range rv = v.range_value ();
      result = create<jit_const_range> (rv);
    }
  else
    fail ("Unknown constant");
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
  if (tsa.op_type () != octave_value::op_asn_eq)
    fail ("Unsupported assign");

  // resolve rhs
  tree_expression *rhs = tsa.right_hand_side ();
  jit_value *rhsv = visit (rhs);

  do_assign (tsa.left_hand_side (), rhsv);
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
          jit_const_string *name = create<jit_const_string> (expr->name ());
          block->append (create<jit_call> (fn, name, expr_result));
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

      if (breaking)
        break;
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
jit_convert::append (jit_block *ablock)
{
  blocks.push_back (ablock);
  ablock->stash_location (--blocks.end ());
}

void
jit_convert::insert_before (block_iterator iter, jit_block *ablock)
{
  iter = blocks.insert (iter, ablock);
  ablock->stash_location (iter);
}

void
jit_convert::insert_after (block_iterator iter, jit_block *ablock)
{
  ++iter;
  insert_before (iter, ablock);
}

jit_variable *
jit_convert::get_variable (const std::string& vname)
{
  vmap_t::iterator iter;
  iter = vmap.find (vname);
  if (iter != vmap.end ())
    return iter->second;

  jit_variable *var = create<jit_variable> (vname);
  octave_value val = symbol_table::find (vname);
  jit_type *type = jit_typeinfo::type_of (val);
  jit_extract_argument *extract;
  extract = create<jit_extract_argument> (type, var);
  entry_block->prepend (extract);

  return vmap[vname] = var;
}

std::pair<jit_value *, jit_value *>
jit_convert::resolve (tree_index_expression& exp)
{
  std::string type = exp.type_tags ();
  if (! (type.size () == 1 && type[0] == '('))
    fail ("Unsupported index operation");

  std::list<tree_argument_list *> args = exp.arg_lists ();
  if (args.size () != 1)
    fail ("Bad number of arguments in tree_index_expression");

  tree_argument_list *arg_list = args.front ();
  if (! arg_list)
    fail ("null argument list");

  if (arg_list->size () != 1)
    fail ("Bad number of arguments in arg_list");

  tree_expression *tree_object = exp.expression ();
  jit_value *object = visit (tree_object);
  tree_expression *arg0 = arg_list->front ();
  jit_value *index = visit (arg0);

  return std::make_pair (object, index);
}

jit_value *
jit_convert::do_assign (tree_expression *exp, jit_value *rhs, bool artificial)
{
  if (! exp)
    fail ("NULL lhs in assign");

  if (isa<tree_identifier> (exp))
    return do_assign (exp->name (), rhs, exp->print_result (), artificial);
  else if (tree_index_expression *idx
           = dynamic_cast<tree_index_expression *> (exp))
    {
      std::pair<jit_value *, jit_value *> res = resolve (*idx);
      jit_value *object = res.first;
      jit_value *index = res.second;
      jit_call *new_object = create<jit_call> (&jit_typeinfo::paren_subsasgn,
                                               object, index, rhs);
      block->append (new_object);
      do_assign (idx->expression (), new_object, true);
      create_check (new_object);

      // FIXME: Will not work for values that must be release/grabed
      return rhs;
    }
  else
    fail ("Unsupported assignment");
}

jit_value *
jit_convert::do_assign (const std::string& lhs, jit_value *rhs,
                        bool print, bool artificial)
{
  jit_variable *var = get_variable (lhs);
  jit_assign *assign = block->append (create<jit_assign> (var, rhs));

  if (artificial)
    assign->mark_artificial ();

  if (print)
    {
      const jit_function& print_fn = jit_typeinfo::print_value ();
      jit_const_string *name = create<jit_const_string> (lhs);
      block->append (create<jit_call> (print_fn, name, var));
    }

  return var;
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
jit_convert::append_users_term (jit_terminator *term)
{
  for (size_t i = 0; i < term->successor_count (); ++i)
    {
      if (term->alive (i))
        {
          jit_block *succ = term->successor (i);
          for (jit_block::iterator iter = succ->begin (); iter != succ->end ()
                 && isa<jit_phi> (*iter); ++iter)
            push_worklist (*iter);

          jit_terminator *sterm = succ->terminator ();
          if (sterm)
            push_worklist (sterm);
        }
    }
}

void
jit_convert::merge_blocks (void)
{
  std::vector<jit_block *> dead;
  for (block_list::iterator iter = blocks.begin (); iter != blocks.end ();
       ++iter)
    {
      jit_block *b = *iter;
      jit_block *merged = b->maybe_merge ();

      if (merged)
        {
          if (merged == final_block)
            final_block = b;

          if (merged == entry_block)
            entry_block = b;

          dead.push_back (merged);
        }
    }

  for (size_t i = 0; i < dead.size (); ++i)
    blocks.erase (dead[i]->location ());
}

void
jit_convert::construct_ssa (void)
{
  merge_blocks ();
  final_block->label ();
  final_block->compute_idom (entry_block);
  entry_block->compute_df ();
  entry_block->create_dom_tree ();

  // insert phi nodes where needed, this is done on a per variable basis
  for (vmap_t::iterator iter = vmap.begin (); iter != vmap.end (); ++iter)
    {
      jit_block::df_set visited, added_phi;
      std::list<jit_block *> ssa_worklist;
      iter->second->use_blocks (visited);
      ssa_worklist.insert (ssa_worklist.begin (), visited.begin (),
                           visited.end ());

      while (ssa_worklist.size ())
        {
          jit_block *b = ssa_worklist.front ();
          ssa_worklist.pop_front ();

          for (jit_block::df_iterator diter = b->df_begin ();
               diter != b->df_end (); ++diter)
            {
              jit_block *dblock = *diter;
              if (! added_phi.count (dblock))
                {
                  jit_phi *phi = create<jit_phi> (iter->second,
                                                  dblock->use_count ());
                  dblock->prepend (phi);
                  added_phi.insert (dblock);
                }

              if (! visited.count (dblock))
                {
                  ssa_worklist.push_back (dblock);
                  visited.insert (dblock);
                }
            }
        }
    }

  do_construct_ssa (*entry_block, entry_block->visit_count ());
}

void
jit_convert::do_construct_ssa (jit_block& ablock, size_t avisit_count)
{
  if (ablock.visited (avisit_count))
    return;

  // replace variables with their current SSA value
  for (jit_block::iterator iter = ablock.begin (); iter != ablock.end (); ++iter)
    {
      jit_instruction *instr = *iter;
      instr->construct_ssa ();
      instr->push_variable ();
    }

  // finish phi nodes of successors
  for (size_t i = 0; i < ablock.successor_count (); ++i)
    {
      jit_block *finish = ablock.successor (i);

      for (jit_block::iterator iter = finish->begin (); iter != finish->end ()
             && isa<jit_phi> (*iter);)
        {
          jit_phi *phi = static_cast<jit_phi *> (*iter);
          jit_variable *var = phi->dest ();
          if (var->has_top ())
            {
              phi->add_incomming (&ablock, var->top ());
              ++iter;
            }
          else
            {
              // temporaries may have extranious phi nodes which can be removed
              assert (! phi->use_count ());
              assert (var->name ().size () && var->name ()[0] == '#');
              iter = finish->remove (iter);
            }
        }
    }

  for (size_t i = 0; i < ablock.dom_successor_count (); ++i)
    do_construct_ssa (*ablock.dom_successor (i), avisit_count);

  ablock.pop_all ();
}

void
jit_convert::remove_dead ()
{
  block_list::iterator biter;
  for (biter = blocks.begin (); biter != blocks.end (); ++biter)
    {
      jit_block *b = *biter;
      if (b->alive ())
        {
          for (jit_block::iterator iter = b->begin (); iter != b->end ()
                 && isa<jit_phi> (*iter);)
            {
              jit_phi *phi = static_cast<jit_phi *> (*iter);
              if (phi->prune ())
                iter = b->remove (iter);
              else
                ++iter;
            }
        }
    }

  for (biter = blocks.begin (); biter != blocks.end ();)
    {
      jit_block *b = *biter;
      if (b->alive ())
        {
          // FIXME: A special case for jit_error_check, if we generalize to
          // we will need to change!
          jit_terminator *term = b->terminator ();
          if (term && term->successor_count () == 2 && ! term->alive (0))
            {
              jit_block *succ = term->successor (1);
              term->remove ();
              jit_branch *abreak = b->append (create<jit_branch> (succ));
              abreak->infer ();
            }

          ++biter;
        }
      else
        {
          jit_terminator *term = b->terminator ();
          if (term)
            term->remove ();
          biter = blocks.erase (biter);
        }
    }
}

void
jit_convert::place_releases (void)
{
  std::set<jit_value *> temporaries;
  for (block_list::iterator iter = blocks.begin (); iter != blocks.end ();
       ++iter)
    {
      jit_block& ablock = **iter;
      if (ablock.id () != jit_block::NO_ID)
        {
          release_temp (ablock, temporaries);
          release_dead_phi (ablock);
        }
    }
}

void
jit_convert::release_temp (jit_block& ablock, std::set<jit_value *>& temp)
{
  for (jit_block::iterator iter = ablock.begin (); iter != ablock.end ();
       ++iter)
    {
      jit_instruction *instr = *iter;

      // check for temporaries that require release and live across
      // multiple blocks
      if (instr->needs_release ())
        {
          jit_block *fu_block = instr->first_use_block ();
          if (fu_block && fu_block != &ablock)
            temp.insert (instr);
        }

      if (isa<jit_call> (instr))
        {
          // place releases for temporary arguments
          for (size_t i = 0; i < instr->argument_count (); ++i)
            {
              jit_value *arg = instr->argument (i);
              if (arg->needs_release ())
                {
                  jit_call *release = create<jit_call> (&jit_typeinfo::release,
                                                        arg);
                  release->infer ();
                  ablock.insert_after (iter, release);
                  ++iter;
                  temp.erase (arg);
                }
            }
        }
    }

  if (! temp.size () || ! isa<jit_error_check> (ablock.terminator ()))
    return;

  // FIXME: If we support try/catch or unwind_protect final_block may not be the
  // destination
  jit_block *split = ablock.maybe_split (*this, final_block);
  jit_terminator *term = split->terminator ();
  for (std::set<jit_value *>::const_iterator iter = temp.begin ();
       iter != temp.end (); ++iter)
    {
      jit_value *value = *iter;
      jit_call *release = create<jit_call> (&jit_typeinfo::release, value);
      split->insert_before (term, release);
      release->infer ();
    }
}

void
jit_convert::release_dead_phi (jit_block& ablock)
{
  jit_block::iterator iter = ablock.begin ();
  while (iter != ablock.end () && isa<jit_phi> (*iter))
    {
      jit_phi *phi = static_cast<jit_phi *> (*iter);
      ++iter;

      jit_use *use = phi->first_use ();
      if (phi->use_count () == 1 && isa<jit_assign> (use->user ()))
        {
          // instead of releasing on assign, release on all incomming branches,
          // this can get rid of casts inside loops
          for (size_t i = 0; i < phi->argument_count (); ++i)
            {
              jit_value *arg = phi->argument (i);
              jit_block *inc = phi->incomming (i);
              jit_block *split = inc->maybe_split (*this, ablock);
              jit_terminator *term = split->terminator ();
              jit_call *release = create<jit_call> (jit_typeinfo::release, arg);
              release->infer ();
              split->insert_before (term, release);
            }

          phi->replace_with (0);
          phi->remove ();
        }
    }
}

void
jit_convert::simplify_phi (void)
{
  for (block_list::iterator biter = blocks.begin (); biter != blocks.end ();
       ++biter)
    {
      jit_block &ablock = **biter;
      for (jit_block::iterator iter = ablock.begin (); iter != ablock.end ()
             && isa<jit_phi> (*iter); ++iter)
        simplify_phi (*static_cast<jit_phi *> (*iter));
    }
}

void
jit_convert::simplify_phi (jit_phi& phi)
{
  jit_block& pblock = *phi.parent ();
  const jit_function& cast_fn = jit_typeinfo::cast (phi.type ());
  jit_variable *dest = phi.dest ();
  for (size_t i = 0; i < phi.argument_count (); ++i)
    {
      jit_value *arg = phi.argument (i);
      if (arg->type () != phi.type ())
        {
          jit_block *pred = phi.incomming (i);
          jit_block *split = pred->maybe_split (*this, pblock);
          jit_terminator *term = split->terminator ();
          jit_instruction *cast = create<jit_call> (cast_fn, arg);
          jit_assign *assign = create<jit_assign> (dest, cast);

          split->insert_before (term, cast);
          split->insert_before (term, assign);
          cast->infer ();
          assign->infer ();
          phi.stash_argument (i, assign);
        }
    }
}

void
jit_convert::finish_breaks (jit_block *dest, const block_list& lst)
{
  for (block_list::const_iterator iter = lst.begin (); iter != lst.end ();
       ++iter)
    {
      jit_block *b = *iter;
      b->append (create<jit_branch> (dest));
    }
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
      prelude = llvm::BasicBlock::Create (context, "prelude", function);
      builder.SetInsertPoint (prelude);

      llvm::Value *arg = function->arg_begin ();
      for (size_t i = 0; i < args.size (); ++i)
        {
          llvm::Value *loaded_arg = builder.CreateConstInBoundsGEP1_32 (arg, i);
          arguments[args[i].first] = loaded_arg;
        }

      std::list<jit_block *>::const_iterator biter;
      for (biter = blocks.begin (); biter != blocks.end (); ++biter)
        {
          jit_block *jblock = *biter;
          llvm::BasicBlock *block = llvm::BasicBlock::Create (context,
                                                              jblock->name (),
                                                              function);
          jblock->stash_llvm (block);
        }

      jit_block *first = *blocks.begin ();
      builder.CreateBr (first->to_llvm ());

      // constants aren't in the IR, we visit those first
      for (std::list<jit_value *>::const_iterator iter = constants.begin ();
           iter != constants.end (); ++iter)
        if (! isa<jit_instruction> (*iter))
          visit (*iter);

      // convert all instructions
      for (biter = blocks.begin (); biter != blocks.end (); ++biter)
        visit (*biter);

      // now finish phi nodes
      for (biter = blocks.begin (); biter != blocks.end (); ++biter)
        {
          jit_block& block = **biter;
          for (jit_block::iterator piter = block.begin ();
               piter != block.end () && isa<jit_phi> (*piter); ++piter)
            {
              jit_instruction *phi = *piter;
              finish_phi (static_cast<jit_phi *> (phi));
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
jit_convert::convert_llvm::finish_phi (jit_phi *phi)
{
  llvm::PHINode *llvm_phi = phi->to_llvm ();
  for (size_t i = 0; i < phi->argument_count (); ++i)
    {
      llvm::BasicBlock *pred = phi->incomming_llvm (i);
      llvm_phi->addIncoming (phi->argument_llvm (i), pred);
    }
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
  llvm::Type *scalar_t = jit_typeinfo::get_scalar_llvm ();
  llvm::Type *idx = jit_typeinfo::get_index_llvm ();
  const jit_range& rng = cr.value ();

  llvm::Constant *constants[4];
  constants[0] = llvm::ConstantFP::get (scalar_t, rng.base);
  constants[1] = llvm::ConstantFP::get (scalar_t, rng.limit);
  constants[2] = llvm::ConstantFP::get (scalar_t, rng.inc);
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
jit_convert::convert_llvm::visit (jit_branch& b)
{
  b.stash_llvm (builder.CreateBr (b.successor_llvm ()));
}

void
jit_convert::convert_llvm::visit (jit_cond_branch& cb)
{
  llvm::Value *cond = cb.cond_llvm ();
  llvm::Value *br;
  br = builder.CreateCondBr (cond, cb.successor_llvm (0),
                             cb.successor_llvm (1));
  cb.stash_llvm (br);
}

void
jit_convert::convert_llvm::visit (jit_call& call)
{
  llvm::Value *ret = create_call (call.overload (), call.arguments ());
  call.stash_llvm (ret);
}

void
jit_convert::convert_llvm::visit (jit_extract_argument& extract)
{
  llvm::Value *arg = arguments[extract.name ()];
  assert (arg);
  arg = builder.CreateLoad (arg);

  jit_value *jarg = jthis.create<jit_argument> (jit_typeinfo::get_any (), arg);
  extract.stash_llvm (create_call (extract.overload (), jarg));
}

void
jit_convert::convert_llvm::visit (jit_store_argument& store)
{
  llvm::Value *arg_value = create_call (store.overload (), store.result ());

  llvm::Value *arg = arguments[store.name ()];
  store.stash_llvm (builder.CreateStore (arg_value, arg));
}

void
jit_convert::convert_llvm::visit (jit_phi& phi)
{
  // we might not have converted all incoming branches, so we don't
  // set incomming branches now
  llvm::PHINode *node = llvm::PHINode::Create (phi.type_llvm (),
                                               phi.argument_count ());
  builder.Insert (node);
  phi.stash_llvm (node);
}

void
jit_convert::convert_llvm::visit (jit_variable&)
{
  fail ("ERROR: SSA construction should remove all variables");
}

void
jit_convert::convert_llvm::visit (jit_error_check& check)
{
  llvm::Value *cond = jit_typeinfo::insert_error_check ();
  llvm::Value *br = builder.CreateCondBr (cond, check.successor_llvm (0),
                                          check.successor_llvm (1));
  check.stash_llvm (br);
}

void
jit_convert::convert_llvm::visit (jit_assign& assign)
{
  assign.stash_llvm (assign.src ()->to_llvm ());

  if (assign.artificial ())
    return;

  jit_value *new_value = assign.src ();
  if (isa<jit_assign_base> (new_value))
    {
      const jit_function::overload& ol
        = jit_typeinfo::get_grab (new_value->type ());
      if (ol.function)
        assign.stash_llvm (create_call (ol, new_value));
    }

  jit_value *overwrite = assign.overwrite ();
  if (isa<jit_assign_base> (overwrite))
    {
      const jit_function::overload& ol
        = jit_typeinfo::get_release (overwrite->type ());
      if (ol.function)
        create_call (ol, overwrite);
    }
}

void
jit_convert::convert_llvm::visit (jit_argument&)
{}

llvm::Value *
jit_convert::convert_llvm::create_call (const jit_function::overload& ol,
                                        const std::vector<jit_value *>& jargs)
{
  llvm::IRBuilder<> alloca_inserter (prelude, prelude->begin ());

   llvm::Function *fun = ol.function;
   if (! fun)
     fail ("Missing overload");

  const llvm::Function::ArgumentListType& alist = fun->getArgumentList ();
  size_t nargs = alist.size ();
  bool sret = false;
  if (nargs != jargs.size ())
    {
      // first argument is the structure return value
      assert (nargs == jargs.size () + 1);
      sret = true;
    }

  std::vector<llvm::Value *> args (nargs);
  llvm::Function::arg_iterator llvm_arg = fun->arg_begin ();
  if (sret)
    {
      args[0] = alloca_inserter.CreateAlloca (ol.result->to_llvm ());
      ++llvm_arg;
    }

  for (size_t i = 0; i < jargs.size (); ++i, ++llvm_arg)
    {
      llvm::Value *arg = jargs[i]->to_llvm ();
      llvm::Type *arg_type = arg->getType ();
      llvm::Type *llvm_arg_type = llvm_arg->getType ();

      if (arg_type == llvm_arg_type)
        args[i + sret] = arg;
      else
        {
          // pass structure by pointer
          assert (arg_type->getPointerTo () == llvm_arg_type);
          llvm::Value *new_arg = alloca_inserter.CreateAlloca (arg_type);
          builder.CreateStore (arg, new_arg);
          args[i + sret] = new_arg;
        }
    }

  llvm::Value *llvm_call = builder.CreateCall (fun, args);
  return sret ? builder.CreateLoad (args[0]) : llvm_call;
}

llvm::Value *
jit_convert::convert_llvm::create_call (const jit_function::overload& ol,
                                        const std::vector<jit_use>& uses)
{
  std::vector<jit_value *> values (uses.size ());
  for (size_t i = 0; i < uses.size (); ++i)
    values[i] = uses[i].value ();

  return create_call (ol, values);
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
  : engine (tjit.get_engine ()), llvm_function (0)
{
  try
    {
      jit_convert conv (tjit.get_module (), tee);
      llvm_function = conv.get_function ();
      arguments = conv.get_arguments ();
      bounds = conv.get_bounds ();
    }
  catch (const jit_fail_exception& e)
    {
#ifdef OCTAVE_JIT_DEBUG
      if (e.known ())
        std::cout << "jit fail: " << e.what () << std::endl;
#endif
    }

  if (! llvm_function)
    {
      function = 0;
      return;
    }

  tjit.optimize (llvm_function);

#ifdef OCTAVE_JIT_DEBUG
  std::cout << "-------------------- optimized llvm ir --------------------\n";
  llvm::raw_os_ostream llvm_cout (std::cout);
  llvm_function->print (llvm_cout);
  std::cout << std::endl;
#endif

  void *void_fn = engine->getPointerToFunction (llvm_function);
  function = reinterpret_cast<jited_function> (void_fn);
}

jit_info::~jit_info (void)
{
  if (llvm_function)
    llvm_function->eraseFromParent ();
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
#endif
