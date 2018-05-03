/*

Copyright (C) 2012-2018 Max Brister

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

// Author: Max Brister <max@2bass.com>

// defines required by llvm
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_LLVM)

#if defined (HAVE_LLVM_IR_VERIFIER_H)
#  include <llvm/IR/Verifier.h>
#else
#  include <llvm/Analysis/Verifier.h>
#endif

#include <llvm/ExecutionEngine/ExecutionEngine.h>

#if defined (HAVE_LLVM_IR_FUNCTION_H)
#  include <llvm/IR/GlobalVariable.h>
#  include <llvm/IR/LLVMContext.h>
#  include <llvm/IR/Function.h>
#  include <llvm/IR/Instructions.h>
#  include <llvm/IR/Intrinsics.h>
#else
#  include <llvm/GlobalVariable.h>
#  include <llvm/LLVMContext.h>
#  include <llvm/Function.h>
#  include <llvm/Instructions.h>
#  include <llvm/Intrinsics.h>
#endif

#if defined (HAVE_LLVM_SUPPORT_IRBUILDER_H)
#  include <llvm/Support/IRBuilder.h>
#  elif defined(HAVE_LLVM_IR_IRBUILDER_H)
#  include <llvm/IR/IRBuilder.h>
#else
#  include <llvm/IRBuilder.h>
#endif

#include <llvm/Support/raw_os_ostream.h>

#include "jit-typeinfo.h"
#include "pt-jit.h"
#include "jit-ir.h"
#include "ov.h"
#include "ov-builtin.h"
#include "ov-complex.h"
#include "ov-scalar.h"
#include "pager.h"
#include "interpreter-private.h"

namespace octave
{
  static llvm::LLVMContext& context = llvm::getGlobalContext ();

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
  octave_jit_print_scalar (const char *name, double value)
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
    return rng.numel ();
  }

  extern "C" void
  octave_jit_release_any (octave_base_value *obv)
  {
    obv->release ();
  }

  extern "C" void
  octave_jit_release_matrix (jit_matrix *m)
  {
    delete m->m_array;
  }

  extern "C" octave_base_value *
  octave_jit_grab_any (octave_base_value *obv)
  {
    obv->grab ();
    return obv;
  }

  extern "C" jit_matrix
  octave_jit_grab_matrix (jit_matrix *m)
  {
    return *m->m_array;
  }

  extern "C" octave_base_value *
  octave_jit_cast_any_matrix (jit_matrix *m)
  {
    octave_value ret (*m->m_array);
    octave_base_value *rep = ret.internal_rep ();
    rep->grab ();
    delete m->m_array;

    return rep;
  }

  extern "C" jit_matrix
  octave_jit_cast_matrix_any (octave_base_value *obv)
  {
    NDArray m = obv->array_value ();
    obv->release ();
    return m;
  }

  extern "C" octave_base_value *
  octave_jit_cast_any_range (jit_range *rng)
  {
    Range temp (*rng);
    octave_value ret (temp);
    octave_base_value *rep = ret.internal_rep ();
    rep->grab ();

    return rep;
  }
  extern "C" jit_range
  octave_jit_cast_range_any (octave_base_value *obv)
  {

    jit_range r (obv->range_value ());
    obv->release ();
    return r;
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

  extern "C" Complex
  octave_jit_cast_complex_any (octave_base_value *obv)
  {
    Complex ret = obv->complex_value ();
    obv->release ();
    return ret;
  }

  extern "C" octave_base_value *
  octave_jit_cast_any_complex (Complex c)
  {
    if (c.imag () == 0)
      return new octave_scalar (c.real ());
    else
      return new octave_complex (c);
  }

  extern "C" void
  octave_jit_err_nan_to_logical_conversion (void)
  {
    err_nan_to_logical_conversion ();
  }

  extern "C" void
  octave_jit_ginvalid_index (void)
  {
    // FIXME: 0-argument form of octave::err_invalid_index removed in
    //        cset dd6345fd8a97.  Report -1 as the bad index for all
    //        occurrences.
    err_invalid_index (static_cast<octave_idx_type> (-1));
  }

  extern "C" void
  octave_jit_gindex_range (int nd, int dim, octave_idx_type iext,
                           octave_idx_type ext)
  {
    err_index_out_of_range (nd, dim, iext, ext);
  }

  extern "C" jit_matrix
  octave_jit_paren_subsasgn_impl (jit_matrix *mat, octave_idx_type index,
                                  double value)
  {
    NDArray *array = mat->m_array;
    if (array->numel () < index)
      array->resize1 (index);

    double *data = array->fortran_vec ();
    data[index - 1] = value;

    mat->update ();
    return *mat;
  }

  static void
  make_indices (double *indices, octave_idx_type idx_count,
                Array<idx_vector>& result)
  {
    result.resize (dim_vector (1, idx_count));
    for (octave_idx_type i = 0; i < idx_count; ++i)
      result(i) = idx_vector (indices[i]);
  }

  extern "C" double
  octave_jit_paren_scalar (jit_matrix *mat, double *indicies,
                           octave_idx_type idx_count)
  {
    // FIXME: Replace this with a more optimal version
    Array<idx_vector> idx;
    make_indices (indicies, idx_count, idx);

    Array<double> ret = mat->m_array->index (idx);

    return ret.xelem (0);
  }

  extern "C" jit_matrix
  octave_jit_paren_scalar_subsasgn (jit_matrix *mat, double *indices,
                                    octave_idx_type idx_count, double value)
  {
    // FIXME: Replace this with a more optimal version
    jit_matrix ret;

    Array<idx_vector> idx;
    make_indices (indices, idx_count, idx);

    Matrix temp (1, 1);
    temp.xelem(0) = value;
    mat->m_array->assign (idx, temp);
    ret.update (mat->m_array);

    return ret;
  }

  extern "C" jit_matrix
  octave_jit_paren_subsasgn_matrix_range (jit_matrix *mat, jit_range *index,
                                          double value)
  {
    NDArray *array = mat->m_array;
    bool done = false;

    // optimize for the simple case (no resizing and no errors)
    if (*array->jit_ref_count () == 1
        && index->all_elements_are_ints ())
      {
        // this code is similar to idx_vector::fill, but we avoid allocating an
        // idx_vector and its associated rep
        octave_idx_type start = static_cast<octave_idx_type> (index->m_base)-1;
        octave_idx_type step = static_cast<octave_idx_type> (index->m_inc);
        octave_idx_type nelem = index->m_nelem;
        octave_idx_type final = start + nelem * step;
        if (step < 0)
          {
            step = -step;
            std::swap (final, start);
          }

        if (start >= 0 && final < mat->m_slice_len)
          {
            done = true;

            double *data = array->jit_slice_data ();
            if (step == 1)
              std::fill (data + start, data + start + nelem, value);
            else
              {
                for (octave_idx_type i = start; i < final; i += step)
                  data[i] = value;
              }
          }
      }

    if (! done)
      {
        idx_vector idx (*index);
        NDArray avalue (dim_vector (1, 1));
        avalue.xelem (0) = value;
        array->assign (idx, avalue);
      }

    jit_matrix ret;
    ret.update (array);
    return ret;
  }

  extern "C" double
  octave_jit_end_matrix (jit_matrix *mat, octave_idx_type idx,
                         octave_idx_type count)
  {
    octave_idx_type ndim = mat->m_dimensions[-1];
    if (ndim == count)
      return mat->m_dimensions[idx];
    else if (ndim > count)
      {
        if (idx == count - 1)
          {
            double ret = mat->m_dimensions[idx];
            for (octave_idx_type i = idx + 1; i < ndim; ++i)
              ret *= mat->m_dimensions[idx];
            return ret;
          }

        return mat->m_dimensions[idx];
      }
    else // ndim < count
      return idx < ndim ? mat->m_dimensions[idx] : 1;
  }

  extern "C" octave_base_value *
  octave_jit_create_undef (void)
  {
    octave_value undef;
    octave_base_value *ret = undef.internal_rep ();
    ret->grab ();

    return ret;
  }

  extern "C" Complex
  octave_jit_complex_mul (Complex lhs, Complex rhs)
  {
    if (lhs.imag () == 0 && rhs.imag() == 0)
      return Complex (lhs.real () * rhs.real (), 0);

    return lhs * rhs;
  }

  extern "C" Complex
  octave_jit_complex_div (Complex lhs, Complex rhs)
  {
    // see src/OPERATORS/op-cs-cs.cc
    if (rhs == 0.0)
      warn_divide_by_zero ();

    return lhs / rhs;
  }

  // FIXME: CP form src/xpow.cc
  static inline int
  xisint (double x)
  {
    return (math::x_nint (x) == x
            && ((x >= 0 && x < std::numeric_limits<int>::max ())
                || (x <= 0 && x > std::numeric_limits<int>::min ())));
  }

  extern "C" Complex
  octave_jit_pow_scalar_scalar (double lhs, double rhs)
  {
    // FIXME: almost CP from src/xpow.cc
    if (lhs < 0.0 && ! xisint (rhs))
      return std::pow (Complex (lhs), rhs);
    return std::pow (lhs, rhs);
  }

  extern "C" Complex
  octave_jit_pow_complex_complex (Complex lhs, Complex rhs)
  {
    if (lhs.imag () == 0 && rhs.imag () == 0)
      return octave_jit_pow_scalar_scalar (lhs.real (), rhs.real ());
    return std::pow (lhs, rhs);
  }

  extern "C" Complex
  octave_jit_pow_complex_scalar (Complex lhs, double rhs)
  {
    if (lhs.imag () == 0)
      return octave_jit_pow_scalar_scalar (lhs.real (), rhs);
    return std::pow (lhs, rhs);
  }

  extern "C" Complex
  octave_jit_pow_scalar_complex (double lhs, Complex rhs)
  {
    if (rhs.imag () == 0)
      return octave_jit_pow_scalar_scalar (lhs, rhs.real ());
    return std::pow (lhs, rhs);
  }

  extern "C" void
  octave_jit_print_matrix (jit_matrix *m)
  {
    std::cout << *m << std::endl;
  }

  OCTAVE_NORETURN static
  void
  err_bad_result (void)
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

    // FIXME: Check result_type somehow
    if (result_type)
      {
        if (ovl.length () < 1)
          err_bad_result ();

        octave_value result = ovl.xelem(0);
        octave_base_value *ret = result.internal_rep ();
        ret->grab ();
        return ret;
      }

    if (! (ovl.empty ()
           || (ovl.length () == 1 && ovl.xelem (0).is_undefined ())))
      err_bad_result ();

    return 0;
  }


  // -------------------- jit_range --------------------
  bool
  jit_range::all_elements_are_ints () const
  {
    Range r (*this);
    return r.all_elements_are_ints ();
  }

  std::ostream&
  operator<< (std::ostream& os, const jit_range& rng)
  {
    return os << "Range[" << rng.m_base << ", " << rng.m_limit
              << ", " << rng.m_inc << ", " << rng.m_nelem << ']';
  }


  // -------------------- jit_matrix --------------------

  std::ostream&
  operator<< (std::ostream& os, const jit_matrix& mat)
  {
    return os << "Matrix[" << mat.m_ref_count << ", "
              << mat.m_slice_data << ", " << mat.m_slice_len << ", "
              << mat.m_dimensions << ", " << mat.m_array << ']';
  }


  // -------------------- jit_type --------------------

  jit_type::jit_type (const std::string& aname, jit_type *aparent,
                      llvm::Type *allvm_type, bool askip_paren, int aid)
    : m_name (aname), m_parent (aparent), m_llvm_type (allvm_type), m_id (aid),
      m_depth (aparent ? aparent->m_depth + 1 : 0), m_skip_paren (askip_paren)
  {
    std::memset (m_sret, 0, sizeof (m_sret));
    std::memset (m_pointer_arg, 0, sizeof (m_pointer_arg));
    std::memset (m_pack, 0, sizeof (m_pack));
    std::memset (m_unpack, 0, sizeof (m_unpack));

    for (size_t i = 0; i < jit_convention::length; ++i)
      m_packed_type[i] = m_llvm_type;
  }

  llvm::Type *
  jit_type::to_llvm_arg (void) const
  {
    return m_llvm_type ? m_llvm_type->getPointerTo () : nullptr;
  }

  jit_type*
  jit_type_join (jit_type *lhs, jit_type *rhs)
  {
    // empty case
    if (! lhs)
      return rhs;

    if (! rhs)
      return lhs;

    // check for a shared parent
    while (lhs != rhs)
      {
        if (lhs->depth () > rhs->depth ())
          lhs = lhs->parent ();
        else if (lhs->depth () < rhs->depth ())
          rhs = rhs->parent ();
        else
          {
            // we MUST have depth > 0 as any is the base type of everything
            do
              {
                lhs = lhs->parent ();
                rhs = rhs->parent ();
              }
            while (lhs != rhs);
          }
      }
    return lhs;
  }


  // -------------------- jit_function --------------------
  jit_function::jit_function ()
    : m_module (nullptr), m_llvm_function (nullptr), m_result (nullptr),
      m_call_conv (jit_convention::length), m_can_error (false)
  { }

  jit_function::jit_function (const jit_module *amodule,
                              jit_convention::type acall_conv,
                              const llvm::Twine& aname, jit_type *aresult,
                              const std::vector<jit_type *>& aargs)
    : m_module (amodule), m_result (aresult), m_args (aargs),
      m_call_conv (acall_conv), m_can_error (false)
  {
    llvm::SmallVector<llvm::Type *, 15> llvm_args;

    llvm::Type *rtype = llvm::Type::getVoidTy (context);
    if (m_result)
      {
        rtype = m_result->packed_type (m_call_conv);
        if (sret ())
          {
            llvm_args.push_back (rtype->getPointerTo ());
            rtype = llvm::Type::getVoidTy (context);
          }
      }

    for (auto iter = m_args.cbegin (); iter != m_args.cend (); ++iter)
      {
        jit_type *ty = *iter;
        assert (ty);
        llvm::Type *argty = ty->packed_type (m_call_conv);
        if (ty->pointer_arg (m_call_conv))
          argty = argty->getPointerTo ();

        llvm_args.push_back (argty);
      }

    llvm::FunctionType *ft = llvm::FunctionType::get (rtype, llvm_args, false);
    m_llvm_function = m_module->create_llvm_function (ft, aname);

    if (sret ())
      {
#if defined (FUNCTION_ADDATTRIBUTE_ARG_IS_ATTRIBUTES)
        llvm::AttrBuilder attr_builder;
        attr_builder.addAttribute (llvm::Attributes::StructRet);
        llvm::Attributes attrs = llvm::Attributes::get(context, attr_builder);
        m_llvm_function->addAttribute (1, attrs);
#else
        m_llvm_function->addAttribute (1, llvm::Attribute::StructRet);
#endif
      }

    if (m_call_conv == jit_convention::internal)
#if defined (FUNCTION_ADDFNATTR_ARG_IS_ATTRIBUTES)
      m_llvm_function->addFnAttr (llvm::Attributes::AlwaysInline);
#else
      m_llvm_function->addFnAttr (llvm::Attribute::AlwaysInline);
#endif
  }

  jit_function::jit_function (const jit_function& fn, jit_type *aresult,
                              const std::vector<jit_type *>& aargs)
    : m_module (fn.m_module), m_llvm_function (fn.m_llvm_function),
      m_result (aresult), m_args (aargs), m_call_conv (fn.m_call_conv),
      m_can_error (fn.m_can_error)
  { }

  jit_function::jit_function (const jit_function& fn)
    : m_module (fn.m_module), m_llvm_function (fn.m_llvm_function),
      m_result (fn.m_result), m_args (fn.m_args), m_call_conv (fn.m_call_conv),
      m_can_error (fn.m_can_error)
  { }

  void
  jit_function::erase (void)
  {
    if (! m_llvm_function)
      return;

    m_llvm_function->eraseFromParent ();
    m_llvm_function = 0;
  }

  std::string
  jit_function::name (void) const
  {
    return m_llvm_function->getName ();
  }

  llvm::BasicBlock *
  jit_function::new_block (const std::string& aname,
                           llvm::BasicBlock *insert_before)
  {
    return llvm::BasicBlock::Create (context, aname, m_llvm_function,
                                     insert_before);
  }

  llvm::Value *
  jit_function::call (llvm::IRBuilderD& builder,
                      const std::vector<jit_value *>& in_args) const
  {
    if (! valid ())
      throw jit_fail_exception ("Call not implemented");

    assert (in_args.size () == m_args.size ());
    std::vector<llvm::Value *> llvm_args (m_args.size ());
    for (size_t i = 0; i < in_args.size (); ++i)
      llvm_args[i] = in_args[i]->to_llvm ();

    return call (builder, llvm_args);
  }

  llvm::Value *
  jit_function::call (llvm::IRBuilderD& builder,
                      const std::vector<llvm::Value *>& in_args) const
  {
    if (! valid ())
      throw jit_fail_exception ("Call not implemented");

    assert (in_args.size () == m_args.size ());
    llvm::SmallVector<llvm::Value *, 10> llvm_args;
    llvm_args.reserve (in_args.size () + sret ());

    llvm::BasicBlock *insert_block = builder.GetInsertBlock ();
    llvm::Function *parent = insert_block->getParent ();
    assert (parent);

    // Insert allocas inside the prelude block to prevent stack overflows
    llvm::BasicBlock& prelude = parent->getEntryBlock ();
    llvm::IRBuilder<> pre_builder (&prelude, prelude.begin ());

    llvm::AllocaInst *sret_mem = nullptr;
    if (sret ())
      {
        sret_mem = pre_builder.CreateAlloca (m_result->packed_type (m_call_conv));
        llvm_args.push_back (sret_mem);
      }

    for (size_t i = 0; i < in_args.size (); ++i)
      {
        llvm::Value *arg = in_args[i];
        jit_type::convert_fn convert = m_args[i]->pack (m_call_conv);
        if (convert)
          arg = convert (builder, arg);

        if (m_args[i]->pointer_arg (m_call_conv))
          {
            llvm::Type *ty = m_args[i]->packed_type (m_call_conv);
            llvm::Value *alloca = pre_builder.CreateAlloca (ty);
            builder.CreateStore (arg, alloca);
            arg = alloca;
          }

        llvm_args.push_back (arg);
      }

    llvm::CallInst *callinst = builder.CreateCall (m_llvm_function, llvm_args);
    llvm::Value *ret = callinst;

    if (sret ())
      {
#if defined (CALLINST_ADDATTRIBUTE_ARG_IS_ATTRIBUTES)
        llvm::AttrBuilder attr_builder;
        attr_builder.addAttribute(llvm::Attributes::StructRet);
        llvm::Attributes attrs = llvm::Attributes::get(context, attr_builder);
        callinst->addAttribute (1, attrs);
#else
        callinst->addAttribute (1, llvm::Attribute::StructRet);
#endif
        ret = builder.CreateLoad (sret_mem);
      }

    if (m_result)
      {
        jit_type::convert_fn unpack = m_result->unpack (m_call_conv);
        if (unpack)
          ret = unpack (builder, ret);
      }

    return ret;
  }

  llvm::Value *
  jit_function::argument (llvm::IRBuilderD& builder, size_t idx) const
  {
    assert (idx < m_args.size ());

    // FIXME: We should be treating arguments like a list, not a vector.
    // Shouldn't matter much for now, as the number of arguments shouldn't
    // be much bigger than 4
    auto iter = m_llvm_function->arg_begin ();
    if (sret ())
      ++iter;

    for (size_t i = 0; i < idx; ++i, ++iter);

    if (m_args[idx]->pointer_arg (m_call_conv))
      return builder.CreateLoad (&*iter);

    return &*iter;
  }

  void
  jit_function::do_return (llvm::IRBuilderD& builder, llvm::Value *rval,
                           bool verify)
  {
    assert (! rval == ! m_result);

    if (rval)
      {
        jit_type::convert_fn convert = m_result->pack (m_call_conv);
        if (convert)
          rval = convert (builder, rval);

        if (sret ())
          {
            builder.CreateStore (rval, &*(m_llvm_function->arg_begin ()));
            builder.CreateRetVoid ();
          }
        else
          builder.CreateRet (rval);
      }
    else
      builder.CreateRetVoid ();

    if (verify)
      llvm::verifyFunction (*m_llvm_function);
  }

  std::ostream&
  operator<< (std::ostream& os, const jit_function& fn)
  {
    llvm::Function *lfn = fn.to_llvm ();
    os << "jit_function: cc=" << fn.m_call_conv;
    llvm::raw_os_ostream llvm_out (os);
    lfn->print (llvm_out);
    llvm_out.flush ();
    return os;
  }

  // -------------------- jit_operation --------------------
  jit_operation::~jit_operation (void)
  {
    for (auto iter = m_generated.begin (); iter != m_generated.end (); ++iter)
      {
        delete iter->first;
        delete iter->second;
      }
  }

  void
  jit_operation::add_overload (const jit_function& func,
                               const std::vector<jit_type*>& args)
  {
    // Number of input arguments of the overload that is being registered
    size_t nargs = args.size ();

    if (nargs >= m_overloads.size ())
      m_overloads.resize (nargs + 1);

    Array<jit_function>& over = m_overloads[nargs];
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

  const jit_function&
  jit_operation::overload (const std::vector<jit_type*>& types) const
  {
    // Number of input arguments of the overload that is being looked for
    size_t nargs = types.size ();

    static jit_function null_overload;
    for (size_t i = 0; i < nargs; ++i)
      if (! types[i])
        return null_overload;

    if (nargs >= m_overloads.size ())
      return do_generate (types);

    const Array<jit_function>& over = m_overloads[nargs];
    dim_vector dv (over.dims ());
    Array<octave_idx_type> idx = to_idx (types);
    for (octave_idx_type i = 0; i < dv.length (); ++i)
      if (idx(i) >= dv(i))
        return do_generate (types);

    const jit_function& ret = over(idx);
    if (! ret.valid ())
      return do_generate (types);

    return ret;
  }

  Array<octave_idx_type>
  jit_operation::to_idx (const std::vector<jit_type*>& types) const
  {
    octave_idx_type numel = types.size ();
    numel = std::max (numel, static_cast<octave_idx_type>(2));

    Array<octave_idx_type> idx (dim_vector (1, numel));
    for (octave_idx_type i = 0;
         i < static_cast<octave_idx_type> (types.size ());
         ++i)
      idx(i) = types[i]->type_id ();

    if (types.size () == 0)
      idx(0) = idx(1) = 0;
    if (types.size () == 1)
      {
        idx(1) = idx(0);
        idx(0) = 0;
      }

    return idx;
  }

  const jit_function&
  jit_operation::do_generate (const signature_vec& types) const
  {
    static jit_function null_overload;
    generated_map::const_iterator find = m_generated.find (&types);
    if (find != m_generated.end ())
      {
        if (find->second)
          return *find->second;
        else
          return null_overload;
      }

    jit_function *ret = generate (types);
    m_generated[new signature_vec (types)] = ret;
    return ret ? *ret : null_overload;
  }

  jit_function *
  jit_operation::generate (const signature_vec&) const
  {
    return 0;
  }

  bool
  jit_operation::signature_cmp::operator () (const signature_vec *lhs,
                                             const signature_vec *rhs) const
  {
    const signature_vec& l = *lhs;
    const signature_vec& r = *rhs;

    if (l.size () < r.size ())
      return true;
    else if (l.size () > r.size ())
      return false;

    for (size_t i = 0; i < l.size (); ++i)
      {
        if (l[i]->type_id () < r[i]->type_id ())
          return true;
        else if (l[i]->type_id () > r[i]->type_id ())
          return false;
      }

    return false;
  }

  // -------------------- jit_index_operation --------------------
  jit_function *
  jit_index_operation::generate (const signature_vec& types) const
  {
    if (types.size () > 2 && types[0] == jit_typeinfo::get_matrix ())
      {
        // indexing a matrix with scalars
        jit_type *scalar = jit_typeinfo::get_scalar ();
        for (size_t i = 1; i < types.size (); ++i)
          if (types[i] != scalar)
            return 0;

        return generate_matrix (types);
      }

    return 0;
  }

  llvm::Value *
  jit_index_operation::create_arg_array (llvm::IRBuilderD& builder,
                                         const jit_function& fn,
                                         size_t start_idx,
                                         size_t end_idx) const
  {
    size_t n = end_idx - start_idx;
    llvm::Type *scalar_t = jit_typeinfo::get_scalar_llvm ();
    llvm::ArrayType *array_t = llvm::ArrayType::get (scalar_t, n);
    llvm::Value *array = llvm::UndefValue::get (array_t);
    for (size_t i = start_idx; i < end_idx; ++i)
      {
        llvm::Value *idx = fn.argument (builder, i);
        array = builder.CreateInsertValue (array, idx, i - start_idx);
      }

    llvm::Value *array_mem = builder.CreateAlloca (array_t);
    builder.CreateStore (array, array_mem);
    return builder.CreateBitCast (array_mem, scalar_t->getPointerTo ());
  }

  // -------------------- jit_paren_subsref --------------------

  jit_paren_subsref::jit_paren_subsref (const jit_typeinfo& ti)
    : jit_index_operation (ti, "()subsref"), m_paren_scalar (nullptr)
  { }

  jit_paren_subsref::~jit_paren_subsref (void)
  {
    delete m_paren_scalar;
  }

  jit_function *
  jit_paren_subsref::generate_matrix (const signature_vec& types) const
  {
    if (m_paren_scalar == nullptr)
      panic_impossible ();

    std::stringstream ss;
    ss << "jit_paren_subsref_matrix_scalar" << (types.size () - 1);

    // FIXME: Where will this be deleted?
    jit_function *fn = new jit_function
      (m_typeinfo.create_internal (ss.str (), m_typeinfo.m_scalar, types));

    fn->mark_can_error ();
    llvm::BasicBlock *body = fn->new_block ();
    llvm::IRBuilder<> builder (body);

    llvm::Value *array = create_arg_array (builder, *fn, 1, types.size ());
    llvm::Value *nelem = llvm::ConstantInt::get (m_typeinfo.m_index_t,
                                                 types.size () - 1);
    llvm::Value *mat = fn->argument (builder, 0);
    llvm::Value *ret = m_paren_scalar->call (builder, mat, array, nelem);
    fn->do_return (builder, ret);
    return fn;
  }

  void
  jit_paren_subsref::init_paren_scalar ()
  {
    std::vector<jit_type *> types (3);
    types[0] = m_typeinfo.m_matrix;
    types[1] = m_typeinfo.m_scalar_ptr;
    types[2] = m_typeinfo.m_index;

    m_paren_scalar = new jit_function
      (m_typeinfo.create_external (&octave_jit_paren_scalar,
                                   "octave_jit_paren_scalar",
                                   m_typeinfo.m_scalar, types));

    m_paren_scalar->mark_can_error ();
  }

  // -------------------- jit_paren_subsasgn --------------------

  jit_paren_subsasgn::jit_paren_subsasgn (const jit_typeinfo& ti)
    : jit_index_operation (ti, "()subsasgn"), m_paren_scalar (nullptr)
  { }

  jit_paren_subsasgn::~jit_paren_subsasgn (void)
  {
    delete m_paren_scalar;
  }

  jit_function *
  jit_paren_subsasgn::generate_matrix (const signature_vec& types) const
  {
    if (m_paren_scalar == nullptr)
      panic_impossible ();

    std::stringstream ss;
    ss << "jit_paren_subsasgn_matrix_scalar" << (types.size () - 2);

    // FIXME: Where will this be deleted?
    jit_function *fn = new jit_function
      (m_typeinfo.create_internal (ss.str (), m_typeinfo.m_matrix, types));

    fn->mark_can_error ();
    llvm::BasicBlock *body = fn->new_block ();
    llvm::IRBuilder<> builder (body);

    llvm::Value *array = create_arg_array (builder, *fn, 1, types.size () - 1);
    llvm::Value *nelem = llvm::ConstantInt::get (m_typeinfo.m_index_t,
                                                 types.size () - 2);

    llvm::Value *mat = fn->argument (builder, 0);
    llvm::Value *value = fn->argument (builder, types.size () - 1);
    llvm::Value *ret = m_paren_scalar->call (builder, mat, array, nelem, value);
    fn->do_return (builder, ret);

    return fn;
  }

  void
  jit_paren_subsasgn::init_paren_scalar ()
  {
    std::vector<jit_type *> types (4);
    types[0] = m_typeinfo.m_matrix;
    types[1] = m_typeinfo.m_scalar_ptr;
    types[2] = m_typeinfo.m_index;
    types[3] = m_typeinfo.m_scalar;

    m_paren_scalar = new jit_function
      (m_typeinfo.create_external (&octave_jit_paren_scalar_subsasgn,
                                   "octave_jit_paren_scalar",
                                   m_typeinfo.m_matrix, types));

    m_paren_scalar->mark_can_error ();
  }


  // -------------------- jit_typeinfo --------------------

  bool jit_typeinfo::s_in_construction = false;

  // Static method that holds the singleton instance
  jit_typeinfo&
  jit_typeinfo::instance (void)
  {
    if (s_in_construction)
      // This state is typically reached when the constructor calls one
      // of the static methods of the singleton class...
      panic_impossible ();

    static jit_typeinfo typeinfo;
    return typeinfo;
  }

  jit_typeinfo::~jit_typeinfo (void)
  {
    while (! m_id_to_type.empty ())
      {
        delete m_id_to_type.back ();
        m_id_to_type.pop_back ();
      }

    delete m_builder_ptr;
    delete m_base_jit_module;
  }

  // wrap function names to simplify jit_typeinfo::create_external
#define JIT_FN(fn) &fn, #fn

  jit_typeinfo::jit_typeinfo (void)
    : paren_subsref_fn (*this),
      paren_subsasgn_fn (*this),
      m_next_id (0),
      m_grab_fn ("grab"),
      m_release_fn ("release"),
      m_destroy_fn ("destroy"),
      m_print_fn ("print"),
      m_for_init_fn ("for_init"),
      m_for_check_fn ("for_check"),
      m_for_index_fn ("for_index"),
      m_logically_true_fn ("logically_true"),
      m_make_range_fn ("make_range"),
      m_end1_fn ("end1"),
      m_end_fn ("end"),
      m_create_undef_fn ("create_undef"),
      m_base_jit_module (new jit_module ("octaveJITBaseModule")),
      m_builder_ptr (new llvm::IRBuilderD (context)),
      // FIXME: Use a pointer directly in the constructor, and get rid of this
      m_builder (*m_builder_ptr)
  {
    s_in_construction = true;

    // ----- Register basic JIT types -----

    // FIXME: It seems that our type lattice is not really a lattice
    //        since any and any_ptr have no common upper_bound (?!?)

    // jit_types: "any"     < (nullptr)
    //            "any_ptr" < (nullptr)
    m_any_t = llvm::StructType::create (context, "octave_base_value");
    m_any_t = m_any_t->getPointerTo ();
    m_any = do_register_new_type ("any", nullptr, m_any_t);
    m_any_ptr = do_register_new_type ("any_ptr", nullptr,
                                      m_any_t->getPointerTo ());

    // jit_types: "scalar"     < "complex" < "any"
    //       and: "scalar_ptr" < (nullptr)
    // FIXME: what about sing-precision floats ???
    // FIXME: shouldn't we make scalar_ptr a sub_type of any_ptr ?
    m_scalar_t = llvm::Type::getDoubleTy (context);
    m_complex_t = llvm::ArrayType::get (m_scalar_t, 2);
    m_complex = do_register_new_type ("complex", m_any, m_complex_t);
    m_scalar = do_register_new_type ("scalar", m_complex, m_scalar_t);
    m_scalar_ptr = do_register_new_type ("scalar_ptr", nullptr,
                                         m_scalar_t->getPointerTo ());

    // jit_type: "bool" < "any"
    m_bool_t = llvm::Type::getInt1Ty (context);
    m_boolean = do_register_new_type ("bool", m_any, m_bool_t);

    // jit_types: "int8", "int16", "int32", "int64" < "any"
    m_ints[ 8] = do_register_new_type ("int8",  m_any,
                                       llvm::Type::getIntNTy (context,  8));
    m_ints[16] = do_register_new_type ("int16", m_any,
                                       llvm::Type::getIntNTy (context, 16));
    m_ints[32] = do_register_new_type ("int32", m_any,
                                       llvm::Type::getIntNTy (context, 32));
    m_ints[64] = do_register_new_type ("int64", m_any,
                                       llvm::Type::getIntNTy (context, 64));

    // jit_type: "string" < "any"
    m_string_t = llvm::Type::getInt8Ty (context);
    m_string_t = m_string_t->getPointerTo ();
    m_string = do_register_new_type ("string", m_any, m_string_t);

    // jit_type: "index" < "any"
    m_index_t = llvm::Type::getIntNTy (context, sizeof (octave_idx_type) * 8);
    m_index = do_register_new_type ("index", m_any, m_index_t);

    // jit_type: "range" < "any"
    m_range_t = llvm::StructType::create (context, "range");
    {
      std::vector<llvm::Type *> range_contents (4, m_scalar_t);
      range_contents[3] = m_index_t;
      m_range_t->setBody (range_contents);
    }
    m_range = do_register_new_type ("range", m_any, m_range_t);

    // jit_type: "matrix" < "any"
    m_matrix_t = llvm::StructType::create (context, "matrix");
    {
      llvm::Type *refcount_t = llvm::Type::getIntNTy (context, sizeof(int) * 8);
      llvm::Type *matrix_contents[5];
      matrix_contents[0] = refcount_t->getPointerTo ();
      matrix_contents[1] = m_scalar_t->getPointerTo ();
      matrix_contents[2] = m_index_t;
      matrix_contents[3] = m_index_t->getPointerTo ();
      matrix_contents[4] = m_string_t;
      m_matrix_t->setBody (llvm::makeArrayRef (matrix_contents, 5));
    }
    m_matrix = do_register_new_type ("matrix", m_any, m_matrix_t);

    // ----- Specify calling conventions -----

    // complex_ret is what is passed to C functions
    // in order to get calling convention right
    m_complex_ret = llvm::StructType::create (context, "complex_ret");
    {
      llvm::Type *cmplx_inner_cont[] = {m_scalar_t, m_scalar_t};
      llvm::StructType *cmplx_inner = llvm::StructType::create (cmplx_inner_cont);
      llvm::Type *contents[] = {cmplx_inner};
      m_complex_ret->setBody (contents);
    }

    // FIXME: We should detect architecture and do something sane
    //        based on that here we assume x86 or x86_64
    m_matrix->mark_sret (jit_convention::external);
    m_matrix->mark_pointer_arg (jit_convention::external);

    m_range->mark_sret (jit_convention::external);
    m_range->mark_pointer_arg (jit_convention::external);

    m_complex->set_pack (jit_convention::external,
                         &jit_typeinfo::pack_complex);
    m_complex->set_unpack (jit_convention::external,
                           &jit_typeinfo::unpack_complex);
    m_complex->set_packed_type (jit_convention::external, m_complex_ret);

    if (sizeof (void *) == 4)
      m_complex->mark_sret (jit_convention::external);

    paren_subsref_fn.init_paren_scalar ();
    paren_subsasgn_fn.init_paren_scalar ();

    // bind global variables
    m_lerror_state = m_base_jit_module->create_global_variable (m_bool_t, false,
                                                                "error_state");

    m_base_jit_module->add_global_mapping (m_lerror_state, &error_state);

    // sig_atomic_type is going to be some sort of integer
    m_sig_atomic_type = llvm::Type::getIntNTy (context,
                                               sizeof(sig_atomic_t) * 8);

    m_loctave_interrupt_state = m_base_jit_module->create_global_variable
      (m_sig_atomic_type, false, "octave_interrupt_state");

    m_base_jit_module->add_global_mapping (m_loctave_interrupt_state,
                                           &octave_interrupt_state);

    // generic call function
    {
      jit_type *int_t = do_get_intN (sizeof (octave_builtin::fcn) * 8);
      m_any_call = create_external (JIT_FN (octave_jit_call), m_any, int_t,
                                    int_t, m_any_ptr, int_t);
    }

    // any with anything is an any op
    jit_function fn;
    jit_type *binary_op_type = do_get_intN (sizeof (octave_value::binary_op) * 8);
    llvm::Type *llvm_bo_type = binary_op_type->to_llvm ();
    jit_function any_binary = create_external (JIT_FN (octave_jit_binary_any_any),
                                               m_any, binary_op_type,
                                               m_any, m_any);
    any_binary.mark_can_error ();

    for (size_t i = 0; i < octave_value::num_binary_ops; ++i)
      {
        octave_value::binary_op op = static_cast<octave_value::binary_op> (i);
        std::string op_name = octave_value::binary_op_as_string (op);
        m_binary_ops.push_back (jit_operation ("binary" + op_name));
      }

    for (size_t i = 0; i < octave_value::num_unary_ops; ++i)
      {
        octave_value::unary_op op = static_cast<octave_value::unary_op> (i);
        std::string op_name = octave_value::unary_op_as_string (op);
        m_unary_ops.push_back (jit_operation ("unary" + op_name));
      }

    for (int op = 0; op < octave_value::num_binary_ops; ++op)
      {
        const llvm::Twine &fn_name =
          "octave_jit_binary_any_any_" + llvm::Twine (op);

        fn = create_internal (fn_name, m_any, m_any, m_any);
        fn.mark_can_error ();
        llvm::BasicBlock *block = fn.new_block ();
        m_builder.SetInsertPoint (block);
        llvm::APInt op_int(sizeof (octave_value::binary_op) * 8, op,
                           std::numeric_limits<octave_value::binary_op>::is_signed);
        llvm::Value *op_as_llvm = llvm::ConstantInt::get (llvm_bo_type, op_int);
        llvm::Value *ret = any_binary.call (m_builder, op_as_llvm,
                                            fn.argument (m_builder, 0),
                                            fn.argument (m_builder, 1));
        fn.do_return (m_builder, ret);
        m_binary_ops[op].add_overload (fn);
      }

    // grab matrix
    fn = create_external (JIT_FN (octave_jit_grab_matrix), m_matrix, m_matrix);
    m_grab_fn.add_overload (fn);
    m_grab_fn.add_overload (create_identity (m_scalar));
    m_grab_fn.add_overload (create_identity (m_scalar_ptr));
    m_grab_fn.add_overload (create_identity (m_any_ptr));
    m_grab_fn.add_overload (create_identity (m_boolean));
    m_grab_fn.add_overload (create_identity (m_complex));
    m_grab_fn.add_overload (create_identity (m_index));

    // release any
    fn = create_external (JIT_FN (octave_jit_release_any), nullptr, m_any);
    m_release_fn.add_overload (fn);

    // release matrix
    fn = create_external (JIT_FN (octave_jit_release_matrix), nullptr,
                          m_matrix);
    m_release_fn.add_overload (fn);

    // destroy
    m_destroy_fn = m_release_fn;
    m_destroy_fn.add_overload (create_identity(m_scalar));
    m_destroy_fn.add_overload (create_identity(m_boolean));
    m_destroy_fn.add_overload (create_identity(m_index));
    m_destroy_fn.add_overload (create_identity(m_complex));

    // -------------------- scalar related operations --------------------

    // now for binary scalar operations
    add_binary_op (m_scalar, octave_value::op_add, llvm::Instruction::FAdd);
    add_binary_op (m_scalar, octave_value::op_sub, llvm::Instruction::FSub);
    add_binary_op (m_scalar, octave_value::op_mul, llvm::Instruction::FMul);
    add_binary_op (m_scalar, octave_value::op_el_mul, llvm::Instruction::FMul);

    add_binary_fcmp (m_scalar, octave_value::op_lt, llvm::CmpInst::FCMP_ULT);
    add_binary_fcmp (m_scalar, octave_value::op_le, llvm::CmpInst::FCMP_ULE);
    add_binary_fcmp (m_scalar, octave_value::op_eq, llvm::CmpInst::FCMP_UEQ);
    add_binary_fcmp (m_scalar, octave_value::op_ge, llvm::CmpInst::FCMP_UGE);
    add_binary_fcmp (m_scalar, octave_value::op_gt, llvm::CmpInst::FCMP_UGT);
    add_binary_fcmp (m_scalar, octave_value::op_ne, llvm::CmpInst::FCMP_UNE);

    jit_function gripe_div0 = create_external (JIT_FN (warn_divide_by_zero),
                                               nullptr);
    gripe_div0.mark_can_error ();

    // divide is annoying because it might error
    fn = create_internal ("octave_jit_div_scalar_scalar", m_scalar, m_scalar,
                          m_scalar);
    fn.mark_can_error ();

    llvm::BasicBlock *body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::BasicBlock *warn_block = fn.new_block ("warn");
      llvm::BasicBlock *normal_block = fn.new_block ("normal");

      llvm::Value *zero = llvm::ConstantFP::get (m_scalar_t, 0);
      llvm::Value *check = m_builder.CreateFCmpUEQ (zero,
                                                    fn.argument (m_builder, 1));
      m_builder.CreateCondBr (check, warn_block, normal_block);

      m_builder.SetInsertPoint (warn_block);
      gripe_div0.call (m_builder);
      m_builder.CreateBr (normal_block);

      m_builder.SetInsertPoint (normal_block);
      llvm::Value *ret = m_builder.CreateFDiv (fn.argument (m_builder, 0),
                                               fn.argument (m_builder, 1));
      fn.do_return (m_builder, ret);
    }
    m_binary_ops[octave_value::op_div].add_overload (fn);
    m_binary_ops[octave_value::op_el_div].add_overload (fn);

    // ldiv is the same as div with the operators reversed
    fn = mirror_binary (fn);
    m_binary_ops[octave_value::op_ldiv].add_overload (fn);
    m_binary_ops[octave_value::op_el_ldiv].add_overload (fn);

    // In general, the result of scalar ^ scalar is a complex number.  We might
    // be able to improve on this if we keep track of the range of values
    // variables can take on.
    fn = create_external (JIT_FN (octave_jit_pow_scalar_scalar), m_complex,
                          m_scalar, m_scalar);
    m_binary_ops[octave_value::op_pow].add_overload (fn);
    m_binary_ops[octave_value::op_el_pow].add_overload (fn);

    // now for unary scalar operations
    // FIXME: Impelment not
    fn = create_internal ("octave_jit_++", m_scalar, m_scalar);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *one = llvm::ConstantFP::get (m_scalar_t, 1);
      llvm::Value *val = fn.argument (m_builder, 0);
      val = m_builder.CreateFAdd (val, one);
      fn.do_return (m_builder, val);
    }
    m_unary_ops[octave_value::op_incr].add_overload (fn);

    fn = create_internal ("octave_jit_--", m_scalar, m_scalar);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *one = llvm::ConstantFP::get (m_scalar_t, 1);
      llvm::Value *val = fn.argument (m_builder, 0);
      val = m_builder.CreateFSub (val, one);
      fn.do_return (m_builder, val);
    }
    m_unary_ops[octave_value::op_decr].add_overload (fn);

    fn = create_internal ("octave_jit_uminus", m_scalar, m_scalar);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *mone = llvm::ConstantFP::get (m_scalar_t, -1);
      llvm::Value *val = fn.argument (m_builder, 0);
      val = m_builder.CreateFMul (val, mone);
      fn.do_return (m_builder, val);
    }
    m_unary_ops[octave_value::op_uminus].add_overload (fn);

    fn = create_identity (m_scalar);
    m_unary_ops[octave_value::op_uplus].add_overload (fn);
    m_unary_ops[octave_value::op_transpose].add_overload (fn);
    m_unary_ops[octave_value::op_hermitian].add_overload (fn);

    // now for binary complex operations
    fn = create_internal ("octave_jit_+_complex_complex", m_complex, m_complex,
                          m_complex);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *lhs = fn.argument (m_builder, 0);
      llvm::Value *rhs = fn.argument (m_builder, 1);
      llvm::Value *real = m_builder.CreateFAdd (complex_real (lhs),
                                                complex_real (rhs));
      llvm::Value *imag = m_builder.CreateFAdd (complex_imag (lhs),
                                                complex_imag (rhs));
      fn.do_return (m_builder, complex_new (real, imag));
    }
    m_binary_ops[octave_value::op_add].add_overload (fn);

    fn = create_internal ("octave_jit_-_complex_complex", m_complex, m_complex,
                          m_complex);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *lhs = fn.argument (m_builder, 0);
      llvm::Value *rhs = fn.argument (m_builder, 1);
      llvm::Value *real = m_builder.CreateFSub (complex_real (lhs),
                                                complex_real (rhs));
      llvm::Value *imag = m_builder.CreateFSub (complex_imag (lhs),
                                                complex_imag (rhs));
      fn.do_return (m_builder, complex_new (real, imag));
    }
    m_binary_ops[octave_value::op_sub].add_overload (fn);

    fn = create_external (JIT_FN (octave_jit_complex_mul),
                          m_complex, m_complex, m_complex);
    m_binary_ops[octave_value::op_mul].add_overload (fn);
    m_binary_ops[octave_value::op_el_mul].add_overload (fn);

    jit_function complex_div = create_external (JIT_FN (octave_jit_complex_div),
                                                m_complex, m_complex, m_complex);
    complex_div.mark_can_error ();
    m_binary_ops[octave_value::op_div].add_overload (fn);
    m_binary_ops[octave_value::op_ldiv].add_overload (fn);

    fn = create_external (JIT_FN (octave_jit_pow_complex_complex), m_complex,
                          m_complex, m_complex);
    m_binary_ops[octave_value::op_pow].add_overload (fn);
    m_binary_ops[octave_value::op_el_pow].add_overload (fn);

    fn = create_internal ("octave_jit_*_scalar_complex", m_complex, m_scalar,
                          m_complex);
    jit_function mul_scalar_complex = fn;
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::BasicBlock *complex_mul = fn.new_block ("complex_mul");
      llvm::BasicBlock *scalar_mul = fn.new_block ("scalar_mul");

      llvm::Value *fzero = llvm::ConstantFP::get (m_scalar_t, 0);
      llvm::Value *lhs = fn.argument (m_builder, 0);
      llvm::Value *rhs = fn.argument (m_builder, 1);

      llvm::Value *cmp = m_builder.CreateFCmpUEQ (complex_imag (rhs), fzero);
      m_builder.CreateCondBr (cmp, scalar_mul, complex_mul);

      m_builder.SetInsertPoint (scalar_mul);
      llvm::Value *temp = complex_real (rhs);
      temp = m_builder.CreateFMul (lhs, temp);
      fn.do_return (m_builder, complex_new (temp, fzero), false);

      m_builder.SetInsertPoint (complex_mul);
      temp = complex_new (m_builder.CreateFMul (lhs, complex_real (rhs)),
                          m_builder.CreateFMul (lhs, complex_imag (rhs)));
      fn.do_return (m_builder, temp);
    }
    m_binary_ops[octave_value::op_mul].add_overload (fn);
    m_binary_ops[octave_value::op_el_mul].add_overload (fn);

    fn = mirror_binary (mul_scalar_complex);
    m_binary_ops[octave_value::op_mul].add_overload (fn);
    m_binary_ops[octave_value::op_el_mul].add_overload (fn);

    fn = create_internal ("octave_jit_+_scalar_complex", m_complex, m_scalar,
                          m_complex);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *lhs = fn.argument (m_builder, 0);
      llvm::Value *rhs = fn.argument (m_builder, 1);
      llvm::Value *real = m_builder.CreateFAdd (lhs, complex_real (rhs));
      fn.do_return (m_builder, complex_real (rhs, real));
    }
    m_binary_ops[octave_value::op_add].add_overload (fn);

    fn = mirror_binary (fn);
    m_binary_ops[octave_value::op_add].add_overload (fn);

    fn = create_internal ("octave_jit_-_complex_scalar", m_complex, m_complex,
                          m_scalar);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *lhs = fn.argument (m_builder, 0);
      llvm::Value *rhs = fn.argument (m_builder, 1);
      llvm::Value *real = m_builder.CreateFSub (complex_real (lhs), rhs);
      fn.do_return (m_builder, complex_real (lhs, real));
    }
    m_binary_ops[octave_value::op_sub].add_overload (fn);

    fn = create_internal ("octave_jit_-_scalar_complex", m_complex, m_scalar,
                          m_complex);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *lhs = fn.argument (m_builder, 0);
      llvm::Value *rhs = fn.argument (m_builder, 1);
      llvm::Value *real = m_builder.CreateFSub (lhs, complex_real (rhs));
      fn.do_return (m_builder, complex_real (rhs, real));
    }
    m_binary_ops[octave_value::op_sub].add_overload (fn);

    fn = create_external (JIT_FN (octave_jit_pow_scalar_complex), m_complex,
                          m_scalar, m_complex);
    m_binary_ops[octave_value::op_pow].add_overload (fn);
    m_binary_ops[octave_value::op_el_pow].add_overload (fn);

    fn = create_external (JIT_FN (octave_jit_pow_complex_scalar), m_complex,
                          m_complex, m_scalar);
    m_binary_ops[octave_value::op_pow].add_overload (fn);
    m_binary_ops[octave_value::op_el_pow].add_overload (fn);

    // now for binary index operators
    add_binary_op (m_index, octave_value::op_add, llvm::Instruction::Add);

    // and binary bool operators
    add_binary_op (m_boolean, octave_value::op_el_or, llvm::Instruction::Or);
    add_binary_op (m_boolean, octave_value::op_el_and, llvm::Instruction::And);

    // now for printing functions
    add_print (m_any, reinterpret_cast<void *> (&octave_jit_print_any));
    add_print (m_scalar, reinterpret_cast<void *> (&octave_jit_print_scalar));

    // initialize for loop
    fn = create_internal ("octave_jit_for_range_init", m_index, m_range);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *zero = llvm::ConstantInt::get (m_index_t, 0);
      fn.do_return (m_builder, zero);
    }
    m_for_init_fn.add_overload (fn);

    // bounds check for for loop
    fn = create_internal ("octave_jit_for_range_check", m_boolean, m_range,
                          m_index);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *nelem
        = m_builder.CreateExtractValue (fn.argument (m_builder, 0), 3);
      llvm::Value *idx = fn.argument (m_builder, 1);
      llvm::Value *ret = m_builder.CreateICmpULT (idx, nelem);
      fn.do_return (m_builder, ret);
    }
    m_for_check_fn.add_overload (fn);

    // index variabe for for loop
    fn = create_internal ("octave_jit_for_range_idx", m_scalar, m_range,
                          m_index);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *idx = fn.argument (m_builder, 1);
      llvm::Value *didx = m_builder.CreateSIToFP (idx, m_scalar_t);
      llvm::Value *rng = fn.argument (m_builder, 0);
      llvm::Value *base = m_builder.CreateExtractValue (rng, 0);
      llvm::Value *inc = m_builder.CreateExtractValue (rng, 2);

      llvm::Value *ret = m_builder.CreateFMul (didx, inc);
      ret = m_builder.CreateFAdd (base, ret);
      fn.do_return (m_builder, ret);
    }
    m_for_index_fn.add_overload (fn);

    // logically true
    jit_function gripe_nantl
      = create_external (JIT_FN (octave_jit_err_nan_to_logical_conversion),
                         nullptr);
    gripe_nantl.mark_can_error ();
    fn = create_internal ("octave_jit_logically_true_scalar", m_boolean,
                          m_scalar);
    fn.mark_can_error ();
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::BasicBlock *error_block = fn.new_block ("error");
      llvm::BasicBlock *normal_block = fn.new_block ("normal");

      llvm::Value *check = m_builder.CreateFCmpUNE (fn.argument (m_builder, 0),
                                                    fn.argument (m_builder, 0));
      m_builder.CreateCondBr (check, error_block, normal_block);

      m_builder.SetInsertPoint (error_block);
      gripe_nantl.call (m_builder);
      m_builder.CreateBr (normal_block);
      m_builder.SetInsertPoint (normal_block);

      llvm::Value *zero = llvm::ConstantFP::get (m_scalar_t, 0);
      llvm::Value *ret = m_builder.CreateFCmpONE (fn.argument (m_builder, 0), zero);
      fn.do_return (m_builder, ret);
    }
    m_logically_true_fn.add_overload (fn);

    // logically_true boolean
    fn = create_identity (m_boolean);
    m_logically_true_fn.add_overload (fn);

    // make_range
    // FIXME: May be benificial to implement all in LLVM
    jit_function compute_nelem
      = create_external (JIT_FN (octave_jit_compute_nelem),
                         m_index, m_scalar, m_scalar, m_scalar);
    fn = create_internal ("octave_jit_make_range", m_range, m_scalar, m_scalar,
                          m_scalar);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *base = fn.argument (m_builder, 0);
      llvm::Value *limit = fn.argument (m_builder, 1);
      llvm::Value *inc = fn.argument (m_builder, 2);
      llvm::Value *nelem = compute_nelem.call (m_builder, base, limit, inc);

      llvm::Value *dzero = llvm::ConstantFP::get (m_scalar_t, 0);
      llvm::Value *izero = llvm::ConstantInt::get (m_index_t, 0);
      llvm::Value *rng = llvm::ConstantStruct::get (m_range_t, dzero, dzero,
                                                    dzero, izero, NULL);
      rng = m_builder.CreateInsertValue (rng, base, 0);
      rng = m_builder.CreateInsertValue (rng, limit, 1);
      rng = m_builder.CreateInsertValue (rng, inc, 2);
      rng = m_builder.CreateInsertValue (rng, nelem, 3);
      fn.do_return (m_builder, rng);
    }
    m_make_range_fn.add_overload (fn);

    // paren_subsref
    jit_type *jit_int = do_get_intN (sizeof (int) * 8);
    llvm::Type *int_t = jit_int->to_llvm ();
    jit_function ginvalid_index
      = create_external (JIT_FN (octave_jit_ginvalid_index), nullptr);
    jit_function gindex_range = create_external (JIT_FN (octave_jit_gindex_range),
                                                 nullptr, jit_int, jit_int,
                                                 m_index, m_index);

    fn = create_internal ("()subsref", m_scalar, m_matrix, m_scalar);
    fn.mark_can_error ();

    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *one_idx = llvm::ConstantInt::get (m_index_t, 1);
      llvm::Value *one_int = llvm::ConstantInt::get (int_t, 1);

      llvm::Value *undef = llvm::UndefValue::get (m_scalar_t);
      llvm::Value *mat = fn.argument (m_builder, 0);
      llvm::Value *idx = fn.argument (m_builder, 1);

      // convert index to scalar to integer, and check index >= 1
      llvm::Value *int_idx = m_builder.CreateFPToSI (idx, m_index_t);
      llvm::Value *check_idx = m_builder.CreateSIToFP (int_idx, m_scalar_t);
      llvm::Value *cond0 = m_builder.CreateFCmpUNE (idx, check_idx);
      llvm::Value *cond1 = m_builder.CreateICmpSLT (int_idx, one_idx);
      llvm::Value *cond = m_builder.CreateOr (cond0, cond1);

      llvm::BasicBlock *done = fn.new_block ("done");
      llvm::BasicBlock *conv_error = fn.new_block ("conv_error", done);
      llvm::BasicBlock *normal = fn.new_block ("normal", done);
      m_builder.CreateCondBr (cond, conv_error, normal);

      m_builder.SetInsertPoint (conv_error);
      ginvalid_index.call (m_builder);
      m_builder.CreateBr (done);

      m_builder.SetInsertPoint (normal);
      llvm::Value *len
        = m_builder.CreateExtractValue (mat, llvm::ArrayRef<unsigned> (2));
      cond = m_builder.CreateICmpSGT (int_idx, len);

      llvm::BasicBlock *bounds_error = fn.new_block ("bounds_error", done);
      llvm::BasicBlock *success = fn.new_block ("success", done);
      m_builder.CreateCondBr (cond, bounds_error, success);

      m_builder.SetInsertPoint (bounds_error);
      gindex_range.call (m_builder, one_int, one_int, int_idx, len);
      m_builder.CreateBr (done);

      m_builder.SetInsertPoint (success);
      llvm::Value *data = m_builder.CreateExtractValue (mat,
                                                        llvm::ArrayRef<unsigned> (1));
      llvm::Value *gep = m_builder.CreateInBoundsGEP (data, int_idx);
      llvm::Value *ret = m_builder.CreateLoad (gep);
      m_builder.CreateBr (done);

      m_builder.SetInsertPoint (done);

      llvm::PHINode *merge = llvm::PHINode::Create (m_scalar_t, 3);
      m_builder.Insert (merge);
      merge->addIncoming (undef, conv_error);
      merge->addIncoming (undef, bounds_error);
      merge->addIncoming (ret, success);
      fn.do_return (m_builder, merge);
    }
    paren_subsref_fn.add_overload (fn);

    // paren subsasgn
    jit_function resize_paren_subsasgn
      = create_external (JIT_FN (octave_jit_paren_subsasgn_impl), m_matrix,
                         m_matrix, m_index, m_scalar);
    fn = create_internal ("octave_jit_paren_subsasgn", m_matrix, m_matrix,
                          m_scalar, m_scalar);
    fn.mark_can_error ();
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *one_idx = llvm::ConstantInt::get (m_index_t, 1);
      llvm::Value *one_int = llvm::ConstantInt::get (int_t, 1);

      llvm::Value *mat = fn.argument (m_builder, 0);
      llvm::Value *idx = fn.argument (m_builder, 1);
      llvm::Value *value = fn.argument (m_builder, 2);

      llvm::Value *int_idx = m_builder.CreateFPToSI (idx, m_index_t);
      llvm::Value *check_idx = m_builder.CreateSIToFP (int_idx, m_scalar_t);
      llvm::Value *cond0 = m_builder.CreateFCmpUNE (idx, check_idx);
      llvm::Value *cond1 = m_builder.CreateICmpSLT (int_idx, one_idx);
      llvm::Value *cond = m_builder.CreateOr (cond0, cond1);

      llvm::BasicBlock *done = fn.new_block ("done");

      llvm::BasicBlock *conv_error = fn.new_block ("conv_error", done);
      llvm::BasicBlock *normal = fn.new_block ("normal", done);
      m_builder.CreateCondBr (cond, conv_error, normal);
      m_builder.SetInsertPoint (conv_error);
      ginvalid_index.call (m_builder);
      m_builder.CreateBr (done);

      m_builder.SetInsertPoint (normal);
      llvm::Value *len = m_builder.CreateExtractValue (mat, 2);
      cond0 = m_builder.CreateICmpSGT (int_idx, len);

      llvm::Value *rcount = m_builder.CreateExtractValue (mat, 0);
      rcount = m_builder.CreateLoad (rcount);
      cond1 = m_builder.CreateICmpSGT (rcount, one_int);
      cond = m_builder.CreateOr (cond0, cond1);

      llvm::BasicBlock *bounds_error = fn.new_block ("bounds_error", done);
      llvm::BasicBlock *success = fn.new_block ("success", done);
      m_builder.CreateCondBr (cond, bounds_error, success);

      // resize on out of bounds access
      m_builder.SetInsertPoint (bounds_error);
      llvm::Value *resize_result = resize_paren_subsasgn.call (m_builder, mat,
                                                               int_idx, value);
      m_builder.CreateBr (done);

      m_builder.SetInsertPoint (success);
      llvm::Value *data
        = m_builder.CreateExtractValue (mat, llvm::ArrayRef<unsigned> (1));
      llvm::Value *gep = m_builder.CreateInBoundsGEP (data, int_idx);
      m_builder.CreateStore (value, gep);
      m_builder.CreateBr (done);

      m_builder.SetInsertPoint (done);

      llvm::PHINode *merge = llvm::PHINode::Create (m_matrix_t, 3);
      m_builder.Insert (merge);
      merge->addIncoming (mat, conv_error);
      merge->addIncoming (resize_result, bounds_error);
      merge->addIncoming (mat, success);
      fn.do_return (m_builder, merge);
    }
    paren_subsasgn_fn.add_overload (fn);

    fn = create_external (JIT_FN (octave_jit_paren_subsasgn_matrix_range),
                          m_matrix, m_matrix, m_range, m_scalar);
    fn.mark_can_error ();
    paren_subsasgn_fn.add_overload (fn);

    fn = create_internal ("octave_jit_end1_matrix", m_scalar, m_matrix,
                          m_index, m_index);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *mat = fn.argument (m_builder, 0);
      llvm::Value *ret = m_builder.CreateExtractValue (mat, 2);
      fn.do_return (m_builder, m_builder.CreateSIToFP (ret, m_scalar_t));
    }
    m_end1_fn.add_overload (fn);

    fn = create_external (JIT_FN (octave_jit_end_matrix),m_scalar, m_matrix,
                          m_index, m_index);
    m_end_fn.add_overload (fn);

    // -------------------- create_undef --------------------
    fn = create_external (JIT_FN (octave_jit_create_undef), m_any);
    m_create_undef_fn.add_overload (fn);

    m_casts[m_any->type_id ()].stash_name ("(any)");
    m_casts[m_scalar->type_id ()].stash_name ("(scalar)");
    m_casts[m_complex->type_id ()].stash_name ("(complex)");
    m_casts[m_matrix->type_id ()].stash_name ("(matrix)");
    m_casts[m_range->type_id ()].stash_name ("(range)");

    // cast m_any <- matrix
    fn = create_external (JIT_FN (octave_jit_cast_any_matrix), m_any,
                          m_matrix);
    m_casts[m_any->type_id ()].add_overload (fn);

    // cast matrix <- any
    fn = create_external (JIT_FN (octave_jit_cast_matrix_any), m_matrix,
                          m_any);
    m_casts[m_matrix->type_id ()].add_overload (fn);

    // cast any <- range
    fn = create_external (JIT_FN (octave_jit_cast_any_range), m_any, m_range);
    m_casts[m_any->type_id ()].add_overload (fn);

    // cast range <- any
    fn = create_external (JIT_FN (octave_jit_cast_range_any), m_range, m_any);
    m_casts[m_range->type_id ()].add_overload (fn);

    // cast any <- scalar
    fn = create_external (JIT_FN (octave_jit_cast_any_scalar), m_any,
                          m_scalar);
    m_casts[m_any->type_id ()].add_overload (fn);

    // cast scalar <- any
    fn = create_external (JIT_FN (octave_jit_cast_scalar_any), m_scalar,
                          m_any);
    m_casts[m_scalar->type_id ()].add_overload (fn);

    // cast any <- complex
    fn = create_external (JIT_FN (octave_jit_cast_any_complex), m_any,
                          m_complex);
    m_casts[m_any->type_id ()].add_overload (fn);

    // cast complex <- any
    fn = create_external (JIT_FN (octave_jit_cast_complex_any), m_complex,
                          m_any);
    m_casts[m_complex->type_id ()].add_overload (fn);

    // cast complex <- scalar
    fn = create_internal ("octave_jit_cast_complex_scalar", m_complex,
                          m_scalar);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    {
      llvm::Value *zero = llvm::ConstantFP::get (m_scalar_t, 0);
      fn.do_return (m_builder, complex_new (fn.argument (m_builder, 0), zero));
    }
    m_casts[m_complex->type_id ()].add_overload (fn);

    // cast scalar <- complex
    fn = create_internal ("octave_jit_cast_scalar_complex", m_scalar,
                          m_complex);
    body = fn.new_block ();
    m_builder.SetInsertPoint (body);
    fn.do_return (m_builder, complex_real (fn.argument (m_builder, 0)));
    m_casts[m_scalar->type_id ()].add_overload (fn);

    // cast any <- any
    fn = create_identity (m_any);
    m_casts[m_any->type_id ()].add_overload (fn);

    // cast scalar <- scalar
    fn = create_identity (m_scalar);
    m_casts[m_scalar->type_id ()].add_overload (fn);

    // cast complex <- complex
    fn = create_identity (m_complex);
    m_casts[m_complex->type_id ()].add_overload (fn);

    // -------------------- builtin functions --------------------
    add_builtin ("#unknown_function");
    m_unknown_function = m_builtins["#unknown_function"];

    add_builtin ("sin");
    register_intrinsic ("sin", llvm::Intrinsic::sin, m_scalar, m_scalar);
    register_generic ("sin", m_matrix, m_matrix);

    add_builtin ("cos");
    register_intrinsic ("cos", llvm::Intrinsic::cos, m_scalar, m_scalar);
    register_generic ("cos", m_matrix, m_matrix);

    add_builtin ("exp");
    register_intrinsic ("exp", llvm::Intrinsic::exp, m_scalar, m_scalar);
    register_generic ("exp", m_matrix, m_matrix);

    add_builtin ("balance");
    register_generic ("balance", m_matrix, m_matrix);

    add_builtin ("cond");
    register_generic ("cond", m_scalar, m_matrix);

    add_builtin ("det");
    register_generic ("det", m_scalar, m_matrix);

    add_builtin ("norm");
    register_generic ("norm", m_scalar, m_matrix);

    add_builtin ("rand");
    register_generic ("rand", m_matrix, m_scalar);
    register_generic ("rand", m_matrix, std::vector<jit_type *> (2, m_scalar));

    add_builtin ("magic");
    register_generic ("magic", m_matrix, m_scalar);
    register_generic ("magic", m_matrix,
                      std::vector<jit_type *> (2, m_scalar));

    add_builtin ("eye");
    register_generic ("eye", m_matrix, m_scalar);
    register_generic ("eye", m_matrix, std::vector<jit_type *> (2, m_scalar));

    add_builtin ("mod");
    register_generic ("mod", m_scalar, std::vector<jit_type *> (2, m_scalar));

    // m_casts.resize (m_next_id + 1);
    jit_function any_id = create_identity (m_any);
    jit_function grab_any = create_external (JIT_FN (octave_jit_grab_any),
                                             m_any, m_any);

    jit_function release_any = m_release_fn.overload (m_any);

    std::vector<jit_type *> args;
    args.resize (1);

    for (auto iter = m_builtins.begin ();
         iter != m_builtins.end (); ++iter)
      {
        jit_type *btype = iter->second;
        args[0] = btype;

        m_grab_fn.add_overload (jit_function (grab_any, btype, args));
        m_release_fn.add_overload (jit_function (release_any, 0, args));
        m_casts[m_any->type_id ()].add_overload (jit_function (any_id, m_any,
                                                               args));

        args[0] = m_any;
        m_casts[btype->type_id ()].add_overload (jit_function (any_id, btype,
                                                               args));
      }

    m_base_jit_module->finalizeObject ();

    s_in_construction = false;
  }

  // create a function with an external calling convention
  // forces the function pointer to be specified
  template <typename fn_ptr_type> jit_function
  jit_typeinfo::create_external (fn_ptr_type fn,
                                 const llvm::Twine& name,
                                 jit_type *ret,
                                 const std::vector<jit_type *>& args) const
  {
    jit_function retval (m_base_jit_module, jit_convention::external,
                         name, ret, args);

    m_base_jit_module->add_global_mapping (retval.to_llvm (), fn);

    return retval;
  }

  jit_type*
  jit_typeinfo::do_register_new_type (const std::string& name,
                                      jit_type *parent,
                                      llvm::Type *llvm_type,
                                      bool skip_paren)
  {
    // FIXME: Currently our types do *not* form a lattice
    assert ((name == "any") || (name == "any_ptr") ||
            (name == "scalar_ptr") || (parent != nullptr));

    jit_type *ret = new jit_type (name, parent, llvm_type, skip_paren,
                                  m_next_id++);
    m_id_to_type.push_back (ret);

    m_casts.push_back (jit_operation ("(" + name + ")"));
    m_identities.push_back (jit_function ());

    return ret;
  }

  jit_type*
  jit_typeinfo::do_get_intN (size_t nbits) const
  {
    std::map<size_t, jit_type *>::const_iterator iter = m_ints.find (nbits);
    if (iter != m_ints.end ())
      return iter->second;

    throw jit_fail_exception ("No such integer type");
  }

  const jit_function&
  jit_typeinfo::do_end (jit_value *value, jit_value *idx, jit_value *count)
  {
    jit_const_index *ccount = dynamic_cast<jit_const_index *> (count);
    if (ccount && ccount->value () == 1)
      return m_end1_fn.overload (value->type (), idx->type (), count->type ());

    return m_end_fn.overload (value->type (), idx->type (), count->type ());
  }

  void
  jit_typeinfo::add_print (jit_type *ty, void *fptr)
  {
    std::stringstream name;
    name << "octave_jit_print_" << ty->name ();

    jit_function fn = create_external (fptr, name.str (), nullptr,
                                       do_get_intN (8), ty);

    m_print_fn.add_overload (fn);
  }

  // FIXME: cp between add_binary_op, add_binary_icmp, and add_binary_fcmp
  void
  jit_typeinfo::add_binary_op (jit_type *ty, int op, int llvm_op)
  {
    std::stringstream fname;
    octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
    fname << "octave_jit_" << octave_value::binary_op_as_string (ov_op)
          << '_' << ty->name ();

    jit_function fn = create_internal (fname.str (), ty, ty, ty);
    llvm::BasicBlock *block = fn.new_block ();
    m_builder.SetInsertPoint (block);
    llvm::Instruction::BinaryOps temp
      = static_cast<llvm::Instruction::BinaryOps>(llvm_op);

    llvm::Value *ret = m_builder.CreateBinOp (temp, fn.argument (m_builder, 0),
                                              fn.argument (m_builder, 1));
    fn.do_return (m_builder, ret);
    m_binary_ops[op].add_overload (fn);
  }

  void
  jit_typeinfo::add_binary_icmp (jit_type *ty, int op, int llvm_op)
  {
    std::stringstream fname;
    octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
    fname << "octave_jit" << octave_value::binary_op_as_string (ov_op)
          << '_' << ty->name ();

    jit_function fn = create_internal (fname.str (), m_boolean, ty, ty);
    llvm::BasicBlock *block = fn.new_block ();
    m_builder.SetInsertPoint (block);
    llvm::CmpInst::Predicate temp
      = static_cast<llvm::CmpInst::Predicate>(llvm_op);
    llvm::Value *ret = m_builder.CreateICmp (temp, fn.argument (m_builder, 0),
                                             fn.argument (m_builder, 1));
    fn.do_return (m_builder, ret);
    m_binary_ops[op].add_overload (fn);
  }

  void
  jit_typeinfo::add_binary_fcmp (jit_type *ty, int op, int llvm_op)
  {
    std::stringstream fname;
    octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
    fname << "octave_jit" << octave_value::binary_op_as_string (ov_op)
          << '_' << ty->name ();

    jit_function fn = create_internal (fname.str (), m_boolean, ty, ty);
    llvm::BasicBlock *block = fn.new_block ();
    m_builder.SetInsertPoint (block);
    llvm::CmpInst::Predicate temp
      = static_cast<llvm::CmpInst::Predicate>(llvm_op);
    llvm::Value *ret = m_builder.CreateFCmp (temp, fn.argument (m_builder, 0),
                                             fn.argument (m_builder, 1));
    fn.do_return (m_builder, ret);
    m_binary_ops[op].add_overload (fn);
  }

  jit_function
  jit_typeinfo::create_identity (jit_type *type)
  {
    size_t id = type->type_id ();
    if (id >= m_identities.size ())
      m_identities.resize (id + 1);

    if (! m_identities[id].valid ())
      {
        std::stringstream name;
        name << "id_" << type->name ();

        jit_function fn = create_internal (name.str (), type, type);
        llvm::BasicBlock *body = fn.new_block ();
        m_builder.SetInsertPoint (body);
        fn.do_return (m_builder, fn.argument (m_builder, 0));
        return m_identities[id] = fn;
      }

    return m_identities[id];
  }

  llvm::Value *
  jit_typeinfo::do_insert_error_check (llvm::IRBuilderD& abuilder)
  {
    return abuilder.CreateLoad (m_lerror_state);
  }

  llvm::Value *
  jit_typeinfo::do_insert_interrupt_check (llvm::IRBuilderD& abuilder)
  {
    llvm::LoadInst *val = abuilder.CreateLoad (m_loctave_interrupt_state);
    val->setVolatile (true);
    return abuilder.CreateICmpSGT (val, abuilder.getInt32 (0));
  }

  void
  jit_typeinfo::add_builtin (const std::string& name)
  {
    jit_type *btype = do_register_new_type (name, m_any, m_any_t, true);
    m_builtins[name] = btype;

    octave_builtin *ov_builtin = find_builtin (name);
    if (ov_builtin)
      ov_builtin->stash_jit (*btype);
  }

  void
  jit_typeinfo::register_intrinsic (const std::string& name, size_t iid,
                                    jit_type *result,
                                    const std::vector<jit_type *>& args)
  {
    jit_type *builtin_type = m_builtins[name];
    size_t nargs = args.size ();
    std::vector<llvm::Type*> llvm_args (nargs);
    for (size_t i = 0; i < nargs; ++i)
      llvm_args[i] = args[i]->to_llvm ();

    llvm::Function *ifun = m_base_jit_module->
      get_intrinsic_declaration (iid, llvm_args);

    std::stringstream fn_name;
    fn_name << "octave_jit_" << name;

    std::vector<jit_type *> args1 (nargs + 1);
    args1[0] = builtin_type;
    std::copy (args.begin (), args.end (), args1.begin () + 1);

    // The first argument will be the Octave function, but we already know that
    // the function call is the equivalent of the intrinsic, so we ignore it
    // and call the intrinsic with the remaining arguments.
    jit_function fn = create_internal (fn_name.str (), result, args1);
    llvm::BasicBlock *body = fn.new_block ();
    m_builder.SetInsertPoint (body);

    llvm::SmallVector<llvm::Value *, 5> fargs (nargs);
    for (size_t i = 0; i < nargs; ++i)
      fargs[i] = fn.argument (m_builder, i + 1);

    llvm::Value *ret = m_builder.CreateCall (ifun, fargs);
    fn.do_return (m_builder, ret);
    paren_subsref_fn.add_overload (fn);
  }

  octave_builtin *
  jit_typeinfo::find_builtin (const std::string& name)
  {
    symbol_table& symtab = __get_symbol_table__ ("jit_typeinfo::find_builtin");

    // FIXME: Finalize what we want to store in octave_builtin, then add
    // functions to access these values in octave_value
    octave_value ov_builtin = symtab.find (name);
    return dynamic_cast<octave_builtin *> (ov_builtin.internal_rep ());
  }

  void
  jit_typeinfo::register_generic (const std::string& name, jit_type *result,
                                  const std::vector<jit_type *>& args)
  {
    octave_builtin *builtin = find_builtin (name);
    if (! builtin)
      return;

    std::vector<jit_type *> fn_args (args.size () + 1);
    fn_args[0] = m_builtins[name];
    std::copy (args.begin (), args.end (), fn_args.begin () + 1);
    jit_function fn = create_internal (name, result, fn_args);
    fn.mark_can_error ();
    llvm::BasicBlock *block = fn.new_block ();
    m_builder.SetInsertPoint (block);
    llvm::ArrayType *array_t = llvm::ArrayType::get (m_any_t, args.size ());
    llvm::Value *array = llvm::UndefValue::get (array_t);
    for (size_t i = 0; i < args.size (); ++i)
      {
        llvm::Value *arg = fn.argument (m_builder, i + 1);
        jit_function agrab = m_grab_fn.overload (args[i]);
        if (agrab.valid ())
          arg = agrab.call (m_builder, arg);
        jit_function acast = do_cast (m_any, args[i]);
        array = m_builder.CreateInsertValue (array,
                                             acast.call (m_builder, arg), i);
      }

    llvm::Value *array_mem = m_builder.CreateAlloca (array_t);
    m_builder.CreateStore (array, array_mem);
    array = m_builder.CreateBitCast (array_mem, m_any_t->getPointerTo ());

    jit_type *jintTy = do_get_intN (sizeof (octave_builtin::fcn) * 8);
    llvm::Type *intTy = jintTy->to_llvm ();
    size_t fcn_int = reinterpret_cast<size_t> (builtin->function ());
    llvm::Value *fcn = llvm::ConstantInt::get (intTy, fcn_int);
    llvm::Value *nargin = llvm::ConstantInt::get (intTy, args.size ());
    size_t result_int = reinterpret_cast<size_t> (result);
    llvm::Value *res_llvm = llvm::ConstantInt::get (intTy, result_int);
    llvm::Value *ret = m_any_call.call (m_builder, fcn, nargin, array,
                                        res_llvm);

    jit_function cast_result = do_cast (result, m_any);
    fn.do_return (m_builder, cast_result.call (m_builder, ret));
    paren_subsref_fn.add_overload (fn);
  }

  jit_function
  jit_typeinfo::mirror_binary (const jit_function& fn)
  {
    jit_function ret = create_internal (fn.name () + "_reverse",
                                        fn.result (), fn.argument_type (1),
                                        fn.argument_type (0));
    if (fn.can_error ())
      ret.mark_can_error ();

    llvm::BasicBlock *body = ret.new_block ();
    m_builder.SetInsertPoint (body);
    llvm::Value *result = fn.call (m_builder, ret.argument (m_builder, 1),
                                   ret.argument (m_builder, 0));
    if (ret.result ())
      ret.do_return (m_builder, result);
    else
      ret.do_return (m_builder);

    return ret;
  }

  llvm::Value *
  jit_typeinfo::do_pack_complex (llvm::IRBuilderD& bld, llvm::Value *cplx) const
  {
    llvm::Value *real = bld.CreateExtractValue (cplx, 0);
    llvm::Value *imag = bld.CreateExtractValue (cplx, 1);
    llvm::Value *ret = llvm::UndefValue::get (m_complex_ret);

    unsigned int re_idx[] = {0, 0};
    unsigned int im_idx[] = {0, 1};
    ret = bld.CreateInsertValue (ret, real, re_idx);
    return bld.CreateInsertValue (ret, imag, im_idx);
  }

  llvm::Value *
  jit_typeinfo::unpack_complex (llvm::IRBuilderD& bld, llvm::Value *result)
  {
    unsigned int re_idx[] = {0, 0};
    unsigned int im_idx[] = {0, 1};

    llvm::Type *m_complex_t = get_complex ()->to_llvm ();
    llvm::Value *real = bld.CreateExtractValue (result, re_idx);
    llvm::Value *imag = bld.CreateExtractValue (result, im_idx);
    llvm::Value *ret = llvm::UndefValue::get (m_complex_t);

    ret = bld.CreateInsertValue (ret, real, 0);
    return bld.CreateInsertValue (ret, imag, 1);
  }

  llvm::Value *
  jit_typeinfo::complex_real (llvm::Value *cx)
  {
    return m_builder.CreateExtractValue (cx, 0);
  }

  llvm::Value *
  jit_typeinfo::complex_real (llvm::Value *cx, llvm::Value *real)
  {
    return m_builder.CreateInsertValue (cx, real, 0);
  }

  llvm::Value *
  jit_typeinfo::complex_imag (llvm::Value *cx)
  {
    return m_builder.CreateExtractValue (cx, 1);
  }

  llvm::Value *
  jit_typeinfo::complex_imag (llvm::Value *cx, llvm::Value *imag)
  {
    return m_builder.CreateInsertValue (cx, imag, 1);
  }

  llvm::Value *
  jit_typeinfo::complex_new (llvm::Value *real, llvm::Value *imag)
  {
    llvm::Value *ret = llvm::UndefValue::get (m_complex->to_llvm ());
    ret = complex_real (ret, real);
    return complex_imag (ret, imag);
  }

  jit_type *
  jit_typeinfo::do_type_of (const octave_value& ov) const
  {
    if (ov.is_function ())
      {
        // FIXME: This is ugly, we need to finalize how we want to do this,
        // then have octave_value fully support the needed functionality
        octave_builtin *builtin
          = dynamic_cast<octave_builtin *> (ov.internal_rep ());
        return builtin && builtin->to_jit () ? builtin->to_jit ()
                                             : m_unknown_function;
      }

    if (ov.is_range ())
      return m_range;

    if (ov.is_double_type () && ! ov.iscomplex ())
      {
        if (ov.is_real_scalar ())
          return m_scalar;

        if (ov.is_matrix_type ())
          return m_matrix;
      }

    if (ov.is_complex_scalar ())
      {
        Complex cv = ov.complex_value ();

        // We don't really represent complex values, instead we represent
        // complex_or_scalar.  If the imag value is zero, we assume a scalar.
        if (cv.imag () != 0)
          return m_complex;
      }

    return m_any;
  }
}

#endif
