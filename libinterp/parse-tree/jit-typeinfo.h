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

#if ! defined (octave_jit_typeinfo_h)
#define octave_jit_typeinfo_h 1

#include "octave-config.h"

#if defined (HAVE_LLVM)

#include <map>
#include <vector>

#include "Range.h"
#include "jit-util.h"

namespace octave
{
  class jit_typeinfo;
  class jit_module;

  // Defines the type system used by jit and a singleton class, jit_typeinfo, to
  // manage the types.
  //
  // FIXME:
  // Operations are defined and implemented in jit_typeinfo.  Eventually they
  // should be moved elsewhere. (just like with octave_typeinfo)

  // jit_range is compatible with the llvm range structure
  struct
  jit_range
  {
    jit_range (const Range& from)
      : m_base (from.base ()), m_limit (from.limit ()), m_inc (from.inc ()),
        m_nelem (from.numel ())
    { }

    operator Range () const
    {
      return Range (m_base, m_limit, m_inc);
    }

    bool all_elements_are_ints (void) const;

    double m_base;
    double m_limit;
    double m_inc;
    octave_idx_type m_nelem;
  };

  std::ostream& operator << (std::ostream& os, const jit_range& rng);

  // jit_array is compatible with the llvm array/matrix structures
  template <typename T, typename U>
  struct
  jit_array
  {
    jit_array (void) : m_array (0) { }

    jit_array (T& from) : m_array (new T (from))
    {
      update ();
    }

    void update (void)
    {
      m_ref_count = m_array->jit_ref_count ();
      m_slice_data = m_array->jit_slice_data () - 1;
      m_slice_len = m_array->numel ();
      m_dimensions = m_array->jit_dimensions ();
    }

    void update (T *aarray)
    {
      m_array = aarray;
      update ();
    }

    operator T () const
    {
      return *m_array;
    }

    int *m_ref_count;

    U *m_slice_data;
    octave_idx_type m_slice_len;
    octave_idx_type *m_dimensions;

    T *m_array;
  };

  typedef jit_array<NDArray, double> jit_matrix;

  std::ostream& operator << (std::ostream& os, const jit_matrix& mat);

  // calling convention
  namespace jit_convention
  {
    enum
    type
    {
      // internal to jit
      internal,

      // an external C call
      external,

      length
    };
  }

  // Used to keep track of estimated (infered) types during JIT.  This is a
  // hierarchical type system which includes both concrete and abstract types.
  //
  // The types form a lattice.  Currently we only allow for one parent type, but
  // eventually we may allow for multiple predecessors.
  class
  jit_type
  {
  public:

    typedef llvm::Value *(*convert_fn) (llvm::IRBuilderD&, llvm::Value *);

    jit_type (const std::string& aname, jit_type *aparent, llvm::Type *allvm_type,
              bool askip_paren, int aid);

    // a user readable type name
    const std::string& name (void) const { return m_name; }

    // a unique id for the type
    int type_id (void) const { return m_id; }

    // An abstract base type, may be null
    jit_type * parent (void) const { return m_parent; }

    // convert to an llvm type
    llvm::Type * to_llvm (void) const { return m_llvm_type; }

    // how this type gets passed as a function argument
    llvm::Type * to_llvm_arg (void) const;

    size_t depth (void) const { return m_depth; }

    bool skip_paren (void) const { return m_skip_paren; }

    // -------------------- Calling Convention information --------------------

    // A function declared like: mytype foo (int arg0, int arg1);
    // Will be converted to: void foo (mytype *retval, int arg0, int arg1)
    // if mytype is sret.  The caller is responsible for allocating space for
    // retval. (on the stack)
    bool sret (jit_convention::type cc) const { return m_sret[cc]; }

    void mark_sret (jit_convention::type cc)
    { m_sret[cc] = true; }

    // A function like: void foo (mytype arg0)
    // Will be converted to: void foo (mytype *arg0)
    // Basically just pass by reference.
    bool pointer_arg (jit_convention::type cc) const { return m_pointer_arg[cc]; }

    void mark_pointer_arg (jit_convention::type cc)
    { m_pointer_arg[cc] = true; }

    // Convert into an equivalent form before calling.  For example, complex is
    // represented as two values llvm vector, but we need to pass it as a two
    // valued llvm structure to C functions.
    convert_fn pack (jit_convention::type cc) { return m_pack[cc]; }

    void set_pack (jit_convention::type cc, convert_fn fn) { m_pack[cc] = fn; }

    // The inverse operation of pack.
    convert_fn unpack (jit_convention::type cc) { return m_unpack[cc]; }

    void set_unpack (jit_convention::type cc, convert_fn fn)
    { m_unpack[cc] = fn; }

    // The resulting type after pack is called.
    llvm::Type * packed_type (jit_convention::type cc)
    { return m_packed_type[cc]; }

    void set_packed_type (jit_convention::type cc, llvm::Type *ty)
    { m_packed_type[cc] = ty; }

  private:

    std::string m_name;
    jit_type *m_parent;
    llvm::Type *m_llvm_type;
    int m_id;
    size_t m_depth;
    bool m_skip_paren;

    bool m_sret[jit_convention::length];
    bool m_pointer_arg[jit_convention::length];

    convert_fn m_pack[jit_convention::length];
    convert_fn m_unpack[jit_convention::length];

    llvm::Type *m_packed_type[jit_convention::length];
  };

  // seperate print function to allow easy printing if type is null
  std::ostream& jit_print (std::ostream& os, jit_type *atype);

  // Find common type
  jit_type* jit_type_join (jit_type *lhs, jit_type *rhs);



  class jit_value;

  // An abstraction for calling llvm functions with jit_values.  Deals with
  // calling convention details.
  class
  jit_function
  {
    friend std::ostream& operator << (std::ostream& os, const jit_function& fn);

  public:

    // create a function in an invalid state
    jit_function (void);

    jit_function (const jit_module *amodule, jit_convention::type acall_conv,
                  const llvm::Twine& aname, jit_type *aresult,
                  const std::vector<jit_type *>& aargs);

    // Use an existing function, but change the argument types.  The new argument
    // types must behave the same for the current calling convention.
    jit_function (const jit_function& fn, jit_type *aresult,
                  const std::vector<jit_type *>& aargs);

    jit_function (const jit_function& fn);

    // erase the interal LLVM function (if it exists).  Will become invalid.
    void erase (void);

    bool valid (void) const { return m_llvm_function; }

    std::string name (void) const;

    llvm::BasicBlock * new_block (const std::string& aname = "body",
                                  llvm::BasicBlock *insert_before = nullptr);

    typedef std::vector<llvm::Value *> arg_vec;

    llvm::Value * call (llvm::IRBuilderD& builder,
                        const arg_vec& in_args = arg_vec ()) const;

    llvm::Value * call (llvm::IRBuilderD& builder,
                        const std::vector<jit_value *>& in_args) const;

    template <typename ...Args>
    llvm::Value * call (llvm::IRBuilderD& builder, arg_vec& in_args,
                        llvm::Value * arg1, Args... other_args) const
    {
      in_args.push_back (arg1);
      return call (builder, in_args, other_args...);
    }

    template <typename T, typename ...Args>
    llvm::Value * call (llvm::IRBuilderD& builder, arg_vec& in_args,
                        T * arg1, Args... other_args) const
    {
      in_args.push_back (arg1->to_llvm ());
      return call (builder, in_args, other_args...);
    }

    template <typename ...Args>
    llvm::Value * call (llvm::IRBuilderD& builder, llvm::Value * arg1,
                        Args... other_args) const
    {
      arg_vec in_args;
      in_args.reserve (1 + sizeof... (other_args));
      in_args.push_back (arg1);
      return call (builder, in_args, other_args...);
    }

    template <typename T, typename ...Args>
    llvm::Value * call (llvm::IRBuilderD& builder, T * arg1,
                        Args... other_args) const
    {
      arg_vec in_args;
      in_args.reserve (1 + sizeof... (other_args));
      in_args.push_back (arg1->to_llvm ());
      return call (builder, in_args, other_args...);
    }

    llvm::Value * argument (llvm::IRBuilderD& builder, size_t idx) const;

    void do_return (llvm::IRBuilderD& builder, llvm::Value *rval = nullptr,
                    bool verify = true);

    llvm::Function * to_llvm (void) const { return m_llvm_function; }

    // If true, then the return value is passed as a pointer in the first argument
    bool sret (void) const { return m_result && m_result->sret (m_call_conv); }

    bool can_error (void) const { return m_can_error; }

    void mark_can_error (void) { m_can_error = true; }

    jit_type * result (void) const { return m_result; }

    jit_type * argument_type (size_t idx) const
    {
      assert (idx < m_args.size ());
      return m_args[idx];
    }

    const std::vector<jit_type *>& arguments (void) const { return m_args; }

  private:

    const jit_module *m_module;
    llvm::Function *m_llvm_function;
    jit_type *m_result;
    std::vector<jit_type *> m_args;
    jit_convention::type m_call_conv;
    bool m_can_error;
  };

  std::ostream& operator << (std::ostream& os, const jit_function& fn);

  // Keeps track of information about how to implement operations (+, -, *, ect)
  // and their resulting types.
  class
  jit_operation
  {
  public:

    jit_operation (const std::string& aname)  { m_name = aname; }

    // type signature vector
    typedef std::vector<jit_type *> signature_vec;

    virtual ~jit_operation (void);

    void add_overload (const jit_function& func)
    {
      add_overload (func, func.arguments ());
    }

    void add_overload (const jit_function& func,
                       const signature_vec& args);

    const jit_function& overload (const signature_vec& types) const;

    template <typename ...Args>
    const jit_function& overload (signature_vec& args, jit_type * arg1,
                                  Args... other_args) const
    {
      args.push_back (arg1);
      return overload (args, other_args...);
    }

    template <typename ...Args>
    const jit_function& overload (jit_type * arg1, Args... other_args) const
    {
      signature_vec args;
      args.reserve (1 + sizeof... (other_args));
      args.push_back (arg1);
      return overload (args, other_args...);
    }

    jit_type * result (const signature_vec& types) const
    {
      const jit_function& temp = overload (types);
      return temp.result ();
    }

    template <typename ...Args>
    jit_type * result (signature_vec& args, jit_type * arg1,
                       Args... other_args) const
    {
      args.push_back (arg1);
      return overload (args, other_args...);
    }

    template <typename ...Args>
    jit_type * result (jit_type * arg1, Args... other_args) const
    {
      signature_vec args;
      args.reserve (1 + sizeof... (other_args));
      args.push_back (arg1);
      return overload (args, other_args...);
    }

    const std::string& name (void) const { return m_name; }

    void stash_name (const std::string& aname) { m_name = aname; }

  protected:

    virtual jit_function * generate (const signature_vec& types) const;

  private:

    Array<octave_idx_type> to_idx (const signature_vec& types) const;

    const jit_function& do_generate (const signature_vec& types) const;

    struct signature_cmp
    {
      bool operator () (const signature_vec *lhs, const signature_vec *rhs) const;
    };

    typedef std::map<const signature_vec *, jit_function *, signature_cmp>
    generated_map;

    mutable generated_map m_generated;

    std::vector<Array<jit_function>> m_overloads;

    std::string m_name;
  };


  class
  jit_index_operation : public jit_operation
  {
  public:

    jit_index_operation (const jit_typeinfo& ti, const std::string& name)
      : jit_operation (name), m_typeinfo (ti) { }

  protected:

    virtual jit_function * generate (const signature_vec& types) const;

    virtual jit_function * generate_matrix (const signature_vec& types) const = 0;

    // helper functions
    // [start_idx, end_idx).
    llvm::Value * create_arg_array (llvm::IRBuilderD& builder,
                                    const jit_function& fn, size_t start_idx,
                                    size_t end_idx) const;

    const jit_typeinfo& m_typeinfo;
  };

  class
  jit_paren_subsref : public jit_index_operation
  {
  public:

    // FIXME: Avoid creating object in an invalid state?
    jit_paren_subsref (const jit_typeinfo& ti);
    ~jit_paren_subsref (void);
    void init_paren_scalar (void);

  protected:

    virtual jit_function * generate_matrix (const signature_vec& types) const;

  private:

    jit_function *m_paren_scalar;
  };

  class
  jit_paren_subsasgn : public jit_index_operation
  {
  public:

    // FIXME: Avoid creating object in an invalid state?
    jit_paren_subsasgn (const jit_typeinfo& ti);
    ~jit_paren_subsasgn (void);
    void init_paren_scalar (void);

  protected:

    jit_function * generate_matrix (const signature_vec& types) const;

  private:

    jit_function *m_paren_scalar;
  };


  // A singleton class which handles the construction of jit_types
  class
  jit_typeinfo
  {
    // ----- Constructor/destructor (singleton pattern) -----

  public:

    ~jit_typeinfo (void);

  private:

    static jit_typeinfo& instance (void);
    jit_typeinfo (void);
    static bool s_in_construction;

    // ----- Registering types -----

  public:

    static jit_type *register_new_type (const std::string& name, jit_type *parent,
                                        llvm::Type *llvm_type, bool skip_paren = false)
    {
      return instance ().do_register_new_type (name, parent, llvm_type, skip_paren);
    }

  private:

    // List of all registered types
    std::vector<jit_type*> m_id_to_type;

    // Register a new type
    jit_type *do_register_new_type (const std::string& name, jit_type *parent,
                                    llvm::Type *llvm_type, bool skip_paren = false);

    // ----- Base types -----

  public:

    static jit_type *get_any (void) { return instance ().m_any; }

    static jit_type *get_matrix (void) { return instance ().m_matrix; }

    static jit_type *get_scalar (void) { return instance ().m_scalar; }

    static jit_type *get_scalar_ptr (void) { return instance ().m_scalar_ptr; }

    static jit_type *get_any_ptr (void) { return instance ().m_any_ptr; }

    static jit_type *get_range (void) { return instance ().m_range; }

    static jit_type *get_string (void) { return instance ().m_string; }

    static jit_type *get_bool (void) { return instance ().m_boolean; }

    static jit_type *get_index (void) { return instance ().m_index; }

    static jit_type *get_complex (void) { return instance ().m_complex; }

    static jit_type *intN (size_t nbits) { return instance ().do_get_intN (nbits); }

    // FIXME: do we really need these two ?
    static llvm::Type *get_scalar_llvm (void) { return instance ().m_scalar->to_llvm (); }  // this one is weird

    static llvm::Type *get_index_llvm (void)  { return instance ().m_index->to_llvm (); }  // this one is weird too

  private:

    // Base types as LLVM types

    llvm::Type *m_any_t;
    llvm::Type *m_bool_t;  // FIXME: should be "boolean_t", for consistency
    llvm::Type *m_complex_t;
    llvm::Type *m_index_t;
    llvm::Type *m_scalar_t;
    llvm::Type *m_string_t;

    llvm::StructType *m_range_t;
    llvm::StructType *m_matrix_t;

    // Base types as jit_type objects)

    jit_type *m_any;
    jit_type *m_boolean;
    jit_type *m_complex;
    jit_type *m_index;
    jit_type *m_scalar;
    jit_type *m_string;

    jit_type *m_range;
    jit_type *m_matrix;

    jit_type *m_scalar_ptr;  // a fake type for interfacing with C++
    jit_type *m_any_ptr;     // a fake type for interfacing with C++ (bis)
    jit_type *m_unknown_function;

    // complex_ret is what is passed to C functions
    // in order to get calling convention right
    llvm::StructType *m_complex_ret;

    // Get integer type from number of bits
    jit_type *do_get_intN (size_t nbits) const;

    // map container for integer types: int8, int16, etc.
    // (note that they are also stored in id_to_types)
    std::map<size_t, jit_type *> m_ints;

    // ----- parenthesis subsref/subsasgn -----

    friend jit_paren_subsref;
    friend jit_paren_subsasgn;

  public:

    static const jit_operation& paren_subsref (void)   { return instance ().paren_subsref_fn; }
    static const jit_operation& paren_subsasgn (void)  { return instance ().paren_subsasgn_fn; }

  private:

    jit_paren_subsref paren_subsref_fn;
    jit_paren_subsasgn paren_subsasgn_fn;

    // ----- Miscellaneous (FIXME: needs to be organized) -----

  public:

    // Get the jit_type of an octave_value
    static jit_type *type_of (const octave_value &ov)
    {
      return instance ().do_type_of (ov);
    };

    // Get a unary or binary operation from its integer id
    static const jit_operation& binary_op (int op)
    {
      return instance ().do_binary_op (op);
    }

    static const jit_operation& unary_op (int op)
    {
      return instance ().do_unary_op (op);
    }

    static const jit_operation& grab (void)
    {
      return instance ().m_grab_fn;
    }

    static const jit_function& get_grab (jit_type *type)
    {
      return instance ().m_grab_fn.overload (type);
    }

    static const jit_operation& release (void)
    {
      return instance ().m_release_fn;
    }

    static const jit_function& get_release (jit_type *type)
    {
      return instance ().m_release_fn.overload (type);
    }

    static const jit_operation& destroy (void)
    {
      return instance ().m_destroy_fn;
    }

    static const jit_operation& print_value (void)
    {
      return instance ().m_print_fn;
    }

    static const jit_operation& for_init (void)
    {
      return instance ().m_for_init_fn;
    }

    static const jit_operation& for_check (void)
    {
      return instance ().m_for_check_fn;
    }

    static const jit_operation& for_index (void)
    {
      return instance ().m_for_index_fn;
    }

    static const jit_operation& make_range (void)
    {
      return instance ().m_make_range_fn;
    }

    static const jit_operation& logically_true (void)
    {
      return instance ().m_logically_true_fn;
    }

    static const jit_operation& cast (jit_type *result)
    {
      return instance ().do_cast (result);
    }

    static const jit_function& cast (jit_type *to, jit_type *from)
    {
      return instance ().do_cast (to, from);
    }

    static llvm::Value *insert_error_check (llvm::IRBuilderD& bld)
    {
      return instance ().do_insert_error_check (bld);
    }

    static llvm::Value *insert_interrupt_check (llvm::IRBuilderD& bld)
    {
      return instance ().do_insert_interrupt_check (bld);
    }

    static const jit_operation& end (void)
    {
      return instance ().m_end_fn;
    }

    static const jit_function& end (jit_value *value, jit_value *idx,
                                    jit_value *count)
    {
      return instance ().do_end (value, idx, count);
    }

    static const jit_operation& create_undef (void)
    {
      return instance ().m_create_undef_fn;
    }

    static llvm::Value *create_complex (llvm::Value *real, llvm::Value *imag)
    {
      return instance ().complex_new (real, imag);
    }

    static llvm::Value *pack_complex (llvm::IRBuilderD& bld, llvm::Value *cplx)
    {
      return instance ().do_pack_complex (bld, cplx);
    }

    static llvm::Value *unpack_complex (llvm::IRBuilderD& bld,
                                        llvm::Value *result);

  private:

    jit_type * do_type_of (const octave_value& ov) const;

    const jit_operation& do_binary_op (int op) const
    {
      assert (static_cast<size_t> (op) < m_binary_ops.size ());
      return m_binary_ops[op];
    }

    const jit_operation& do_unary_op (int op) const
    {
      assert (static_cast<size_t> (op) < m_unary_ops.size ());
      return m_unary_ops[op];
    }

    const jit_operation& do_cast (jit_type *to)
    {
      static jit_operation null_function ("null_function");
      if (! to)
        return null_function;

      size_t id = to->type_id ();
      if (id >= m_casts.size ())
        return null_function;
      return m_casts[id];
    }

    const jit_function& do_cast (jit_type *to, jit_type *from)
    {
      return do_cast (to).overload (from);
    }

    const jit_function& do_end (jit_value *value, jit_value *index,
                                jit_value *count);

    void add_print (jit_type *ty, void *fptr);

    void add_binary_op (jit_type *ty, int op, int llvm_op);

    void add_binary_icmp (jit_type *ty, int op, int llvm_op);

    void add_binary_fcmp (jit_type *ty, int op, int llvm_op);

    // type signature vector
    typedef std::vector<jit_type *> signature_vec;

    // create a function with an external calling convention
    // forces the function pointer to be specified
    template <typename T>
    jit_function create_external (T fn, const llvm::Twine& name,
                                  jit_type * ret, const signature_vec& args
                                  = signature_vec ()) const;

    template <typename T, typename ...Args>
    jit_function create_external (T fn, const llvm::Twine& name,
                                  jit_type * ret, signature_vec& args,
                                  jit_type * arg1, Args... other_args) const
    {
      args.push_back (arg1);
      return create_external (fn, name, ret, args, other_args...);
    }

    template <typename T, typename ...Args>
    jit_function create_external (T fn, const llvm::Twine& name, jit_type *ret,
                                  jit_type * arg1, Args... other_args) const
    {
      signature_vec args;
      args.reserve (1 + sizeof... (other_args));
      args.push_back (arg1);
      return create_external (fn, name, ret, args, other_args...);
    }

    // create an internal calling convention (a function defined in llvm)
    jit_function create_internal (const llvm::Twine& name, jit_type *ret,
                                  const signature_vec& args
                                  = signature_vec ()) const
    {
      return jit_function (m_base_jit_module, jit_convention::internal,
                           name, ret, args);
    }

    template <typename ...Args>
    jit_function create_internal (const llvm::Twine& name, jit_type *ret,
                                  signature_vec& args,
                                  jit_type * arg1, Args... other_args) const
    {
      args.push_back (arg1);
      return create_internal (name, ret, args, other_args...);
    }

    template <typename ...Args>
    jit_function create_internal (const llvm::Twine& name, jit_type *ret,
                                  jit_type * arg1, Args... other_args) const
    {
      signature_vec args;
      args.reserve (1 + sizeof... (other_args));
      args.push_back (arg1);
      return create_internal (name, ret, args, other_args...);
    }

    jit_function create_identity (jit_type *type);

    llvm::Value * do_insert_error_check (llvm::IRBuilderD& bld);

    llvm::Value * do_insert_interrupt_check (llvm::IRBuilderD& bld);

    void add_builtin (const std::string& name);

    void register_intrinsic (const std::string& name, size_t id,
                             jit_type *result, jit_type *arg0)
    {
      std::vector<jit_type *> args (1, arg0);
      register_intrinsic (name, id, result, args);
    }

    void register_intrinsic (const std::string& name, size_t id, jit_type *result,
                             const std::vector<jit_type *>& args);

    void register_generic (const std::string& name, jit_type *result,
                           jit_type *arg0)
    {
      std::vector<jit_type *> args (1, arg0);
      register_generic (name, result, args);
    }

    void register_generic (const std::string& name, jit_type *result,
                           const std::vector<jit_type *>& args);

    octave_builtin * find_builtin (const std::string& name);

    jit_function mirror_binary (const jit_function& fn);

    llvm::Function * wrap_complex (llvm::Function *wrap);

    llvm::Value * complex_real (llvm::Value *cx);

    llvm::Value * complex_real (llvm::Value *cx, llvm::Value *real);

    llvm::Value * complex_imag (llvm::Value *cx);

    llvm::Value * complex_imag (llvm::Value *cx, llvm::Value *imag);

    llvm::Value * complex_new (llvm::Value *real, llvm::Value *imag);

    llvm::Value *do_pack_complex (llvm::IRBuilderD& bld, llvm::Value *cplx) const;

    int m_next_id;

    llvm::GlobalVariable *m_lerror_state;
    llvm::GlobalVariable *m_loctave_interrupt_state;

    llvm::Type *m_sig_atomic_type;

    std::map<std::string, jit_type *> m_builtins;

    std::vector<jit_operation> m_binary_ops;
    std::vector<jit_operation> m_unary_ops;
    jit_operation m_grab_fn;
    jit_operation m_release_fn;
    jit_operation m_destroy_fn;
    jit_operation m_print_fn;
    jit_operation m_for_init_fn;
    jit_operation m_for_check_fn;
    jit_operation m_for_index_fn;
    jit_operation m_logically_true_fn;
    jit_operation m_make_range_fn;
    jit_operation m_end1_fn;
    jit_operation m_end_fn;
    jit_operation m_create_undef_fn;

    jit_function m_any_call;

    // type id -> cast function TO that type
    std::vector<jit_operation> m_casts;

    // type id -> identity function
    std::vector<jit_function> m_identities;

    jit_module *m_base_jit_module;

    llvm::IRBuilderD *m_builder_ptr;
    llvm::IRBuilderD& m_builder;
  };
}

#endif
#endif
