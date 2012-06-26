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

#ifdef HAVE_LLVM

#include <list>
#include <map>
#include <set>
#include <stdexcept>
#include <vector>
#include <stack>

#include "Array.h"
#include "Range.h"
#include "pt-walk.h"
#include "symtab.h"

// -------------------- Current status --------------------
// Simple binary operations (+-*/) on octave_scalar's (doubles) are optimized.
// a = 5;
// b = a * 5 + a;
//
// Indexing matrices with scalars works.
//
// if, elseif, else, break, continue, and for compile. Compilation is triggered
// at the start of a simple for loop.
//
// The octave low level IR is a linear IR, it works by converting everything to
// calls to jit_functions. This turns expressions like c = a + b into
// c = call binary+ (a, b)
// The jit_functions contain information about overloads for different types.
// For, example, if we know a and b are scalars, then c must also be a scalar.
//
// Support for function calls is in progress. Currently, calls to sin with a
// scalar argument will compile.
//
// TODO:
// 1. Function calls (In progress)
// 2. Cleanup/documentation
// 3. ...
// ---------------------------------------------------------


// we don't want to include llvm headers here, as they require
// __STDC_LIMIT_MACROS and __STDC_CONSTANT_MACROS be defined in the entire
// compilation unit
namespace llvm
{
  class Value;
  class Module;
  class FunctionPassManager;
  class PassManager;
  class ExecutionEngine;
  class Function;
  class BasicBlock;
  class LLVMContext;
  class Type;
  class Twine;
  class GlobalVariable;
  class TerminatorInst;
  class PHINode;
}

class octave_base_value;
class octave_value;
class tree;
class tree_expression;

template <typename HOLDER_T, typename SUB_T>
class jit_internal_node;

// jit_internal_list and jit_internal_node implement generic embedded doubly
// linked lists. List items extend from jit_internal_list, and can be placed
// in nodes of type jit_internal_node. We use CRTP twice.
template <typename LIST_T, typename NODE_T>
class
jit_internal_list
{
  friend class jit_internal_node<LIST_T, NODE_T>;
public:
  jit_internal_list (void) : use_head (0), use_tail (0), muse_count (0) {}

  virtual ~jit_internal_list (void)
  {
    while (use_head)
      use_head->stash_value (0);
  }

  NODE_T *first_use (void) const { return use_head; }

  size_t use_count (void) const { return muse_count; }
private:
  NODE_T *use_head;
  NODE_T *use_tail;
  size_t muse_count;
};

// a node for internal linked lists
template <typename LIST_T, typename NODE_T>
class
jit_internal_node
{
public:
  typedef jit_internal_list<LIST_T, NODE_T> jit_ilist;

  jit_internal_node (void) : mvalue (0), mnext (0), mprev (0) {}

  ~jit_internal_node (void) { remove (); }

  LIST_T *value (void) const { return mvalue; }

  void stash_value (LIST_T *avalue)
  {
    remove ();

    mvalue = avalue;

    if (mvalue)
      {
        jit_ilist *ilist = mvalue;
        NODE_T *sthis = static_cast<NODE_T *> (this);
        if (ilist->use_head)
          {
            ilist->use_tail->mnext = sthis;
            mprev = ilist->use_tail;
          }
        else
          ilist->use_head = sthis;

        ilist->use_tail = sthis;
        ++ilist->muse_count;
      }
  }

  NODE_T *next (void) const { return mnext; }

  NODE_T *prev (void) const { return mprev; }
private:
  void remove ()
  {
    if (mvalue)
      {
        jit_ilist *ilist = mvalue;
        if (mprev)
          mprev->mnext = mnext;
        else
          // we are the use_head
          ilist->use_head = mnext;

        if (mnext)
          mnext->mprev = mprev;
        else
          // we are the use tail
          ilist->use_tail = mprev;

        mnext = mprev = 0;
        --ilist->muse_count;
        mvalue = 0;
      }
  }

  LIST_T *mvalue;
  NODE_T *mnext;
  NODE_T *mprev;
};

// Use like: isa<jit_phi> (value)
// basically just a short cut type typing dyanmic_cast.
template <typename T, typename U>
bool isa (U *value)
{
  return dynamic_cast<T *> (value);
}

// jit_range is compatable with the llvm range structure
struct
jit_range
{
  jit_range (const Range& from) : base (from.base ()), limit (from.limit ()),
                                  inc (from.inc ()), nelem (from.nelem ())
  {}

  operator Range () const
  {
    return Range (base, limit, inc);
  }

  double base;
  double limit;
  double inc;
  octave_idx_type nelem;
};

std::ostream& operator<< (std::ostream& os, const jit_range& rng);

// jit_array is compatable with the llvm array/matrix structures
template <typename T, typename U>
struct
jit_array
{
  jit_array (T& from) : ref_count (from.jit_ref_count ()),
                        slice_data (from.jit_slice_data () - 1),
                        slice_len (from.capacity ()),
                        dimensions (from.jit_dimensions ()),
                        array (new T (from))
  {
    grab_dimensions ();
  }

  void grab_dimensions (void)
  {
    ++(dimensions[-2]);
  }

  operator T () const
  {
    return *array;
  }

  int *ref_count;

  U *slice_data;
  octave_idx_type slice_len;
  octave_idx_type *dimensions;

  T *array;
};

typedef jit_array<NDArray, double> jit_matrix;

std::ostream& operator<< (std::ostream& os, const jit_matrix& mat);

// Used to keep track of estimated (infered) types during JIT. This is a
// hierarchical type system which includes both concrete and abstract types.
//
// Current, we only support any and scalar types. If we can't figure out what
// type a variable is, we assign it the any type. This allows us to generate
// code even for the case of poor type inference.
class
jit_type
{
public:
  jit_type (const std::string& aname, jit_type *aparent, llvm::Type *allvm_type,
            int aid) :
    mname (aname), mparent (aparent), llvm_type (allvm_type), mid (aid),
    mdepth (aparent ? aparent->mdepth + 1 : 0)
  {}

  // a user readable type name
  const std::string& name (void) const { return mname; }

  // a unique id for the type
  int type_id (void) const { return mid; }

  // An abstract base type, may be null
  jit_type *parent (void) const { return mparent; }

  // convert to an llvm type
  llvm::Type *to_llvm (void) const { return llvm_type; }

  // how this type gets passed as a function argument
  llvm::Type *to_llvm_arg (void) const;

  size_t depth (void) const { return mdepth; }
private:
  std::string mname;
  jit_type *mparent;
  llvm::Type *llvm_type;
  int mid;
  size_t mdepth;
};

// seperate print function to allow easy printing if type is null
std::ostream& jit_print (std::ostream& os, jit_type *atype);

// Keeps track of overloads for a builtin function. Used for both type inference
// and code generation.
class
jit_function
{
public:
  struct
  overload
  {
    overload (void) : function (0), can_error (false), result (0) {}

    overload (llvm::Function *f, bool e, jit_type *r, jit_type *arg0) :
      function (f), can_error (e), result (r), arguments (1)
    {
      arguments[0] = arg0;
    }

    overload (llvm::Function *f, bool e, jit_type *r, jit_type *arg0,
              jit_type *arg1) : function (f), can_error (e),
                                result (r), arguments (2)
    {
      arguments[0] = arg0;
      arguments[1] = arg1;
    }

    overload (llvm::Function *f, bool e, jit_type *r, jit_type *arg0,
              jit_type *arg1, jit_type *arg2) : function (f), can_error (e),
                                                result (r), arguments (3)
    {
      arguments[0] = arg0;
      arguments[1] = arg1;
      arguments[2] = arg2;
    }

    llvm::Function *function;
    bool can_error;
    jit_type *result;
    std::vector<jit_type*> arguments;
  };

  void add_overload (const overload& func)
  {
    add_overload (func, func.arguments);
  }

  void add_overload (llvm::Function *f, bool e, jit_type *r, jit_type *arg0)
  {
    overload ol (f, e, r, arg0);
    add_overload (ol);
  }

  void add_overload (llvm::Function *f, bool e, jit_type *r, jit_type *arg0,
                     jit_type *arg1)
  {
    overload ol (f, e, r, arg0, arg1);
    add_overload (ol);
  }

  void add_overload (llvm::Function *f, bool e, jit_type *r, jit_type *arg0,
                     jit_type *arg1, jit_type *arg2)
  {
    overload ol (f, e, r, arg0, arg1, arg2);
    add_overload (ol);
  }

  void add_overload (const overload& func,
                     const std::vector<jit_type*>& args);

  const overload& get_overload (const std::vector<jit_type *>& types) const;

  const overload& get_overload (jit_type *arg0) const
  {
    std::vector<jit_type *> types (1);
    types[0] = arg0;
    return get_overload (types);
  }

  const overload& get_overload (jit_type *arg0, jit_type *arg1) const
  {
    std::vector<jit_type *> types (2);
    types[0] = arg0;
    types[1] = arg1;
    return get_overload (types);
  }

  jit_type *get_result (const std::vector<jit_type *>& types) const
  {
    const overload& temp = get_overload (types);
    return temp.result;
  }

  jit_type *get_result (jit_type *arg0, jit_type *arg1) const
  {
    const overload& temp = get_overload (arg0, arg1);
    return temp.result;
  }

  const std::string& name (void) const { return mname; }

  void stash_name (const std::string& aname) { mname = aname; }
private:
  Array<octave_idx_type> to_idx (const std::vector<jit_type*>& types) const;

  std::vector<Array<overload> > overloads;

  std::string mname;
};

// Get information and manipulate jit types.
class
jit_typeinfo
{
public:
  static void initialize (llvm::Module *m, llvm::ExecutionEngine *e);

  static jit_type *join (jit_type *lhs, jit_type *rhs)
  {
    return instance->do_join (lhs, rhs);
  }

  static jit_type *get_any (void) { return instance->any; }

  static jit_type *get_matrix (void) { return instance->matrix; }

  static jit_type *get_scalar (void) { return instance->scalar; }

  static llvm::Type *get_scalar_llvm (void)
  { return instance->scalar->to_llvm (); }

  static jit_type *get_range (void) { return instance->range; }

  static jit_type *get_string (void) { return instance->string; }

  static jit_type *get_bool (void) { return instance->boolean; }

  static jit_type *get_index (void) { return instance->index; }

  static llvm::Type *get_index_llvm (void)
  { return instance->index->to_llvm (); }

  static jit_type *type_of (const octave_value& ov)
  {
    return instance->do_type_of (ov);
  }

  static const jit_function& binary_op (int op)
  {
    return instance->do_binary_op (op);
  }

  static const jit_function& grab (void) { return instance->grab_fn; }

  static const jit_function::overload& get_grab (jit_type *type)
  {
    return instance->grab_fn.get_overload (type);
  }

  static const jit_function& release (void)
  {
    return instance->release_fn;
  }

  static const jit_function::overload& get_release (jit_type *type)
  {
    return instance->release_fn.get_overload (type);
  }

  static const jit_function& print_value (void)
  {
    return instance->print_fn;
  }

  static const jit_function& for_init (void)
  {
    return instance->for_init_fn;
  }

  static const jit_function& for_check (void)
  {
    return instance->for_check_fn;
  }

  static const jit_function& for_index (void)
  {
    return instance->for_index_fn;
  }

  static const jit_function& make_range (void)
  {
    return instance->make_range_fn;
  }

  static const jit_function& paren_subsref (void)
  {
    return instance->paren_subsref_fn;
  }

  static const jit_function& paren_subsasgn (void)
  {
    return instance->paren_subsasgn_fn;
  }

  static const jit_function& logically_true (void)
  {
    return instance->logically_true_fn;
  }

  static const jit_function& cast (jit_type *result)
  {
    return instance->do_cast (result);
  }

  static const jit_function::overload& cast (jit_type *to, jit_type *from)
  {
    return instance->do_cast (to, from);
  }

  static llvm::Value *insert_error_check (void)
  {
    return instance->do_insert_error_check ();
  }
private:
  jit_typeinfo (llvm::Module *m, llvm::ExecutionEngine *e);

  // FIXME: Do these methods really need to be in jit_typeinfo?
  jit_type *do_join (jit_type *lhs, jit_type *rhs)
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

  jit_type *do_difference (jit_type *lhs, jit_type *)
  {
    // FIXME: Maybe we can do something smarter?
    return lhs;
  }

  jit_type *do_type_of (const octave_value &ov) const;

  const jit_function& do_binary_op (int op) const
  {
    assert (static_cast<size_t>(op) < binary_ops.size ());
    return binary_ops[op];
  }

  const jit_function& do_cast (jit_type *to)
  {
    static jit_function null_function;
    if (! to)
      return null_function;

    size_t id = to->type_id ();
    if (id >= casts.size ())
      return null_function;
    return casts[id];
  }

  const jit_function::overload& do_cast (jit_type *to, jit_type *from)
  {
    return do_cast (to).get_overload (from);
  }

  jit_type *new_type (const std::string& name, jit_type *parent,
                      llvm::Type *llvm_type);


  void add_print (jit_type *ty, void *call);

  void add_binary_op (jit_type *ty, int op, int llvm_op);

  void add_binary_icmp (jit_type *ty, int op, int llvm_op);

  void add_binary_fcmp (jit_type *ty, int op, int llvm_op);

  llvm::Function *create_function (const llvm::Twine& name, llvm::Type *ret)
  {
    std::vector<llvm::Type *> args;
    return create_function (name, ret, args);
  }

  llvm::Function *create_function (const llvm::Twine& name, llvm::Type *ret,
                                   llvm::Type *arg0)
  {
    std::vector<llvm::Type *> args (1, arg0);
    return create_function (name, ret, args);
  }

  llvm::Function *create_function (const llvm::Twine& name, jit_type *ret,
                                   jit_type *arg0)
  {
    return create_function (name, ret->to_llvm (), arg0->to_llvm ());
  }

  llvm::Function *create_function (const llvm::Twine& name, llvm::Type *ret,
                                   llvm::Type *arg0, llvm::Type *arg1)
  {
    std::vector<llvm::Type *> args (2);
    args[0] = arg0;
    args[1] = arg1;
    return create_function (name, ret, args);
  }

  llvm::Function *create_function (const llvm::Twine& name, jit_type *ret,
                                   jit_type *arg0, jit_type *arg1)
  {
    return create_function (name, ret->to_llvm (), arg0->to_llvm (),
                            arg1->to_llvm ());
  }

  llvm::Function *create_function (const llvm::Twine& name, llvm::Type *ret,
                                   llvm::Type *arg0, llvm::Type *arg1,
                                   llvm::Type *arg2)
  {
    std::vector<llvm::Type *> args (3);
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;
    return create_function (name, ret, args);
  }

  llvm::Function *create_function (const llvm::Twine& name, jit_type *ret,
                                   jit_type *arg0, jit_type *arg1,
                                   jit_type *arg2)
  {
    return create_function (name, ret->to_llvm (), arg0->to_llvm (),
                            arg1->to_llvm (), arg2->to_llvm ());
  }

  llvm::Function *create_function (const llvm::Twine& name, llvm::Type *ret,
                                   llvm::Type *arg0, llvm::Type *arg1,
                                   llvm::Type *arg2, llvm::Type *arg3)
  {
    std::vector<llvm::Type *> args (4);
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;
    args[3] = arg3;
    return create_function (name, ret, args);
  }

  llvm::Function *create_function (const llvm::Twine& name, llvm::Type *ret,
                                   const std::vector<llvm::Type *>& args);

  llvm::Function *create_identity (jit_type *type);

  llvm::Value *do_insert_error_check (void);

  static jit_typeinfo *instance;

  llvm::Module *module;
  llvm::ExecutionEngine *engine;
  int next_id;

  llvm::GlobalVariable *lerror_state;

  std::vector<jit_type*> id_to_type;
  jit_type *any;
  jit_type *matrix;
  jit_type *scalar;
  jit_type *range;
  jit_type *string;
  jit_type *boolean;
  jit_type *index;
  jit_type *sin_type;

  std::vector<jit_function> binary_ops;
  jit_function grab_fn;
  jit_function release_fn;
  jit_function print_fn;
  jit_function for_init_fn;
  jit_function for_check_fn;
  jit_function for_index_fn;
  jit_function logically_true_fn;
  jit_function make_range_fn;
  jit_function paren_subsref_fn;
  jit_function paren_subsasgn_fn;

  // type id -> cast function TO that type
  std::vector<jit_function> casts;

  // type id -> identity function
  std::vector<llvm::Function *> identities;
};

// The low level octave jit ir
// this ir is close to llvm, but contains information for doing type inference.
// We convert the octave parse tree to this IR directly.

#define JIT_VISIT_IR_NOTEMPLATE                 \
  JIT_METH(block);                              \
  JIT_METH(branch);                             \
  JIT_METH(cond_branch);                        \
  JIT_METH(call);                               \
  JIT_METH(extract_argument);                   \
  JIT_METH(store_argument);                     \
  JIT_METH(phi);                                \
  JIT_METH(variable);                           \
  JIT_METH(error_check);                        \
  JIT_METH(assign)                              \
  JIT_METH(argument)

#define JIT_VISIT_IR_CONST                      \
  JIT_METH(const_scalar);                       \
  JIT_METH(const_index);                        \
  JIT_METH(const_string);                       \
  JIT_METH(const_range)

#define JIT_VISIT_IR_CLASSES                    \
  JIT_VISIT_IR_NOTEMPLATE                       \
  JIT_VISIT_IR_CONST

// forward declare all ir classes
#define JIT_METH(cname)                         \
  class jit_ ## cname;

JIT_VISIT_IR_NOTEMPLATE

#undef JIT_METH

class jit_convert;

// ABCs which aren't included in  JIT_VISIT_IR_ALL
class jit_instruction;
class jit_terminator;

template <typename T, jit_type *(*EXTRACT_T)(void), typename PASS_T = T,
          bool QUOTE=false>
class jit_const;

typedef jit_const<double, jit_typeinfo::get_scalar> jit_const_scalar;
typedef jit_const<octave_idx_type, jit_typeinfo::get_index> jit_const_index;

typedef jit_const<std::string, jit_typeinfo::get_string, const std::string&,
                  true> jit_const_string;
typedef jit_const<jit_range, jit_typeinfo::get_range, const jit_range&>
jit_const_range;

class jit_ir_walker;
class jit_use;

class
jit_value : public jit_internal_list<jit_value, jit_use>
{
public:
  jit_value (void) : llvm_value (0), ty (0), mlast_use (0),
                     min_worklist (false) {}

  virtual ~jit_value (void);

  bool in_worklist (void) const
  {
    return min_worklist;
  }

  void stash_in_worklist (bool ain_worklist)
  {
    min_worklist = ain_worklist;
  }

  // The block of the first use which is not a jit_error_check
  // So this is not necessarily first_use ()->parent ().
  jit_block *first_use_block (void);

  // replace all uses with
  virtual void replace_with (jit_value *value);

  jit_type *type (void) const { return ty; }

  llvm::Type *type_llvm (void) const
  {
    return ty ? ty->to_llvm () : 0;
  }

  const std::string& type_name (void) const
  {
    return ty->name ();
  }

  void stash_type (jit_type *new_ty) { ty = new_ty; }

  std::string print_string (void)
  {
    std::stringstream ss;
    print (ss);
    return ss.str ();
  }

  jit_instruction *last_use (void) const { return mlast_use; }

  void stash_last_use (jit_instruction *alast_use)
  {
    mlast_use = alast_use;
  }

  virtual bool needs_release (void) const { return false; }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const = 0;

  virtual std::ostream& short_print (std::ostream& os) const
  { return print (os); }

  virtual void accept (jit_ir_walker& walker) = 0;

  bool has_llvm (void) const
  {
    return llvm_value;
  }

  llvm::Value *to_llvm (void) const
  {
    assert (llvm_value);
    return llvm_value;
  }

  void stash_llvm (llvm::Value *compiled)
  {
    llvm_value = compiled;
  }

protected:
  std::ostream& print_indent (std::ostream& os, size_t indent = 0) const
  {
    for (size_t i = 0; i < indent * 8; ++i)
      os << " ";
    return os;
  }

  llvm::Value *llvm_value;
private:
  jit_type *ty;
  jit_instruction *mlast_use;
  bool min_worklist;
};

std::ostream& operator<< (std::ostream& os, const jit_value& value);
std::ostream& jit_print (std::ostream& os, jit_value *avalue);

class
jit_use : public jit_internal_node<jit_value, jit_use>
{
public:
  jit_use (void) : muser (0), mindex (0) {}

  // we should really have a move operator, but not until c++11 :(
  jit_use (const jit_use& use) : muser (0), mindex (0)
  {
    *this = use;
  }

  jit_use& operator= (const jit_use& use)
  {
    stash_value (use.value (), use.user (), use.index ());
    return *this;
  }

  size_t index (void) const { return mindex; }

  jit_instruction *user (void) const { return muser; }

  jit_block *user_parent (void) const;

  std::list<jit_block *> user_parent_location (void) const;

  void stash_value (jit_value *avalue, jit_instruction *auser = 0,
                    size_t aindex = -1)
  {
    jit_internal_node::stash_value (avalue);
    mindex = aindex;
    muser = auser;
  }
private:
  jit_instruction *muser;
  size_t mindex;
};

class
jit_instruction : public jit_value
{
public:
  // FIXME: this code could be so much pretier with varadic templates...
  jit_instruction (void) : mid (next_id ()), mparent (0)
  {}

  jit_instruction (size_t nargs)
    : already_infered (nargs, reinterpret_cast<jit_type *>(0)),
      mid (next_id ()), mparent (0)
  {
    marguments.reserve (nargs);
  }

  jit_instruction (jit_value *arg0)
    : already_infered (1, reinterpret_cast<jit_type *>(0)), marguments (1),
      mid (next_id ()), mparent (0)
  {
    stash_argument (0, arg0);
  }

  jit_instruction (jit_value *arg0, jit_value *arg1)
    : already_infered (2, reinterpret_cast<jit_type *>(0)), marguments (2),
      mid (next_id ()), mparent (0)
  {
    stash_argument (0, arg0);
    stash_argument (1, arg1);
  }

  jit_instruction (jit_value *arg0, jit_value *arg1, jit_value *arg2)
    : already_infered (3, reinterpret_cast<jit_type *>(0)), marguments (3),
      mid (next_id ()), mparent (0)
  {
    stash_argument (0, arg0);
    stash_argument (1, arg1);
    stash_argument (2, arg2);
  }

  jit_instruction (jit_value *arg0, jit_value *arg1, jit_value *arg2,
                   jit_value *arg3)
    : already_infered (3, reinterpret_cast<jit_type *>(0)), marguments (4),
      mid (next_id ()), mparent (0)
  {
    stash_argument (0, arg0);
    stash_argument (1, arg1);
    stash_argument (2, arg2);
    stash_argument (3, arg3);
  }

  static void reset_ids (void)
  {
    next_id (true);
  }

  jit_value *argument (size_t i) const
  {
    return marguments[i].value ();
  }

  llvm::Value *argument_llvm (size_t i) const
  {
    assert (argument (i));
    return argument (i)->to_llvm ();
  }

  jit_type *argument_type (size_t i) const
  {
    return argument (i)->type ();
  }

  llvm::Type *argument_type_llvm (size_t i) const
  {
    assert (argument (i));
    return argument_type (i)->to_llvm ();
  }

  std::ostream& print_argument (std::ostream& os, size_t i) const
  {
    if (argument (i))
      return argument (i)->short_print (os);
    else
      return os << "NULL";
  }

  void stash_argument (size_t i, jit_value *arg)
  {
    marguments[i].stash_value (arg, this, i);
  }

  void push_argument (jit_value *arg)
  {
    marguments.push_back (jit_use ());
    stash_argument (marguments.size () - 1, arg);
    already_infered.push_back (0);
  }

  size_t argument_count (void) const
  {
    return marguments.size ();
  }

  void resize_arguments (size_t acount, jit_value *adefault = 0)
  {
    size_t old = marguments.size ();
    marguments.resize (acount);
    already_infered.resize (acount);

    if (adefault)
      for (size_t i = old; i < acount; ++i)
        stash_argument (i, adefault);
  }

  const std::vector<jit_use>& arguments (void) const { return marguments; }

  // argument types which have been infered already
  const std::vector<jit_type *>& argument_types (void) const
  { return already_infered; }

  virtual void push_variable (void) {}

  virtual void pop_variable (void) {}

  virtual void construct_ssa (void)
  {
    do_construct_ssa (0, argument_count ());
  }

  virtual bool infer (void) { return false; }

  void remove (void);

  virtual std::ostream& short_print (std::ostream& os) const;

  jit_block *parent (void) const { return mparent; }

  std::list<jit_instruction *>::iterator location (void) const
  {
    return mlocation;
  }

  llvm::BasicBlock *parent_llvm (void) const;

  void stash_parent (jit_block *aparent,
                     std::list<jit_instruction *>::iterator alocation)
  {
    mparent = aparent;
    mlocation = alocation;
  }

  size_t id (void) const { return mid; }
protected:

  // Do SSA replacement on arguments in [start, end)
  void do_construct_ssa (size_t start, size_t end);

  std::vector<jit_type *> already_infered;
private:
  static size_t next_id (bool reset = false)
  {
    static size_t ret = 0;
    if (reset)
      return ret = 0;

    return ret++;
  }

  std::vector<jit_use> marguments;

  size_t mid;
  jit_block *mparent;
  std::list<jit_instruction *>::iterator mlocation;
};

// defnie accept methods for subclasses
#define JIT_VALUE_ACCEPT                        \
  virtual void accept (jit_ir_walker& walker);

// for use as a dummy argument during conversion to LLVM
class
jit_argument : public jit_value
{
public:
  jit_argument (jit_type *atype, llvm::Value *avalue)
  {
    stash_type (atype);
    stash_llvm (avalue);
  }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent);
    return jit_print (os, type ()) << ": DUMMY";
  }

  JIT_VALUE_ACCEPT;
};

template <typename T, jit_type *(*EXTRACT_T)(void), typename PASS_T,
          bool QUOTE>
class
jit_const : public jit_value
{
public:
  typedef PASS_T pass_t;

  jit_const (PASS_T avalue) : mvalue (avalue)
  {
    stash_type (EXTRACT_T ());
  }

  PASS_T value (void) const { return mvalue; }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent);
    jit_print (os, type ()) << ": ";
    if (QUOTE)
      os << "\"";
    os << mvalue;
    if (QUOTE)
      os << "\"";
    return os;
  }

  JIT_VALUE_ACCEPT;
private:
  T mvalue;
};

class jit_phi_incomming;

class
jit_block : public jit_value, public jit_internal_list<jit_block,
                                                       jit_phi_incomming>
{
  typedef jit_internal_list<jit_block, jit_phi_incomming> ILIST_T;
public:
  typedef std::list<jit_instruction *> instruction_list;
  typedef instruction_list::iterator iterator;
  typedef instruction_list::const_iterator const_iterator;

  typedef std::set<jit_block *> df_set;
  typedef df_set::const_iterator df_iterator;

  static const size_t NO_ID = static_cast<size_t> (-1);

  jit_block (const std::string& aname, size_t avisit_count = 0)
    : mvisit_count (avisit_count), mid (NO_ID), idom (0), mname (aname),
      malive (false)
  {}

  virtual void replace_with (jit_value *value);

  void replace_in_phi (jit_block *ablock, jit_block *with);

  // we have a new internal list, but we want to stay compatable with jit_value
  jit_use *first_use (void) const { return jit_value::first_use (); }

  size_t use_count (void) const { return jit_value::use_count (); }

  // if a block is alive, then it might be visited during execution
  bool alive (void) const { return malive; }

  void mark_alive (void) { malive = true; }

  // If we can merge with a successor, do so and return the now empty block
  jit_block *maybe_merge ();

  // merge another block into this block, leaving the merge block empty
  void merge (jit_block& merge);

  const std::string& name (void) const { return mname; }

  jit_instruction *prepend (jit_instruction *instr);

  jit_instruction *prepend_after_phi (jit_instruction *instr);

  template <typename T>
  T *append (T *instr)
  {
    internal_append (instr);
    return instr;
  }

  jit_instruction *insert_before (iterator loc, jit_instruction *instr);

  jit_instruction *insert_before (jit_instruction *loc, jit_instruction *instr)
  {
    return insert_before (loc->location (), instr);
  }

  jit_instruction *insert_after (iterator loc, jit_instruction *instr);

  jit_instruction *insert_after (jit_instruction *loc, jit_instruction *instr)
  {
    return insert_after (loc->location (), instr);
  }

  iterator remove (iterator iter)
  {
    jit_instruction *instr = *iter;
    iter = instructions.erase (iter);
    instr->stash_parent (0, instructions.end ());
    return iter;
  }

  jit_terminator *terminator (void) const;

  // is the jump from pred alive?
  bool branch_alive (jit_block *asucc) const;

  jit_block *successor (size_t i) const;

  size_t successor_count (void) const;

  iterator begin (void) { return instructions.begin (); }

  const_iterator begin (void) const { return instructions.begin (); }

  iterator end (void) { return instructions.end (); }

  const_iterator end (void) const { return instructions.end (); }

  iterator phi_begin (void);

  iterator phi_end (void);

  iterator nonphi_begin (void);

  // must label before id is valid
  size_t id (void) const { return mid; }

  // dominance frontier
  const df_set& df (void) const { return mdf; }

  df_iterator df_begin (void) const { return mdf.begin (); }

  df_iterator df_end (void) const { return mdf.end (); }

  // label with a RPO walk
  void label (void)
  {
    size_t number = 0;
    label (mvisit_count, number);
  }

  void label (size_t avisit_count, size_t& number)
  {
    if (visited (avisit_count))
      return;

    for (jit_use *use = first_use (); use; use = use->next ())
      {
        jit_block *pred = use->user_parent ();
        pred->label (avisit_count, number);
      }

    mid = number++;
  }

  // See for idom computation algorithm
  // Cooper, Keith D.; Harvey, Timothy J; and Kennedy, Ken (2001).
  // "A Simple, Fast Dominance Algorithm"
  void compute_idom (jit_block *entry_block)
  {
    bool changed;
    entry_block->idom = entry_block;
    do
      changed = update_idom (mvisit_count);
    while (changed);
  }

  // compute dominance frontier
  void compute_df (void)
  {
    compute_df (mvisit_count);
  }

  void create_dom_tree (void)
  {
    create_dom_tree (mvisit_count);
  }

  jit_block *dom_successor (size_t idx) const
  {
    return dom_succ[idx];
  }

  size_t dom_successor_count (void) const
  {
    return dom_succ.size ();
  }

  // call pop_varaible on all instructions
  void pop_all (void);

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent);
    short_print (os) << ":        %pred = ";
    for (jit_use *use = first_use (); use; use = use->next ())
      {
        jit_block *pred = use->user_parent ();
        os << *pred;
        if (use->next ())
          os << ", ";
      }
    os << std::endl;

    for (const_iterator iter = begin (); iter != end (); ++iter)
      {
        jit_instruction *instr = *iter;
        instr->print (os, indent + 1) << std::endl;
      }
    return os;
  }

  // ...
  jit_block *maybe_split (jit_convert& convert, jit_block *asuccessor);

  jit_block *maybe_split (jit_convert& convert, jit_block& asuccessor)
  {
    return maybe_split (convert, &asuccessor);
  }

  // print dominator infomration
  std::ostream& print_dom (std::ostream& os) const;

  virtual std::ostream& short_print (std::ostream& os) const
  {
    os << mname;
    if (mid != NO_ID)
      os << mid;
    return os;
  }

  llvm::BasicBlock *to_llvm (void) const;

  std::list<jit_block *>::iterator location (void) const
  { return mlocation; }

  void stash_location (std::list<jit_block *>::iterator alocation)
  { mlocation = alocation; }

  // used to prevent visiting the same node twice in the graph
  size_t visit_count (void) const { return mvisit_count; }

  // check if this node has been visited yet at the given visit count. If we
  // have not been visited yet, mark us as visited.
  bool visited (size_t avisit_count)
  {
    if (mvisit_count <= avisit_count)
      {
        mvisit_count = avisit_count + 1;
        return false;
      }

    return true;
  }

  JIT_VALUE_ACCEPT;
private:
  void internal_append (jit_instruction *instr);

  void compute_df (size_t avisit_count);

  bool update_idom (size_t avisit_count);

  void create_dom_tree (size_t avisit_count);

  jit_block *idom_intersect (jit_block *b);

  size_t mvisit_count;
  size_t mid;
  jit_block *idom;
  df_set mdf;
  std::vector<jit_block *> dom_succ;
  std::string mname;
  instruction_list instructions;
  bool malive;
  std::list<jit_block *>::iterator mlocation;
};

// keeps track of phi functions that use a block on incomming edges
class
jit_phi_incomming : public jit_internal_node<jit_block, jit_phi_incomming>
{
public:
  jit_phi_incomming (void) : muser (0) {}

  jit_phi_incomming (jit_phi *auser) : muser (auser) {}

  jit_phi_incomming (const jit_phi_incomming& use) : jit_internal_node ()
  {
    *this = use;
  }

  jit_phi_incomming& operator= (const jit_phi_incomming& use)
  {
    stash_value (use.value ());
    muser = use.muser;
    return *this;
  }

  jit_phi *user (void) const { return muser; }

  jit_block *user_parent (void) const;
private:
  jit_phi *muser;
};

// A non-ssa variable
class
jit_variable : public jit_value
{
public:
  jit_variable (const std::string& aname) : mname (aname), mlast_use (0) {}

  const std::string &name (void) const { return mname; }

  // manipulate the value_stack, for use during SSA construction. The top of the
  // value stack represents the current value for this variable
  bool has_top (void) const
  {
    return ! value_stack.empty ();
  }

  jit_value *top (void) const
  {
    return value_stack.top ();
  }

  void push (jit_instruction *v)
  {
    value_stack.push (v);
    mlast_use = v;
  }

  void pop (void)
  {
    value_stack.pop ();
  }

  jit_instruction *last_use (void) const
  {
    return mlast_use;
  }

  void stash_last_use (jit_instruction *instr)
  {
    mlast_use = instr;
  }

  // blocks in which we are used
  void use_blocks (jit_block::df_set& result)
  {
    jit_use *use = first_use ();
    while (use)
      {
        result.insert (use->user_parent ());
        use = use->next ();
      }
  }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    return print_indent (os, indent) << mname;
  }

  JIT_VALUE_ACCEPT;
private:
  std::string mname;
  std::stack<jit_value *> value_stack;
  jit_instruction *mlast_use;
};

class
jit_assign_base : public jit_instruction
{
public:
  jit_assign_base (jit_variable *adest) : jit_instruction (), mdest (adest) {}

  jit_assign_base (jit_variable *adest, size_t npred) : jit_instruction (npred),
                                                        mdest (adest) {}

  jit_assign_base (jit_variable *adest, jit_value *arg0, jit_value *arg1)
    : jit_instruction (arg0, arg1), mdest (adest) {}

  jit_variable *dest (void) const { return mdest; }

  virtual void push_variable (void)
  {
    mdest->push (this);
  }

  virtual void pop_variable (void)
  {
    mdest->pop ();
  }

  virtual std::ostream& short_print (std::ostream& os) const
  {
    if (type ())
      jit_print (os, type ()) << ": ";

    dest ()->short_print (os);
    return os << "#" << id ();
  }
private:
  jit_variable *mdest;
};

class
jit_assign : public jit_assign_base
{
public:
  jit_assign (jit_variable *adest, jit_value *asrc)
    : jit_assign_base (adest, adest, asrc), martificial (false) {}

  jit_value *overwrite (void) const
  {
    return argument (0);
  }

  jit_value *src (void) const
  {
    return argument (1);
  }

  // variables don't get modified in an SSA, but COW requires we modify
  // variables. An artificial assign is for when a variable gets modified. We
  // need an assign in the SSA, but the reference counts shouldn't be updated.
  bool artificial (void) const { return martificial; }

  void mark_artificial (void) { martificial = true; }

  virtual bool infer (void)
  {
    jit_type *stype = src ()->type ();
    if (stype != type())
      {
        stash_type (stype);
        return true;
      }

    return false;
  }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent) << *this << " = " << *src ();

    if (artificial ())
      os << " [artificial]";

    return os;
  }

  JIT_VALUE_ACCEPT;
private:
  bool martificial;
};

class
jit_phi : public jit_assign_base
{
public:
  jit_phi (jit_variable *adest, size_t npred)
    : jit_assign_base (adest, npred)
  {
    mincomming.reserve (npred);
  }

  // removes arguments form dead incomming jumps
  bool prune (void);

  void add_incomming (jit_block *from, jit_value *value)
  {
    push_argument (value);
    mincomming.push_back (jit_phi_incomming (this));
    mincomming[mincomming.size () - 1].stash_value (from);
  }

  jit_block *incomming (size_t i) const
  {
    return mincomming[i].value ();
  }

  llvm::BasicBlock *incomming_llvm (size_t i) const
  {
    return incomming (i)->to_llvm ();
  }

  virtual void construct_ssa (void) {}

  virtual bool infer (void);

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    std::stringstream ss;
    print_indent (ss, indent);
    short_print (ss) << " phi ";
    std::string ss_str = ss.str ();
    std::string indent_str (ss_str.size (), ' ');
    os << ss_str;

    for (size_t i = 0; i < argument_count (); ++i)
      {
        if (i > 0)
          os << indent_str;
        os << "| ";

        os << *incomming (i) << " -> ";
        os << *argument (i);

        if (i + 1 < argument_count ())
          os << std::endl;
      }

    return os;
  }

  llvm::PHINode *to_llvm (void) const;

  JIT_VALUE_ACCEPT;
private:
  std::vector<jit_phi_incomming> mincomming;
};

class
jit_terminator : public jit_instruction
{
public:
  jit_terminator (size_t asuccessor_count, jit_value *arg0)
    : jit_instruction (arg0), malive (asuccessor_count, false),
      mbranch_llvm (asuccessor_count, 0) {}

  jit_terminator (size_t asuccessor_count, jit_value *arg0, jit_value *arg1)
    : jit_instruction (arg0, arg1), malive (asuccessor_count, false),
      mbranch_llvm (asuccessor_count, 0) {}

  jit_terminator (size_t asuccessor_count, jit_value *arg0, jit_value *arg1,
                  jit_value *arg2)
    : jit_instruction (arg0, arg1, arg2), malive (asuccessor_count, false),
      mbranch_llvm (asuccessor_count, 0) {}

  jit_block *successor (size_t idx = 0) const
  {
    return static_cast<jit_block *> (argument (idx));
  }

  // the llvm block between our parent and the given successor
  llvm::BasicBlock *branch_llvm (size_t idx = 0) const
  {
    return mbranch_llvm[idx] ? mbranch_llvm[idx] : parent ()->to_llvm ();
  }

  llvm::BasicBlock *branch_llvm (int idx) const
  {
    return branch_llvm (static_cast<size_t> (idx));
  }

  llvm::BasicBlock *branch_llvm (const jit_block *asuccessor) const
  {
    return branch_llvm (successor_index (asuccessor));
  }

  llvm::BasicBlock *successor_llvm (size_t idx = 0) const
  {
    return mbranch_llvm[idx] ? mbranch_llvm[idx] : successor (idx)->to_llvm ();
  }

  size_t successor_index (const jit_block *asuccessor) const;

  std::ostream& print_successor (std::ostream& os, size_t idx = 0) const
  {
    if (alive (idx))
      os << "[live] ";
    else
      os << "[dead] ";

    return successor (idx)->short_print (os);
  }

  // Check if the jump to successor is live
  bool alive (const jit_block *asuccessor) const
  {
    return alive (successor_index (asuccessor));
  }

  bool alive (size_t idx) const { return malive[idx]; }

  bool alive (int idx) const { return malive[idx]; }

  size_t successor_count (void) const { return malive.size (); }

  virtual bool infer (void);

  llvm::TerminatorInst *to_llvm (void) const;
protected:
  virtual bool check_alive (size_t) const { return true; }
private:
  std::vector<bool> malive;
  std::vector<llvm::BasicBlock *> mbranch_llvm;
};

class
jit_branch : public jit_terminator
{
public:
  jit_branch (jit_block *succ) : jit_terminator (1, succ) {}

  virtual size_t successor_count (void) const { return 1; }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent) << "branch: ";
    return print_successor (os);
  }

  JIT_VALUE_ACCEPT;
};

class
jit_cond_branch : public jit_terminator
{
public:
  jit_cond_branch (jit_value *c, jit_block *ctrue, jit_block *cfalse)
    : jit_terminator (2, ctrue, cfalse, c) {}

  jit_value *cond (void) const { return argument (2); }

  std::ostream& print_cond (std::ostream& os) const
  {
    return cond ()->short_print (os);
  }

  llvm::Value *cond_llvm (void) const
  {
    return cond ()->to_llvm ();
  }

  virtual size_t successor_count (void) const { return 2; }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent) << "cond_branch: ";
    print_cond (os) << ", ";
    print_successor (os, 0) << ", ";
    return print_successor (os, 1);
  }

  JIT_VALUE_ACCEPT;
};

class
jit_call : public jit_instruction
{
public:
  jit_call (const jit_function& afunction,
            jit_value *arg0) : jit_instruction (arg0), mfunction (afunction)
  {}

  jit_call (const jit_function& (*afunction) (void),
            jit_value *arg0) : jit_instruction (arg0),
                               mfunction (afunction ()) {}

  jit_call (const jit_function& afunction,
            jit_value *arg0, jit_value *arg1) : jit_instruction (arg0, arg1),
                                                mfunction (afunction) {}

  jit_call (const jit_function& (*afunction) (void),
            jit_value *arg0, jit_value *arg1) : jit_instruction (arg0, arg1),
                                                mfunction (afunction ()) {}

  jit_call (const jit_function& (*afunction) (void),
            jit_value *arg0, jit_value *arg1, jit_value *arg2)
    : jit_instruction (arg0, arg1, arg2), mfunction (afunction ()) {}

  jit_call (const jit_function& (*afunction) (void),
            jit_value *arg0, jit_value *arg1, jit_value *arg2, jit_value *arg3)
    : jit_instruction (arg0, arg1, arg2, arg3), mfunction (afunction ()) {}


  const jit_function& function (void) const { return mfunction; }

  bool can_error (void) const
  {
    return overload ().can_error;
  }

  const jit_function::overload& overload (void) const
  {
    return mfunction.get_overload (argument_types ());
  }

  virtual bool needs_release (void) const
  {
    return type () && jit_typeinfo::get_release (type ()).function;
  }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent);

    if (use_count ())
      short_print (os) << " = ";
    os << "call " << mfunction.name () << " (";

    for (size_t i = 0; i < argument_count (); ++i)
      {
        print_argument (os, i);
        if (i + 1 < argument_count ())
          os << ", ";
      }
    return os << ")";
  }

  virtual bool infer (void);

  JIT_VALUE_ACCEPT;
private:
  const jit_function& mfunction;
};

// FIXME: This is just ugly...
// checks error_state, if error_state is false then goto the normal branche,
// otherwise goto the error branch
class
jit_error_check : public jit_terminator
{
public:
  jit_error_check (jit_call *acheck_for, jit_block *normal, jit_block *error)
    : jit_terminator (2, error, normal, acheck_for) {}

  jit_call *check_for (void) const
  {
    return static_cast<jit_call *> (argument (2));
  }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent) << "error_check " << *check_for () << ", ";
    print_successor (os, 1) << ", ";
    return print_successor (os, 0);
  }

  JIT_VALUE_ACCEPT;
protected:
  virtual bool check_alive (size_t idx) const
  {
    return idx == 1 ? true : check_for ()->can_error ();
  }
};

class
jit_extract_argument : public jit_assign_base
{
public:
  jit_extract_argument (jit_type *atype, jit_variable *adest)
    : jit_assign_base (adest)
  {
    stash_type (atype);
  }

  const std::string& name (void) const
  {
    return dest ()->name ();
  }

  const jit_function::overload& overload (void) const
  {
    return jit_typeinfo::cast (type (), jit_typeinfo::get_any ());
  }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    print_indent (os, indent);

    return short_print (os) << " = extract " << name ();
  }

  JIT_VALUE_ACCEPT;
};

class
jit_store_argument : public jit_instruction
{
public:
  jit_store_argument (jit_variable *var)
  : jit_instruction (var), dest (var)
  {}

  const std::string& name (void) const
  {
    return dest->name ();
  }

  const jit_function::overload& overload (void) const
  {
    return jit_typeinfo::cast (jit_typeinfo::get_any (), result_type ());
  }

  jit_value *result (void) const
  {
    return argument (0);
  }

  jit_type *result_type (void) const
  {
    return result ()->type ();
  }

  llvm::Value *result_llvm (void) const
  {
    return result ()->to_llvm ();
  }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
  {
    jit_value *res = result ();
    print_indent (os, indent) << "store ";
    dest->short_print (os);

    if (! isa<jit_variable> (res))
      {
        os << " = ";
        res->short_print (os);
      }

    return os;
  }

  JIT_VALUE_ACCEPT;
private:
  jit_variable *dest;
};

class
jit_ir_walker
{
public:
  virtual ~jit_ir_walker () {}

#define JIT_METH(clname) \
  virtual void visit (jit_ ## clname&) = 0;

  JIT_VISIT_IR_CLASSES;

#undef JIT_METH
};

template <typename T, jit_type *(*EXTRACT_T)(void), typename PASS_T, bool QUOTE>
void
jit_const<T, EXTRACT_T, PASS_T, QUOTE>::accept (jit_ir_walker& walker)
{
  walker.visit (*this);
}

// convert between IRs
// FIXME: Class relationships are messy from here on down. They need to be
// cleaned up.
class
jit_convert : public tree_walker
{
public:
  typedef std::pair<jit_type *, std::string> type_bound;
  typedef std::vector<type_bound> type_bound_vector;

  jit_convert (llvm::Module *module, tree &tee);

  ~jit_convert (void);

  llvm::Function *get_function (void) const { return function; }

  const std::vector<std::pair<std::string, bool> >& get_arguments(void) const
  { return arguments; }

  const type_bound_vector& get_bounds (void) const { return bounds; }

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

  void visit_unwind_protect_command (tree_unwind_protect_command&);

  void visit_while_command (tree_while_command&);

  void visit_do_until_command (tree_do_until_command&);

  // this would be easier with variadic templates
  template <typename T>
  T *create (void)
  {
    T *ret = new T();
    track_value (ret);
    return ret;
  }

  template <typename T, typename ARG0>
  T *create (const ARG0& arg0)
  {
    T *ret = new T(arg0);
    track_value (ret);
    return ret;
  }

  template <typename T, typename ARG0, typename ARG1>
  T *create (const ARG0& arg0, const ARG1& arg1)
  {
    T *ret = new T(arg0, arg1);
    track_value (ret);
    return ret;
  }

  template <typename T, typename ARG0, typename ARG1, typename ARG2>
  T *create (const ARG0& arg0, const ARG1& arg1, const ARG2& arg2)
  {
    T *ret = new T(arg0, arg1, arg2);
    track_value (ret);
    return ret;
  }

  template <typename T, typename ARG0, typename ARG1, typename ARG2,
            typename ARG3>
  T *create (const ARG0& arg0, const ARG1& arg1, const ARG2& arg2,
             const ARG3& arg3)
  {
    T *ret = new T(arg0, arg1, arg2, arg3);
    track_value (ret);
    return ret;
  }

  template <typename ARG0, typename ARG1>
  jit_call *create_checked (const ARG0& arg0, const ARG1& arg1)
  {
    jit_call *ret = create<jit_call> (arg0, arg1);
    return create_checked_impl (ret);
  }

  template <typename ARG0, typename ARG1, typename ARG2>
  jit_call *create_checked (const ARG0& arg0, const ARG1& arg1,
                            const ARG2& arg2)
  {
    jit_call *ret = create<jit_call> (arg0, arg1, arg2);
    return create_checked_impl (ret);
  }

  template <typename ARG0, typename ARG1, typename ARG2, typename ARG3>
  jit_call *create_checked (const ARG0& arg0, const ARG1& arg1,
                            const ARG2& arg2, const ARG3& arg3)
  {
    jit_call *ret = create<jit_call> (arg0, arg1, arg2, arg3);
    return create_checked_impl (ret);
  }

  typedef std::list<jit_block *> block_list;
  typedef block_list::iterator block_iterator;

  void append (jit_block *ablock);

  void insert_before (block_iterator iter, jit_block *ablock);

  void insert_before (jit_block *loc, jit_block *ablock)
  {
    insert_before (loc->location (), ablock);
  }

  void insert_after (block_iterator iter, jit_block *ablock);

  void insert_after (jit_block *loc, jit_block *ablock)
  {
    insert_after (loc->location (), ablock);
  }
private:
  std::vector<std::pair<std::string, bool> > arguments;
  type_bound_vector bounds;

  // used instead of return values from visit_* functions
  jit_value *result;

  jit_block *entry_block;

  jit_block *final_block;

  jit_block *block;

  llvm::Function *function;

  std::list<jit_block *> blocks;

  std::list<jit_instruction *> worklist;

  std::list<jit_value *> constants;

  std::list<jit_value *> all_values;

  size_t iterator_count;

  typedef std::map<std::string, jit_variable *> vmap_t;
  vmap_t vmap;

  jit_call *create_checked_impl (jit_call *ret)
  {
    block->append (ret);
    create_check (ret);
    return ret;
  }

  jit_error_check *create_check (jit_call *call)
  {
    jit_block *normal = create<jit_block> (block->name ());
    jit_error_check *ret
      = block->append (create<jit_error_check> (call, normal, final_block));
    append (normal);
    block = normal;

    return ret;
  }

  jit_variable *get_variable (const std::string& vname);

  std::pair<jit_value *, jit_value *> resolve (tree_index_expression& exp);

  jit_value *do_assign (tree_expression *exp, jit_value *rhs,
                        bool artificial = false);

  jit_value *do_assign (const std::string& lhs, jit_value *rhs, bool print,
                        bool artificial = false);

  jit_value *visit (tree *tee) { return visit (*tee); }

  jit_value *visit (tree& tee);

  void push_worklist (jit_instruction *instr)
  {
    if (! instr->in_worklist ())
      {
        instr->stash_in_worklist (true);
        worklist.push_back (instr);
      }
  }

  void append_users (jit_value *v)
  {
    for (jit_use *use = v->first_use (); use; use = use->next ())
      push_worklist (use->user ());
  }

  void append_users_term (jit_terminator *term);

  void track_value (jit_value *value)
  {
    if (value->type ())
      constants.push_back (value);
    all_values.push_back (value);
  }

  void merge_blocks (void);

  void construct_ssa (void);

  void do_construct_ssa (jit_block& block, size_t avisit_count);

  void remove_dead ();

  void place_releases (void);

  void release_temp (jit_block& ablock, std::set<jit_value *>& temp);

  void release_dead_phi (jit_block& ablock);

  void simplify_phi (void);

  void simplify_phi (jit_phi& phi);

  void print_blocks (const std::string& header)
  {
    std::cout << "-------------------- " << header << " --------------------\n";
    for (std::list<jit_block *>::iterator iter = blocks.begin ();
         iter != blocks.end (); ++iter)
      {
        assert (*iter);
        (*iter)->print (std::cout, 0);
      }
    std::cout << std::endl;
  }

  void print_dom (void)
  {
    std::cout << "-------------------- dom info --------------------\n";
    for (std::list<jit_block *>::iterator iter = blocks.begin ();
         iter != blocks.end (); ++iter)
      {
        assert (*iter);
        (*iter)->print_dom (std::cout);
      }
    std::cout << std::endl;
  }

  bool breaking; // true if we are breaking OR continuing
  block_list breaks;
  block_list continues;

  void finish_breaks (jit_block *dest, const block_list& lst);

  // this case is much simpler, just convert from the jit ir to llvm
  class
  convert_llvm : public jit_ir_walker
  {
  public:
    convert_llvm (jit_convert& jc) : jthis (jc) {}

    llvm::Function *convert (llvm::Module *module,
                             const std::vector<std::pair<std::string, bool> >& args,
                             const std::list<jit_block *>& blocks,
                             const std::list<jit_value *>& constants);

#define JIT_METH(clname)                        \
    virtual void visit (jit_ ## clname&);

    JIT_VISIT_IR_CLASSES;

#undef JIT_METH
  private:
    // name -> llvm argument
    std::map<std::string, llvm::Value *> arguments;

    void finish_phi (jit_phi *phi);

    void visit (jit_value *jvalue)
    {
      return visit (*jvalue);
    }

    void visit (jit_value &jvalue)
    {
      jvalue.accept (*this);
    }

    llvm::Value *create_call (const jit_function::overload& ol, jit_value *arg0)
    {
      std::vector<jit_value *> args (1, arg0);
      return create_call (ol, args);
    }

    llvm::Value *create_call (const jit_function::overload& ol, jit_value *arg0,
                              jit_value *arg1)
    {
      std::vector<jit_value *> args (2);
      args[0] = arg0;
      args[1] = arg1;

      return create_call (ol, args);
    }

    llvm::Value *create_call (const jit_function::overload& ol,
                              const std::vector<jit_value *>& jargs);

    llvm::Value *create_call (const jit_function::overload& ol,
                              const std::vector<jit_use>& uses);
  private:
    jit_convert &jthis;
    llvm::Function *function;
    llvm::BasicBlock *prelude;
  };
};

class jit_info;

class
tree_jit
{
public:
  tree_jit (void);

  ~tree_jit (void);

  bool execute (tree_simple_for_command& cmd);

  llvm::ExecutionEngine *get_engine (void) const { return engine; }

  llvm::Module *get_module (void) const { return module; }

  void optimize (llvm::Function *fn);
 private:
  bool initialize (void);

  // FIXME: Temorary hack to test
  typedef std::map<tree *, jit_info *> compiled_map;
  llvm::Module *module;
  llvm::PassManager *module_pass_manager;
  llvm::FunctionPassManager *pass_manager;
  llvm::ExecutionEngine *engine;
};

class
jit_info
{
public:
  jit_info (tree_jit& tjit, tree& tee);

  ~jit_info (void);

  bool execute (void) const;

  bool match (void) const;
private:
  typedef jit_convert::type_bound type_bound;
  typedef jit_convert::type_bound_vector type_bound_vector;
  typedef void (*jited_function)(octave_base_value**);

  llvm::ExecutionEngine *engine;
  jited_function function;
  llvm::Function *llvm_function;

  std::vector<std::pair<std::string, bool> > arguments;
  type_bound_vector bounds;
};

// some #defines we use in the header, but not the cc file
#undef JIT_VISIT_IR_CLASSES
#undef JIT_VISIT_IR_CONST
#undef JIT_VALUE_ACCEPT

#endif
#endif
