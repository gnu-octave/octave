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

#include <list>
#include <map>
#include <set>
#include <stdexcept>
#include <vector>

#include "Array.h"
#include "Range.h"
#include "pt-walk.h"
#include "symtab.h"

// -------------------- Current status --------------------
// Simple binary operations (+-*/) on octave_scalar's (doubles) are optimized.
// However, there is no warning emitted on divide by 0. For example,
// a = 5;
// b = a * 5 + a;
//
// For other types all binary operations are compiled but not optimized. For
// example,
// a = [1 2 3]
// b = a + a;
// will compile to do_binary_op (a, a).
//
// For loops are compiled again!
// if, elseif, and else statements compile again!
// Additionally, make check passes using jit.
//
// The octave low level IR is a linear IR, it works by converting everything to
// calls to jit_functions. This turns expressions like c = a + b into
// c = call binary+ (a, b)
// The jit_functions contain information about overloads for differnt types. For
// example, if we know a and b are scalars, then c must also be a scalar.
//
//
// TODO:
// 1. Support error cases
// 2. Support break/continue
// 3. Fix memory leaks in JIT
// 4. Cleanup/documentation
// 5. ...
// ---------------------------------------------------------


// we don't want to include llvm headers here, as they require __STDC_LIMIT_MACROS
// and __STDC_CONSTANT_MACROS be defined in the entire compilation unit
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
}

class octave_base_value;
class octave_value;
class tree;

// jit_range is compatable with the llvm range structure
struct
jit_range
{
  jit_range (void) {}

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
static std::ostream& jit_print (std::ostream& os, jit_type *atype)
{
  if (! atype)
    return os << "null";
  return os << atype->name ();
}

// Keeps track of overloads for a builtin function. Used for both type inference
// and code generation.
class
jit_function
{
public:
  struct overload
  {
    overload (void) : function (0), can_error (true), result (0) {}

    overload (llvm::Function *f, bool e, jit_type *r, jit_type *arg0) :
      function (f), can_error (e), result (r), arguments (1)
    {
      arguments[0] = arg0;
    }

    overload (llvm::Function *f, bool e, jit_type *r, jit_type *arg0,
              jit_type *arg1) : function (f), can_error (e), result (r),
                                arguments (2)
    {
      arguments[0] = arg0;
      arguments[1] = arg1;
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

  static jit_type *get_scalar (void) { return instance->scalar; }

  static llvm::Type *get_scalar_llvm (void) { return instance->scalar->to_llvm (); }

  static jit_type *get_range (void) { return instance->range; }

  static jit_type *get_string (void) { return instance->string; }

  static jit_type *get_bool (void) { return instance->boolean; }

  static jit_type *get_index (void) { return instance->index; }

  static llvm::Type *get_index_llvm (void) { return instance->index->to_llvm (); }

  static jit_type *type_of (const octave_value& ov)
  {
    return instance->do_type_of (ov);
  }

  static const jit_function& binary_op (int op)
  {
    return instance->do_binary_op (op);
  }

  static const jit_function& grab (void) { return instance->grab_fn; }

  static const jit_function& release (void)
  {
    return instance->release_fn;
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

  static const jit_function& cast (jit_type *result)
  {
    return instance->do_cast (result);
  }

  static const jit_function::overload& cast (jit_type *to, jit_type *from)
  {
    return instance->do_cast (to, from);
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
                                   const std::vector<llvm::Type *>& args);

  llvm::Function *create_identity (jit_type *type);

  static jit_typeinfo *instance;

  llvm::Module *module;
  llvm::ExecutionEngine *engine;
  int next_id;

  llvm::Type *ov_t;

  std::vector<jit_type*> id_to_type;
  jit_type *any;
  jit_type *scalar;
  jit_type *range;
  jit_type *string;
  jit_type *boolean;
  jit_type *index;

  std::vector<jit_function> binary_ops;
  jit_function grab_fn;
  jit_function release_fn;
  jit_function print_fn;
  jit_function for_init_fn;
  jit_function for_check_fn;
  jit_function for_index_fn;
  jit_function logically_true;

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
  JIT_METH(break);                              \
  JIT_METH(cond_break);                         \
  JIT_METH(call);                               \
  JIT_METH(extract_argument);                   \
  JIT_METH(store_argument);                     \
  JIT_METH(phi)

#define JIT_VISIT_IR_CLASSES                    \
  JIT_VISIT_IR_NOTEMPLATE;                      \
  JIT_VISIT_IR_CONST


class jit_ir_walker;
class jit_use;

class
jit_value
{
  friend class jit_use;
public:
  jit_value (void) : llvm_value (0), ty (0), use_head (0), myuse_count (0) {}

  virtual ~jit_value (void) {}

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

  jit_use *first_use (void) const { return use_head; }

  size_t use_count (void) const { return myuse_count; }

  std::string print_string (void)
  {
    std::stringstream ss;
    print (ss);
    return ss.str ();
  }

  virtual std::ostream& print (std::ostream& os, size_t indent = 0) = 0;

  virtual std::ostream& short_print (std::ostream& os)
  { return print (os); }

  virtual void accept (jit_ir_walker& walker) = 0;

  llvm::Value *to_llvm (void) const
  {
    return llvm_value;
  }

  void stash_llvm (llvm::Value *compiled)
  {
    llvm_value = compiled;
  }
protected:
  std::ostream& print_indent (std::ostream& os, size_t indent)
  {
    for (size_t i = 0; i < indent; ++i)
      os << "\t";
    return os;
  }

  llvm::Value *llvm_value;
private:
  jit_type *ty;
  jit_use *use_head;
  jit_use *use_tail;
  size_t myuse_count;
};

// defnie accept methods for subclasses
#define JIT_VALUE_ACCEPT(clname)                \
  virtual void accept (jit_ir_walker& walker);

template <typename T, jit_type *(*EXTRACT_T)(void), typename PASS_T = T,
          bool QUOTE=false>
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

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    print_indent (os, indent) << type_name () << ": ";
    if (QUOTE)
      os << "\"";
    os << mvalue;
    if (QUOTE)
      os << "\"";
    return os;
  }

  JIT_VALUE_ACCEPT (jit_const);
private:
  T mvalue;
};

typedef jit_const<double, jit_typeinfo::get_scalar> jit_const_scalar;
typedef jit_const<octave_idx_type, jit_typeinfo::get_index> jit_const_index;

typedef jit_const<std::string, jit_typeinfo::get_string, const std::string&, true>
jit_const_string;
typedef jit_const<jit_range, jit_typeinfo::get_range, const jit_range&>
jit_const_range;

#define JIT_VISIT_IR_CONST                      \
  JIT_METH(const_scalar);                       \
  JIT_METH(const_index);                        \
  JIT_METH(const_string);                       \
  JIT_METH(const_range)

class jit_instruction;
class jit_block;

class
jit_use
{
public:
  jit_use (void) : mvalue (0), mnext (0), mprev (0), muser (0), mindex (0) {}

  // we should really have a move operator, but not until c++11 :(
  jit_use (const jit_use& use) : mvalue (0), mnext (0), mprev (0), muser (0),
                                 mindex (0)
  {
    *this = use;
  }

  ~jit_use (void) { remove (); }

  jit_use& operator= (const jit_use& use)
  {
    stash_value (use.value (), use.user (), use.index ());
    return *this;
  }

  jit_value *value (void) const { return mvalue; }

  size_t index (void) const { return mindex; }

  jit_instruction *user (void) const { return muser; }

  jit_block *user_parent (void) const;

  void stash_value (jit_value *avalue, jit_instruction *auser = 0,
                    size_t aindex = -1)
  {
    remove ();

    mvalue = avalue;

    if (mvalue)
      {
        if (mvalue->use_head)
          {
            mvalue->use_head->mprev = this;
            mnext = mvalue->use_head;
          }
        
        mvalue->use_head = this;
        ++mvalue->myuse_count;
      }

    mindex = aindex;
    muser = auser;
  }

  jit_use *next (void) const { return mnext; }

  jit_use *prev (void) const { return mprev; }
private:
  void remove (void)
  {
    if (mvalue)
      {
        if (this == mvalue->use_head)
            mvalue->use_head = mnext;

        if (mprev)
          mprev->mnext = mnext;

        if (mnext)
          mnext->mprev = mprev;

        mnext = mprev = 0;
        --mvalue->myuse_count;
        mvalue = 0;
      }
  }

  jit_value *mvalue;
  jit_use *mnext;
  jit_use *mprev;
  jit_instruction *muser;
  size_t mindex;
};

class
jit_instruction : public jit_value
{
public:
  // FIXME: this code could be so much pretier with varadic templates...
  jit_instruction (void) : id (next_id ()), mparent (0)
  {}

  jit_instruction (size_t nargs, jit_value *adefault = 0)
  : already_infered (nargs, reinterpret_cast<jit_type *>(0)), arguments (nargs),
    id (next_id ()), mparent (0)
  {
    if (adefault)
      for (size_t i = 0; i < nargs; ++i)
        stash_argument (i, adefault);
  }

  jit_instruction (jit_value *arg0)
    : already_infered (1, reinterpret_cast<jit_type *>(0)), arguments (1), 
      id (next_id ()), mparent (0)
  {
    stash_argument (0, arg0);
  }

  jit_instruction (jit_value *arg0, jit_value *arg1)
    : already_infered (2, reinterpret_cast<jit_type *>(0)), arguments (2), 
      id (next_id ()), mparent (0)
  {
    stash_argument (0, arg0);
    stash_argument (1, arg1);
  }

  jit_instruction (jit_value *arg0, jit_value *arg1, jit_value *arg2)
    : already_infered (3, reinterpret_cast<jit_type *>(0)), arguments (3), 
      id (next_id ()), mparent (0)
  {
    stash_argument (0, arg0);
    stash_argument (1, arg1);
    stash_argument (2, arg2);
  }

  static void reset_ids (void)
  {
    next_id (true);
  }

  jit_value *argument (size_t i) const
  {
    return arguments[i].value ();
  }

  llvm::Value *argument_llvm (size_t i) const
  {
    assert (argument (i));
    return argument (i)->to_llvm ();
  }

  jit_type *argument_type (size_t i) const
  {
    assert (argument (i));
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
    arguments[i].stash_value (arg, this, i);
  }

  size_t argument_count (void) const
  {
    return arguments.size ();
  }

  void resize_arguments (size_t acount, jit_value *adefault = 0)
  {
    size_t old = arguments.size ();
    arguments.resize (acount);

    if (adefault)
      for (size_t i = old; i < acount; ++i)
        stash_argument (i, adefault);
  }

  // argument types which have been infered already
  const std::vector<jit_type *>& argument_types (void) const
  { return already_infered; }

  virtual bool infer (void) { return false; }

  virtual std::ostream& short_print (std::ostream& os)
  {
    if (mtag.empty ())
      jit_print (os, type ()) << ": #" << id;
    else
      jit_print (os, type ()) << ": " << mtag << "." << id;

    return os;
  }

  const std::string& tag (void) const { return mtag; }

  void stash_tag (const std::string& atag) { mtag = atag; }

  jit_block *parent (void) const { return mparent; }

  llvm::BasicBlock *parent_llvm (void) const;

  void stash_parent (jit_block *aparent)
  {
    assert (! mparent);
    mparent = aparent;
  }
protected:
  std::vector<jit_type *> already_infered;
private:
  static size_t next_id (bool reset = false)
  {
    static size_t ret = 0;
    if (reset)
      return ret = 0;

    return ret++;
  }

  std::vector<jit_use> arguments;

  std::string mtag;
  size_t id;
  jit_block *mparent;
};

class jit_terminator;
class jit_phi;

class
jit_block : public jit_value
{
public:
  typedef std::list<jit_instruction *> instruction_list;
  typedef instruction_list::iterator iterator;
  typedef instruction_list::const_iterator const_iterator;

  jit_block (const std::string& aname) : mname (aname)
  {}

  virtual ~jit_block ()
  {
    for (instruction_list::iterator iter = instructions.begin ();
         iter != instructions.end (); ++iter)
      delete *iter;
  }

  const std::string& name (void) const { return mname; }

  jit_instruction *prepend (jit_instruction *instr);

  jit_instruction *append (jit_instruction *instr);

  jit_terminator *terminator (void) const;

  jit_block *pred (size_t idx) const;

  jit_terminator *pred_terminator (size_t idx) const
  {
    return pred (idx)->terminator ();
  }

  llvm::Value *pred_terminator_llvm (size_t idx) const;

  std::ostream& print_pred (std::ostream& os, size_t idx)
  {
    return pred (idx)->short_print (os);
  }

  // takes into account for the addition of phi merges
  llvm::BasicBlock *pred_llvm (size_t idx) const
  {
    if (mpred_llvm.size () < pred_count ())
      mpred_llvm.resize (pred_count ());

    return mpred_llvm[idx] ? mpred_llvm[idx] : pred (idx)->to_llvm ();
  }

  llvm::BasicBlock *pred_llvm (jit_block *apred) const
  {
    return pred_llvm (pred_index (apred));
  }

  size_t pred_index (jit_block *apred) const;

  // create llvm phi merge blocks for all predecessors (if required)
  void create_merge (llvm::Function *inside, size_t pred_idx);

  size_t pred_count (void) const { return use_count (); }

  size_t succ_count (void) const;

  iterator begin (void) { return instructions.begin (); }

  const_iterator begin (void) const { return instructions.begin (); }

  iterator end (void) { return instructions.end (); }

  const_iterator end (void) const { return instructions.begin (); }

  // search for the phi function with the given tag_name, if no function
  // exists then a new phi node is created
  jit_phi *search_phi (const std::string& tag_name, jit_value *adefault);

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    print_indent (os, indent) << mname << ":\tpred = ";
    for (size_t i = 0; i < pred_count (); ++i)
      {
        print_pred (os, i);
        if (i + 1 < pred_count ())
          os << ", ";
      }
    os << std::endl;

    for (iterator iter = begin (); iter != end (); ++iter)
      {
        jit_instruction *instr = *iter;
        instr->print (os, indent + 1) << std::endl;
      }
    return os;
  }

  virtual std::ostream& short_print (std::ostream& os)
  {
    return os << mname;
  }

  llvm::BasicBlock *to_llvm (void) const;

  JIT_VALUE_ACCEPT (block)
private:
  std::string mname;
  instruction_list instructions;
  mutable std::vector<llvm::BasicBlock *> mpred_llvm;
};

class
jit_phi : public jit_instruction
{
public:
  jit_phi (size_t npred, jit_value *adefault = 0)
    : jit_instruction (npred, adefault)
  {}

  virtual bool infer (void)
  {
    jit_type *infered = 0;
    for (size_t i = 0; i < argument_count (); ++i)
      infered = jit_typeinfo::join (infered, argument_type (i));

    if (infered != type ())
      {
        stash_type (infered);
        return true;
      }

    return false;
  }

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    std::stringstream ss;
    print_indent (ss, indent);
    short_print (ss) << " phi ";
    std::string ss_str = ss.str ();
    std::string indent_str (ss_str.size () + 7, ' ');
    os << ss_str;

    jit_block *pblock = parent ();
    for (size_t i = 0; i < argument_count (); ++i)
      {
        if (i > 0)
          os << indent_str;
        os << "| ";

        pblock->print_pred (os, i) << " -> ";
        print_argument (os, i);

        if (i + 1 < argument_count ())
          os << std::endl;
      }

    return os;
  }

  JIT_VALUE_ACCEPT (phi);
};

class
jit_terminator : public jit_instruction
{
public:
  jit_terminator (jit_value *arg0) : jit_instruction (arg0) {}

  jit_terminator (jit_value *arg0, jit_value *arg1, jit_value *arg2)
    : jit_instruction (arg0, arg1, arg2) {}

  virtual jit_block *sucessor (size_t idx = 0) const = 0;

  // return either our sucessors block directly, or the phi merge block
  // between us and our sucessor
  llvm::BasicBlock *sucessor_llvm (size_t idx = 0) const
  {
    jit_block *succ = sucessor (idx);
    llvm::BasicBlock *pllvm = parent_llvm ();
    llvm::BasicBlock *spred_llvm = succ->pred_llvm (parent ());
    llvm::BasicBlock *succ_llvm = succ->to_llvm ();
    return pllvm == spred_llvm ? succ_llvm : spred_llvm;
  }

  std::ostream& print_sucessor (std::ostream& os, size_t idx = 0)
  {
    return sucessor (idx)->short_print (os);
  }

  virtual size_t sucessor_count (void) const = 0;
};

class
jit_break : public jit_terminator
{
public:
  jit_break (jit_block *succ) : jit_terminator (succ) {}

  jit_block *sucessor (size_t idx = 0) const
  {
    jit_value *arg = argument (idx);
    return static_cast<jit_block *> (arg);
  }

  size_t sucessor_count (void) const { return 1; }

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    print_indent (os, indent) << "break: ";
    return print_sucessor (os);
  }

  JIT_VALUE_ACCEPT (break)
};

class
jit_cond_break : public jit_terminator
{
public:
  jit_cond_break (jit_value *c, jit_block *ctrue, jit_block *cfalse)
    : jit_terminator (c, ctrue, cfalse) {}

  jit_value *cond (void) const { return argument (0); }

  std::ostream& print_cond (std::ostream& os)
  {
    return cond ()->short_print (os);
  }

  llvm::Value *cond_llvm (void) const
  {
    return cond ()->to_llvm ();
  }

  jit_block *sucessor (size_t idx) const
  {
    jit_value *arg = argument (idx + 1);
    return static_cast<jit_block *> (arg);
  }

  size_t sucessor_count (void) const { return 2; }

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    print_indent (os, indent) << "cond_break: ";
    print_cond (os) << ", ";
    print_sucessor (os, 0) << ", ";
    return print_sucessor (os, 1);
  }

  JIT_VALUE_ACCEPT (cond_break)
};

class
jit_call : public jit_instruction
{
public:
  jit_call (const jit_function& afunction,
            jit_value *arg0) : jit_instruction (arg0), mfunction (afunction) {}

  jit_call (const jit_function& (*afunction) (void),
            jit_value *arg0) : jit_instruction (arg0), mfunction (afunction ()) {}

  jit_call (const jit_function& afunction,
            jit_value *arg0, jit_value *arg1) : jit_instruction (arg0, arg1),
                                                mfunction (afunction) {}

  jit_call (const jit_function& (*afunction) (void),
            jit_value *arg0, jit_value *arg1) : jit_instruction (arg0, arg1),
                                                mfunction (afunction ()) {}

  const jit_function& function (void) const { return mfunction; }

  const jit_function::overload& overload (void) const
  {
    return mfunction.get_overload (argument_types ());
  }

  virtual std::ostream& print (std::ostream& os, size_t indent)
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

  JIT_VALUE_ACCEPT (call)
private:
  const jit_function& mfunction;
};

class
jit_extract_argument : public jit_instruction
{
public:
  jit_extract_argument (jit_type *atype, const std::string& aname)
    : jit_instruction ()
  {
    stash_type (atype);
    stash_tag (aname);
  }

  const jit_function::overload& overload (void) const
  {
    return jit_typeinfo::cast (type (), jit_typeinfo::get_any ());
  }

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    print_indent (os, indent);
    return short_print (os) << " = extract: " << tag ();
  }

  JIT_VALUE_ACCEPT (extract_argument)
};

class
jit_store_argument : public jit_instruction
{
public:
  jit_store_argument (const std::string& aname, jit_value *aresult)
    : jit_instruction (aresult)
  {
    stash_tag (aname);
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

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    jit_value *res = result ();
    print_indent (os, indent) << tag () << " <- ";
    return res->short_print (os);
  }

  JIT_VALUE_ACCEPT (store_argument)
};

class
jit_ir_walker
{
public:
  virtual ~jit_ir_walker () {}

#define JIT_METH(clname) \
  virtual void visit (jit_ ## clname&) = 0

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
private:
  std::vector<std::pair<std::string, bool> > arguments;
  type_bound_vector bounds;

  class
  variable_map
  {
    // internal variable map
    typedef std::map<std::string, jit_value *> ivar_map;
  public:
    typedef ivar_map::iterator iterator;
    typedef ivar_map::const_iterator const_iterator;

    variable_map (variable_map *aparent, jit_block *ablock) : mparent (aparent),
                                                              mblock (ablock)
    {}

    virtual ~variable_map () {}

    variable_map *parent (void) const { return mparent; }

    jit_block *block (void) const { return mblock; }

    jit_value *get (const std::string& name)
    {
      ivar_map::iterator iter = vars.find (name);
      if (iter != vars.end ())
        return iter->second;

      if (mparent)
        {
          jit_value *pval = mparent->get (name);
          return insert (name, pval);
        }

      return insert (name, 0);
    }

    jit_value *set (const std::string& name, jit_value *val)
    {
      get (name); // force insertion
      return vars[name] = val;
    }

    iterator begin (void) { return vars.begin (); }
    const_iterator begin (void) const { return vars.begin (); }

    iterator end (void) { return vars.end (); }
    const_iterator end (void) const { return vars.end (); }

    size_t size (void) const { return vars.size (); }
  protected:
    virtual jit_value *insert (const std::string& name, jit_value *pval) = 0;

    ivar_map vars;
  private:
    variable_map *mparent;
    jit_block *mblock;
  };

  class
  toplevel_map : public variable_map
  {
  public:
    toplevel_map (jit_block *aentry) : variable_map (0, aentry) {}
  protected:
    virtual jit_value *insert (const std::string& name, jit_value *pval);
  };

  class
  for_map : public variable_map
  {
  public:
    typedef variable_map::iterator iterator;
    typedef variable_map::const_iterator const_iterator;

    for_map (variable_map *aparent, jit_block *ablock)
      : variable_map (aparent, ablock)
    {
      // force insertion of all phi nodes
      for (iterator iter = aparent->begin (); iter != aparent->end (); ++iter)
        get (iter->first);
    }

    void finish_phi (variable_map& from)
    {
      jit_block *for_body = block ();
      for (jit_block::iterator iter = for_body->begin ();
           iter != for_body->end () && dynamic_cast<jit_phi *> (*iter); ++iter)
        {
          jit_instruction *node = *iter;
          if (! node->argument (1))
            node->stash_argument (1, from.get (node->tag ()));
        }
    }
  protected:
    virtual jit_value *insert (const std::string& name, jit_value *pval)
    {
      jit_phi *ret = new jit_phi (2);
      ret->stash_tag (name);
      block ()->prepend (ret);
      ret->stash_argument (0, pval);
      return vars[name] = ret;
    }
  };

  class
  compound_map : public variable_map
  {
  public:
    compound_map (variable_map *aparent) : variable_map (aparent, 0)
    {}
  protected:
    virtual jit_value *insert (const std::string&, jit_value *pval)
    {
      return pval;
    }
  };


  variable_map *variables;

  // used instead of return values from visit_* functions
  jit_value *result;

  jit_block *block;
  jit_block *final_block;

  llvm::Function *function;

  std::list<jit_block *> blocks;

  std::list<jit_block *> cleanup_blocks;

  std::list<jit_instruction *> worklist;

  std::list<jit_value *> constants;

  void do_assign (const std::string& lhs, jit_value *rhs, bool print);

  jit_value *visit (tree *tee) { return visit (*tee); }

  jit_value *visit (tree& tee);

  void append_users (jit_value *v)
  {
    for (jit_use *use = v->first_use (); use; use = use->next ())
      worklist.push_back (use->user ());
  }

  template <typename CONST_T>
  CONST_T *get_const (typename CONST_T::pass_t v)
  {
    CONST_T *ret = new CONST_T (v);
    constants.push_back (ret);
    return ret;
  }

  // place phi nodes in the current block to merge ref with variables
  // we assume the same number of deffinitions
  void merge (jit_block *merge_block, variable_map& merge_vars,
              jit_block *incomming_block,
              const variable_map& incomming_vars);

  // this case is much simpler, just convert from the jit ir to llvm
  class
  convert_llvm : public jit_ir_walker
  {
  public:
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


    void visit (jit_value *jvalue)
    {
      return visit (*jvalue);
    }

    void visit (jit_value &jvalue)
    {
      jvalue.accept (*this);
    }
  private:
    llvm::Function *function;
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

  bool execute (void) const;

  bool match (void) const;
private:
  typedef jit_convert::type_bound type_bound;
  typedef jit_convert::type_bound_vector type_bound_vector;
  typedef void (*jited_function)(octave_base_value**);

  llvm::ExecutionEngine *engine;
  jited_function function;

  std::vector<std::pair<std::string, bool> > arguments;
  type_bound_vector bounds;
};

#endif
