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
// for loops and if statements no longer compile! This is because work has been
// done to introduce a new lower level IR for octave. The low level IR looks
// a lot like llvm's IR, but it makes it much easier to infer types. You can set
// debug_print to true in pt-jit.cc to view the IRs that are created.
//
// The octave low level IR is a linear IR, it works by converting everything to
// calls to jit_functions. This turns expressions like c = a + b into
// c = call binary+ (a, b)
// The jit_functions contain information about overloads for differnt types. For
// example, if we know a and b are scalars, then c must also be a scalar.
//
// You will currently see a LARGE slowdown, as every statement is compiled
// seperatly!
//
// TODO:
// 1. Support for loops
// 2. Support if statements
// 3. Cleanup/documentation
// 4. ...
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

  static jit_type *tunion (jit_type *lhs, jit_type *rhs)
  {
    return instance->do_union (lhs, rhs);
  }

  static jit_type *difference (jit_type *lhs, jit_type *rhs)
  {
    return instance->do_difference (lhs, rhs);
  }

  static jit_type *get_any (void) { return instance->any; }

  static jit_type *get_scalar (void) { return instance->scalar; }

  static jit_type *get_range (void) { return instance->range; }

  static jit_type *get_string (void) { return instance->string; }

  static jit_type *get_bool (void) { return instance->boolean; }

  static jit_type *get_index (void) { return instance->index; }

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
  jit_type *do_union (jit_type *lhs, jit_type *rhs)
  {
    // FIXME: Actually introduce a union type

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
  jit_function simple_for_check;
  jit_function simple_for_incr;
  jit_function simple_for_index;
  jit_function logically_true;

  // type id -> cast function TO that type
  std::vector<jit_function> casts;

  // type id -> identity function
  std::vector<llvm::Function *> identities;
};

// The low level octave jit ir
// this ir is close to llvm, but contains information for doing type inference.
// We convert the octave parse tree to this IR directly.

#define JIT_VISIT_IR_CLASSES                    \
  JIT_METH(const_string);                       \
  JIT_METH(const_scalar);                       \
  JIT_METH(block);                              \
  JIT_METH(break);                              \
  JIT_METH(cond_break);                         \
  JIT_METH(call);                               \
  JIT_METH(extract_argument);                   \
  JIT_METH(store_argument)


#define JIT_METH(clname) class jit_ ## clname
JIT_VISIT_IR_CLASSES;
#undef JIT_METH

class
jit_ir_walker
{
public:
  virtual ~jit_ir_walker () {}

#define JIT_METH(clname) \
  virtual void visit_ ## clname (jit_ ## clname&) = 0

  JIT_VISIT_IR_CLASSES;

#undef JIT_METH
};

class jit_use;

class
jit_value
{
  friend class jit_use;
public:
  jit_value (void) : llvm_value (0), ty (0), use_head (0) {}

  virtual ~jit_value (void) {}

  jit_type *type () const { return ty; }

  void stash_type (jit_type *new_ty) { ty = new_ty; }

  jit_use *first_use (void) const { return use_head; }

  size_t use_count (void) const { return myuse_count; }

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
  size_t myuse_count;
};

// defnie accept methods for subclasses
#define JIT_VALUE_ACCEPT(clname)                \
  virtual void accept (jit_ir_walker& walker)   \
  {                                             \
  walker.visit_ ## clname (*this);              \
  }

class
jit_const_string : public jit_value
{
public:
  jit_const_string (const std::string& v) : val (v)
  {
    stash_type (jit_typeinfo::get_string ());
  }

  const std::string& value (void) const { return val; }

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    return print_indent (os, indent) << "string: \"" << val << "\"";
  }

  JIT_VALUE_ACCEPT (const_string)
private:
  std::string val;
};

class
jit_const_scalar : public jit_value
{
public:
  jit_const_scalar (double avalue) : mvalue (avalue)
  {
    stash_type (jit_typeinfo::get_scalar ());
  }

  double value (void) const { return mvalue; }

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    return print_indent (os, indent) << "scalar: \"" << mvalue << "\"";
  }

  JIT_VALUE_ACCEPT (const_scalar)
private:
  double mvalue;
};

class jit_instruction;

class
jit_use
{
public:
  jit_use (void) : used (0), next_use (0), prev_use (0) {}

  ~jit_use (void) { remove (); }

  jit_value *value (void) const { return used; }

  size_t index (void) const { return idx; }

  jit_instruction *user (void) const { return usr; }

  void stash_value (jit_value *new_value, jit_instruction *u = 0,
                    size_t use_idx = -1)
  {
    remove ();

    used = new_value;

    if (used)
      {
        if (used->use_head)
          {
            used->use_head->prev_use = this;
            next_use = used->use_head;
          }
        
        used->use_head = this;
        ++used->myuse_count;
      }

    idx = use_idx;
    usr = u;
  }

  jit_use *next (void) const { return next_use; }

  jit_use *prev (void) const { return prev_use; }
private:
  void remove (void)
  {
    if (used)
      {
        if (this == used->use_head)
            used->use_head = next_use;

        if (prev_use)
          prev_use->next_use = next_use;

        if (next_use)
          next_use->prev_use = prev_use;

        next_use = prev_use = 0;
        --used->myuse_count;
      }
  }

  jit_value *used;
  jit_use *next_use;
  jit_use *prev_use;
  jit_instruction *usr;
  size_t idx;
};

class
jit_instruction : public jit_value
{
public:
  // FIXME: this code could be so much pretier with varadic templates...
#define JIT_EXTRACT_ARG(idx) arguments[idx].stash_value (arg ## idx, this, idx)

  jit_instruction (void) : id (next_id ())
  {
  }

  jit_instruction (jit_value *arg0)
    : already_infered (1, reinterpret_cast<jit_type *>(0)), arguments (1), 
      id (next_id ())
  {
    JIT_EXTRACT_ARG (0);
  }

  jit_instruction (jit_value *arg0, jit_value *arg1)
    : already_infered (2, reinterpret_cast<jit_type *>(0)), arguments (2), 
      id (next_id ())
  {
    JIT_EXTRACT_ARG (0);
    JIT_EXTRACT_ARG (1);
  }

  jit_instruction (jit_value *arg0, jit_value *arg1, jit_value *arg2)
    : already_infered (3, reinterpret_cast<jit_type *>(0)), arguments (3), 
      id (next_id ())
  {
    JIT_EXTRACT_ARG (0);
    JIT_EXTRACT_ARG (1);
    JIT_EXTRACT_ARG (2);
  }

#undef JIT_EXTRACT_ARG

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
    return arguments[i].value ()->to_llvm ();
  }

  jit_type *argument_type (size_t i) const
  {
    return arguments[i].value ()->type ();
  }

  size_t argument_count (void) const
  {
    return arguments.size ();
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

  std::vector<jit_use> arguments; // DO NOT resize

  std::string mtag;
  size_t id;
};

class
jit_block : public jit_value
{
public:
  typedef std::list<jit_instruction *> instruction_list;
  typedef instruction_list::iterator iterator;
  typedef instruction_list::const_iterator const_iterator;

  jit_block (const std::string& n) : nm (n) {}

  virtual ~jit_block ()
  {
    for (instruction_list::iterator iter = instructions.begin ();
         iter != instructions.end (); ++iter)
      delete *iter;
  }

  const std::string& name (void) const { return nm; }

  jit_instruction *prepend (jit_instruction *instr)
  {
    instructions.push_front (instr);
    return instr;
  }

  jit_instruction *append (jit_instruction *instr)
  {
    instructions.push_back (instr);
    return instr;
  }

  iterator begin () { return instructions.begin (); }

  const_iterator begin () const { return instructions.begin (); }

  iterator end () { return instructions.end (); }

  const_iterator end () const { return instructions.begin (); }

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    print_indent (os, indent) << nm << ":" << std::endl;
    for (iterator iter = begin (); iter != end (); ++iter)
      {
        jit_instruction *instr = *iter;
        instr->print (os, indent + 1) << std::endl;
      }
    return os;
  }

  llvm::BasicBlock *to_llvm (void) const;

  JIT_VALUE_ACCEPT (block)
private:
  std::string nm;
  instruction_list instructions;
};

class jit_terminator : public jit_instruction
{
public:
  jit_terminator (jit_value *arg0) : jit_instruction (arg0) {}

  jit_terminator (jit_value *arg0, jit_value *arg1, jit_value *arg2)
    : jit_instruction (arg0, arg1, arg2) {}

  virtual jit_block *sucessor (size_t idx = 0) const = 0;

  llvm::BasicBlock *sucessor_llvm (size_t idx = 0) const
  {
    return sucessor (idx)->to_llvm ();
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
    return reinterpret_cast<jit_block *> (arg);
  }

  size_t sucessor_count (void) const { return 1; }

  virtual std::ostream& print (std::ostream& os, size_t indent)
  {
    jit_block *succ = sucessor ();
    return print_indent (os, indent) << "break: " << succ->name ();
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

  llvm::Value *cond_llvm (void) const
  {
    return cond ()->to_llvm ();
  }

  jit_block *sucessor (size_t idx) const
  {
    jit_value *arg = argument (idx + 1);
    return reinterpret_cast<jit_block *> (arg);
  }

  size_t sucessor_count (void) const { return 2; }

  JIT_VALUE_ACCEPT (cond_break)
};

class
jit_call : public jit_instruction
{
public:
  jit_call (const jit_function& afunction,
            jit_value *arg0) : jit_instruction (arg0), mfunction (afunction) {}

  jit_call (const jit_function& afunction,
            jit_value *arg0, jit_value *arg1) : jit_instruction (arg0, arg1),
                                                mfunction (afunction) {}

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
        jit_value *arg = argument (i);
        arg->short_print (os);
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

  typedef std::map<std::string, jit_value *> variable_map;
  variable_map variables;

  // used instead of return values from visit_* functions
  jit_value *result;

  jit_block *block;
  jit_block *entry_block;
  jit_block *final_block;

  llvm::Function *function;

  std::list<jit_block *> blocks;

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

  jit_const_scalar *get_scalar (double v)
  {
    jit_const_scalar *ret = new jit_const_scalar (v);
    constants.push_back (ret);
    return ret;
  }

  jit_const_string *get_string (const std::string& v)
  {
    jit_const_string *ret = new jit_const_string (v);
    constants.push_back (ret);
    return ret;
  }

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
    virtual void visit_ ## clname (jit_ ## clname&);

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
  };
};

class jit_info;

class
tree_jit
{
public:
  tree_jit (void);

  ~tree_jit (void);

  bool execute (tree& cmd);

  llvm::ExecutionEngine *get_engine (void) const { return engine; }

  llvm::Module *get_module (void) const { return module; }

  void optimize (llvm::Function *fn);
 private:
  bool initialize (void);

  // FIXME: Temorary hack to test
  typedef std::map<tree *, jit_info *> compiled_map;
  compiled_map compiled;

  llvm::LLVMContext &context;
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
