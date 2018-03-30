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

#if ! defined (octave_jit_ir_h)
#define octave_jit_ir_h 1

#include "octave-config.h"

#if defined (HAVE_LLVM)

#include <list>
#include <stack>
#include <set>

#include "jit-typeinfo.h"

namespace octave
{

  // The low level octave JIT IR.  This ir is close to llvm, but
  // contains information for doing type inference.  We convert the
  // octave parse tree to this IR directly.

#define JIT_VISIT_IR_NOTEMPLATE                 \
  JIT_METH (block);                             \
  JIT_METH (branch);                            \
  JIT_METH (cond_branch);                       \
  JIT_METH (call);                              \
  JIT_METH (extract_argument);                  \
  JIT_METH (store_argument);                    \
  JIT_METH (return);                            \
  JIT_METH (phi);                               \
  JIT_METH (variable);                          \
  JIT_METH (error_check);                       \
  JIT_METH (assign)                             \
  JIT_METH (argument)                           \
  JIT_METH (magic_end)

#define JIT_VISIT_IR_CONST                      \
  JIT_METH (const_bool);                        \
  JIT_METH (const_scalar);                      \
  JIT_METH (const_complex);                     \
  JIT_METH (const_index);                       \
  JIT_METH (const_string);                      \
  JIT_METH (const_range)

#define JIT_VISIT_IR_CLASSES                    \
  JIT_VISIT_IR_NOTEMPLATE                       \
  JIT_VISIT_IR_CONST

  // forward declare all ir classes
#define JIT_METH(cname)                         \
  class jit_ ## cname;

  JIT_VISIT_IR_NOTEMPLATE

#undef JIT_METH

  // ABCs which aren't included in JIT_VISIT_IR_ALL
  class jit_instruction;
  class jit_terminator;

  template <typename T, jit_type *(*EXTRACT_T)(void), typename PASS_T = T,
            bool QUOTE=false>
  class jit_const;

  typedef jit_const<bool, jit_typeinfo::get_bool> jit_const_bool;
  typedef jit_const<double, jit_typeinfo::get_scalar> jit_const_scalar;
  typedef jit_const<Complex, jit_typeinfo::get_complex> jit_const_complex;
  typedef jit_const<octave_idx_type, jit_typeinfo::get_index> jit_const_index;

  typedef jit_const<std::string, jit_typeinfo::get_string, const std::string&,
                    true> jit_const_string;
  typedef jit_const<jit_range, jit_typeinfo::get_range, const jit_range&>
  jit_const_range;

  class jit_ir_walker;
  class jit_use;

  // Creates and tracks memory for jit_value and subclasses.
  // Memory managment is simple, all values that are created live as long as the
  // factory.
  class
  jit_factory
  {
    typedef std::list<jit_value *> value_list;

  public:

    ~jit_factory (void);

    const value_list& constants (void) const { return m_constants; }

    template <typename T, typename ...Args>
    T * create (const Args&... args)
    {
      T *ret = new T (args...);
      track_value (ret);
      return ret;
    }

  private:

    void track_value (jit_value *v);

    value_list m_all_values;

    value_list m_constants;
  };

  // A list of basic blocks (jit_block) which form some body of code.
  //
  // We do not directly inherit from std::list because we need to update the
  // blocks stashed location in push_back and insert.
  class
  jit_block_list
  {
  public:

    typedef std::list<jit_block *>::iterator iterator;
    typedef std::list<jit_block *>::const_iterator const_iterator;

    jit_block * back (void) const { return m_list.back (); }

    iterator begin (void) { return m_list.begin (); }

    const_iterator begin (void) const { return m_list.begin (); }

    iterator end (void)  { return m_list.end (); }

    const_iterator end (void) const  { return m_list.end (); }

    iterator erase (iterator iter) { return m_list.erase (iter); }

    jit_block * front (void) const { return m_list.front (); }

    void insert_after (iterator iter, jit_block *ablock);

    void insert_after (jit_block *loc, jit_block *ablock);

    void insert_before (iterator iter, jit_block *ablock);

    void insert_before (jit_block *loc, jit_block *ablock);

    void label (void);

    std::ostream& print (std::ostream& os, const std::string& header) const;

    std::ostream& print_dom (std::ostream& os) const;

    void push_back (jit_block *b);

  private:

    std::list<jit_block *> m_list;
  };

  std::ostream& operator<<(std::ostream& os, const jit_block_list& blocks);

  class
  jit_value : public jit_internal_list<jit_value, jit_use>
  {
  public:

    jit_value (void)
      : m_llvm_value (0), m_type (0), m_last_use (0), m_in_worklist (false)
    { }

    virtual ~jit_value (void);

    bool in_worklist (void) const
    {
      return m_in_worklist;
    }

    void stash_in_worklist (bool ain_worklist)
    {
      m_in_worklist = ain_worklist;
    }

    // The block of the first use which is not a jit_error_check
    // So this is not necessarily first_use ()->parent ().
    jit_block * first_use_block (void);

    // replace all uses with
    virtual void replace_with (jit_value *m_value);

    jit_type * type (void) const { return m_type; }

    llvm::Type * type_llvm (void) const
    {
      return m_type ? m_type->to_llvm () : nullptr;
    }

    const std::string& type_name (void) const
    {
      return m_type->name ();
    }

    void stash_type (jit_type *new_type) { m_type = new_type; }

    std::string print_string (void)
    {
      std::stringstream ss;
      print (ss);
      return ss.str ();
    }

    jit_instruction * last_use (void) const { return m_last_use; }

    void stash_last_use (jit_instruction *alast_use)
    {
      m_last_use = alast_use;
    }

    virtual bool needs_release (void) const { return false; }

    virtual std::ostream& print (std::ostream& os, size_t indent = 0) const = 0;

    virtual std::ostream& short_print (std::ostream& os) const
    { return print (os); }

    virtual void accept (jit_ir_walker& walker) = 0;

    bool has_llvm (void) const
    {
      return m_llvm_value;
    }

    llvm::Value * to_llvm (void) const
    {
      assert (m_llvm_value);
      return m_llvm_value;
    }

    void stash_llvm (llvm::Value *compiled)
    {
      m_llvm_value = compiled;
    }

  protected:

    std::ostream& print_indent (std::ostream& os, size_t indent = 0) const
    {
      for (size_t i = 0; i < indent * 8; ++i)
        os << ' ';
      return os;
    }

    llvm::Value *m_llvm_value;

  private:

    jit_type *m_type;
    jit_instruction *m_last_use;
    bool m_in_worklist;
  };

  std::ostream& operator<< (std::ostream& os, const jit_value& value);
  std::ostream& jit_print (std::ostream& os, jit_value *avalue);

  class
  jit_use : public jit_internal_node<jit_value, jit_use>
  {
  public:

    // some compilers don't allow us to use jit_internal_node without template
    // paremeters
    typedef jit_internal_node<jit_value, jit_use> PARENT_T;

    jit_use (void) : m_user (0), m_index (0) { }

    // we should really have a move operator, but not until c++11 :(
    jit_use (const jit_use& use) : m_user (0), m_index (0)
    {
      *this = use;
    }

    jit_use& operator= (const jit_use& use)
    {
      stash_value (use.value (), use.user (), use.index ());
      return *this;
    }

    size_t index (void) const { return m_index; }

    jit_instruction * user (void) const { return m_user; }

    jit_block * user_parent (void) const;

    std::list<jit_block *> user_parent_location (void) const;

    void stash_value (jit_value *avalue, jit_instruction *auser = nullptr,
                      size_t aindex = -1)
    {
      PARENT_T::stash_value (avalue);
      m_index = aindex;
      m_user = auser;
    }

  private:

    jit_instruction *m_user;
    size_t m_index;
  };

  class
  jit_instruction : public jit_value
  {
  public:

    // FIXME: this code could be so much pretier with varadic templates...
    jit_instruction (void)
      : m_id (next_id ()), m_parent (0)
    { }

    jit_instruction (size_t nargs)
      : m_id (next_id ()), m_parent (0)
    {
      m_already_infered.reserve (nargs);
      m_arguments.reserve (nargs);
    }

    template <typename ...Args>
    jit_instruction (jit_value * arg1, Args... other_args)
      : m_already_infered (1 + sizeof... (other_args)),
        m_arguments (1 + sizeof... (other_args)),
        m_id (next_id ()), m_parent (nullptr)
    {
      stash_argument (0, arg1, other_args...);
    }

    jit_instruction (const std::vector<jit_value *>& aarguments)
      : m_already_infered (aarguments.size ()), m_arguments (aarguments.size ()),
        m_id (next_id ()), m_parent (0)
    {
      for (size_t i = 0; i < aarguments.size (); ++i)
        stash_argument (i, aarguments[i]);
    }

    static void reset_ids (void)
    {
      next_id (true);
    }

    jit_value * argument (size_t i) const
    {
      return m_arguments[i].value ();
    }

    llvm::Value * argument_llvm (size_t i) const
    {
      assert (argument (i));
      return argument (i)->to_llvm ();
    }

    jit_type * argument_type (size_t i) const
    {
      return argument (i)->type ();
    }

    llvm::Type * argument_type_llvm (size_t i) const
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

    void stash_argument (size_t i, jit_value * arg)
    {
      m_arguments[i].stash_value (arg, this, i);
    }

    template <typename ...Args>
    void stash_argument (size_t i, jit_value * arg1, Args... aargs)
    {
      m_arguments[i].stash_value (arg1, this, i);
      stash_argument (++i, aargs...);
    }

    void push_argument (jit_value *arg)
    {
      m_arguments.push_back (jit_use ());
      stash_argument (m_arguments.size () - 1, arg);
      m_already_infered.push_back (0);
    }

    size_t argument_count (void) const
    {
      return m_arguments.size ();
    }

    void resize_arguments (size_t acount, jit_value *adefault = nullptr)
    {
      size_t old = m_arguments.size ();
      m_arguments.resize (acount);
      m_already_infered.resize (acount);

      if (adefault)
        for (size_t i = old; i < acount; ++i)
          stash_argument (i, adefault);
    }

    const std::vector<jit_use>& arguments (void) const { return m_arguments; }

    // argument types which have been infered already
    const std::vector<jit_type *>& argument_types (void) const
    { return m_already_infered; }

    virtual void push_variable (void) { }

    virtual void pop_variable (void) { }

    virtual void construct_ssa (void)
    {
      do_construct_ssa (0, argument_count ());
    }

    virtual bool infer (void) { return false; }

    void remove (void);

    virtual std::ostream& short_print (std::ostream& os) const;

    jit_block * parent (void) const { return m_parent; }

    std::list<jit_instruction *>::iterator location (void) const
    {
      return m_location;
    }

    llvm::BasicBlock * parent_llvm (void) const;

    void stash_parent (jit_block *aparent,
                       std::list<jit_instruction *>::iterator alocation)
    {
      m_parent = aparent;
      m_location = alocation;
    }

    size_t id (void) const { return m_id; }

  protected:

    // Do SSA replacement on arguments in [start, end)
    void do_construct_ssa (size_t start, size_t end);

    std::vector<jit_type *> m_already_infered;

  private:

    static size_t next_id (bool reset = false)
    {
      static size_t ret = 0;
      if (reset)
        return ret = 0;

      return ret++;
    }

    std::vector<jit_use> m_arguments;

    size_t m_id;
    jit_block *m_parent;
    std::list<jit_instruction *>::iterator m_location;
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

  template <typename T, jit_type *(*EXTRACT_T)(void), typename PASS_T, bool QUOTE>
  class
  jit_const : public jit_value
  {
  public:

    typedef PASS_T pass_t;

    jit_const (PASS_T avalue) : m_value (avalue)
    {
      stash_type (EXTRACT_T ());
    }

    PASS_T value (void) const { return m_value; }

    virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
    {
      print_indent (os, indent);
      jit_print (os, type ()) << ": ";
      if (QUOTE)
        os << '"';
      os << m_value;
      if (QUOTE)
        os << '"';
      return os;
    }

    JIT_VALUE_ACCEPT;

  private:

    T m_value;
  };

  class jit_phi_incoming;

  class
  jit_block : public jit_value, public jit_internal_list<jit_block,
                                                         jit_phi_incoming>
  {
    typedef jit_internal_list<jit_block, jit_phi_incoming> ILIST_T;

  public:

    typedef std::list<jit_instruction *> instruction_list;
    typedef instruction_list::iterator iterator;
    typedef instruction_list::const_iterator const_iterator;

    typedef std::set<jit_block *> df_set;
    typedef df_set::const_iterator df_iterator;

    static const size_t NO_ID = static_cast<size_t> (-1);

    jit_block (const std::string& aname, size_t avisit_count = 0)
      : m_visit_count (avisit_count), m_id (NO_ID), m_idom (0), m_name (aname),
        m_alive (false)
    { }

    virtual void replace_with (jit_value *value);

    void replace_in_phi (jit_block *ablock, jit_block *with);

    // we have a new internal list, but we want to stay compatible with jit_value
    jit_use * first_use (void) const { return jit_value::first_use (); }

    size_t use_count (void) const { return jit_value::use_count (); }

    // if a block is alive, then it might be visited during execution
    bool alive (void) const { return m_alive; }

    void mark_alive (void) { m_alive = true; }

    // If we can merge with a successor, do so and return the now empty block
    jit_block * maybe_merge ();

    // merge another block into this block, leaving the merge block empty
    void merge (jit_block& merge);

    const std::string& name (void) const { return m_name; }

    jit_instruction * prepend (jit_instruction *instr);

    jit_instruction * prepend_after_phi (jit_instruction *instr);

    template <typename T>
    T * append (T *instr)
    {
      internal_append (instr);
      return instr;
    }

    jit_instruction * insert_before (iterator loc, jit_instruction *instr);

    jit_instruction * insert_before (jit_instruction *loc, jit_instruction *instr)
    {
      return insert_before (loc->location (), instr);
    }

    jit_instruction * insert_after (iterator loc, jit_instruction *instr);

    jit_instruction * insert_after (jit_instruction *loc, jit_instruction *instr)
    {
      return insert_after (loc->location (), instr);
    }

    iterator remove (iterator iter)
    {
      jit_instruction *instr = *iter;
      iter = m_instructions.erase (iter);
      instr->stash_parent (0, m_instructions.end ());
      return iter;
    }

    jit_terminator * terminator (void) const;

    // is the jump from pred alive?
    bool branch_alive (jit_block *asucc) const;

    jit_block * successor (size_t i) const;

    size_t successor_count (void) const;

    iterator begin (void) { return m_instructions.begin (); }

    const_iterator begin (void) const { return m_instructions.begin (); }

    iterator end (void) { return m_instructions.end (); }

    const_iterator end (void) const { return m_instructions.end (); }

    iterator phi_begin (void);

    iterator phi_end (void);

    iterator nonphi_begin (void);

    // must label before id is valid
    size_t id (void) const { return m_id; }

    // dominance frontier
    const df_set& df (void) const { return m_df; }

    df_iterator df_begin (void) const { return m_df.begin (); }

    df_iterator df_end (void) const { return m_df.end (); }

    // label with a RPO walk
    void label (void)
    {
      size_t number = 0;
      label (m_visit_count, number);
    }

    void label (size_t avisit_count, size_t& number);

    // See for idom computation algorithm
    // Cooper, Keith D.; Harvey, Timothy J; and Kennedy, Ken (2001).
    // "A Simple, Fast Dominance Algorithm"
    void compute_idom (jit_block& entry_block)
    {
      bool changed;
      entry_block.m_idom = &entry_block;
      do
        changed = update_idom (m_visit_count);
      while (changed);
    }

    // compute dominance frontier
    void compute_df (void)
    {
      compute_df (m_visit_count);
    }

    void create_dom_tree (void)
    {
      create_dom_tree (m_visit_count);
    }

    jit_block * dom_successor (size_t idx) const
    {
      return m_dom_succ[idx];
    }

    size_t dom_successor_count (void) const
    {
      return m_dom_succ.size ();
    }

    // call pop_variable on all instructions
    void pop_all (void);

    virtual std::ostream& print (std::ostream& os, size_t indent = 0) const;

    jit_block * maybe_split (jit_factory& factory, jit_block_list& blocks,
                             jit_block *asuccessor);

    jit_block * maybe_split (jit_factory& factory, jit_block_list& blocks,
                             jit_block& asuccessor)
    {
      return maybe_split (factory, blocks, &asuccessor);
    }

    // print dominator infomration
    std::ostream& print_dom (std::ostream& os) const;

    virtual std::ostream& short_print (std::ostream& os) const
    {
      os << m_name;
      if (m_id != NO_ID)
        os << m_id;
      else
        os << '!';
      return os;
    }

    llvm::BasicBlock * to_llvm (void) const;

    std::list<jit_block *>::iterator location (void) const
    { return m_location; }

    void stash_location (std::list<jit_block *>::iterator alocation)
    { m_location = alocation; }

    // used to prevent visiting the same node twice in the graph
    size_t visit_count (void) const { return m_visit_count; }

    // check if this node has been visited yet at the given visit count.
    // If we have not been visited yet, mark us as visited.
    bool visited (size_t avisit_count)
    {
      if (m_visit_count <= avisit_count)
        {
          m_visit_count = avisit_count + 1;
          return false;
        }

      return true;
    }

    jit_instruction * front (void) { return m_instructions.front (); }

    jit_instruction * back (void) { return m_instructions.back (); }

    JIT_VALUE_ACCEPT;

  private:

    void internal_append (jit_instruction *instr);

    void compute_df (size_t avisit_count);

    bool update_idom (size_t avisit_count);

    void create_dom_tree (size_t avisit_count);

    static jit_block * idom_intersect (jit_block *i, jit_block *j);

    size_t m_visit_count;
    size_t m_id;
    jit_block *m_idom;
    df_set m_df;
    std::vector<jit_block *> m_dom_succ;
    std::string m_name;
    instruction_list m_instructions;
    bool m_alive;
    std::list<jit_block *>::iterator m_location;
  };

  // keeps track of phi functions that use a block on incoming edges
  class
  jit_phi_incoming : public jit_internal_node<jit_block, jit_phi_incoming>
  {
  public:

    jit_phi_incoming (void) : m_user (0) { }

    jit_phi_incoming (jit_phi *auser) : m_user (auser) { }

    jit_phi_incoming (const jit_phi_incoming& use)
    {
      *this = use;
    }

    jit_phi_incoming& operator= (const jit_phi_incoming& use)
    {
      stash_value (use.value ());
      m_user = use.m_user;
      return *this;
    }

    jit_phi * user (void) const { return m_user; }

    jit_block * user_parent (void) const;

  private:

    jit_phi *m_user;
  };

  // A non-ssa variable
  class
  jit_variable : public jit_value
  {
  public:

    jit_variable (const std::string& aname) : m_name (aname), m_last_use (0) { }

    const std::string& name (void) const { return m_name; }

    // manipulate the value_stack, for use during SSA construction.  The top of
    // the value stack represents the current value for this variable
    bool has_top (void) const
    {
      return ! value_stack.empty ();
    }

    jit_value * top (void) const
    {
      return value_stack.top ();
    }

    void push (jit_instruction *v)
    {
      value_stack.push (v);
      m_last_use = v;
    }

    void pop (void)
    {
      value_stack.pop ();
    }

    jit_instruction * last_use (void) const
    {
      return m_last_use;
    }

    void stash_last_use (jit_instruction *instr)
    {
      m_last_use = instr;
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
      return print_indent (os, indent) << m_name;
    }

    JIT_VALUE_ACCEPT;

  private:

    std::string m_name;
    std::stack<jit_value *> value_stack;
    jit_instruction *m_last_use;
  };

  class
  jit_assign_base : public jit_instruction
  {
  public:

    jit_assign_base (jit_variable *adest)
      : jit_instruction (), m_dest (adest)
    { }

    jit_assign_base (jit_variable *adest, size_t npred)
      : jit_instruction (npred), m_dest (adest)
    { }

    jit_assign_base (jit_variable *adest, jit_value *arg0, jit_value *arg1)
      : jit_instruction (arg0, arg1), m_dest (adest)
    { }

    jit_variable * dest (void) const { return m_dest; }

    virtual void push_variable (void)
    {
      m_dest->push (this);
    }

    virtual void pop_variable (void)
    {
      m_dest->pop ();
    }

    virtual std::ostream& short_print (std::ostream& os) const
    {
      if (type ())
        jit_print (os, type ()) << ": ";

      dest ()->short_print (os);
      return os << '#' << id ();
    }

  private:

    jit_variable *m_dest;
  };

  class
  jit_assign : public jit_assign_base
  {
  public:

    jit_assign (jit_variable *adest, jit_value *asrc)
      : jit_assign_base (adest, adest, asrc), m_artificial (false)
    { }

    jit_value * overwrite (void) const
    {
      return argument (0);
    }

    jit_value * src (void) const
    {
      return argument (1);
    }

    // Variables don't get modified in an SSA, but COW requires we
    // modify variables.  An artificial assign is for when a variable
    // gets modified.  We need an assign in the SSA, but the reference
    // counts shouldn't be updated.

    bool artificial (void) const { return m_artificial; }

    void mark_artificial (void) { m_artificial = true; }

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

    bool m_artificial;
  };

  class
  jit_phi : public jit_assign_base
  {
  public:

    jit_phi (jit_variable *adest, size_t npred)
      : jit_assign_base (adest, npred)
    {
      m_incoming.reserve (npred);
    }

    // removes arguments form dead incoming jumps
    bool prune (void);

    void add_incoming (jit_block *from, jit_value *value)
    {
      push_argument (value);
      m_incoming.push_back (jit_phi_incoming (this));
      m_incoming[m_incoming.size () - 1].stash_value (from);
    }

    jit_block * incoming (size_t i) const
    {
      return m_incoming[i].value ();
    }

    llvm::BasicBlock * incoming_llvm (size_t i) const
    {
      return incoming (i)->to_llvm ();
    }

    virtual void construct_ssa (void) { }

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

          os << *incoming (i) << " -> ";
          os << *argument (i);

          if (i + 1 < argument_count ())
            os << std::endl;
        }

      return os;
    }

    llvm::PHINode * to_llvm (void) const;

    JIT_VALUE_ACCEPT;

  private:

    std::vector<jit_phi_incoming> m_incoming;
  };

  class
  jit_terminator : public jit_instruction
  {
  public:

    template <typename ...Args>
    jit_terminator (size_t asuccessor_count, Args... args)
      : jit_instruction (args...),
        m_alive (asuccessor_count, false) { }

    jit_block * successor (size_t idx = 0) const
    {
      return static_cast<jit_block *> (argument (idx));
    }

    llvm::BasicBlock * successor_llvm (size_t idx = 0) const
    {
      return successor (idx)->to_llvm ();
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

    bool alive (size_t idx) const { return m_alive[idx]; }

    bool alive (int idx) const { return m_alive[idx]; }

    size_t successor_count (void) const { return m_alive.size (); }

    virtual bool infer (void);

    llvm::TerminatorInst * to_llvm (void) const;

  protected:

    virtual bool check_alive (size_t) const { return true; }

  private:

    std::vector<bool> m_alive;
  };

  class
  jit_branch : public jit_terminator
  {
  public:

    jit_branch (jit_block *succ) : jit_terminator (1, succ) { }

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
      : jit_terminator (2, ctrue, cfalse, c) { }

    jit_value * cond (void) const { return argument (2); }

    std::ostream& print_cond (std::ostream& os) const
    {
      return cond ()->short_print (os);
    }

    llvm::Value * cond_llvm (void) const
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

    jit_call (const jit_operation& (*aoperation) (void))
      : m_operation (aoperation ())
    {
      const jit_function& ol = overload ();
      if (ol.valid ())
        stash_type (ol.result ());
    }

    jit_call (const jit_operation& aoperation) : m_operation (aoperation)
    {
      const jit_function& ol = overload ();
      if (ol.valid ())
        stash_type (ol.result ());
    }

    template <typename ...Args>
    jit_call (const jit_operation& aoperation,
              jit_value * arg1, Args... other_args)
      : jit_instruction (arg1, other_args...), m_operation (aoperation)
  { }

    template <typename ...Args>
    jit_call (const jit_operation& (*aoperation) (void),
              jit_value * arg1, Args... other_args)
      : jit_instruction (arg1, other_args...), m_operation (aoperation ())
    { }

    jit_call (const jit_operation& aoperation,
              const std::vector<jit_value *>& args)
      : jit_instruction (args), m_operation (aoperation)
    { }

    const jit_operation& operation (void) const { return m_operation; }

    bool can_error (void) const
    {
      return overload ().can_error ();
    }

    const jit_function& overload (void) const
    {
      return m_operation.overload (argument_types ());
    }

    virtual bool needs_release (void) const;

    virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
    {
      print_indent (os, indent);

      if (use_count ())
        short_print (os) << " = ";
      os << "call " << m_operation.name () << " (";

      for (size_t i = 0; i < argument_count (); ++i)
        {
          print_argument (os, i);
          if (i + 1 < argument_count ())
            os << ", ";
        }
      return os << ')';
    }

    virtual bool infer (void);

    JIT_VALUE_ACCEPT;

  private:

    const jit_operation& m_operation;
  };

  // FIXME: This is just ugly...
  // checks error_state, if error_state is false then goto the normal branch,
  // otherwise goto the error branch
  class
  jit_error_check : public jit_terminator
  {
  public:

    // Which variable is the error check for?
    enum variable
      {
        var_error_state,
        var_interrupt
      };

    static std::string variable_to_string (variable v);

    jit_error_check (variable var, jit_call *acheck_for, jit_block *normal,
                     jit_block *error)
      : jit_terminator (2, error, normal, acheck_for), m_variable (var) { }

    jit_error_check (variable var, jit_block *normal, jit_block *error)
      : jit_terminator (2, error, normal), m_variable (var) { }

    variable check_variable (void) const { return m_variable; }

    bool has_check_for (void) const
    {
      return argument_count () == 3;
    }

    jit_call * check_for (void) const
    {
      assert (has_check_for ());
      return static_cast<jit_call *> (argument (2));
    }

    virtual std::ostream& print (std::ostream& os, size_t indent = 0) const;

    JIT_VALUE_ACCEPT;

  protected:

    virtual bool check_alive (size_t idx) const
    {
      if (! has_check_for ())
        return true;
      return idx == 1 ? true : check_for ()->can_error ();
    }

  private:

    variable m_variable;
  };

  // for now only handles the 1D case
  class
  jit_magic_end : public jit_instruction
  {
  public:

    class
    context
    {
    public:

      context (void) : m_value (0), m_index (0), m_count (0) { }

      context (jit_factory& factory, jit_value *avalue, size_t aindex,
               size_t acount);

      jit_value *m_value;
      jit_const_index *m_index;
      jit_const_index *m_count;
    };

    jit_magic_end (const std::vector<context>& full_context);

    virtual bool infer (void);

    const jit_function& overload () const;

    virtual std::ostream& print (std::ostream& os, size_t indent = 0) const;

    context resolve_context (void) const;

    virtual std::ostream& short_print (std::ostream& os) const
    {
      return os << "magic_end" << '#' << id ();
    }

    JIT_VALUE_ACCEPT;

  private:

    std::vector<context> m_contexts;
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

    const jit_function& overload (void) const
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
      : jit_instruction (var), m_dest (var)
    { }

    const std::string& name (void) const
    {
      return m_dest->name ();
    }

    const jit_function& overload (void) const
    {
      return jit_typeinfo::cast (jit_typeinfo::get_any (), result_type ());
    }

    jit_value * result (void) const
    {
      return argument (0);
    }

    jit_type * result_type (void) const
    {
      return result ()->type ();
    }

    llvm::Value * result_llvm (void) const
    {
      return result ()->to_llvm ();
    }

    virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
    {
      jit_value *res = result ();
      print_indent (os, indent) << "store ";
      m_dest->short_print (os);

      if (! isa<jit_variable> (res))
        {
          os << " = ";
          res->short_print (os);
        }

      return os;
    }

    JIT_VALUE_ACCEPT;

  private:

    jit_variable *m_dest;
  };

  class
  jit_return : public jit_instruction
  {
  public:

    jit_return (void) { }

    jit_return (jit_value *retval) : jit_instruction (retval) { }

    jit_value * result (void) const
    {
      return argument_count () ? argument (0) : nullptr;
    }

    jit_type * result_type (void) const
    {
      jit_value *res = result ();
      return res ? res->type () : nullptr;
    }

    virtual std::ostream& print (std::ostream& os, size_t indent = 0) const
    {
      print_indent (os, indent) << "return";

      if (result ())
        os << ' ' << *result ();

      return os;
    }

    JIT_VALUE_ACCEPT;
  };

  class
  jit_ir_walker
  {
  public:

    virtual ~jit_ir_walker (void) { }

#define JIT_METH(clname)                        \
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

#undef JIT_VALUE_ACCEPT
}

#endif

#endif
