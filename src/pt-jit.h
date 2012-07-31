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

#include "jit-ir.h"

#include "pt-walk.h"

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
// calls to jit_operations. This turns expressions like c = a + b into
// c = call binary+ (a, b)
// The jit_operations contain information about overloads for different types.
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

// convert between IRs
// FIXME: Class relationships are messy from here on down. They need to be
// cleaned up.
class
jit_convert : public tree_walker
{
public:
  typedef std::pair<jit_type *, std::string> type_bound;
  typedef std::vector<type_bound> type_bound_vector;

  jit_convert (llvm::Module *module, tree &tee, jit_type *for_bounds = 0);

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

#define DECL_ARG(n) const ARG ## n& arg ## n
#define JIT_CREATE(N)                                           \
  template <typename T, OCT_MAKE_DECL_LIST (typename, ARG, N)>  \
  T *create (OCT_MAKE_LIST (DECL_ARG, N))                       \
  {                                                             \
    T *ret = new T (OCT_MAKE_ARG_LIST (arg, N));                \
    track_value (ret);                                          \
    return ret;                                                 \
  }

  JIT_CREATE (1)
  JIT_CREATE (2)
  JIT_CREATE (3)
  JIT_CREATE (4)

#undef JIT_CREATE

#define JIT_CREATE_CHECKED(N)                                           \
  template <OCT_MAKE_DECL_LIST (typename, ARG, N)>                      \
  jit_call *create_checked (OCT_MAKE_LIST (DECL_ARG, N))                \
  {                                                                     \
    jit_call *ret = create<jit_call> (OCT_MAKE_ARG_LIST (arg, N));      \
    return create_checked_impl (ret);                                   \
  }

  JIT_CREATE_CHECKED (1)
  JIT_CREATE_CHECKED (2)
  JIT_CREATE_CHECKED (3)
  JIT_CREATE_CHECKED (4)

#undef JIT_CREATE_CHECKED
#undef DECL_ARG

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

  std::vector<jit_value *> end_context;

  size_t iterator_count;
  size_t for_bounds_count;
  size_t short_count;

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

  // get an existing vairable. If the variable does not exist, it will not be
  // created
  jit_variable *find_variable (const std::string& vname) const;

  // get a variable, create it if it does not exist. The type will default to
  // the variable's current type in the symbol table.
  jit_variable *get_variable (const std::string& vname);

  // create a variable of the given name and given type. Will also insert an
  // extract statement
  jit_variable *create_variable (const std::string& vname, jit_type *type);

  // The name of the next for loop iterator. If inc is false, then the iterator
  // counter will not be incremented.
  std::string next_iterator (bool inc = true)
  { return next_name ("#iter", iterator_count, inc); }

  std::string next_for_bounds (bool inc = true)
  { return next_name ("#for_bounds", for_bounds_count, inc); }

  std::string next_shortcircut_result (bool inc = true)
  { return next_name ("#shortcircut_result", short_count, inc); }

  std::string next_name (const char *prefix, size_t& count, bool inc);

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

  bool execute (tree_simple_for_command& cmd, const octave_value& bounds);

  bool execute (tree_while_command& cmd);

  llvm::ExecutionEngine *get_engine (void) const { return engine; }

  llvm::Module *get_module (void) const { return module; }

  void optimize (llvm::Function *fn);
 private:
  bool initialize (void);

  size_t trip_count (const octave_value& bounds) const;

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
  // we use a pointer here so we don't have to include ov.h
  typedef std::map<std::string, const octave_value *> vmap;

  jit_info (tree_jit& tjit, tree& tee);

  jit_info (tree_jit& tjit, tree& tee, const octave_value& for_bounds);

  ~jit_info (void);

  bool execute (const vmap& extra_vars = vmap ()) const;

  bool match (const vmap& extra_vars = vmap ()) const;
private:
  typedef jit_convert::type_bound type_bound;
  typedef jit_convert::type_bound_vector type_bound_vector;
  typedef void (*jited_function)(octave_base_value**);

  void initialize (tree_jit& tjit, jit_convert& conv);

  octave_value find (const vmap& extra_vars, const std::string& vname) const;

  llvm::ExecutionEngine *engine;
  jited_function function;
  llvm::Function *llvm_function;

  std::vector<std::pair<std::string, bool> > arguments;
  type_bound_vector bounds;
};

#endif
#endif
