////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2021 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_pt_jit_h)
#define octave_pt_jit_h 1

#include "octave-config.h"

#if defined (HAVE_LLVM)

#include "jit-util.h"
#include "jit-typeinfo.h"
#include "jit-ir.h"
#include "pt-walk.h"
#include "symscope.h"

// octave_value_list is not (yet) in the octave namespace
class octave_value_list;

namespace octave
{
  namespace jit
  {
    typedef std::unique_ptr<llvm::Module> ModuleOwner;
    typedef std::unique_ptr<llvm::ExecutionEngine> EngineOwner;
  }

  // Convert from the parse tree (AST) to the low level Octave IR.
  class
  jit_convert : public tree_walker
  {
  public:

    typedef std::pair<jit_type *, std::string> type_bound;
    typedef std::vector<type_bound> type_bound_vector;
    typedef std::map<std::string, jit_variable *> variable_map;

    jit_convert (tree& tee, jit_type *for_bounds = nullptr);

    jit_convert (octave_user_function& fcn, const std::vector<jit_type *>& args);

    template <typename ...Args>
    jit_call * create_checked (const Args&... args)
    {
      jit_call *ret = m_factory.create<jit_call> (args...);
      return create_checked_impl (ret);
    }

    jit_block_list& get_blocks (void) { return m_blocks; }

    const type_bound_vector& get_bounds (void) const { return m_bounds; }

    jit_factory& get_factory (void) { return m_factory; }

    llvm::Function *get_function (void) const { return m_function; }

    const variable_map& get_variable_map (void) const { return m_vmap; }

    void visit_anon_fcn_handle (tree_anon_fcn_handle&);

    void visit_argument_list (tree_argument_list&);

    void visit_binary_expression (tree_binary_expression&);

    void visit_boolean_expression (tree_boolean_expression&);

    void visit_break_command (tree_break_command&);

    void visit_colon_expression (tree_colon_expression&);

    void visit_continue_command (tree_continue_command&);

    void visit_decl_command (tree_decl_command&);

    void visit_decl_init_list (tree_decl_init_list&);

    void visit_decl_elt (tree_decl_elt&);

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

    std::vector<std::pair<std::string, bool>> m_arguments;
    type_bound_vector m_bounds;

    bool m_converting_function;

    // the scope of the function we are converting, or the current scope
    symbol_scope m_scope;

    jit_factory m_factory;

    // used instead of return values from visit_* functions
    jit_value *m_result;

    jit_block *m_entry_block;

    jit_block *m_final_block;

    jit_block *m_block;

    llvm::Function *m_function;

    jit_block_list m_blocks;

    std::vector<jit_magic_end::context> m_end_context;

    std::size_t m_iterator_count;
    std::size_t m_for_bounds_count;
    std::size_t m_short_count;

    variable_map m_vmap;

    void initialize (const symbol_scope& s);

    jit_call * create_checked_impl (jit_call *ret);

    // get an existing vairable.  If the variable does not exist, it will not be
    // created
    jit_variable * find_variable (const std::string& vname) const;

    // get a variable, create it if it does not exist.  The type will default to
    // the variable's current type in the symbol table.
    jit_variable * get_variable (const std::string& vname);

    // create a variable of the given name and given type.  Will also insert an
    // extract statement
    jit_variable * create_variable (const std::string& vname, jit_type *type,
                                    bool isarg = true);

    // The name of the next for loop iterator.  If inc is false, then the
    // iterator counter will not be incremented.
    std::string next_iterator (bool inc = true)
    { return next_name ("#iter", m_iterator_count, inc); }

    std::string next_for_bounds (bool inc = true)
    { return next_name ("#for_bounds", m_for_bounds_count, inc); }

    std::string next_shortcircut_result (bool inc = true)
    { return next_name ("#shortcircut_result", m_short_count, inc); }

    std::string next_name (const char *prefix, std::size_t& count, bool inc);

    jit_instruction * resolve (tree_index_expression& exp,
                               jit_value *extra_arg = nullptr, bool lhs = false);

    jit_value * do_assign (tree_expression *exp, jit_value *rhs,
                           bool artificial = false);

    jit_value * do_assign (const std::string& lhs, jit_value *rhs, bool print,
                           bool artificial = false);

    jit_value * visit (tree *tee) { return visit (*tee); }

    jit_value * visit (tree& tee);

    typedef std::list<jit_block *> block_list;
    block_list m_breaks;
    block_list m_continues;

    void finish_breaks (jit_block *dest, const block_list& lst);
  };

  // Convert from the low level Octave IR to LLVM
  class
  jit_convert_llvm : public jit_ir_walker
  {
  public:

    llvm::Function * convert_loop (const jit_module& module,
                                   const jit_block_list& blocks,
                                   const std::list<jit_value *>& constants,
                                   const std::string& llvm_function_name);

    jit_function convert_function (const jit_module& module,
                                   const jit_block_list& blocks,
                                   const std::list<jit_value *>& constants,
                                   octave_user_function& fcn,
                                   const std::vector<jit_type *>& args);

    // arguments to the llvm::Function for loops
    const std::vector<std::pair<std::string, bool>>& get_arguments(void) const
    { return m_argument_vec; }

#define JIT_METH(clname)                        \
    virtual void visit (jit_ ## clname&);

    JIT_VISIT_IR_CLASSES;

#undef JIT_METH

  private:

    // name -> argument index (used for compiling functions)
    std::map<std::string, int> m_argument_index;

    std::vector<std::pair<std::string, bool>> m_argument_vec;

    // name -> llvm argument (used for compiling loops)
    std::map<std::string, llvm::Value *> m_arguments;

    bool m_converting_function;

    // only used if we are converting a function
    jit_function m_creating;

    llvm::Function *m_function;
    llvm::BasicBlock *m_prelude;

    void convert (const jit_block_list& blocks,
                  const std::list<jit_value *>& constants);

    void finish_phi (jit_phi *phi);

    void visit (jit_value *jvalue)
    {
      return visit (*jvalue);
    }

    void visit (jit_value& jvalue)
    {
      jvalue.accept (*this);
    }
  };

  // type inference and SSA construction on the low level Octave IR
  class
  jit_infer
  {
  public:

    typedef jit_convert::variable_map variable_map;

    jit_infer (jit_factory& afactory, jit_block_list& ablocks,
               const variable_map& avmap);

    jit_block_list& get_blocks (void) const { return m_blocks; }

    jit_factory& get_factory (void) const { return m_factory; }

    void infer (void);

  private:

    jit_block_list& m_blocks;
    jit_factory& m_factory;
    const variable_map& m_vmap;
    std::list<jit_instruction *> m_worklist;

    void append_users (jit_value *v);

    void append_users_term (jit_terminator *term);

    void construct_ssa (void);

    void do_construct_ssa (jit_block& block, std::size_t avisit_count);

    jit_block& entry_block (void) { return *m_blocks.front (); }

    jit_block& final_block (void) { return *m_blocks.back (); }

    void place_releases (void);

    void push_worklist (jit_instruction *instr);

    void remove_dead ();

    void release_dead_phi (jit_block& ablock);

    void release_temp (jit_block& ablock, std::set<jit_value *>& temp);

    void simplify_phi (void);

    void simplify_phi (jit_phi& phi);
  };


  class jit_module;

  class
  tree_jit
  {
    // ----- Constructor/destructor (singleton pattern) -----

  public:

    ~tree_jit (void);

  private:

    tree_jit (void);
    static tree_jit& instance (void);

    // ----- Initialization -----

  private:

    static bool initialized;
    bool do_initialize (void);

    // ----- LLVM context -----

  public:

    static llvm::LLVMContext llvm_context;

    // ----- Target machine ----

  public:

    static const llvm::TargetMachine* get_target_machine (void)
    { return instance ().target_machine; }

  private:

    llvm::TargetMachine *target_machine;

    // ----- Create LLVM modules and engines -----

  public:

    static jit::ModuleOwner
    open_new_module (const std::string& module_name = generate_unique_module_name ());

    static jit::EngineOwner
    create_new_engine (jit::ModuleOwner module_owner);

  private:

    jit::ModuleOwner
    do_open_new_module (const std::string& module_name) const;

    // ----- Registering JIT modules (module+engine pairs) -----

  public:

    static void register_jit_module (jit_module* jm)
    { instance ().do_register_jit_module (jm); }

    static void unregister_jit_module (jit_module* jm)
    { instance ().do_unregister_jit_module (jm); }

  private:

    // List of all currently registered jit modules
    std::list<jit_module*> jm_list;
    void do_register_jit_module (jit_module* jm);
    void do_unregister_jit_module (jit_module* jm);
    void do_dump_all_modules (void) const;

    // ----- Symbol resolution -----

  public:

    static void* getPointerToNamedFunction (const std::string &name)
    { return instance ().do_getPointerToNamedFunction (name); }

    static uint64_t getSymbolAddress (const std::string &name)
    { return instance ().do_getSymbolAddress (name); }

  private:

    void* do_getPointerToNamedFunction (const std::string &Name) const;

    uint64_t do_getSymbolAddress (const std::string &name) const;

    // ----- Generate unique identifiers -----

  public:

    static std::string generate_unique_forloop_name (void)
    {
      return std::string ("jittedForLoop")
             + std::to_string (next_forloop_number ++);
    }
    // FIXME: Check that the identifier does not exist

    static std::string generate_unique_function_name (void)
    {
      return std::string ("jittedFunction")
             + std::to_string (next_function_number ++);
    }
    // FIXME: Check that the identifier does not exist

    static std::string generate_unique_module_name (void)
    {
      return std::string ("octaveJITModule")
             + std::to_string (next_module_number ++);
    }
    // FIXME: Check that the identifier does not exist

  private:

    static int next_forloop_number;
    static int next_function_number;
    static int next_module_number;

    // ----- JIT and execute ASTs -----

  public:

    static bool execute (tree_simple_for_command& cmd,
                         const octave_value& bounds)
    { return instance ().do_execute (cmd, bounds); }

    static bool execute (tree_while_command& cmd)
    { return instance ().do_execute (cmd); }

    static bool execute (octave_user_function& fcn,
                         const octave_value_list& args,
                         octave_value_list& retval)
    { return instance ().do_execute (fcn, args, retval); }

  private:

    bool do_execute (tree_simple_for_command& cmd,
                     const octave_value& bounds);

    bool do_execute (tree_while_command& cmd);

    bool do_execute (octave_user_function& fcn,
                     const octave_value_list& args,
                     octave_value_list& retval);

    // ----- Miscellaneous -----

    bool enabled (void);

    std::size_t trip_count (const octave_value& bounds) const;
  };


  class
  jit_module
  {
    // TODO: Encapsulate all operations that can modify the module,
    //       and prevent them if the module has been finalized

    // TODO: Consider creating the engine at the end only?
    //       I have read somewhere that this is more efficient (nor sure)

  public:

    // Create an open module and associated JIT engine
    jit_module (const std::string& module_name
                = tree_jit::generate_unique_module_name ());

    // Delete the underlying JIT engine
    ~jit_module ();

    // Create an LLVM function in the module, with external linkage
    llvm::Function*
    create_llvm_function (llvm::FunctionType *ftype,
                          const llvm::Twine &name) const;

    // Create a global in the module, with external linkage
    llvm::GlobalVariable*
    create_global_variable (llvm::Type *type, bool is_constant,
                            const llvm::Twine& name) const;

    // Create or insert an LLVM Function declaration for an intrinsic,
    // and return it
    llvm::Function*
    get_intrinsic_declaration (std::size_t id,
                               std::vector<llvm::Type*> types) const;

    // Underlying type of enums defined in yet-inconplete types
    typedef unsigned llvm_gv_linkage;  // FIXME: autoconf this

    // add_global_mapping tells the execution engine where a specified
    // global value (variable or function) is
    template <typename ptr_type>
    void add_global_mapping (const llvm::GlobalValue* gv, ptr_type p) const
    {
      do_add_global_mapping (gv, reinterpret_cast<void *> (p));
    }

    // Return the address of the specified function.
    uint64_t getFunctionAddress (const std::string &name) const;

    // Optimize a function in the LLVM module
    void optimize (llvm::Function *fn) const;

    // FIXME: Once this has been called, we should not be able
    // to change anything in the module...
    void finalizeObject (void);

  private:

    void do_add_global_mapping (const llvm::GlobalValue* gv, void* p) const;

    llvm::Module *m_module;
    llvm::ExecutionEngine *m_engine;
  };


  class
  jit_info : public jit_module
  {
  public:

    // we use a pointer here so we don't have to include ov.h
    typedef std::map<std::string, const octave_value *> vmap;

    jit_info (tree& tee);

    jit_info (tree& tee, const octave_value& for_bounds);

    jit_info (tree_simple_for_command& tee, const octave_value& for_bounds);

    bool execute (const vmap& extra_vars = vmap ()) const;

    bool match (const vmap& extra_vars = vmap ()) const;

  private:

    typedef jit_convert::type_bound type_bound;
    typedef jit_convert::type_bound_vector type_bound_vector;
    typedef void (*jited_function)(octave_base_value**);

    void compile (tree& tee, jit_type *for_bounds = 0);

    octave_value find (const vmap& extra_vars, const std::string& vname) const;

    // LLVM function associated to this jit_info object
    std::string m_llvm_function_name;
    jited_function m_function;

    std::vector<std::pair<std::string, bool>> m_arguments;
    type_bound_vector m_bounds;
  };


  class
  jit_function_info : public jit_module
  {
  public:

    jit_function_info (octave_user_function& fcn,
                       const octave_value_list& ov_args);

    bool execute (const octave_value_list& ov_args,
                  octave_value_list& retval) const;

    bool match (const octave_value_list& ov_args) const;

  private:

    typedef octave_base_value *(*jited_function)(octave_base_value**);

    // LLVM function associated to this jit_info object
    std::string m_llvm_function_name;
    jited_function m_function;

    std::vector<jit_type *> m_argument_types;
  };
}

#endif

#endif
