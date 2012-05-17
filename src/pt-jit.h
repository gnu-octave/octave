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
// for loops with ranges compile. For example,
// for i=1:1000
//   result = i + 1;
// endfor
// Will compile. Nested for loops with constant bounds are also supported.
//
// TODO:
// 1. Cleanup
// 2. Support if statements
// 3. Support iteration over matricies
// 4. Check error state
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
  class GenericValue;
}

class octave_base_value;
class octave_value;
class tree;

// jit_range is compatable with the llvm range structure
struct
OCTINTERP_API
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
OCTINTERP_API
jit_type
{
public:
  jit_type (const std::string& n, bool fi, jit_type *mparent, llvm::Type *lt,
            int tid) :
    mname (n), finit (fi), p (mparent), llvm_type (lt), id (tid)
  {}

  // a user readable type name
  const std::string& name (void) const { return mname; }

  // do we need to initialize variables of this type, even if they are not
  // input arguments?
  bool force_init (void) const { return finit; }

  // a unique id for the type
  int type_id (void) const { return id; }

  // An abstract base type, may be null
  jit_type *parent (void) const { return p; }

  // convert to an llvm type
  llvm::Type *to_llvm (void) const { return llvm_type; }

  // how this type gets passed as a function argument
  llvm::Type *to_llvm_arg (void) const;
private:
  std::string mname;
  bool finit;
  jit_type *p;
  llvm::Type *llvm_type;
  int id;
};

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
private:
  Array<octave_idx_type> to_idx (const std::vector<jit_type*>& types) const;

  std::vector<Array<overload> > overloads;
};

// Get information and manipulate jit types.
class
OCTINTERP_API
jit_typeinfo
{
public:
  jit_typeinfo (llvm::Module *m, llvm::ExecutionEngine *e);

  jit_type *get_any (void) const { return any; }

  jit_type *get_scalar (void) const { return scalar; }

  llvm::Type *get_scalar_llvm (void) const { return scalar->to_llvm (); }

  jit_type *get_range (void) const { return range; }

  llvm::Type *get_range_llvm (void) const { return range->to_llvm (); }

  jit_type *get_bool (void) const { return boolean; }

  jit_type *get_index (void) const { return index; }

  llvm::Type *get_index_llvm (void) const { return index->to_llvm (); }

  jit_type *type_of (const octave_value& ov) const;

  const jit_function& binary_op (int op) const;

  const jit_function::overload& binary_op_overload (int op, jit_type *lhs,
                                                    jit_type *rhs) const
    {
      const jit_function& jf = binary_op (op);
      return jf.get_overload (lhs, rhs);
    }

  jit_type *binary_op_result (int op, jit_type *lhs, jit_type *rhs) const
    {
      const jit_function::overload& ol = binary_op_overload (op, lhs, rhs);
      return ol.result;
    }

  const jit_function::overload& assign_op (jit_type *lhs, jit_type *rhs) const;

  const jit_function::overload& print_value (jit_type *to_print) const;

  const jit_function::overload& get_simple_for_check (jit_type *bounds) const
  {
    return simple_for_check.get_overload (bounds, index);
  }

  const jit_function::overload& get_simple_for_index (jit_type *bounds) const
  {
    return simple_for_index.get_overload (bounds, index);
  }

  jit_type *get_simple_for_index_result (jit_type *bounds) const
  {
    const jit_function::overload& ol = get_simple_for_index (bounds);
    return ol.result;
  }

  // FIXME: generic creation should probably be handled seperatly
  void to_generic (jit_type *type, llvm::GenericValue& gv);
  void to_generic (jit_type *type, llvm::GenericValue& gv, octave_value ov);

  octave_value to_octave_value (jit_type *type, llvm::GenericValue& gv);

  void reset_generic (size_t nargs);
private:
  typedef std::map<std::string, jit_type *> type_map;

  jit_type *new_type (const std::string& name, bool force_init,
                      jit_type *parent, llvm::Type *llvm_type);

  void add_print (jit_type *ty, void *call);

  void add_binary_op (jit_type *ty, int op, int llvm_op);

  llvm::Module *module;
  llvm::ExecutionEngine *engine;
  int next_id;

  llvm::Type *ov_t;

  std::vector<jit_type*> id_to_type;
  jit_type *any;
  jit_type *scalar;
  jit_type *range;
  jit_type *boolean;
  jit_type *index;

  std::vector<jit_function> binary_ops;
  jit_function assign_fn;
  jit_function print_fn;
  jit_function simple_for_check;
  jit_function simple_for_incr;
  jit_function simple_for_index;

  size_t scalar_idx;
  std::vector<double> scalar_out;

  size_t ov_idx;
  std::vector<octave_base_value*> ov_out;

  size_t range_idx;
  std::vector<jit_range> range_out;
};

class
OCTINTERP_API
jit_infer : public tree_walker
{
  typedef std::map<std::string, jit_type *> type_map;
public:
  jit_infer (jit_typeinfo *ti) : tinfo (ti), is_lvalue (false),
                                  rvalue_type (0)
  {}

  const std::set<std::string>& get_argin () const { return argin; }

  const type_map& get_types () const { return types; }

  void infer (tree_simple_for_command& cmd, jit_type *bounds);

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
  void infer_simple_for (tree_simple_for_command& cmd,
                         jit_type *bounds);

  void handle_identifier (const std::string& name, octave_value v);

  jit_typeinfo *tinfo;

  bool is_lvalue;
  jit_type *rvalue_type;

  type_map types;
  std::set<std::string> argin;

  std::vector<jit_type *> type_stack;
};

class
OCTINTERP_API
jit_generator : public tree_walker
{
  typedef std::map<std::string, jit_type *> type_map;
public:
  jit_generator (jit_typeinfo *ti, llvm::Module *module, tree &tee,
                 const std::set<std::string>& argin,
                 const type_map& infered_types, bool have_bounds = true);

  llvm::Function *get_function () const { return function; }

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
  typedef std::pair<jit_type *, llvm::Value *> value;

  void emit_simple_for (tree_simple_for_command& cmd, value over,
                        bool atleast_once);

  void emit_print (const std::string& name, const value& v);

  void push_value (jit_type *type, llvm::Value *v)
  {
    value_stack.push_back (value (type, v));
  }

  jit_typeinfo *tinfo;
  llvm::Function *function;

  bool is_lvalue;
  std::map<std::string, value> variables;
  std::vector<value> value_stack;
};

class
OCTINTERP_API
tree_jit
{
public:
  tree_jit (void);

  ~tree_jit (void);

  bool execute (tree_simple_for_command& cmd, const octave_value& bounds);

  jit_typeinfo *get_typeinfo (void) const { return tinfo; }

  llvm::ExecutionEngine *get_engine (void) const { return engine; }

  llvm::Module *get_module (void) const { return module; }

  void optimize (llvm::Function *fn);
 private:
  bool initialize (void);

  llvm::LLVMContext &context;
  llvm::Module *module;
  llvm::PassManager *module_pass_manager;
  llvm::FunctionPassManager *pass_manager;
  llvm::ExecutionEngine *engine;

  jit_typeinfo *tinfo;
};

class
OCTINTERP_API
jit_info
{
public:
  jit_info (tree_jit& tjit, tree_simple_for_command& cmd, jit_type *bounds);

  bool execute (const octave_value& bounds) const;

  bool match (void) const;
private:
  typedef std::map<std::string, jit_type *> type_map;
  
  jit_typeinfo *tinfo;
  llvm::ExecutionEngine *engine;
  std::set<std::string> argin;
  type_map types;
  llvm::Function *function;
};

#endif
