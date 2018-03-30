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

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>
#include <sstream>
#include <iostream>

#include "bp-table.h"
#include "defun.h"
#include "errwarn.h"
#include "ov.h"
#include "pt-all.h"
#include "pt-jit.h"
#include "sighandlers.h"
#include "symtab.h"
#include "variables.h"
#include "interpreter-private.h"

// Programming Note: As of hg id 2b2c8ac44cd2, this file builds with
// LLVM 3.8 but not with 3.9 (or probably any later version).

#if defined (HAVE_LLVM)

#include <llvm/Analysis/CallGraph.h>
#include <llvm/Analysis/Passes.h>

#if defined (HAVE_LLVM_IR_VERIFIER_H)
#  include <llvm/IR/Verifier.h>
#else
#  include <llvm/Analysis/Verifier.h>
#endif

#if defined (HAVE_LLVM_ANALYSIS_BASICALIASANALYSIS_H)
// In LLVM 3.8.x and later, we use createBasicAAWrapperPass from:
#  include <llvm/Analysis/BasicAliasAnalysis.h>
#endif
// In LLVM 3.7.x and earlier, we use createBasicAliasAnalysisPass
// from llvm/Analysis/Passes.h (already included above)

#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
// #include <llvm/ExecutionEngine/JIT.h>  // old JIT, LLVM < 3.6.0
#include <llvm/ExecutionEngine/MCJIT.h>   // MCJIT, LLVM >= 3.0.0
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

#if defined (LEGACY_PASSMANAGER)
#  include <llvm/IR/LegacyPassManager.h>
#else
#  include <llvm/PassManager.h>
#endif

#if defined (HAVE_LLVM_IR_FUNCTION_H)
#  include <llvm/IR/LLVMContext.h>
#  include <llvm/IR/Module.h>
#  include <llvm/IR/Intrinsics.h>
#else
#  include <llvm/LLVMContext.h>
#  include <llvm/Module.h>
#  include <llvm/Intrinsics.h>
#endif

#if defined (HAVE_LLVM_SUPPORT_IRBUILDER_H)
#  include <llvm/Support/IRBuilder.h>
#elif defined(HAVE_LLVM_IR_IRBUILDER_H)
#  include <llvm/IR/IRBuilder.h>
#else
#  include <llvm/IRBuilder.h>
#endif

#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Support/TargetSelect.h>

#if defined (HAVE_LLVM_IR_DATALAYOUT_H)
#  include <llvm/IR/DataLayout.h>
#elif defined(HAVE_LLVM_DATALAYOUT_H)
#  include <llvm/DataLayout.h>
#else
#  include <llvm/Target/TargetData.h>
#endif

#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>

static bool Vdebug_jit = false;

static bool Vjit_enable = false;

static int Vjit_startcnt = 1000;

static int Vjit_failcnt = 0;

namespace octave
{
  static llvm::IRBuilder<> builder (llvm::getGlobalContext ());

  static llvm::LLVMContext& context = llvm::getGlobalContext ();

  // -------------------- jit_break_exception --------------------

  // jit_break is thrown whenever a branch we are converting has only breaks or
  // continues.  This is because all code that follows a break or continue
  // is dead.
  class jit_break_exception : public std::exception
  { };

  // -------------------- jit_convert --------------------
  jit_convert::jit_convert (tree& tee, jit_type *for_bounds)
    : m_converting_function (false)
  {
    initialize (__get_current_scope__ ("jit_convert::jit_convert"));

    if (for_bounds)
      create_variable (next_for_bounds (false), for_bounds);

    try
      {
        visit (tee);
      }
    catch (const jit_break_exception&)
      { }

    // breaks must have been handled by the top level loop
    assert (m_breaks.empty ());
    assert (m_continues.empty ());

    m_block->append (m_factory.create<jit_branch> (m_final_block));
    m_blocks.push_back (m_final_block);

    for (variable_map::iterator iter = m_vmap.begin (); iter != m_vmap.end (); ++iter)
      {
        jit_variable *var = iter->second;
        const std::string& name = var->name ();
        if (name.size () && name[0] != '#')
          m_final_block->append (m_factory.create<jit_store_argument> (var));
      }

    m_final_block->append (m_factory.create<jit_return> ());
  }

  jit_convert::jit_convert (octave_user_function& fcn,
                            const std::vector<jit_type *>& args)
    : m_converting_function (true)
  {
    initialize (fcn.scope ());

    tree_parameter_list *plist = fcn.parameter_list ();
    tree_parameter_list *rlist = fcn.return_list ();
    if (plist && plist->takes_varargs ())
      throw jit_fail_exception ("varags not supported");

    if (rlist && (rlist->size () > 1 || rlist->takes_varargs ()))
      throw jit_fail_exception ("multiple returns not supported");

    if (plist)
      {
        tree_parameter_list::iterator piter = plist->begin ();
        for (size_t i = 0; i < args.size (); ++i, ++piter)
          {
            if (piter == plist->end ())
              throw jit_fail_exception ("Too many parameter to function");

            tree_decl_elt *elt = *piter;
            std::string name = elt->name ();
            create_variable (name, args[i]);
          }
      }

    jit_value *return_value = nullptr;
    bool all_breaking = false;
    if (fcn.is_special_expr ())
      {
        tree_expression *expr = fcn.special_expr ();
        if (expr)
          {
            jit_variable *retvar = get_variable ("#return");
            jit_value *retval = nullptr;
            try
              {
                retval = visit (expr);
              }
            catch (const jit_break_exception&)
              { }

            if (m_breaks.size () || m_continues.size ())
              throw jit_fail_exception ("break/continue not supported in "
                                        "anonymous functions");

            m_block->append (m_factory.create<jit_assign> (retvar, retval));
            return_value = retvar;
          }
      }
    else
      {
        try
          {
            visit_statement_list (*fcn.body ());
          }
        catch (const jit_break_exception&)
          {
            all_breaking = true;
          }

        // the user may use break or continue to exit the function
        finish_breaks (m_final_block, m_continues);
        finish_breaks (m_final_block, m_breaks);
      }

    if (! all_breaking)
      m_block->append (m_factory.create<jit_branch> (m_final_block));

    m_blocks.push_back (m_final_block);
    m_block = m_final_block;

    if (! return_value && rlist && rlist->size () == 1)
      {
        tree_decl_elt *elt = rlist->front ();
        return_value = get_variable (elt->name ());
      }

    // FIXME: We should use live range analysis to delete variables where needed.
    // For now we just delete everything at the end of the function.
    for (variable_map::iterator iter = m_vmap.begin (); iter != m_vmap.end (); ++iter)
      {
        if (iter->second != return_value)
          {
            jit_call *call;
            call = m_factory.create<jit_call> (&jit_typeinfo::destroy,
                                             iter->second);
            m_final_block->append (call);
          }
      }

    if (return_value)
      m_final_block->append (m_factory.create<jit_return> (return_value));
    else
      m_final_block->append (m_factory.create<jit_return> ());
  }

  void
  jit_convert::visit_anon_fcn_handle (tree_anon_fcn_handle&)
  {
    throw jit_fail_exception ("No visit_anon_fcn_handle implementation");
  }

  void
  jit_convert::visit_argument_list (tree_argument_list&)
  {
    throw jit_fail_exception ("No visit_argument_list implementation");
  }

  void
  jit_convert::visit_binary_expression (tree_binary_expression& be)
  {
    tree_expression *lhs = be.lhs ();
    jit_value *lhsv = visit (lhs);

    tree_expression *rhs = be.rhs ();
    jit_value *rhsv = visit (rhs);

    const jit_operation& fn = jit_typeinfo::binary_op (be.op_type ());
    m_result = create_checked (fn, lhsv, rhsv);
  }

  void
  jit_convert::visit_boolean_expression (tree_boolean_expression& be)
  {
    bool is_and = be.op_type () == tree_boolean_expression::bool_and;

    std::string short_name = next_shortcircut_result ();
    jit_variable *short_result = m_factory.create<jit_variable> (short_name);
    m_vmap[short_name] = short_result;

    jit_block *done = m_factory.create<jit_block> (m_block->name ());
    tree_expression *lhs = be.lhs ();
    jit_value *lhsv = visit (lhs);
    lhsv = create_checked (&jit_typeinfo::logically_true, lhsv);

    jit_block *short_early = m_factory.create<jit_block> ("short_early");
    m_blocks.push_back (short_early);

    jit_block *short_cont = m_factory.create<jit_block> ("short_cont");

    if (is_and)
      m_block->append (m_factory.create<jit_cond_branch> (lhsv, short_cont,
                                                      short_early));
    else
      m_block->append (m_factory.create<jit_cond_branch> (lhsv, short_early,
                                                      short_cont));

    m_block = short_early;

    jit_value *early_result = m_factory.create<jit_const_bool> (! is_and);
    m_block->append (m_factory.create<jit_assign> (short_result, early_result));
    m_block->append (m_factory.create<jit_branch> (done));

    m_blocks.push_back (short_cont);
    m_block = short_cont;

    tree_expression *rhs = be.rhs ();
    jit_value *rhsv = visit (rhs);
    rhsv = create_checked (&jit_typeinfo::logically_true, rhsv);
    m_block->append (m_factory.create<jit_assign> (short_result, rhsv));
    m_block->append (m_factory.create<jit_branch> (done));

    m_blocks.push_back (done);
    m_block = done;
    m_result = short_result;
  }

  void
  jit_convert::visit_break_command (tree_break_command&)
  {
    m_breaks.push_back (m_block);
    throw jit_break_exception ();
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
      increment = m_factory.create<jit_const_scalar> (1);

    m_result = m_block->append (m_factory.create<jit_call> (jit_typeinfo::make_range,
                                                      base, limit, increment));
  }

  void
  jit_convert::visit_continue_command (tree_continue_command&)
  {
    m_continues.push_back (m_block);
    throw jit_break_exception ();
  }

  void
jit_convert::visit_decl_command (tree_decl_command&)
{
  throw jit_fail_exception ("No visit_decl_command implementation");
}

void
  jit_convert::visit_decl_elt (tree_decl_elt&)
  {
    throw jit_fail_exception ("No visit_decl_elt implementation");
  }

  void
  jit_convert::visit_decl_init_list (tree_decl_init_list&)
  {
    throw jit_fail_exception ("No visit_decl_init_list implementation");
  }

  void
  jit_convert::visit_simple_for_command (tree_simple_for_command& cmd)
  {
    // Note we do an initial check to see if the loop will run atleast once.
    // This allows us to get better type inference bounds on variables defined
    // and used only inside the for loop (e.g., the index variable)

    // If we are a nested for loop we need to store the previous breaks
    unwind_protect frame;
    frame.protect_var (m_breaks);
    frame.protect_var (m_continues);
    m_breaks.clear ();
    m_continues.clear ();

    // we need a variable for our iterator, because it is used in multiple blocks
    std::string iter_name = next_iterator ();
    jit_variable *iterator = m_factory.create<jit_variable> (iter_name);
    m_factory.create<jit_variable> (iter_name);
    m_vmap[iter_name] = iterator;

    jit_block *body = m_factory.create<jit_block> ("for_body");
    jit_block *tail = m_factory.create<jit_block> ("for_tail");

    // do control expression, iter init, and condition check in prev_block (block)
    // if we are the top level for loop, the bounds is an input argument.
    jit_value *control = find_variable (next_for_bounds ());
    if (! control)
      control = visit (cmd.control_expr ());
    jit_call *init_iter = m_factory.create<jit_call> (jit_typeinfo::for_init,
                                                    control);
    m_block->append (init_iter);
    m_block->append (m_factory.create<jit_assign> (iterator, init_iter));

    jit_call *check = m_factory.create<jit_call> (jit_typeinfo::for_check, control,
                                                iterator);
    m_block->append (check);
    m_block->append (m_factory.create<jit_cond_branch> (check, body, tail));

    m_blocks.push_back (body);
    m_block = body;

    // compute the syntactical iterator
    jit_call *idx_rhs = m_factory.create<jit_call> (jit_typeinfo::for_index,
                                                  control, iterator);
    m_block->append (idx_rhs);
    do_assign (cmd.left_hand_side (), idx_rhs);

    // do loop
    tree_statement_list *pt_body = cmd.body ();
    bool all_breaking = false;
    try
      {
        pt_body->accept (*this);
      }
    catch (const jit_break_exception&)
      {
        if (m_continues.empty ())
          {
            // WTF are you doing user? Every branch was a break, why did you have
            // a loop??? Users are silly people...
            finish_breaks (tail, m_breaks);
            m_blocks.push_back (tail);
            m_block = tail;
            return;
          }

        all_breaking = true;
      }

    // check our condition, continues jump to this block
    jit_block *check_block = m_factory.create<jit_block> ("for_check");
    m_blocks.push_back (check_block);

    jit_block *interrupt_check = m_factory.create<jit_block> ("for_interrupt");
    m_blocks.push_back (interrupt_check);

    if (! all_breaking)
      m_block->append (m_factory.create<jit_branch> (check_block));
    finish_breaks (check_block, m_continues);

    m_block = check_block;
    const jit_operation& add_fn = jit_typeinfo::binary_op (octave_value::op_add);
    jit_value *one = m_factory.create<jit_const_index> (1);
    jit_call *iter_inc = m_factory.create<jit_call> (add_fn, iterator, one);
    m_block->append (iter_inc);
    m_block->append (m_factory.create<jit_assign> (iterator, iter_inc));
    check = m_block->append (m_factory.create<jit_call> (jit_typeinfo::for_check,
                                                     control, iterator));
    m_block->append (m_factory.create<jit_cond_branch> (check, interrupt_check,
                                                    tail));

    m_block = interrupt_check;
    jit_error_check *ec
      = m_factory.create<jit_error_check> (jit_error_check::var_interrupt,
                                         body, m_final_block);
    m_block->append (ec);

    // breaks will go to our tail
    m_blocks.push_back (tail);
    finish_breaks (tail, m_breaks);
    m_block = tail;
  }

  void
  jit_convert::visit_complex_for_command (tree_complex_for_command&)
  {
    throw jit_fail_exception ("No visit_complex_for_command implementation");
  }

  void
  jit_convert::visit_octave_user_script (octave_user_script&)
  {
    throw jit_fail_exception ("No visit_octave_user_script implementation");
  }

  void
  jit_convert::visit_octave_user_function (octave_user_function&)
  {
    throw jit_fail_exception ("No visit_octave_user_function implementation");
  }

  void
  jit_convert::visit_octave_user_function_header (octave_user_function&)
  {
    throw jit_fail_exception ("No visit_octave_user_function_header implementation");
  }

  void
  jit_convert::visit_octave_user_function_trailer (octave_user_function&)
  {
    throw jit_fail_exception ("No visit_octave_user_function_trailer implementation");
  }

  void
  jit_convert::visit_function_def (tree_function_def&)
  {
    throw jit_fail_exception ("No visit_function_def implementation");
  }

  void
  jit_convert::visit_identifier (tree_identifier& ti)
  {
    if (ti.has_magic_end ())
      {
        if (! m_end_context.size ())
          throw jit_fail_exception ("Illegal end");
        m_result = m_block->append (m_factory.create<jit_magic_end> (m_end_context));
      }
    else
      {
        jit_variable *var = get_variable (ti.name ());
        jit_instruction *instr;
        instr = m_factory.create<jit_call> (&jit_typeinfo::grab, var);
        m_result = m_block->append (instr);
      }
  }

  void
  jit_convert::visit_if_clause (tree_if_clause&)
  {
    throw jit_fail_exception ("No visit_if_clause implementation");
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
    // the condition check for the ith clause.  For the else, it is simple the
    // else body.  If there is no else body, then it is padded with the tail.
    std::vector<jit_block *> entry_blocks (lst.size () + 1 - last_else);
    entry_blocks[0] = m_block;

    // we need to construct blocks first, because they have jumps to each other.
    tree_if_command_list::iterator iter = lst.begin ();
    ++iter;
    for (size_t i = 1; iter != lst.end (); ++iter, ++i)
      {
        tree_if_clause *tic = *iter;
        if (tic->is_else_clause ())
          entry_blocks[i] = m_factory.create<jit_block> ("else");
        else
          entry_blocks[i] = m_factory.create<jit_block> ("ifelse_cond");
      }

    jit_block *tail = m_factory.create<jit_block> ("if_tail");
    if (! last_else)
      entry_blocks[entry_blocks.size () - 1] = tail;

    // each branch in the if statement will have different breaks/continues
    block_list current_breaks = m_breaks;
    block_list current_continues = m_continues;
    m_breaks.clear ();
    m_continues.clear ();

    size_t num_incoming = 0; // number of incoming blocks to our tail
    iter = lst.begin ();
    for (size_t i = 0; iter != lst.end (); ++iter, ++i)
      {
        tree_if_clause *tic = *iter;
        m_block = entry_blocks[i];
        assert (m_block);

        if (i) // the first block is prev_block, so it has already been added
          m_blocks.push_back (entry_blocks[i]);

        if (! tic->is_else_clause ())
          {
            tree_expression *expr = tic->condition ();
            jit_value *cond = visit (expr);
            jit_call *check = create_checked (&jit_typeinfo::logically_true,
                                              cond);
            jit_block *body = m_factory.create<jit_block> (i == 0 ? "if_body"
                                                         : "ifelse_body");
            m_blocks.push_back (body);

            jit_instruction *br = m_factory.create<jit_cond_branch> (check, body,
                                                                   entry_blocks[i + 1]);
            m_block->append (br);
            m_block = body;
          }

        tree_statement_list *stmt_lst = tic->commands ();
        assert (stmt_lst); // jwe: Can this be null?

        try
          {
            stmt_lst->accept (*this);
            ++num_incoming;
            m_block->append (m_factory.create<jit_branch> (tail));
          }
        catch (const jit_break_exception&)
          { }

        current_breaks.splice (current_breaks.end (), m_breaks);
        current_continues.splice (current_continues.end (), m_continues);
      }

    m_breaks.splice (m_breaks.end (), current_breaks);
    m_continues.splice (m_continues.end (), current_continues);

    if (num_incoming || ! last_else)
      {
        m_blocks.push_back (tail);
        m_block = tail;
      }
    else
      // every branch broke, so we don't have a tail
      throw jit_break_exception ();
  }

  void
  jit_convert::visit_index_expression (tree_index_expression& exp)
  {
    m_result = resolve (exp);
  }

  void
  jit_convert::visit_matrix (tree_matrix&)
  {
    throw jit_fail_exception ("No visit_matrix implementation");
  }

  void
  jit_convert::visit_cell (tree_cell&)
  {
    throw jit_fail_exception ("No visit_cell implementation");
  }

  void
  jit_convert::visit_multi_assignment (tree_multi_assignment&)
  {
    throw jit_fail_exception ("No visit_multi_assignment implementation");
  }

  void
  jit_convert::visit_no_op_command (tree_no_op_command&)
  {
    throw jit_fail_exception ("No visit_no_op_command implementation");
  }

  void
  jit_convert::visit_constant (tree_constant& tc)
  {
    octave_value v = tc.value ();

    jit_type *ty = jit_typeinfo::type_of (v);

    if (ty == jit_typeinfo::get_scalar ())
      {
        double dv = v.double_value ();
        m_result = m_factory.create<jit_const_scalar> (dv);
      }
    else if (ty == jit_typeinfo::get_range ())
      {
        Range rv = v.range_value ();
        m_result = m_factory.create<jit_const_range> (rv);
      }
    else if (ty == jit_typeinfo::get_complex ())
      {
        Complex cv = v.complex_value ();
        m_result = m_factory.create<jit_const_complex> (cv);
      }
    else
      throw jit_fail_exception ("Unknown constant");
  }

  void
  jit_convert::visit_fcn_handle (tree_fcn_handle&)
  {
    throw jit_fail_exception ("No visit_fcn_handle implementation");
  }

  void
  jit_convert::visit_funcall (tree_funcall&)
  {
    throw jit_fail_exception ();
  }

  void
  jit_convert::visit_parameter_list (tree_parameter_list&)
  {
    throw jit_fail_exception ("No visit_parameter_list implementation");
  }

  void
  jit_convert::visit_postfix_expression (tree_postfix_expression& tpe)
  {
    octave_value::unary_op etype = tpe.op_type ();
    tree_expression *operand = tpe.operand ();
    jit_value *operandv = visit (operand);

    const jit_operation& fn = jit_typeinfo::unary_op (etype);
    m_result = create_checked (fn, operandv);

    if (etype == octave_value::op_incr || etype == octave_value::op_decr)
      {
        jit_value *ret = create_checked (&jit_typeinfo::grab, operandv);
        do_assign (operand, m_result);
        m_result = ret;
      }
  }

  void
  jit_convert::visit_prefix_expression (tree_prefix_expression& tpe)
  {
    octave_value::unary_op etype = tpe.op_type ();
    tree_expression *operand = tpe.operand ();
    const jit_operation& fn = jit_typeinfo::unary_op (etype);
    m_result = create_checked (fn, visit (operand));

    if (etype == octave_value::op_incr || etype == octave_value::op_decr)
      do_assign (operand, m_result);
  }

  void
  jit_convert::visit_return_command (tree_return_command&)
  {
    throw jit_fail_exception ("No visit_return_command implementation");
  }

  void
  jit_convert::visit_return_list (tree_return_list&)
  {
    throw jit_fail_exception ("No visit_return_list implementation");
  }

  void
  jit_convert::visit_simple_assignment (tree_simple_assignment& tsa)
  {
    tree_expression *rhs = tsa.right_hand_side ();
    jit_value *rhsv = visit (rhs);
    octave_value::assign_op op = tsa.op_type ();

    if (op != octave_value::op_asn_eq)
      {
        // Do the equivalent binary operation, then assign.
        // This is always correct, but it isn't always optimal.
        tree_expression *lhs = tsa.left_hand_side ();
        jit_value *lhsv = visit (lhs);
        octave_value::binary_op bop = octave_value::assign_op_to_binary_op (op);
        const jit_operation& fn = jit_typeinfo::binary_op (bop);
        rhsv = create_checked (fn, lhsv, rhsv);
      }

    m_result = do_assign (tsa.left_hand_side (), rhsv);
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
        // stolen from octave::tree_evaluator::visit_statement
        bool do_bind_ans = false;

        if (expr->is_identifier ())
          {
            tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

            do_bind_ans = (! id->is_variable (m_scope.current_context ()));
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
            const jit_operation& fn = jit_typeinfo::print_value ();
            jit_const_string *name = m_factory.create<jit_const_string>
              (expr->name ());
            m_block->append (m_factory.create<jit_call> (fn, name, expr_result));
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
      }
  }

  void
  jit_convert::visit_switch_case (tree_switch_case&)
  {
    throw jit_fail_exception ("No visit_switch_case implementation");
  }

  void
  jit_convert::visit_switch_case_list (tree_switch_case_list&)
  {
    throw jit_fail_exception ("No visit_switch_case_list implementation");
  }

  void
  jit_convert::visit_switch_command (tree_switch_command& cmd)
  {
    tree_switch_case_list *lst = cmd.case_list ();

    // always visit switch expression
    tree_expression *expr = cmd.switch_value ();
    assert (expr && "Switch value can not be null");
    jit_value *value = visit (expr);
    assert (value);

    size_t case_blocks_num = lst->size ();

    if (! case_blocks_num)  // there's nothing to do
      return;

    // check for otherwise, it's interpreted as last 'else' condition
    size_t has_otherwise = 0;
    tree_switch_case *last = lst->back ();
    if (last->is_default_case ())
      has_otherwise = 1;

    std::vector<jit_block *> entry_blocks (case_blocks_num + 1 - has_otherwise);

    // the first entry point is always the actual block.  Afterward, new blocks
    // are created for every case and the otherwise branch
    entry_blocks[0] = m_block;
    for (size_t i = 1; i < case_blocks_num; ++i)
      entry_blocks[i] = m_factory.create<jit_block> ("case_cond");

    jit_block *tail = m_factory.create<jit_block> ("switch_tail");

    // if there's no otherwise branch, the 'else' of the last branch
    // has to point to the tail
    if (! has_otherwise)
      entry_blocks[entry_blocks.size()-1] = tail;

    // each branch in the case statement will have different breaks/continues
    block_list current_breaks = m_breaks;
    block_list current_continues = m_continues;
    m_breaks.clear ();
    m_continues.clear ();

    size_t num_incoming = 0; // number of incoming blocks to our tail

    tree_switch_case_list::iterator iter = lst->begin ();
    for (size_t i = 0; i < case_blocks_num; ++iter, ++i)
      {
        tree_switch_case *twc = *iter;
        m_block = entry_blocks[i]; // case_cond
        assert (m_block);

        if (i)
          m_blocks.push_back (entry_blocks[i]);  // first block already pushed

        if (! twc->is_default_case ())
          {
            // compare result of switch expression with actual case label
            tree_expression *te = twc->case_label ();
            jit_value *label = visit (te);
            assert(label);

            const jit_operation& fn = jit_typeinfo::binary_op (octave_value::op_eq);
            jit_value *cond = create_checked (fn, value, label);
            assert(cond);

            jit_call *check = create_checked (&jit_typeinfo::logically_true,
                                              cond);

            jit_block *body = m_factory.create<jit_block> ("case_body");
            m_blocks.push_back (body);

            m_block->append (m_factory.create<jit_cond_branch> (check, body,
                                                            entry_blocks[i+1]));
            m_block = body; // case_body
          }

        tree_statement_list *stmt_lst = twc->commands ();
        assert(stmt_lst);

        try
          {
            stmt_lst->accept (*this);
            num_incoming++;
            m_block->append (m_factory.create<jit_branch> (tail));
          }
        catch (const jit_break_exception&)
          { }

        // each branch in the case statement will have different breaks/continues
        current_breaks.splice (current_breaks.end (), m_breaks);
        current_continues.splice (current_continues.end (), m_continues);
      }

    // each branch in the case statement will have different breaks/continues
    m_breaks.splice (m_breaks.end (), current_breaks);
    m_continues.splice (m_continues.end (), current_continues);

    if (num_incoming || ! has_otherwise)
      {
        m_blocks.push_back (tail);
        m_block = tail; // switch_tail
      }
    else
      throw jit_break_exception ();   // every branch broke
  }

  void
  jit_convert::visit_try_catch_command (tree_try_catch_command&)
  {
    throw jit_fail_exception ("No visit_try_catch_command implementation");
  }

  void
  jit_convert::visit_unwind_protect_command (tree_unwind_protect_command&)
  {
    throw jit_fail_exception ("No visit_unwind_protect_command implementation");
  }

  void
  jit_convert::visit_while_command (tree_while_command& wc)
  {
    unwind_protect frame;
    frame.protect_var (m_breaks);
    frame.protect_var (m_continues);
    m_breaks.clear ();
    m_continues.clear ();

    jit_block *cond_check = m_factory.create<jit_block> ("while_cond_check");
    m_block->append (m_factory.create<jit_branch> (cond_check));
    m_blocks.push_back (cond_check);
    m_block = cond_check;

    tree_expression *expr = wc.condition ();
    assert (expr && "While expression can not be null");
    jit_value *check = visit (expr);
    check = create_checked (&jit_typeinfo::logically_true, check);

    jit_block *body = m_factory.create<jit_block> ("while_body");
    m_blocks.push_back (body);

    jit_block *tail = m_factory.create<jit_block> ("while_tail");
    m_block->append (m_factory.create<jit_cond_branch> (check, body, tail));
    m_block = body;

    tree_statement_list *loop_body = wc.body ();
    bool all_breaking = false;
    if (loop_body)
      {
        try
          {
            loop_body->accept (*this);
          }
        catch (const jit_break_exception&)
          {
            all_breaking = true;
          }
      }

    finish_breaks (tail, m_breaks);

    if (! all_breaking || m_continues.size ())
      {
        jit_block *interrupt_check
          = m_factory.create<jit_block> ("interrupt_check");
        m_blocks.push_back (interrupt_check);
        finish_breaks (interrupt_check, m_continues);
        if (! all_breaking)
          m_block->append (m_factory.create<jit_branch> (interrupt_check));

        m_block = interrupt_check;
        jit_error_check *ec
          = m_factory.create<jit_error_check> (jit_error_check::var_interrupt,
                                             cond_check, m_final_block);
        m_block->append (ec);
      }

    m_blocks.push_back (tail);
    m_block = tail;
  }

  void
  jit_convert::visit_do_until_command (tree_do_until_command& duc)
  {
    unwind_protect frame;
    frame.protect_var (m_breaks);
    frame.protect_var (m_continues);
    m_breaks.clear ();
    m_continues.clear ();

    jit_block *body = m_factory.create<jit_block> ("do_until_body");
    jit_block *cond_check = m_factory.create<jit_block> ("do_until_cond_check");
    jit_block *tail = m_factory.create<jit_block> ("do_until_tail");

    m_block->append (m_factory.create<jit_branch> (body));
    m_blocks.push_back (body);
    m_block = body;

    tree_statement_list *loop_body = duc.body ();
    bool all_breaking = false;
    if (loop_body)
      {
        try
          {
            loop_body->accept (*this);
          }
        catch (const jit_break_exception&)
          {
            all_breaking = true;
          }
      }

    finish_breaks (tail, m_breaks);

    if (! all_breaking || m_continues.size ())
      {
        jit_block *interrupt_check
          = m_factory.create<jit_block> ("interrupt_check");
        m_blocks.push_back (interrupt_check);
        finish_breaks (interrupt_check, m_continues);
        if (! all_breaking)
          m_block->append (m_factory.create<jit_branch> (interrupt_check));

        m_block = interrupt_check;
        jit_error_check *ec
          = m_factory.create<jit_error_check> (jit_error_check::var_interrupt,
                                             cond_check, m_final_block);
        m_block->append (ec);

        m_blocks.push_back (cond_check);
        m_block = cond_check;

        tree_expression *expr = duc.condition ();
        assert (expr && "Do-Until expression can not be null");
        jit_value *check = visit (expr);
        check = create_checked (&jit_typeinfo::logically_true, check);

        m_block->append (m_factory.create<jit_cond_branch> (check, tail, body));
      }

    m_blocks.push_back (tail);
    m_block = tail;
  }

  void
  jit_convert::initialize (const symbol_scope& s)
  {
    m_scope = s;
    m_iterator_count = 0;
    m_for_bounds_count = 0;
    m_short_count = 0;
    jit_instruction::reset_ids ();

    m_entry_block = m_factory.create<jit_block> ("body");
    m_final_block = m_factory.create<jit_block> ("final");
    m_blocks.push_back (m_entry_block);
    m_entry_block->mark_alive ();
    m_block = m_entry_block;
  }

  jit_call *
  jit_convert::create_checked_impl (jit_call *ret)
  {
    m_block->append (ret);

    jit_block *normal = m_factory.create<jit_block> (m_block->name ());
    jit_error_check *check
      = m_factory.create<jit_error_check> (jit_error_check::var_error_state, ret,
                                         normal, m_final_block);
    m_block->append (check);
    m_blocks.push_back (normal);
    m_block = normal;

    return ret;
  }

  jit_variable *
  jit_convert::find_variable (const std::string& vname) const
  {
    variable_map::const_iterator iter;
    iter = m_vmap.find (vname);
    return iter != m_vmap.end () ? iter->second : nullptr;
  }

  jit_variable *
  jit_convert::get_variable (const std::string& vname)
  {
    jit_variable *ret = find_variable (vname);
    if (ret)
      return ret;

    symbol_table& symtab = __get_symbol_table__ ("jit_convert::find_variable");

    symbol_record record = symtab.find_symbol (vname, m_scope);
    if (record.is_persistent () || record.is_global ())
      throw jit_fail_exception ("Persistent and global not yet supported");

    if (m_converting_function)
      return create_variable (vname, jit_typeinfo::get_any (), false);
    else
      {
        octave_value val = record.varval (m_scope.current_context ());
        if (val.is_undefined ())
          val = symtab.find_function (vname);

        jit_type *type = jit_typeinfo::type_of (val);
        m_bounds.push_back (type_bound (type, vname));

        return create_variable (vname, type);
      }
  }

  jit_variable *
  jit_convert::create_variable (const std::string& vname, jit_type *type,
                                bool isarg)
  {
    jit_variable *var = m_factory.create<jit_variable> (vname);

    if (isarg)
      {
        jit_extract_argument *extract;
        extract = m_factory.create<jit_extract_argument> (type, var);
        m_entry_block->prepend (extract);
      }
    else
      {
        jit_call *init = m_factory.create<jit_call> (&jit_typeinfo::create_undef);
        jit_assign *assign = m_factory.create<jit_assign> (var, init);
        m_entry_block->prepend (assign);
        m_entry_block->prepend (init);
      }

    return m_vmap[vname] = var;
  }

  std::string
  jit_convert::next_name (const char *prefix, size_t& count, bool inc)
  {
    std::stringstream ss;
    ss << prefix << count;
    if (inc)
      ++count;
    return ss.str ();
  }

  jit_instruction *
  jit_convert::resolve (tree_index_expression& exp, jit_value *extra_arg,
                        bool lhs)
  {
    std::string type = exp.type_tags ();
    if (! (type.size () == 1 && type[0] == '('))
      throw jit_fail_exception ("Unsupported index operation");

    std::list<tree_argument_list *> args = exp.arg_lists ();
    if (args.size () != 1)
      throw jit_fail_exception ("Bad number of arguments in "
                                "tree_index_expression");

    tree_argument_list *arg_list = args.front ();
    if (! arg_list)
      throw jit_fail_exception ("null argument list");

    if (arg_list->size () < 1)
      throw jit_fail_exception ("Empty arg_list");

    tree_expression *tree_object = exp.expression ();
    jit_value *object;
    if (lhs)
      {
        tree_identifier *id = dynamic_cast<tree_identifier *> (tree_object);
        if (! id)
          throw jit_fail_exception ("expected identifier");
        object = get_variable (id->name ());
      }
    else
      object = visit (tree_object);

    size_t narg = arg_list->size ();
    tree_argument_list::iterator iter = arg_list->begin ();
    bool have_extra = extra_arg;
    std::vector<jit_value *> call_args (narg + 1 + have_extra);
    call_args[0] = object;

    for (size_t idx = 0; iter != arg_list->end (); ++idx, ++iter)
      {
        unwind_protect frame;
        frame.add_method (&m_end_context,
                          &std::vector<jit_magic_end::context>::pop_back);

        jit_magic_end::context ctx (m_factory, object, idx, narg);
        m_end_context.push_back (ctx);
        call_args[idx + 1] = visit (*iter);
      }

    if (extra_arg)
      call_args[call_args.size () - 1] = extra_arg;

    const jit_operation& fres = (lhs ? jit_typeinfo::paren_subsasgn ()
                                 : jit_typeinfo::paren_subsref ());

    return create_checked (fres, call_args);
  }

  jit_value *
  jit_convert::do_assign (tree_expression *exp, jit_value *rhs, bool artificial)
  {
    if (! exp)
      throw jit_fail_exception ("NULL lhs in assign");

    if (isa<tree_identifier> (exp))
      return do_assign (exp->name (), rhs, exp->print_result (), artificial);
    else if (tree_index_expression *idx
             = dynamic_cast<tree_index_expression *> (exp))
      {
        jit_value *new_object = resolve (*idx, rhs, true);
        do_assign (idx->expression (), new_object, true);

        // FIXME: Will not work for values that must be release/grabed
        return rhs;
      }
    else
      throw jit_fail_exception ("Unsupported assignment");
  }

  jit_value *
  jit_convert::do_assign (const std::string& lhs, jit_value *rhs,
                          bool print, bool artificial)
  {
    jit_variable *var = get_variable (lhs);
    jit_assign *assign = m_block->append (m_factory.create<jit_assign> (var, rhs));

    if (artificial)
      assign->mark_artificial ();

    if (print)
      {
        const jit_operation& print_fn = jit_typeinfo::print_value ();
        jit_const_string *name = m_factory.create<jit_const_string> (lhs);
        m_block->append (m_factory.create<jit_call> (print_fn, name, var));
      }

    return var;
  }

  jit_value *
  jit_convert::visit (tree& tee)
  {
    unwind_protect frame;
    frame.protect_var (m_result);

    tee.accept (*this);
    return m_result;
  }

  void
  jit_convert::finish_breaks (jit_block *dest, const block_list& lst)
  {
    for (block_list::const_iterator iter = lst.begin (); iter != lst.end ();
         ++iter)
      {
        jit_block *b = *iter;
        b->append (m_factory.create<jit_branch> (dest));
      }
  }

  // -------------------- jit_convert_llvm --------------------
  llvm::Function *
  jit_convert_llvm::convert_loop (const jit_module& module,
                                  const jit_block_list& blocks,
                                  const std::list<jit_value *>& constants,
                                  const std::string& llvm_function_name)
  {
    m_converting_function = false;

    // for now just init arguments from entry, later we will have to do something
    // more interesting
    jit_block *m_entry_block = blocks.front ();
    for (jit_block::iterator iter = m_entry_block->begin ();
         iter != m_entry_block->end (); ++iter)
      if (jit_extract_argument *extract
          = dynamic_cast<jit_extract_argument *> (*iter))
        m_argument_vec.push_back (std::make_pair (extract->name (), true));

    jit_type *any = jit_typeinfo::get_any ();

    // argument is an array of octave_base_value*, or octave_base_value**
    llvm::Type *arg_type = any->to_llvm (); // this is octave_base_value*
    llvm::FunctionType *ft;
    ft = llvm::FunctionType::get (llvm::Type::getVoidTy (context),
                                  arg_type->getPointerTo (), false);

    m_function = module.create_llvm_function (ft, llvm_function_name);
    try
      {
        m_prelude = llvm::BasicBlock::Create (context, "prelude", m_function);
        builder.SetInsertPoint (m_prelude);

        // The jitted function will have only one function argument, of octave_base_value** type
        llvm::Value *arg = &*(m_function->arg_begin ());

        for (size_t i = 0; i < m_argument_vec.size (); ++i)
          {
            // llvm::Value *loaded_arg = builder.CreateConstInBoundsGEP1_32 (arg, i);         // LLVM <= 3.6
            llvm::Value *loaded_arg = builder.CreateConstInBoundsGEP1_32 (arg_type, arg, i);  // LLVM >= 3.7

            m_arguments[m_argument_vec[i].first] = loaded_arg;
          }

        convert (blocks, constants);
      }
    catch (const jit_fail_exception& e)
      {
        m_function->eraseFromParent ();
        throw;
      }

    return m_function;
  }

  jit_function
  jit_convert_llvm::convert_function (const jit_module& module,
                                      const jit_block_list& blocks,
                                      const std::list<jit_value *>& constants,
                                      octave_user_function& fcn,
                                      const std::vector<jit_type *>& args)
  {
    m_converting_function = true;

    jit_block *m_final_block = blocks.back ();
    jit_return *ret = dynamic_cast<jit_return *> (m_final_block->back ());
    assert (ret);

    m_creating = jit_function (&module, jit_convention::internal,
                             "foobar", ret->result_type (), args);
    m_function = m_creating.to_llvm ();

    try
      {
        m_prelude = m_creating.new_block ("prelude");
        builder.SetInsertPoint (m_prelude);

        tree_parameter_list *plist = fcn.parameter_list ();
        if (plist)
          {
            tree_parameter_list::iterator piter = plist->begin ();
            tree_parameter_list::iterator pend = plist->end ();
            for (size_t i = 0; i < args.size () && piter != pend; ++i, ++piter)
              {
                tree_decl_elt *elt = *piter;
                std::string arg_name = elt->name ();
                m_arguments[arg_name] = m_creating.argument (builder, i);
              }
          }

        convert (blocks, constants);
      }
    catch (const jit_fail_exception& e)
      {
        m_function->eraseFromParent ();
        throw;
      }

    return m_creating;
  }

  void
  jit_convert_llvm::convert (const jit_block_list& blocks,
                             const std::list<jit_value *>& constants)
  {
    std::list<jit_block *>::const_iterator biter;
    for (biter = blocks.begin (); biter != blocks.end (); ++biter)
      {
        jit_block *jblock = *biter;
        llvm::BasicBlock *m_block = llvm::BasicBlock::Create (context,
                                                            jblock->name (),
                                                            m_function);
        jblock->stash_llvm (m_block);
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
        jit_block& m_block = **biter;
        for (jit_block::iterator piter = m_block.begin ();
             piter != m_block.end () && isa<jit_phi> (*piter); ++piter)
          {
            jit_instruction *phi = *piter;
            finish_phi (static_cast<jit_phi *> (phi));
          }
      }
  }

  void
  jit_convert_llvm::finish_phi (jit_phi *phi)
  {
    llvm::PHINode *llvm_phi = phi->to_llvm ();
    for (size_t i = 0; i < phi->argument_count (); ++i)
      {
        llvm::BasicBlock *pred = phi->incoming_llvm (i);
        llvm_phi->addIncoming (phi->argument_llvm (i), pred);
      }
  }

  void
  jit_convert_llvm::visit (jit_const_string& cs)
  {
    cs.stash_llvm (builder.CreateGlobalStringPtr (cs.value ()));
  }

  void
  jit_convert_llvm::visit (jit_const_bool& cb)
  {
    cb.stash_llvm (llvm::ConstantInt::get (cb.type_llvm (), cb.value ()));
  }

  void
  jit_convert_llvm::visit (jit_const_scalar& cs)
  {
    cs.stash_llvm (llvm::ConstantFP::get (cs.type_llvm (), cs.value ()));
  }

  void
  jit_convert_llvm::visit (jit_const_complex& cc)
  {
    llvm::Type *scalar_t = jit_typeinfo::get_scalar_llvm ();
    Complex value = cc.value ();
    llvm::Value *real = llvm::ConstantFP::get (scalar_t, value.real ());
    llvm::Value *imag = llvm::ConstantFP::get (scalar_t, value.imag ());
    cc.stash_llvm (jit_typeinfo::create_complex (real, imag));
  }

  void jit_convert_llvm::visit (jit_const_index& ci)
  {
    ci.stash_llvm (llvm::ConstantInt::get (ci.type_llvm (), ci.value ()));
  }

  void
  jit_convert_llvm::visit (jit_const_range& cr)
  {
    llvm::StructType *stype = llvm::cast<llvm::StructType>(cr.type_llvm ());
    llvm::Type *scalar_t = jit_typeinfo::get_scalar_llvm ();
    llvm::Type *idx = jit_typeinfo::get_index_llvm ();
    const jit_range& rng = cr.value ();

    llvm::Constant *constants[4];
    constants[0] = llvm::ConstantFP::get (scalar_t, rng.m_base);
    constants[1] = llvm::ConstantFP::get (scalar_t, rng.m_limit);
    constants[2] = llvm::ConstantFP::get (scalar_t, rng.m_inc);
    constants[3] = llvm::ConstantInt::get (idx, rng.m_nelem);

    llvm::Value *as_llvm;
    as_llvm = llvm::ConstantStruct::get (stype,
                                         llvm::makeArrayRef (constants, 4));
    cr.stash_llvm (as_llvm);
  }

  void
  jit_convert_llvm::visit (jit_block& b)
  {
    llvm::BasicBlock *m_block = b.to_llvm ();
    builder.SetInsertPoint (m_block);
    for (jit_block::iterator iter = b.begin (); iter != b.end (); ++iter)
      visit (*iter);
  }

  void
  jit_convert_llvm::visit (jit_branch& b)
  {
    b.stash_llvm (builder.CreateBr (b.successor_llvm ()));
  }

  void
  jit_convert_llvm::visit (jit_cond_branch& cb)
  {
    llvm::Value *cond = cb.cond_llvm ();
    llvm::Value *br;
    br = builder.CreateCondBr (cond, cb.successor_llvm (0),
                               cb.successor_llvm (1));
    cb.stash_llvm (br);
  }

  void
  jit_convert_llvm::visit (jit_call& call)
  {
    const jit_function& ol = call.overload ();

    std::vector<jit_value *> args (call.arguments ().size ());
    for (size_t i = 0; i < args.size (); ++i)
      args[i] = call.argument (i);

    llvm::Value *ret = ol.call (builder, args);
    call.stash_llvm (ret);
  }

  void
  jit_convert_llvm::visit (jit_extract_argument& extract)
  {
    llvm::Value *arg = m_arguments[extract.name ()];
    assert (arg);

    if (m_converting_function)
      extract.stash_llvm (arg);
    else
      {
        arg = builder.CreateLoad (arg);

        const jit_function& ol = extract.overload ();
        extract.stash_llvm (ol.call (builder, arg));
      }
  }

  void
  jit_convert_llvm::visit (jit_store_argument& store)
  {
    const jit_function& ol = store.overload ();
    llvm::Value *arg_value = ol.call (builder, store.result ());
    llvm::Value *arg = m_arguments[store.name ()];
    store.stash_llvm (builder.CreateStore (arg_value, arg));
  }

  void
  jit_convert_llvm::visit (jit_return& ret)
  {
    jit_value *res = ret.result ();

    if (m_converting_function)
      m_creating.do_return (builder, res->to_llvm (), false);
    else
      {
        if (res)
          builder.CreateRet (res->to_llvm ());
        else
          builder.CreateRetVoid ();
      }
  }

  void
  jit_convert_llvm::visit (jit_phi& phi)
  {
    // we might not have converted all incoming branches, so we don't
    // set incoming branches now
    llvm::PHINode *node = llvm::PHINode::Create (phi.type_llvm (),
                                                 phi.argument_count ());
    builder.Insert (node);
    phi.stash_llvm (node);
  }

  void
  jit_convert_llvm::visit (jit_variable&)
  {
    throw jit_fail_exception ("ERROR: SSA construction should remove all variables");
  }

  void
  jit_convert_llvm::visit (jit_error_check& check)
  {
    llvm::Value *cond;

    switch (check.check_variable ())
      {
      case jit_error_check::var_error_state:
        cond = jit_typeinfo::insert_error_check (builder);
        break;
      case jit_error_check::var_interrupt:
        cond = jit_typeinfo::insert_interrupt_check (builder);
        break;
      default:
        panic_impossible ();
      }

    llvm::Value *br = builder.CreateCondBr (cond, check.successor_llvm (0),
                                            check.successor_llvm (1));
    check.stash_llvm (br);
  }

  void
  jit_convert_llvm::visit (jit_assign& assign)
  {
    jit_value *new_value = assign.src ();
    assign.stash_llvm (new_value->to_llvm ());

    if (assign.artificial ())
      return;

    jit_value *overwrite = assign.overwrite ();
    if (isa<jit_assign_base> (overwrite))
      {
        const jit_function& ol = jit_typeinfo::get_release (overwrite->type ());
        if (ol.valid ())
          ol.call (builder, overwrite);
      }
  }

  void
  jit_convert_llvm::visit (jit_argument&)
  { }

  void
  jit_convert_llvm::visit (jit_magic_end& me)
  {
    const jit_function& ol = me.overload ();

    jit_magic_end::context ctx = me.resolve_context ();
    llvm::Value *ret = ol.call (builder, ctx.m_value, ctx.m_index, ctx.m_count);
    me.stash_llvm (ret);
  }

  // -------------------- jit_infer --------------------
  jit_infer::jit_infer (jit_factory& afactory, jit_block_list& ablocks,
                        const variable_map& avmap)
    : m_blocks (ablocks), m_factory (afactory), m_vmap (avmap) { }

  void
  jit_infer::infer (void)
  {
    construct_ssa ();

    // initialize the worklist to instructions derived from constants
    const std::list<jit_value *>& constants = m_factory.constants ();
    for (std::list<jit_value *>::const_iterator iter = constants.begin ();
         iter != constants.end (); ++iter)
      append_users (*iter);

    // the entry block terminator may be a regular branch statement
    if (entry_block ().terminator ())
      push_worklist (entry_block ().terminator ());

    // FIXME: Describe algorithm here
    while (m_worklist.size ())
      {
        jit_instruction *next = m_worklist.front ();
        m_worklist.pop_front ();
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
    m_blocks.label ();
    place_releases ();
    simplify_phi ();
  }

  void
  jit_infer::append_users (jit_value *v)
  {
    for (jit_use *use = v->first_use (); use; use = use->next ())
      push_worklist (use->user ());
  }

  void
  jit_infer::append_users_term (jit_terminator *term)
  {
    for (size_t i = 0; i < term->successor_count (); ++i)
      {
        if (term->alive (i))
          {
            jit_block *succ = term->successor (i);
            for (jit_block::iterator iter = succ->begin ();
                 iter != succ->end () && isa<jit_phi> (*iter); ++iter)
              push_worklist (*iter);

            jit_terminator *sterm = succ->terminator ();
            if (sterm)
              push_worklist (sterm);
          }
      }
  }

  void
  jit_infer::construct_ssa (void)
  {
    m_blocks.label ();
    final_block ().compute_idom (entry_block ());
    entry_block ().compute_df ();
    entry_block ().create_dom_tree ();

    // insert phi nodes where needed, this is done on a per variable basis
    for (variable_map::const_iterator iter = m_vmap.begin (); iter != m_vmap.end ();
         ++iter)
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
                    jit_phi *phi = m_factory.create<jit_phi> (iter->second,
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

    do_construct_ssa (entry_block (), entry_block ().visit_count ());
  }

  void
  jit_infer::do_construct_ssa (jit_block& ablock, size_t avisit_count)
  {
    if (ablock.visited (avisit_count))
      return;

    // replace variables with their current SSA value
    for (jit_block::iterator iter = ablock.begin (); iter != ablock.end ();
         ++iter)
      {
        jit_instruction *instr = *iter;
        instr->construct_ssa ();
        instr->push_variable ();
      }

    // finish phi nodes of successors
    for (size_t i = 0; i < ablock.successor_count (); ++i)
      {
        jit_block *finish = ablock.successor (i);

        for (jit_block::iterator iter = finish->begin ();
             iter != finish->end () && isa<jit_phi> (*iter);)
          {
            jit_phi *phi = static_cast<jit_phi *> (*iter);
            jit_variable *var = phi->dest ();
            ++iter;

            if (var->has_top ())
              phi->add_incoming (&ablock, var->top ());
            else
              {
                // temporaries may have extranious phi nodes which can be removed
                assert (! phi->use_count ());
                assert (var->name ().size () && var->name ()[0] == '#');
                phi->remove ();
              }
          }
      }

    for (size_t i = 0; i < ablock.dom_successor_count (); ++i)
      do_construct_ssa (*ablock.dom_successor (i), avisit_count);

    ablock.pop_all ();
  }

  void
  jit_infer::place_releases (void)
  {
    std::set<jit_value *> temporaries;
    for (jit_block_list::iterator iter = m_blocks.begin (); iter != m_blocks.end ();
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
  jit_infer::push_worklist (jit_instruction *instr)
  {
    if (! instr->in_worklist ())
      {
        instr->stash_in_worklist (true);
        m_worklist.push_back (instr);
      }
  }

  void
  jit_infer::remove_dead ()
  {
    jit_block_list::iterator biter;
    for (biter = m_blocks.begin (); biter != m_blocks.end (); ++biter)
      {
        jit_block *b = *biter;
        if (b->alive ())
          {
            for (jit_block::iterator iter = b->begin ();
                 iter != b->end () && isa<jit_phi> (*iter);)
              {
                jit_phi *phi = static_cast<jit_phi *> (*iter);
                if (phi->prune ())
                  iter = b->remove (iter);
                else
                  ++iter;
              }
          }
      }

    for (biter = m_blocks.begin (); biter != m_blocks.end ();)
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
                jit_branch *abreak = m_factory.create<jit_branch> (succ);
                b->append (abreak);
                abreak->infer ();
              }

            ++biter;
          }
        else
          {
            jit_terminator *term = b->terminator ();
            if (term)
              term->remove ();
            biter = m_blocks.erase (biter);
          }
      }
  }

  void
  jit_infer::release_dead_phi (jit_block& ablock)
  {
    jit_block::iterator iter = ablock.begin ();
    while (iter != ablock.end () && isa<jit_phi> (*iter))
      {
        jit_phi *phi = static_cast<jit_phi *> (*iter);
        ++iter;

        jit_use *use = phi->first_use ();
        if (phi->use_count () == 1 && isa<jit_assign> (use->user ()))
          {
            // instead of releasing on assign, release on all incoming branches,
            // this can get rid of casts inside loops
            for (size_t i = 0; i < phi->argument_count (); ++i)
              {
                jit_value *arg = phi->argument (i);
                if (! arg->needs_release ())
                  continue;

                jit_block *inc = phi->incoming (i);
                jit_block *split = inc->maybe_split (m_factory, m_blocks, ablock);
                jit_terminator *term = split->terminator ();
                jit_call *release
                  = m_factory.create<jit_call> (jit_typeinfo::release, arg);
                release->infer ();
                split->insert_before (term, release);
              }

            phi->replace_with (0);
            phi->remove ();
          }
      }
  }

  void
  jit_infer::release_temp (jit_block& ablock, std::set<jit_value *>& temp)
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
            if (fu_block && fu_block != &ablock && instr->needs_release ())
              temp.insert (instr);
          }

        if (isa<jit_call> (instr))
          {
            // place releases for temporary arguments
            for (size_t i = 0; i < instr->argument_count (); ++i)
              {
                jit_value *arg = instr->argument (i);
                if (! arg->needs_release ())
                  continue;

                jit_call *release
                  = m_factory.create<jit_call> (&jit_typeinfo::release, arg);
                release->infer ();
                ablock.insert_after (iter, release);
                ++iter;
                temp.erase (arg);
              }
          }
      }

    if (! temp.size () || ! isa<jit_error_check> (ablock.terminator ()))
      return;

    // FIXME: If we support try/catch or unwind_protect final_block
    //        may not be the destination
    jit_block *split = ablock.maybe_split (m_factory, m_blocks, final_block ());
    jit_terminator *term = split->terminator ();
    for (std::set<jit_value *>::const_iterator iter = temp.begin ();
         iter != temp.end (); ++iter)
      {
        jit_value *value = *iter;
        jit_call *release
          = m_factory.create<jit_call> (&jit_typeinfo::release, value);
        split->insert_before (term, release);
        release->infer ();
      }
  }

  void
  jit_infer::simplify_phi (void)
  {
    for (jit_block_list::iterator biter = m_blocks.begin (); biter != m_blocks.end ();
         ++biter)
      {
        jit_block &ablock = **biter;
        for (jit_block::iterator iter = ablock.begin ();
             iter != ablock.end () && isa<jit_phi> (*iter); ++iter)
          simplify_phi (*static_cast<jit_phi *> (*iter));
      }
  }

  void
  jit_infer::simplify_phi (jit_phi& phi)
  {
    jit_block& pblock = *phi.parent ();
    const jit_operation& cast_fn = jit_typeinfo::cast (phi.type ());
    jit_variable *dest = phi.dest ();
    for (size_t i = 0; i < phi.argument_count (); ++i)
      {
        jit_value *arg = phi.argument (i);
        if (arg->type () != phi.type ())
          {
            jit_block *pred = phi.incoming (i);
            jit_block *split = pred->maybe_split (m_factory, m_blocks, pblock);
            jit_terminator *term = split->terminator ();
            jit_instruction *cast = m_factory.create<jit_call> (cast_fn, arg);
            jit_assign *assign = m_factory.create<jit_assign> (dest, cast);

            split->insert_before (term, cast);
            split->insert_before (term, assign);
            cast->infer ();
            assign->infer ();
            phi.stash_argument (i, assign);
          }
      }
  }


  // ---------------- jit_memory_manager ------------------

  // A simple memory manager for our LLVM engines,
  // based on LLVM's Kaleidoscope example

  class jit_memory_manager : public llvm::SectionMemoryManager
  {
    jit_memory_manager (const jit_memory_manager&) = delete;
    void operator= (const jit_memory_manager&) = delete;
  public:
    jit_memory_manager () {}
    virtual ~jit_memory_manager () {}

    // The Kaleidoscope example in LLVM 3.8 indicates that
    // getPointerToNamedFunction has to be overloaded, but actually it is
    // getSymbolAddress that must be overloaded.
    virtual uint64_t getSymbolAddress (const std::string &name);

    // Is it still useful to overload getPointerToNamedFunction to support
    // some older version of LLVM?  Are there others virtual functions
    // that must be overloaded?
    virtual void* getPointerToNamedFunction (const std::string& name, bool abort_on_failure);
  };

  void*
  jit_memory_manager::getPointerToNamedFunction (const std::string& name,
                                                 bool abort_on_failure)
  {
    // Try the standard symbol resolution first, but ask it not to abort
    void *pfn = llvm::RTDyldMemoryManager::getPointerToNamedFunction (name, false);
    if (pfn)
      return pfn;

    pfn = tree_jit::getPointerToNamedFunction (name);
    if ((pfn == nullptr) && abort_on_failure)
      llvm::report_fatal_error ("Program used external function '" + name +
                                "' which could not be resolved!");
    return pfn;
  }

  uint64_t
  jit_memory_manager::getSymbolAddress (const std::string &name)
  {
    uint64_t addr = llvm::SectionMemoryManager::getSymbolAddress (name);
    if (addr)
      return addr;

    addr = tree_jit::getSymbolAddress (name);
    if (addr == 0)
      llvm::report_fatal_error ("Program used extern function '" + name +
                                "' which could not be resolved!");

    return addr;
  }


  // -------------------- tree_jit --------------------

  bool tree_jit::initialized = false;

  int tree_jit::next_forloop_number = 0;
  int tree_jit::next_function_number = 0;
  int tree_jit::next_module_number = 0;

  tree_jit::tree_jit (void)
    : target_machine (nullptr)
  {
    // target_machine will be truly initialized by tree_jit::do_initialize ()
  }

  tree_jit::~tree_jit (void)
  {
    delete target_machine;
  }

  tree_jit&
  tree_jit::instance (void)
  {
    static tree_jit ret;  // singleton instance of tree_jit

    if (! initialized)
      // Try to initialize the singleton instance
      ret.do_initialize ();

    return ret;
  }

  jit::EngineOwner
  tree_jit::create_new_engine (jit::ModuleOwner module_owner)
  {
    std::string err;

    llvm::ExecutionEngine *e = llvm::EngineBuilder (std::move (module_owner))
      .setErrorStr (&err)
      .setMCJITMemoryManager(llvm::make_unique<jit_memory_manager> ())
      .create ();

    // Note: in some versions of LLVM, we should call .setUseMCJIT (true) before .create () ?
    // FIXME: autconf this

    if (e == nullptr)
      {
        std::cerr << "Failed to create JIT engine" << std::endl;
        std::cerr << err << std::endl;
      }

    return jit::EngineOwner (e);
  }

  void
  tree_jit::do_register_jit_module (jit_module* jm)
  {
    jm_list.push_back (jm);
  }

  void
  tree_jit::do_unregister_jit_module (jit_module* jm)
  {
    jm_list.remove (jm);
  }

  void*
  tree_jit::do_getPointerToNamedFunction (const std::string &name) const
  {
    std::list<jit_module*>::const_iterator it;

    for (it = jm_list.begin (); it != jm_list.end (); it++)
      {
        uint64_t addr = (*it)->getFunctionAddress (name);

        if (addr)
          return reinterpret_cast<void*> (addr);
      }

    return nullptr;
  }

  uint64_t
  tree_jit::do_getSymbolAddress(const std::string &name) const
  {
    std::list<jit_module*>::const_iterator it;

    for (it = jm_list.begin (); it != jm_list.end (); it++)
      {
        uint64_t addr = (*it)->getFunctionAddress (name);

        if (addr)
          return addr;
      }

    return 0;
  }

  bool
  tree_jit::do_initialize (void)
  {
    if (initialized)
      return true;

    llvm::InitializeNativeTarget ();
    llvm::InitializeNativeTargetAsmPrinter ();
    llvm::InitializeNativeTargetAsmParser ();
    // FIXME: Check that these three initializations succeed

    if (target_machine == nullptr)
      {
        target_machine = llvm::EngineBuilder ().selectTarget ();
        if (target_machine == nullptr)
          return false;
      }

    return (initialized = true);
  }

  jit::ModuleOwner
  tree_jit::open_new_module (const std::string& module_name)
  {
    return instance ().do_open_new_module (module_name);
  }

  jit::ModuleOwner
  tree_jit::do_open_new_module (const std::string& module_name) const
  {
    if (! initialized)
      return nullptr;

    jit::ModuleOwner m (new llvm::Module (module_name, context));


    if (m != nullptr)
      m->setDataLayout (target_machine->createDataLayout ());

    return m;
  }

  bool
  tree_jit::do_execute (tree_simple_for_command& cmd,
                        const octave_value& bounds)
  {
    size_t tc = trip_count (bounds);
    if (! tc || ! initialized || ! enabled ())
      return false;

    jit_info::vmap extra_vars;
    extra_vars["#for_bounds0"] = &bounds;

    jit_info *info = cmd.get_info ();

    if (! info || ! info->match (extra_vars))
      {
        if (tc < static_cast<size_t> (Vjit_startcnt))
          return false;

        delete info;

        info = new jit_info (cmd, bounds);

        cmd.stash_info (info);
      }

    return info->execute (extra_vars);
  }

  bool
  tree_jit::do_execute (tree_while_command& cmd)
  {
    if (! initialized || ! enabled ())
      return false;

    jit_info *info = cmd.get_info ();
    if (! info || ! info->match ())
      {
        delete info;
        info = new jit_info (cmd);
        cmd.stash_info (info);
      }

    return info->execute ();
  }

  bool
  tree_jit::do_execute (octave_user_function& fcn,
                        const octave_value_list& args,
                        octave_value_list& retval)
  {
    if (! initialized || ! enabled ())
      return false;

    jit_function_info *info = fcn.get_info ();
    if (! info || ! info->match (args))
      {
        delete info;
        info = new jit_function_info (fcn, args);
        fcn.stash_info (info);
      }

    return info->execute (args, retval);
  }

  bool
  tree_jit::enabled (void)
  {
    octave::bp_table& bptab
      = octave::__get_bp_table__ ("tree_jit::enabled");

    // Ideally, we should only disable JIT if there is a breakpoint in the code
    // we are about to run. However, we can't figure this out in O(1) time, so
    // we conservatively check for the existence of any breakpoints.
    return (Vjit_enable && ! bptab.have_breakpoints ()
            && ! Vdebug_on_interrupt && ! Vdebug_on_error);
  }

  size_t
  tree_jit::trip_count (const octave_value& bounds) const
  {
    if (bounds.is_range ())
      {
        Range rng = bounds.range_value ();
        return rng.numel ();
      }

    // unsupported type
    return 0;
  }


  // -------------------- jit_module --------------------

  jit_module::jit_module (const std::string& module_name)
    : m_module (nullptr), m_engine (nullptr)
  {
    jit::ModuleOwner module_owner = tree_jit::open_new_module (module_name);
    // FIXME: what if this fails? exception?

    // Get a pointer to the module before ownership is transfered to engine
    m_module = module_owner.get ();

    jit::EngineOwner engine_owner = std::move
      (tree_jit::create_new_engine (std::move (module_owner)));
    // FIXME: what if this fails? exception?

    // TODO?: Consider creating the engine just before jitting

    // We take responsibility for deleting the engine
    m_engine = engine_owner.get ();
    engine_owner.release ();

    tree_jit::register_jit_module (this);
  }

  jit_module::~jit_module ()
  {
    tree_jit::unregister_jit_module (this);

    delete m_engine;
  }

  // Create an LLVM function in the module, with external linkage
  llvm::Function*
  jit_module::create_llvm_function (llvm::FunctionType *ftype,
                                    const llvm::Twine &name) const
  {
    // we mark all functinos as external linkage because this prevents
    // llvm from getting rid of always inline functions

    return llvm::Function::Create (ftype, llvm::Function::ExternalLinkage,
                                   name, m_module);
  }

  // Create or insert an LLVM Function declaration for an intrinsic and return it
  llvm::Function*
  jit_module::get_intrinsic_declaration (size_t id,
                                         std::vector<llvm::Type*> types) const
  {
    return llvm::Intrinsic::getDeclaration
      (m_module, static_cast<llvm::Intrinsic::ID> (id), types);
  }

  // Create a global in the module
  llvm::GlobalVariable*
  jit_module::create_global_variable (llvm::Type *type, bool is_constant,
                                      const llvm::Twine& name) const
  {
    return new llvm::GlobalVariable (*m_module, type, is_constant,
                                     llvm::GlobalValue::ExternalLinkage,
                                     nullptr, name);
  }

  void
  jit_module::do_add_global_mapping (const llvm::GlobalValue* gv, void* p) const
  {
    assert (gv);
    m_engine->addGlobalMapping (gv, p);
  }

  // Return the address of the specified function.
  uint64_t
  jit_module::getFunctionAddress (const std::string &name) const
  {
    return m_engine->getFunctionAddress (name);
  }

  void
  jit_module::optimize (llvm::Function *fn) const
  {
    if (Vdebug_jit)
      llvm::verifyModule (*m_module);

    // DOCUMENT-ME: Why do we need two separate pass managers?

    jit::PassManager *module_pass_manager = new jit::PassManager ();
    jit::FunctionPassManager *pass_manager = new jit::FunctionPassManager (m_module);

    module_pass_manager->add (llvm::createAlwaysInlinerPass ());

    // In 3.6, a pass was inserted in the pipeline to make the DataLayout accessible:
    //    MyPassManager->add(new DataLayoutPass(MyTargetMachine->getDataLayout()));
    // In 3.7, you don’t need a pass, you set the DataLayout on the Module:
    //    MyModule->setDataLayout(MyTargetMachine->createDataLayout());
    //
    // FIXME: autoconf to support <= 3.6
    //
    // #if defined (HAVE_LLVM_DATALAYOUT)
    //   pass_manager->add (new llvm::DataLayout (*m_engine->getDataLayout ()));
    // #else
    //   // For very old LLVM releases ???
    //   pass_manager->add (new llvm::TargetData (*m_engine->getTargetData ()));
    // #endif

    // DOCUMENT-ME: What does each of these passes actually do?

    pass_manager->add (llvm::createCFGSimplificationPass ());

#if defined (HAVE_LLVM_ANALYSIS_BASICALIASANALYSIS_H)
    pass_manager->add (llvm::createBasicAAWrapperPass ());
#else
    pass_manager->add (llvm::createBasicAliasAnalysisPass ());
#endif

    pass_manager->add (llvm::createPromoteMemoryToRegisterPass ());
    pass_manager->add (llvm::createInstructionCombiningPass ());
    pass_manager->add (llvm::createReassociatePass ());
    pass_manager->add (llvm::createGVNPass ());
    pass_manager->add (llvm::createCFGSimplificationPass ());
    pass_manager->doInitialization ();

    module_pass_manager->run (*m_module);
    pass_manager->run (*fn);

    delete module_pass_manager;
    delete pass_manager;

    if (Vdebug_jit)
      {
        // This should be OK in LLVM 3.6 -- 3.8 (and later ?)
        std::error_code ec;
        llvm::raw_fd_ostream fout ("test.bc", ec, llvm::sys::fs::F_None);

        //      std::string error;
        //#if defined (RAW_FD_OSTREAM_ARG_IS_LLVM_SYS_FS)
        //      llvm::raw_fd_ostream fout ("test.bc", error, llvm::sys::fs::F_Binary);
        //#else
        //      llvm::raw_fd_ostream fout ("test.bc", error, llvm::raw_fd_ostream::F_Binary);
        //#endif

        llvm::WriteBitcodeToFile (m_module, fout);
      }
  }

  void
  jit_module::finalizeObject (void)
  {
    m_engine->finalizeObject ();
  }


  // -------------------- jit_function_info --------------------
  jit_function_info::jit_function_info (octave_user_function& fcn,
                                        const octave_value_list& ov_args)
    : m_llvm_function_name (""),
      m_function (nullptr),
      m_argument_types (ov_args.length ())
  {
    size_t nargs = ov_args.length ();
    for (size_t i = 0; i < nargs; ++i)
      m_argument_types[i] = jit_typeinfo::type_of (ov_args(i));

    jit_function raw_fn;
    jit_function wrapper;

    try
      {
        jit_convert conv (fcn, m_argument_types);
        jit_infer infer (conv.get_factory (), conv.get_blocks (),
                         conv.get_variable_map ());
        infer.infer ();

        if (Vdebug_jit)
          {
            jit_block_list& blocks = infer.get_blocks ();
            blocks.label ();
            std::cout << "-------------------- Compiling function ";
            std::cout << "--------------------\n";

            tree_print_code tpc (std::cout);
            tpc.visit_octave_user_function_header (fcn);
            tpc.visit_statement_list (*fcn.body ());
            tpc.visit_octave_user_function_trailer (fcn);
            blocks.print (std::cout, "octave jit ir");
          }

        jit_factory& factory = conv.get_factory ();
        jit_convert_llvm to_llvm;
        raw_fn = to_llvm.convert_function (*this, infer.get_blocks (),
                                           factory.constants (), fcn,
                                           m_argument_types);

        if (Vdebug_jit)
          {
            std::cout << "-------------------- raw function ";
            std::cout << "--------------------\n";
            std::cout << *raw_fn.to_llvm () << std::endl;
            llvm::verifyFunction (*raw_fn.to_llvm ());
          }

        m_llvm_function_name = fcn.name () + "_wrapper";
        jit_type *any_t = jit_typeinfo::get_any ();
        std::vector<jit_type *> wrapper_args (1, jit_typeinfo::get_any_ptr ());
        wrapper = jit_function (this, jit_convention::internal,
                                m_llvm_function_name, any_t, wrapper_args);

        llvm::BasicBlock *wrapper_body = wrapper.new_block ();
        builder.SetInsertPoint (wrapper_body);

        llvm::Value *wrapper_arg = wrapper.argument (builder, 0);
        std::vector<llvm::Value *> raw_args (nargs);
        for (size_t i = 0; i < nargs; ++i)
          {
            llvm::Value *arg;
            // arg = builder.CreateConstInBoundsGEP1_32 (wrapper_arg, i);                  // LLVM <= 3.6
            arg = builder.CreateConstInBoundsGEP1_32 (any_t->to_llvm (), wrapper_arg, i);  // LLVM >= 3.7
            arg = builder.CreateLoad (arg);

            jit_type *arg_type = m_argument_types[i];
            const jit_function& cast = jit_typeinfo::cast (arg_type, any_t);
            raw_args[i] = cast.call (builder, arg);
          }

        llvm::Value *result = raw_fn.call (builder, raw_args);
        if (raw_fn.result ())
          {
            jit_type *raw_result_t = raw_fn.result ();
            const jit_function& cast = jit_typeinfo::cast (any_t, raw_result_t);
            result = cast.call (builder, result);
          }
        else
          {
            llvm::Value *zero = builder.getInt32 (0);
            result = builder.CreateBitCast (zero, any_t->to_llvm ());
          }

        wrapper.do_return (builder, result);

        llvm::Function *llvm_function = wrapper.to_llvm ();
        optimize (llvm_function);

        if (Vdebug_jit)
          {
            std::cout << "-------------------- optimized and wrapped ";
            std::cout << "--------------------\n";
            std::cout << *llvm_function << std::endl;
            llvm::verifyFunction (*llvm_function);
          }

        finalizeObject ();

        uint64_t void_fn = getFunctionAddress (m_llvm_function_name);

        if (void_fn == 0)
          {
            llvm_function->eraseFromParent ();
            llvm_function = nullptr;
            m_function = nullptr;
          }
        else
          {
            m_function = reinterpret_cast<jited_function> (void_fn);
          }
      }
    catch (const jit_fail_exception& e)
      {
        m_argument_types.clear ();

        if (Vdebug_jit)
          {
            if (e.known ())
              std::cout << "jit fail: " << e.what () << std::endl;
          }

        Vjit_failcnt++;

        wrapper.erase ();
        raw_fn.erase ();
      }
  }

  bool
  jit_function_info::execute (const octave_value_list& ov_args,
                              octave_value_list& retval) const
  {
    if (! m_function)
      return false;

    // FIXME: figure out a way to delete ov_args so we avoid duplicating refcount
    size_t nargs = ov_args.length ();
    std::vector<octave_base_value *> args (nargs);
    for (size_t i = 0; i < nargs; ++i)
      {
        octave_base_value *obv = ov_args(i).internal_rep ();
        obv->grab ();
        args[i] = obv;
      }

    octave_base_value *ret = m_function (&args[0]);
    if (ret)
      retval(0) = octave_value (ret);

    octave_quit ();

    return true;
  }

  bool
  jit_function_info::match (const octave_value_list& ov_args) const
  {
    if (! m_function)
      return true;

    size_t nargs = ov_args.length ();
    if (nargs != m_argument_types.size ())
      return false;

    for (size_t i = 0; i < nargs; ++i)
      if (jit_typeinfo::type_of (ov_args(i)) != m_argument_types[i])
        return false;

    return true;
  }


  // -------------------- jit_info --------------------
  jit_info::jit_info (tree& tee)
    : m_llvm_function_name (tree_jit::generate_unique_function_name ()),
      m_function (nullptr)
  {
    compile (tee);
  }

  jit_info::jit_info (tree& tee, const octave_value& for_bounds)
    : m_llvm_function_name (tree_jit::generate_unique_function_name ()),
      m_function (nullptr)
  {
    compile (tee, jit_typeinfo::type_of (for_bounds));
  }

  jit_info::jit_info (tree_simple_for_command& tee, const octave_value& for_bounds)
    : m_llvm_function_name (tree_jit::generate_unique_forloop_name ()),
      m_function (nullptr)
  {
    compile (tee, jit_typeinfo::type_of (for_bounds));
  }

  bool
  jit_info::execute (const vmap& extra_vars) const
  {
    if (! m_function)
      return false;

    std::vector<octave_base_value *> real_arguments (m_arguments.size ());
    for (size_t i = 0; i < m_arguments.size (); ++i)
      {
        if (m_arguments[i].second)
          {
            octave_value current = find (extra_vars, m_arguments[i].first);
            octave_base_value *obv = current.internal_rep ();

            obv->grab ();

            real_arguments[i] = obv;
          }
      }

    m_function (&real_arguments[0]);

    symbol_scope scope = __require_current_scope__ ("jit_info::execute");

    for (size_t i = 0; i < m_arguments.size (); ++i)
      {
        const std::string& name = m_arguments[i].first;

        // do not store for loop bounds temporary
        if (name.size () && name[0] != '#')
          scope.assign (m_arguments[i].first, real_arguments[i]);
      }

    octave_quit ();

    return true;
  }

  bool
  jit_info::match (const vmap& extra_vars) const
  {
    if (! m_function)
      return true;

    for (size_t i = 0; i < m_bounds.size (); ++i)
      {
        const std::string& arg_name = m_bounds[i].second;
        octave_value value = find (extra_vars, arg_name);
        jit_type *type = jit_typeinfo::type_of (value);

        // FIXME: Check for a parent relationship
        if (type != m_bounds[i].first)
          return false;
      }

    return true;
  }

  void
  jit_info::compile (tree& tee, jit_type *for_bounds)
  {
    llvm::Function * llvm_function = nullptr;

    try
      {
        jit_convert conv (tee, for_bounds);
        jit_infer infer (conv.get_factory (), conv.get_blocks (),
                         conv.get_variable_map ());

        infer.infer ();

        if (Vdebug_jit)
          {
            jit_block_list& blocks = infer.get_blocks ();
            blocks.label ();
            std::cout << "-------------------- Compiling tree --------------------\n";
            std::cout << tee.str_print_code () << std::endl;
            blocks.print (std::cout, "octave jit ir");
          }

        jit_factory& factory = conv.get_factory ();
        jit_convert_llvm to_llvm;

        llvm_function = to_llvm.convert_loop (*this, infer.get_blocks (),
                                              factory.constants (),
                                              m_llvm_function_name);

        m_arguments = to_llvm.get_arguments ();

        m_bounds = conv.get_bounds ();
      }
    catch (const jit_fail_exception& e)
      {
        if (Vdebug_jit)
          {
            if (e.known ())
              std::cout << "jit fail: " << e.what () << std::endl;
          }

        Vjit_failcnt++;

      }

    if (llvm_function)
      {
        if (Vdebug_jit)
          {
            std::cout << "-------------------- llvm ir --------------------";
            std::cout << *llvm_function << std::endl;
            llvm::verifyFunction (*llvm_function);
          }

        optimize (llvm_function);

        if (Vdebug_jit)
          {
            std::cout << "-------------------- optimized llvm ir "
                      << "--------------------\n";
            std::cout << *llvm_function << std::endl;
          }

        finalizeObject ();

        uint64_t void_fn = getFunctionAddress (m_llvm_function_name);

        if (void_fn == 0)
          {
            llvm_function->eraseFromParent ();
            llvm_function = nullptr;
            m_function = nullptr;
          }
        else
          {
            m_function = reinterpret_cast<jited_function> (void_fn);
          }
      }
  }

  octave_value
  jit_info::find (const vmap& extra_vars, const std::string& vname) const
  {
    vmap::const_iterator iter = extra_vars.find (vname);

    if (iter == extra_vars.end ())
      {
        symbol_scope scope = __require_current_scope__ ("jit_convert::find");

        return scope.varval (vname);
      }
    else
      return *iter->second;
  }
}

#endif

DEFUN (jit_failcnt, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} jit_failcnt ()
@deftypefnx {} {@var{old_val} =} jit_failcnt (@var{new_val})
@deftypefnx {} {} jit_failcnt (@var{new_val}, "local")
Query or set the internal variable that counts the number of JIT fail
exceptions for Octave's JIT compiler.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{jit_enable, jit_startcnt, debug_jit}
@end deftypefn */)
{
#if defined (HAVE_LLVM)
  return SET_INTERNAL_VARIABLE (jit_failcnt);
#else
  octave_unused_parameter (args);
  octave_unused_parameter (nargout);
  warn_disabled_feature ("jit_failcnt", "JIT compiling");
  return ovl ();
#endif
}

DEFUN (debug_jit, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} debug_jit ()
@deftypefnx {} {@var{old_val} =} debug_jit (@var{new_val})
@deftypefnx {} {} debug_jit (@var{new_val}, "local")
Query or set the internal variable that determines whether
debugging/tracing is enabled for Octave's JIT compiler.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{jit_enable, jit_startcnt}
@end deftypefn */)
{
#if defined (HAVE_LLVM)
  return SET_INTERNAL_VARIABLE (debug_jit);
#else
  octave_unused_parameter (args);
  octave_unused_parameter (nargout);
  warn_disabled_feature ("debug_jit", "JIT");
  return ovl ();
#endif
}

DEFUN (jit_enable, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} jit_enable ()
@deftypefnx {} {@var{old_val} =} jit_enable (@var{new_val})
@deftypefnx {} {} jit_enable (@var{new_val}, "local")
Query or set the internal variable that enables Octave's JIT compiler.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{jit_startcnt, debug_jit}
@end deftypefn */)
{
#if defined (HAVE_LLVM)
  return SET_INTERNAL_VARIABLE (jit_enable);
#else
  octave_unused_parameter (args);
  octave_unused_parameter (nargout);
  warn_disabled_feature ("jit_enable", "JIT");
  return ovl ();
#endif
}

DEFUN (jit_startcnt, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} jit_startcnt ()
@deftypefnx {} {@var{old_val} =} jit_startcnt (@var{new_val})
@deftypefnx {} {} jit_startcnt (@var{new_val}, "local")
Query or set the internal variable that determines whether JIT compilation
will take place for a specific loop.

Because compilation is a costly operation it does not make sense to employ
JIT when the loop count is low.  By default only loops with greater than
1000 iterations will be accelerated.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{jit_enable, jit_failcnt, debug_jit}
@end deftypefn */)
{
#if defined (HAVE_LLVM)
  return SET_INTERNAL_VARIABLE_WITH_LIMITS (jit_startcnt, 1,
                                            std::numeric_limits<int>::max ());
#else
  octave_unused_parameter (args);
  octave_unused_parameter (nargout);
  warn_disabled_feature ("jit_enable", "JIT");
  return ovl ();
#endif
}
