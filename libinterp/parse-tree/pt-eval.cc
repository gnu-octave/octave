/*

Copyright (C) 2009-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>

#include <iostream>

#include <fstream>
#include <typeinfo>

#include "bp-table.h"
#include "call-stack.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "interpreter.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "profiler.h"
#include "pt-all.h"
#include "pt-eval.h"
#include "pt-tm-const.h"
#include "symtab.h"
#include "unwind-prot.h"
#include "variables.h"

//FIXME: This should be part of tree_evaluator
#include "pt-jit.h"

// Maximum nesting level for functions, scripts, or sourced files called
// recursively.
int Vmax_recursion_depth = 256;

// If TRUE, turn off printing of results in functions (as if a
// semicolon has been appended to each statement).
static bool Vsilent_functions = false;

namespace octave
{
  int tree_evaluator::dbstep_flag = 0;

  size_t tree_evaluator::current_frame = 0;

  bool tree_evaluator::debug_mode = false;

  bool tree_evaluator::quiet_breakpoint_flag = false;

  tree_evaluator::stmt_list_type tree_evaluator::statement_context
    = tree_evaluator::other;

  bool tree_evaluator::in_loop_command = false;

  // Normal evaluator.

  void
  tree_evaluator::reset (void)
  {
    m_value_stack.clear ();
    m_lvalue_list_stack.clear ();
    m_nargout_stack.clear ();
  }

  void
  tree_evaluator::visit_anon_fcn_handle (tree_anon_fcn_handle& expr)
  {
    // FIXME: should CMD_LIST be limited to a single expression?
    // I think that is what Matlab does.

    tree_parameter_list *param_list = expr.parameter_list ();
    tree_parameter_list *ret_list = expr.return_list ();
    tree_statement_list *cmd_list = expr.body ();
    symbol_table::scope_id this_scope = expr.scope ();

    symbol_table::scope_id new_scope = symbol_table::dup_scope (this_scope);

    if (new_scope > 0)
      symbol_table::inherit (new_scope, symbol_table::current_scope (),
                             symbol_table::current_context ());

    octave_user_function *uf
      = new octave_user_function (new_scope,
                                  param_list ? param_list->dup (new_scope, 0) : 0,
                                  ret_list ? ret_list->dup (new_scope, 0) : 0,
                                  cmd_list ? cmd_list->dup (new_scope, 0) : 0);

    octave_function *curr_fcn = m_call_stack.current ();

    if (curr_fcn)
      {
        // FIXME: maybe it would be better to just stash curr_fcn
        // instead of individual bits of info about it?

        uf->stash_parent_fcn_name (curr_fcn->name ());
        uf->stash_dir_name (curr_fcn->dir_name ());

        symbol_table::scope_id parent_scope = curr_fcn->parent_fcn_scope ();

        if (parent_scope < 0)
          parent_scope = curr_fcn->scope ();

        uf->stash_parent_fcn_scope (parent_scope);

        if (curr_fcn->is_class_method () || curr_fcn->is_class_constructor ())
          uf->stash_dispatch_class (curr_fcn->dispatch_class ());
      }

    uf->mark_as_anonymous_function ();
    uf->stash_fcn_file_name (expr.file_name ());
    uf->stash_fcn_location (expr.line (), expr.column ());

    octave_value ov_fcn (uf);

    octave_value fh (octave_fcn_binder::maybe_binder (ov_fcn, this));

    m_value_stack.push (ovl (fh));
  }

  void
  tree_evaluator::visit_argument_list (tree_argument_list&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_binary_expression (tree_binary_expression& expr)
  {
    octave_value val;

    tree_expression *op_lhs = expr.lhs ();
    tree_expression *op_rhs = expr.rhs ();
    octave_value::binary_op etype = expr.op_type ();

    if (expr.is_eligible_for_braindead_shortcircuit ())
      {
        if (op_lhs)
          {
            octave_value a = evaluate (op_lhs);

            if (a.ndims () == 2 && a.rows () == 1 && a.columns () == 1)
              {
                bool result = false;

                bool a_true = a.is_true ();

                if (a_true)
                  {
                    if (etype == octave_value::op_el_or)
                      {
                        expr.matlab_style_short_circuit_warning ("|");
                        m_value_stack.push (ovl (octave_value (true)));
                        return;
                      }
                  }
                else
                  {
                    if (etype == octave_value::op_el_and)
                      {
                        expr.matlab_style_short_circuit_warning ("&");
                        m_value_stack.push (ovl (octave_value (false)));
                        return;
                      }
                  }

                if (op_rhs)
                  {
                    octave_value b = evaluate (op_rhs);

                    result = b.is_true ();
                  }

                m_value_stack.push (ovl (octave_value (result)));
                return;
              }
          }
      }

    if (op_lhs)
      {
        octave_value a = evaluate (op_lhs);

        if (a.is_defined () && op_rhs)
          {
            octave_value b = evaluate (op_rhs);

            if (b.is_defined ())
              {
                profile_data_accumulator::enter<tree_binary_expression>
                  block (profiler, expr);

                // Note: The profiler does not catch the braindead
                // short-circuit evaluation code above, but that should be
                // ok.  The evaluation of operands and the operator itself
                // is entangled and it's not clear where to start/stop
                // timing the operator to make it reasonable.

                val = ::do_binary_op (etype, a, b);
              }
          }
      }

    m_value_stack.push (ovl (val));
  }

  void
  tree_evaluator::visit_boolean_expression (tree_boolean_expression& expr)
  {
    octave_value val;

    bool result = false;

    // This evaluation is not caught by the profiler, since we can't find
    // a reasonable place where to time.  Note that we don't want to
    // include evaluation of LHS or RHS into the timing, but this is
    // entangled together with short-circuit evaluation here.

    tree_expression *op_lhs = expr.lhs ();

    if (op_lhs)
      {
        octave_value a = evaluate (op_lhs);

        bool a_true = a.is_true ();

        tree_boolean_expression::type etype = expr.op_type ();

        if (a_true)
          {
            if (etype == tree_boolean_expression::bool_or)
              {
                m_value_stack.push (ovl (octave_value (true)));
                return;
              }
          }
        else
          {
            if (etype == tree_boolean_expression::bool_and)
              {
                m_value_stack.push (ovl (octave_value (false)));
                return;
              }
          }

        tree_expression *op_rhs = expr.rhs ();

        if (op_rhs)
          {
            octave_value b = evaluate (op_rhs);

            result = b.is_true ();
          }

        val = octave_value (result);
      }

    m_value_stack.push (ovl (val));
  }

  void
  tree_evaluator::visit_compound_binary_expression (tree_compound_binary_expression& expr)
  {
    octave_value val;

    tree_expression *op_lhs = expr.clhs ();

    if (op_lhs)
      {
        octave_value a = evaluate (op_lhs);

        tree_expression *op_rhs = expr.crhs ();

        if (a.is_defined () && op_rhs)
          {
            octave_value b = evaluate (op_rhs);

            if (b.is_defined ())
              {
                octave_value::compound_binary_op etype = expr.cop_type ();

                val = ::do_binary_op (etype, a, b);
              }
          }
      }

    m_value_stack.push (ovl (val));
  }

  void
  tree_evaluator::visit_break_command (tree_break_command& cmd)
  {
    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    if (in_loop_command)
      tree_break_command::breaking = 1;
    else
      error ("break must appear in a loop in the same file as loop command");
  }

  void
  tree_evaluator::visit_colon_expression (tree_colon_expression& expr)
  {
    octave_value val;

    tree_expression *op_base = expr.base ();
    tree_expression *op_limit = expr.limit ();

    if (! op_base || ! op_limit)
      {
        m_value_stack.push (ovl (octave_value (val)));
        return;
      }

    octave_value ov_base = evaluate (op_base);

    octave_value ov_limit = evaluate (op_limit);

    tree_expression *op_increment = expr.increment ();

    if (ov_base.isobject () || ov_limit.isobject ())
      {
        octave_value_list tmp1;

        if (op_increment)
          {
            octave_value ov_increment = evaluate (op_increment);

            tmp1(2) = ov_limit;
            tmp1(1) = ov_increment;
            tmp1(0) = ov_base;
          }
        else
          {
            tmp1(1) = ov_limit;
            tmp1(0) = ov_base;
          }

        octave_value fcn = symbol_table::find_function ("colon", tmp1);

        if (! fcn.is_defined ())
          error ("can not find overloaded colon function");

        octave_value_list tmp2 = octave::feval (fcn, tmp1, 1);

        val = tmp2 (0);
      }
    else
      {
        octave_value ov_increment = 1.0;

        if (op_increment)
          ov_increment = evaluate (op_increment);

        val = do_colon_op (ov_base, ov_increment, ov_limit,
                           expr.is_for_cmd_expr ());
      }

    m_value_stack.push (ovl (val));
  }

  void
  tree_evaluator::visit_continue_command (tree_continue_command& cmd)
  {
    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    if (in_loop_command)
      tree_continue_command::continuing = 1;
  }

  void
  tree_evaluator::reset_debug_state (void)
  {
    debug_mode = bp_table::have_breakpoints () || Vdebugging;

    dbstep_flag = 0;
  }

  bool
  tree_evaluator::statement_printing_enabled (void)
  {
    return ! (Vsilent_functions && (statement_context == function
                                    || statement_context == script));
  }

  Matrix
  tree_evaluator::ignored_fcn_outputs (void) const
  {
    Matrix retval;

    if (m_lvalue_list_stack.empty ())
      return retval;

    const std::list<octave_lvalue> *lvalue_list = m_lvalue_list_stack.top ();

    if (! lvalue_list)
      return retval;

    octave_idx_type nbh = 0;

    for (const auto& lval : *lvalue_list)
      nbh += lval.is_black_hole ();

    if (nbh > 0)
      {
        retval.resize (1, nbh);

        octave_idx_type k = 0;
        octave_idx_type l = 0;

        for (const auto& lval : *lvalue_list)
          {
            if (lval.is_black_hole ())
              retval(l++) = k+1;

            k += lval.numel ();
          }
      }

    return retval;
  }

  octave_value
  tree_evaluator::evaluate (tree_decl_elt *elt)
  {
    // Do not allow functions to return null values.

    tree_identifier *id = elt->ident ();

    return id ? evaluate (id).storable_value () : octave_value ();
  }

  void
  tree_evaluator::initialize_undefined_parameter_list_elements
    (tree_parameter_list *param_list, const std::string& warnfor,
     int nargout, const octave_value& val)
  {
    bool warned = false;

    int count = 0;

    octave_value tmp = symbol_table::varval (".ignored.");
    const Matrix ignored = (tmp.is_defined () ? tmp.matrix_value () : Matrix ());

    octave_idx_type k = 0;

    for (tree_decl_elt *elt : *param_list)
      {
        if (++count > nargout)
          break;

        if (! elt->is_variable ())
          {
            if (! warned)
              {
                warned = true;

                while (k < ignored.numel ())
                  {
                    octave_idx_type l = ignored (k);
                    if (l == count)
                      {
                        warned = false;
                        break;
                      }
                    else if (l > count)
                      break;
                    else
                      k++;
                  }

                if (warned)
                  {
                    warning_with_id
                      ("Octave:undefined-return-values",
                       "%s: some elements in list of return values are undefined",
                       warnfor.c_str ());
                  }
              }

            octave_lvalue lval = elt->lvalue (this);

            lval.assign (octave_value::op_asn_eq, val);
          }
      }
  }

  void
  tree_evaluator::define_parameter_list_from_arg_vector
    (tree_parameter_list *param_list, const octave_value_list& args)
  {
    int i = -1;

    for (tree_decl_elt *elt : *param_list)
      {
        i++;

        octave_lvalue ref = elt->lvalue (this);

        if (i < args.length ())
          {
            if (args(i).is_defined () && args(i).is_magic_colon ())
              {
                if (! eval_decl_elt (elt))
                  error ("no default value for argument %d", i+1);
              }
            else
              ref.define (args(i));
          }
        else
          eval_decl_elt (elt);
      }
  }

  void
  tree_evaluator::undefine_parameter_list (tree_parameter_list *param_list)
  {
    for (tree_decl_elt *elt : *param_list)
      {
        octave_lvalue ref = elt->lvalue (this);

        ref.assign (octave_value::op_asn_eq, octave_value ());
      }
  }

  octave_value_list
  tree_evaluator::convert_parameter_list_to_const_vector
    (tree_parameter_list *param_list, int nargout, const Cell& varargout)
  {
    octave_idx_type vlen = varargout.numel ();
    int len = param_list->length ();

    // Special case.  Will do a shallow copy.
    if (len == 0)
      return varargout;
    else if (nargout <= len)
      {
        octave_value_list retval (nargout);

        int i = 0;

        for (tree_decl_elt *elt : *param_list)
          {
            if (elt->is_defined ())
              retval(i++) = evaluate (elt);
            else
              break;
          }

        return retval;
      }
    else
      {
        octave_value_list retval (len + vlen);

        int i = 0;

        for (tree_decl_elt *elt : *param_list)
          retval(i++) = evaluate (elt);

        for (octave_idx_type j = 0; j < vlen; j++)
          retval(i++) = varargout(j);

        return retval;
      }
  }

  bool
  tree_evaluator::eval_decl_elt (tree_decl_elt *elt)
  {
    bool retval = false;

    tree_identifier *id = elt->ident ();
    tree_expression *expr = elt->expression ();

    if (id && expr)
      {
        octave_lvalue ult = id->lvalue (this);

        octave_value init_val = evaluate (expr);

        ult.assign (octave_value::op_asn_eq, init_val);

        retval = true;
      }

    return retval;
  }

  bool
  tree_evaluator::switch_case_label_matches (tree_switch_case *expr,
                                             const octave_value& val)
  {
    tree_expression *label = expr->case_label ();

    octave_value label_value = evaluate (label);

    if (label_value.is_defined ())
      {
        if (label_value.iscell ())
          {
            Cell cell (label_value.cell_value ());

            for (octave_idx_type i = 0; i < cell.rows (); i++)
              {
                for (octave_idx_type j = 0; j < cell.columns (); j++)
                  {
                    bool match = val.is_equal (cell(i,j));

                    if (match)
                      return true;
                  }
              }
          }
        else
          return val.is_equal (label_value);
      }

    return false;
  }

  void
  tree_evaluator::visit_decl_command (tree_decl_command& cmd)
  {
    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    tree_decl_init_list *init_list = cmd.initializer_list ();

    if (init_list)
      init_list->accept (*this);
  }

  void
  tree_evaluator::visit_decl_init_list (tree_decl_init_list& lst)
  {
    for (tree_decl_elt *elt : lst)
      elt->accept (*this);
  }

  void
  tree_evaluator::visit_decl_elt (tree_decl_elt& elt)
  {
    octave::tree_identifier *id = elt.ident ();

    if (id)
      {
        if (elt.is_global ())
          id->mark_global ();
        else if (elt.is_persistent ())
          id->mark_persistent ();
        else
          error ("declaration list element not global or persistent");

        octave_lvalue ult = id->lvalue (this);

        if (ult.is_undefined ())
          {
            octave::tree_expression *expr = elt.expression ();

            octave_value init_val;

            if (expr)
              init_val = evaluate (expr);
            else
              init_val = Matrix ();

            ult.assign (octave_value::op_asn_eq, init_val);
          }
      }
  }
}

// Decide if it's time to quit a for or while loop.
static inline bool
quit_loop_now (void)
{
  octave_quit ();

  // Maybe handle 'continue N' someday...

  if (octave::tree_continue_command::continuing)
    octave::tree_continue_command::continuing--;

  bool quit = (octave::tree_return_command::returning
               || octave::tree_break_command::breaking
               || octave::tree_continue_command::continuing);

  if (octave::tree_break_command::breaking)
    octave::tree_break_command::breaking--;

  return quit;
}

namespace octave
{
  void
  tree_evaluator::visit_simple_for_command (tree_simple_for_command& cmd)
  {
    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    // FIXME: need to handle PARFOR loops here using cmd.in_parallel ()
    // and cmd.maxproc_expr ();

    octave::unwind_protect frame;

    frame.protect_var (in_loop_command);

    in_loop_command = true;

    tree_expression *expr = cmd.control_expr ();

    octave_value rhs = evaluate (expr);

#if defined (HAVE_LLVM)
    if (tree_jit::execute (cmd, rhs))
      return;
#endif

    if (rhs.is_undefined ())
      return;

    tree_expression *lhs = cmd.left_hand_side ();

    octave_lvalue ult = lhs->lvalue (this);

    tree_statement_list *loop_body = cmd.body ();

    if (rhs.is_range ())
      {
        Range rng = rhs.range_value ();

        octave_idx_type steps = rng.numel ();

        for (octave_idx_type i = 0; i < steps; i++)
          {
            octave_value val (rng.elem (i));

            ult.assign (octave_value::op_asn_eq, val);

            if (loop_body)
              loop_body->accept (*this);

            if (quit_loop_now ())
              break;
          }
      }
    else if (rhs.is_scalar_type ())
      {
        ult.assign (octave_value::op_asn_eq, rhs);

        if (loop_body)
          loop_body->accept (*this);

        // Maybe decrement break and continue states.
        quit_loop_now ();
      }
    else if (rhs.is_matrix_type () || rhs.iscell () || rhs.is_string ()
             || rhs.isstruct ())
      {
        // A matrix or cell is reshaped to 2 dimensions and iterated by
        // columns.

        dim_vector dv = rhs.dims ().redim (2);

        octave_idx_type nrows = dv(0);
        octave_idx_type steps = dv(1);

        octave_value arg = rhs;
        if (rhs.ndims () > 2)
          arg = arg.reshape (dv);

        if (nrows > 0 && steps > 0)
          {
            octave_value_list idx;
            octave_idx_type iidx;

            // for row vectors, use single index to speed things up.
            if (nrows == 1)
              {
                idx.resize (1);
                iidx = 0;
              }
            else
              {
                idx.resize (2);
                idx(0) = octave_value::magic_colon_t;
                iidx = 1;
              }

            for (octave_idx_type i = 1; i <= steps; i++)
              {
                // do_index_op expects one-based indices.
                idx(iidx) = i;
                octave_value val = arg.do_index_op (idx);

                ult.assign (octave_value::op_asn_eq, val);

                if (loop_body)
                  loop_body->accept (*this);

                if (quit_loop_now ())
                  break;
              }
          }
        else
          {
            // Handle empty cases, while still assigning to loop var.
            ult.assign (octave_value::op_asn_eq, arg);
          }
      }
    else
      error ("invalid type in for loop expression near line %d, column %d",
             cmd.line (), cmd.column ());
  }

  void
  tree_evaluator::visit_complex_for_command (tree_complex_for_command& cmd)
  {
    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    octave::unwind_protect frame;

    frame.protect_var (in_loop_command);

    in_loop_command = true;

    tree_expression *expr = cmd.control_expr ();

    octave_value rhs = evaluate (expr);

    if (rhs.is_undefined ())
      return;

    if (! rhs.isstruct ())
      error ("in statement 'for [X, Y] = VAL', VAL must be a structure");

    // Cycle through structure elements.  First element of id_list
    // is set to value and the second is set to the name of the
    // structure element.

    tree_argument_list *lhs = cmd.left_hand_side ();

    tree_argument_list::iterator p = lhs->begin ();

    tree_expression *elt = *p++;

    octave_lvalue val_ref = elt->lvalue (this);

    elt = *p;

    octave_lvalue key_ref = elt->lvalue (this);

    const octave_map tmp_val = rhs.map_value ();

    tree_statement_list *loop_body = cmd.body ();

    string_vector keys = tmp_val.keys ();

    octave_idx_type nel = keys.numel ();

    for (octave_idx_type i = 0; i < nel; i++)
      {
        std::string key = keys[i];

        const Cell val_lst = tmp_val.contents (key);

        octave_idx_type n = val_lst.numel ();

        octave_value val = (n == 1) ? val_lst(0) : octave_value (val_lst);

        val_ref.assign (octave_value::op_asn_eq, val);
        key_ref.assign (octave_value::op_asn_eq, key);

        if (loop_body)
          loop_body->accept (*this);

        if (quit_loop_now ())
          break;
      }
  }

  void
  tree_evaluator::visit_octave_user_script (octave_user_script&)
  {
    // ??
    panic_impossible ();
  }

  void
  tree_evaluator::visit_octave_user_function (octave_user_function&)
  {
    // ??
    panic_impossible ();
  }

  void
  tree_evaluator::visit_octave_user_function_header (octave_user_function&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_octave_user_function_trailer (octave_user_function&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_function_def (tree_function_def& cmd)
  {
    octave_value fcn = cmd.function ();

    octave_function *f = fcn.function_value ();

    if (f)
      {
        std::string nm = f->name ();

        symbol_table::install_cmdline_function (nm, fcn);

        // Make sure that any variable with the same name as the new
        // function is cleared.

        symbol_table::assign (nm);
      }
  }

  void
  tree_evaluator::visit_identifier (tree_identifier& expr)
  {
    octave_value_list retval;

    symbol_table::symbol_reference sym = expr.symbol ();

    octave_value val = sym->find ();

    if (val.is_defined ())
      {
        // GAGME -- this would be cleaner if we required
        // parens to indicate function calls.
        //
        // If this identifier refers to a function, we need to know
        // whether it is indexed so that we can do the same thing
        // for 'f' and 'f()'.  If the index is present and the function
        // object declares it can handle it, return the function object
        // and let tree_index_expression::rvalue handle indexing.
        // Otherwise, arrange to call the function here, so that we don't
        // return the function definition as a value.

        octave_function *fcn = nullptr;

        if (val.is_function ())
          fcn = val.function_value (true);

        int nargout = m_nargout_stack.top ();

        if (fcn && ! (expr.is_postfix_indexed ()
                      && fcn->accepts_postfix_index (expr.postfix_index ())))
          {
            retval = fcn->call (*this, nargout);
          }
        else
          {
            if (expr.print_result () && nargout == 0
                && octave::tree_evaluator::statement_printing_enabled ())
              {
                octave_value_list args = ovl (val);
                args.stash_name_tags (string_vector (expr.name ()));
                octave::feval ("display", args);
              }

            retval = val;
          }
      }
    else if (sym->is_added_static ())
      expr.static_workspace_error ();
    else
      expr.eval_undefined_error ();

    m_value_stack.push (retval);
  }

  void
  tree_evaluator::visit_if_clause (tree_if_clause&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_if_command (tree_if_command& cmd)
  {
    tree_if_command_list *lst = cmd.cmd_list ();

    if (lst)
      lst->accept (*this);
  }

  void
  tree_evaluator::visit_if_command_list (tree_if_command_list& lst)
  {
    for (tree_if_clause *tic : lst)
      {
        tree_expression *expr = tic->condition ();

        if (statement_context == function || statement_context == script)
          m_call_stack.set_location (tic->line (), tic->column ());

        if (debug_mode && ! tic->is_else_clause ())
          do_breakpoint (tic->is_breakpoint (true));

        if (tic->is_else_clause () || is_logically_true (expr, "if"))
          {
            tree_statement_list *stmt_lst = tic->commands ();

            if (stmt_lst)
              stmt_lst->accept (*this);

            break;
          }
      }
  }
}

// Final step of processing an indexing error.  Add the name of the
// variable being indexed, if any, then issue an error.  (Will this also
// be needed by pt-lvalue, which calls subsref?)

static void
final_index_error (octave::index_exception& e,
                   const octave::tree_expression *expr)
{
  std::string extra_message;

  if (expr->is_identifier ()
      && dynamic_cast<const octave::tree_identifier *> (expr)->is_variable ())
    {
      std::string var = expr->name ();

      e.set_var (var);

      octave_value fcn = symbol_table::find_function (var);

      if (fcn.is_function ())
        {
          octave_function *fp = fcn.function_value ();

          if (fp && fp->name () == var)
            extra_message = " (note: variable '" + var + "' shadows function)";
        }
    }

  std::string msg = e.message () + extra_message;

  error_with_id (e.err_id (), msg.c_str ());
}

namespace octave
{
  // Unlike Matlab, which does not allow the result of a function call
  // or array indexing expression to be further indexed, Octave attempts
  // to handle arbitrary index expressions.  For example, Octave allows
  // expressions like
  //
  //   svd (rand (10))(1:5)
  //
  // Although octave_value objects may contain function objects, no
  // indexing operation or function call is supposed to return them
  // directly.  Instead, the language is supposed to only allow function
  // objects to be stored as function handles (named or anonymous) or as
  // inline functions.  The only place a function object should appear
  // directly is if the symbol stored in a tree_identifier object
  // resolves to a function.  This means that the only place we need to
  // look for functions is in the first element of the index
  // expression.
  //
  // Steps:
  //
  //  * Obtain the initial value from the expression component of the
  //    tree_index_expression object.  If it is a tree_identifier object
  //    indexed by '(args)' and the identifier is not a variable, then
  //    peform a function call.  Use the (optional) arguments to perform
  //    the function lookup so we choose the correct function or class
  //    method to call.  Otherwise, evaluate the first expression
  //    without any additional arguments.
  //
  //  * Iterate over the remaining elements of the index expression and
  //    call the octave_value::subsref method.  If indexing a class or
  //    classdef object, build up a list of indices for a call to the
  //    subsref method for the object.  Otherwise, use the result of
  //    each temporary evaluation for the next index element.
  //
  //  * If not indexing a class or classdef object and any partial
  //    expression evaluation produces a class or classdef object, then
  //    build up a complete argument list from that point on for a final
  //    subsref call for that object.
  //
  //    Multiple partial evaluations may be required.  For example,
  //    given a class or classdef object X, then for the expression
  //
  //      x.a{end}(2:end).b
  //
  //    we must evaluate x.a to obtain the size for the first {end}
  //    expression, then we must evaluate x.a{end} to obtain the size
  //    for the second (2:end) expression.  Finally, the complete
  //    expression may be evaluated.
  //
  //    If X is a cell array in the above expression, and none of the
  //    intermediate evaluations produces a class or classdef object,
  //    then the evaluation is performed as the following series of
  //    steps
  //
  //      tmp = x.a
  //      tmp = tmp{end}
  //      tmp = tmp(2:end)
  //      result = tmp.b
  //
  //    If any of the partial evaluations produces a class or classdef
  //    object, then the subsref method for that object is called as
  //    described above.  For example, suppose x.a produces a classdef
  //    object.  Then the evaluation is performed as the following
  //    series of steps
  //
  //      base_expr = tmp = x.a
  //      tmp = base_expr{end}
  //      base_expr{end}(2:end).b
  //
  //    In the last two steps, the partial value computed in the
  //    previous step is used to determine the value of END.

  void
  tree_evaluator::visit_index_expression (tree_index_expression& idx_expr)
  {
    octave_value_list retval;

    int nargout = m_nargout_stack.top ();

    std::string type = idx_expr.type_tags ();
    std::list<tree_argument_list *> args = idx_expr.arg_lists ();
    std::list<string_vector> arg_nm = idx_expr.arg_names ();
    std::list<tree_expression *> dyn_field = idx_expr.dyn_fields ();

    assert (! args.empty ());

    std::list<tree_argument_list *>::iterator p_args = args.begin ();
    std::list<string_vector>::iterator p_arg_nm = arg_nm.begin ();
    std::list<tree_expression *>::iterator p_dyn_field = dyn_field.begin ();

    int n = args.size ();
    int beg = 0;

    octave_value base_expr_val;

    tree_expression *expr = idx_expr.expression ();

    if (expr->is_identifier () && type[beg] == '(')
      {
        tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

        if (! id->is_variable ())
          {
            octave_value_list first_args;

            tree_argument_list *al = *p_args;

            if (al && al->length () > 0)
              {
                // Function calls inside an argument list can't have
                // ignored output arguments.

                unwind_protect frame;

                m_lvalue_list_stack.push (0);

                frame.add_method (m_lvalue_list_stack,
                                  &value_stack<const std::list<octave_lvalue>*>::pop);

                string_vector anm = *p_arg_nm;
                first_args = al->convert_to_const_vector (this);
                first_args.stash_name_tags (anm);
              }

            octave_function *fcn = nullptr;

            octave_value val = id->do_lookup (first_args);

            if (val.is_function ())
              fcn = val.function_value (true);

            if (fcn)
              {
                try
                  {
                    retval = fcn->call (*this, nargout, first_args);
                  }
                catch (octave::index_exception& e)
                  {
                    final_index_error (e, expr);
                  }

                beg++;
                p_args++;
                p_arg_nm++;
                p_dyn_field++;

                if (n > beg)
                  {
                    // More indices to follow.  Silently ignore
                    // extra output values.

                    if (retval.length () == 0)
                      error ("indexing undefined value");
                    else
                      base_expr_val = retval(0);
                  }
                else
                  {
                    // No more indices, so we are done.

                    m_value_stack.push (retval);
                    return;
                  }
              }
          }
      }

    if (base_expr_val.is_undefined ())
      base_expr_val = evaluate (expr);

    bool indexing_object = base_expr_val.isobject ();

    std::list<octave_value_list> idx;

    octave_value partial_expr_val = base_expr_val;

    for (int i = beg; i < n; i++)
      {
        if (i > beg)
          {
            tree_argument_list *al = *p_args;

            if (! indexing_object || (al && al->has_magic_end ()))
              {
                // Evaluate what we have so far to find the value to
                // pass to the END function.

                try
                  {
                    // Silently ignore extra output values.

                    octave_value_list tmp_list
                      = base_expr_val.subsref (type.substr (beg, i-beg), idx, nargout);

                    partial_expr_val = tmp_list.length () ? tmp_list(0) : octave_value ();

                    if (! indexing_object)
                      {
                        base_expr_val = partial_expr_val;

                        if (partial_expr_val.is_cs_list ())
                          err_indexed_cs_list ();

                        retval = partial_expr_val;

                        beg = i;
                        idx.clear ();

                        if (partial_expr_val.isobject ())
                          {
                            // Found an object, so now we'll build up
                            // complete index list for one big subsref
                            // call from this point on.

                            indexing_object = true;
                          }
                      }
                  }
                catch (octave::index_exception& e)
                  {
                    final_index_error (e, expr);
                  }
              }
          }

        switch (type[i])
          {
          case '(':
            idx.push_back (make_value_list (*p_args, *p_arg_nm, &partial_expr_val));
            break;

          case '{':
            idx.push_back (make_value_list (*p_args, *p_arg_nm, &partial_expr_val));
            break;

          case '.':
            idx.push_back (octave_value
                           (idx_expr.get_struct_index (this, p_arg_nm, p_dyn_field)));
            break;

          default:
            panic_impossible ();
          }

        p_args++;
        p_arg_nm++;
        p_dyn_field++;
      }

    if (! idx.empty ())
      {
        try
          {
            retval = base_expr_val.subsref (type.substr (beg, n-beg), idx, nargout);
          }
        catch (octave::index_exception& e)  // range problems, bad index type, etc.
          {
            final_index_error (e, expr);
          }
      }

    m_value_stack.push (retval);
  }

  void
  tree_evaluator::visit_matrix (tree_matrix& expr)
  {
    octave_value retval = Matrix ();

    bool all_strings_p = false;
    bool all_sq_strings_p = false;
    bool all_dq_strings_p = false;
    bool all_empty_p = false;
    bool all_real_p = false;
    bool any_sparse_p = false;
    bool any_class_p = false;
    bool frc_str_conv = false;

    tm_const tmp (expr, this);

    if (tmp && ! tmp.empty ())
      {
        dim_vector dv = tmp.dims ();
        all_strings_p = tmp.all_strings_p ();
        all_sq_strings_p = tmp.all_sq_strings_p ();
        all_dq_strings_p = tmp.all_dq_strings_p ();
        all_empty_p = tmp.all_empty_p ();
        all_real_p = tmp.all_real_p ();
        any_sparse_p = tmp.any_sparse_p ();
        any_class_p = tmp.any_class_p ();
        frc_str_conv = tmp.some_strings_p ();

        // Try to speed up the common cases.

        std::string result_type = tmp.class_name ();

        if (any_class_p)
          {
            retval = do_class_concat (tmp);
          }
        else if (result_type == "double")
          {
            if (any_sparse_p)
              {
                if (all_real_p)
                  retval = do_single_type_concat<SparseMatrix> (dv, tmp);
                else
                  retval = do_single_type_concat<SparseComplexMatrix> (dv, tmp);
              }
            else
              {
                if (all_real_p)
                  retval = do_single_type_concat<NDArray> (dv, tmp);
                else
                  retval = do_single_type_concat<ComplexNDArray> (dv, tmp);
              }
          }
        else if (result_type == "single")
          {
            if (all_real_p)
              retval = do_single_type_concat<FloatNDArray> (dv, tmp);
            else
              retval = do_single_type_concat<FloatComplexNDArray> (dv, tmp);
          }
        else if (result_type == "char")
          {
            char type = (all_dq_strings_p ? '"' : '\'');

            if (! all_strings_p)
              warn_implicit_conversion ("Octave:num-to-str",
                                        "numeric", result_type);
            else
              maybe_warn_string_concat (all_dq_strings_p, all_sq_strings_p);

            charNDArray result (dv, Vstring_fill_char);

            single_type_concat<charNDArray> (result, tmp);

            retval = octave_value (result, type);
          }
        else if (result_type == "logical")
          {
            if (any_sparse_p)
              retval = do_single_type_concat<SparseBoolMatrix> (dv, tmp);
            else
              retval = do_single_type_concat<boolNDArray> (dv, tmp);
          }
        else if (result_type == "int8")
          retval = do_single_type_concat<int8NDArray> (dv, tmp);
        else if (result_type == "int16")
          retval = do_single_type_concat<int16NDArray> (dv, tmp);
        else if (result_type == "int32")
          retval = do_single_type_concat<int32NDArray> (dv, tmp);
        else if (result_type == "int64")
          retval = do_single_type_concat<int64NDArray> (dv, tmp);
        else if (result_type == "uint8")
          retval = do_single_type_concat<uint8NDArray> (dv, tmp);
        else if (result_type == "uint16")
          retval = do_single_type_concat<uint16NDArray> (dv, tmp);
        else if (result_type == "uint32")
          retval = do_single_type_concat<uint32NDArray> (dv, tmp);
        else if (result_type == "uint64")
          retval = do_single_type_concat<uint64NDArray> (dv, tmp);
        else if (result_type == "cell")
          retval = do_single_type_concat<Cell> (dv, tmp);
        else if (result_type == "struct")
          retval = do_single_type_concat<octave_map> (dv, tmp);
        else
          {
            // The line below might seem crazy, since we take a copy of
            // the first argument, resize it to be empty and then resize
            // it to be full.  This is done since it means that there is
            // no recopying of data, as would happen if we used a single
            // resize.  It should be noted that resize operation is also
            // significantly slower than the do_cat_op function, so it
            // makes sense to have an empty matrix and copy all data.
            //
            // We might also start with a empty octave_value using
            //
            //    ctmp = octave_value_typeinfo::lookup_type
            //          (tmp.begin() -> begin() -> type_name());
            //
            // and then directly resize.  However, for some types there
            // might be some additional setup needed, and so this should
            // be avoided.

            octave_value ctmp;

            // Find the first non-empty object

            if (any_sparse_p)
              {
                // Start with sparse matrix to avoid issues memory issues
                // with things like [ones(1,4),sprandn(1e8,4,1e-4)]
                if (all_real_p)
                  ctmp = octave_sparse_matrix ().resize (dv);
                else
                  ctmp = octave_sparse_complex_matrix ().resize (dv);
              }
            else
              {
                for (tm_row_const& row : tmp)
                  {
                    octave_quit ();

                    for (auto& elt : row)
                      {
                        octave_quit ();

                        ctmp = elt;

                        if (! ctmp.all_zero_dims ())
                          goto found_non_empty;
                      }
                  }

                ctmp = (*(tmp.begin () -> begin ()));

              found_non_empty:

                if (! all_empty_p)
                  ctmp = ctmp.resize (dim_vector (0,0)).resize (dv);
              }

            // Now, extract the values from the individual elements and
            // insert them in the result matrix.

            int dv_len = dv.ndims ();
            octave_idx_type ntmp = (dv_len > 1 ? dv_len : 2);
            Array<octave_idx_type> ra_idx (dim_vector (ntmp, 1), 0);

            for (tm_row_const& row : tmp)
              {
                octave_quit ();

                for (auto& elt : row)
                  {
                    octave_quit ();

                    if (elt.isempty ())
                      continue;

                    ctmp = do_cat_op (ctmp, elt, ra_idx);

                    ra_idx (1) += elt.columns ();
                  }

                ra_idx (0) += row.rows ();
                ra_idx (1) = 0;
              }

            retval = ctmp;

            if (frc_str_conv && ! retval.is_string ())
              retval = retval.convert_to_str ();
          }
      }

    m_value_stack.push (retval);
  }

  void
  tree_evaluator::visit_cell (tree_cell& expr)
  {
    octave_value retval;

    // Function calls inside an argument list can't have ignored
    // output arguments.

    unwind_protect frame;

    m_lvalue_list_stack.push (0);

    frame.add_method (m_lvalue_list_stack,
                      &value_stack<const std::list<octave_lvalue>*>::pop);

    octave_idx_type nr = expr.length ();
    octave_idx_type nc = -1;

    Cell val;

    octave_idx_type i = 0;

    for (tree_argument_list *elt : expr)
      {
        octave_value_list row = elt->convert_to_const_vector (this);

        if (nr == 1)
          // Optimize the single row case.
          val = row.cell_value ();
        else if (nc < 0)
          {
            nc = row.length ();

            val = Cell (nr, nc);
          }
        else
          {
            octave_idx_type this_nc = row.length ();

            if (this_nc != nc)
              {
                if (this_nc == 0)
                  continue;  // blank line
                else
                  error ("number of columns must match");
              }
          }

        for (octave_idx_type j = 0; j < nc; j++)
          val(i,j) = row(j);

        i++;
      }

    if (i < nr)
      val.resize (dim_vector (i, nc));  // there were blank rows

    retval = val;

    m_value_stack.push (retval);
  }

  void
  tree_evaluator::visit_multi_assignment (tree_multi_assignment& expr)
  {
    octave_value_list val;

    tree_expression *rhs = expr.right_hand_side ();

    if (rhs)
      {
        unwind_protect frame;

        tree_argument_list *lhs = expr.left_hand_side ();

        std::list<octave_lvalue> lvalue_list = make_lvalue_list (lhs);

        m_lvalue_list_stack.push (&lvalue_list);

        frame.add_method (m_lvalue_list_stack,
                          &value_stack<const std::list<octave_lvalue>*>::pop);

        octave_idx_type n_out = 0;

        for (const auto& lval : lvalue_list)
          n_out += lval.numel ();

        // The following trick is used to keep rhs_val constant.
        const octave_value_list rhs_val1 = evaluate_n (rhs, n_out);
        const octave_value_list rhs_val = (rhs_val1.length () == 1
                                           && rhs_val1(0).is_cs_list ()
                                           ? rhs_val1(0).list_value ()
                                           : rhs_val1);

        octave_idx_type k = 0;

        octave_idx_type n = rhs_val.length ();

        // To avoid copying per elements and possible optimizations, we
        // postpone joining the final values.
        std::list<octave_value_list> retval_list;

        tree_argument_list::iterator q = lhs->begin ();

        for (octave_lvalue ult : lvalue_list)
          {
            tree_expression *lhs_elt = *q++;

            octave_idx_type nel = ult.numel ();

            if (nel != 1)
              {
                // Huge kluge so that wrapper scripts with lines like
                //
                //   [varargout{1:nargout}] = fcn (args);
                //
                // Will work the same as calling fcn directly when nargout
                // is 0 and fcn produces more than one output even when
                // nargout is 0.  This only works if varargout has not yet
                // been defined.  See also bug #43813.

                if (lvalue_list.size () == 1 && nel == 0 && n > 0
                    && ! ult.is_black_hole () && ult.is_undefined ()
                    && ult.index_type () == "{" && ult.index_is_empty ())
                  {
                    // Convert undefined lvalue with empty index to a cell
                    // array with a single value and indexed by 1 to
                    // handle a single output.

                    nel = 1;

                    ult.define (Cell (1, 1));

                    ult.clear_index ();
                    std::list<octave_value_list> idx;
                    idx.push_back (octave_value_list (octave_value (1)));
                    ult.set_index ("{", idx);
                  }

                if (k + nel > n)
                  error ("some elements undefined in return list");

                // This element of the return list expects a
                // comma-separated list of values.  Slicing avoids
                // copying.

                octave_value_list ovl = rhs_val.slice (k, nel);

                ult.assign (octave_value::op_asn_eq, octave_value (ovl));

                retval_list.push_back (ovl);

                k += nel;
              }
            else
              {
                if (k < n)
                  {
                    ult.assign (octave_value::op_asn_eq, rhs_val(k));

                    if (ult.is_black_hole ())
                      {
                        k++;
                        continue;
                      }
                    else
                      {
                        retval_list.push_back (rhs_val(k));

                        k++;
                      }
                  }
                else
                  {
                    // This can happen for a function like
                    //
                    //   function varargout = f ()
                    //     varargout{1} = nargout;
                    //   endfunction
                    //
                    // called with
                    //
                    //    [a, ~] = f ();
                    //
                    // Then the list of of RHS values will contain one
                    // element but we are iterating over the list of all
                    // RHS values.  We shouldn't complain that a value we
                    // don't need is missing from the list.

                    if (! ult.is_black_hole ())
                      error ("element number %d undefined in return list", k+1);

                    k++;
                    continue;
                  }
              }

            if (expr.print_result ()
                && octave::tree_evaluator::statement_printing_enabled ())
              {
                // We clear any index here so that we can get
                // the new value of the referenced object below,
                // instead of the indexed value (which should be
                // the same as the right hand side value).

                ult.clear_index ();

                octave_value lhs_val = ult.value ();

                octave_value_list args = ovl (lhs_val);
                args.stash_name_tags (string_vector (lhs_elt->name ()));
                octave::feval ("display", args);
              }
          }

        // Concatenate return values.
        val = retval_list;
      }

    m_value_stack.push (val);
  }

  void
  tree_evaluator::visit_no_op_command (tree_no_op_command& cmd)
  {
    if (debug_mode && cmd.is_end_of_fcn_or_script ())
      do_breakpoint (cmd.is_breakpoint (true), true);
  }

  void
  tree_evaluator::visit_constant (tree_constant& expr)
  {
    int nargout = m_nargout_stack.top ();

    if (nargout > 1)
      error ("invalid number of output arguments for constant expression");

    m_value_stack.push (ovl (expr.value ()));
  }

  void
  tree_evaluator::visit_fcn_handle (tree_fcn_handle& expr)
  {
    std::string nm = expr.name ();

    octave_value fh = make_fcn_handle (nm);

    m_value_stack.push (ovl (fh));
  }

  void
  tree_evaluator::visit_funcall (tree_funcall& expr)
  {
    octave_value_list retval;

    octave_value fcn = expr.function ();

    octave_value_list args = expr.arguments ();

    int nargout = m_nargout_stack.top ();

    retval = octave::feval (fcn.function_value (), args, nargout);

    if (retval.length () == 1 && retval(0).is_function ())
      {
        // The return object is a function.  We may need to re-index it
        // using the same logic as for identifier.  This is primarily
        // used for superclass references in classdef.

        octave_value val = retval(0);
        octave_function *f = val.function_value (true);

        if (f && ! (expr.is_postfix_indexed ()
                    && f->accepts_postfix_index (expr.postfix_index ())))
          {
            retval = f->call (*this, nargout);
          }
      }

    m_value_stack.push (retval);
  }

  void
  tree_evaluator::visit_parameter_list (tree_parameter_list&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_postfix_expression (tree_postfix_expression& expr)
  {
    octave_value val;

    tree_expression *op = expr.operand ();

    if (op)
      {
        octave_value::unary_op etype = expr.op_type ();

        if (etype == octave_value::op_incr || etype == octave_value::op_decr)
          {
            octave_lvalue ref = op->lvalue (this);

            val = ref.value ();

            profile_data_accumulator::enter<tree_postfix_expression>
              block (profiler, expr);

            ref.do_unary_op (etype);
          }
        else
          {
            octave_value op_val = evaluate (op);

            if (op_val.is_defined ())
              {
                profile_data_accumulator::enter<tree_postfix_expression>
                  block (profiler, expr);

                val = ::do_unary_op (etype, op_val);
              }
          }
      }

    m_value_stack.push (ovl (val));
  }

  void
  tree_evaluator::visit_prefix_expression (tree_prefix_expression& expr)
  {
    octave_value val;

    tree_expression *op = expr.operand ();

    if (op)
      {
        octave_value::unary_op etype = expr.op_type ();

        if (etype == octave_value::op_incr || etype == octave_value::op_decr)
          {
            octave_lvalue op_ref = op->lvalue (this);

            profile_data_accumulator::enter<tree_prefix_expression>
              block (profiler, expr);

            op_ref.do_unary_op (etype);

            val = op_ref.value ();
          }
        else
          {
            octave_value op_val = evaluate (op);

            if (op_val.is_defined ())
              {
                profile_data_accumulator::enter<tree_prefix_expression>
                  block (profiler, expr);

                // Attempt to do the operation in-place if it is unshared
                // (a temporary expression).
                if (op_val.get_count () == 1)
                  val = op_val.do_non_const_unary_op (etype);
                else
                  val = ::do_unary_op (etype, op_val);
              }
          }
      }

    m_value_stack.push (ovl (val));
  }

  void
  tree_evaluator::visit_return_command (tree_return_command& cmd)
  {
    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    // Act like dbcont.

    if (Vdebugging && m_call_stack.current_frame () == current_frame)
      {
        Vdebugging = false;

        reset_debug_state ();
      }
    else if (statement_context == function || statement_context == script
             || in_loop_command)
      tree_return_command::returning = 1;
  }

  void
  tree_evaluator::visit_return_list (tree_return_list&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_simple_assignment (tree_simple_assignment& expr)
  {
    octave_value val;

    tree_expression *rhs = expr.right_hand_side ();

    if (rhs)
      {
        octave_value rhs_val = evaluate (rhs);

        if (rhs_val.is_undefined ())
          error ("value on right hand side of assignment is undefined");

        if (rhs_val.is_cs_list ())
          {
            const octave_value_list lst = rhs_val.list_value ();

            if (lst.empty ())
              error ("invalid number of elements on RHS of assignment");

            rhs_val = lst(0);
          }

        tree_expression *lhs = expr.left_hand_side ();

        try
          {
            octave::unwind_protect frame;

            octave_lvalue ult = lhs->lvalue (this);

            std::list<octave_lvalue> lvalue_list;
            lvalue_list.push_back (ult);

            m_lvalue_list_stack.push (&lvalue_list);

            frame.add_method (m_lvalue_list_stack,
                              &value_stack<const std::list<octave_lvalue>*>::pop);

            if (ult.numel () != 1)
              err_nonbraced_cs_list_assignment ();

            octave_value::assign_op etype = expr.op_type ();

            ult.assign (etype, rhs_val);

            if (etype == octave_value::op_asn_eq)
              val = rhs_val;
            else
              val = ult.value ();

            if (expr.print_result ()
                && octave::tree_evaluator::statement_printing_enabled ())
              {
                // We clear any index here so that we can
                // get the new value of the referenced
                // object below, instead of the indexed
                // value (which should be the same as the
                // right hand side value).

                ult.clear_index ();

                octave_value lhs_val = ult.value ();

                octave_value_list args = ovl (lhs_val);
                args.stash_name_tags (string_vector (lhs->name ()));
                octave::feval ("display", args);
              }
          }
        catch (octave::index_exception& e)
          {
            e.set_var (lhs->name ());
            std::string msg = e.message ();
            error_with_id (e.err_id (), msg.c_str ());
          }
      }

    m_value_stack.push (ovl (val));
  }

  void
  tree_evaluator::visit_statement (tree_statement& stmt)
  {
    tree_command *cmd = stmt.command ();
    tree_expression *expr = stmt.expression ();

    if (cmd || expr)
      {
        if (statement_context == function || statement_context == script)
          {
            // Skip commands issued at a debug> prompt to avoid disturbing
            // the state of the program we are debugging.

            if (Vtrack_line_num)
              m_call_stack.set_location (stmt.line (), stmt.column ());

            if ((statement_context == script
                 && ((Vecho_executing_commands & ECHO_SCRIPTS
                      && m_call_stack.all_scripts ())
                     || Vecho_executing_commands & ECHO_FUNCTIONS))
                || (statement_context == function
                    && Vecho_executing_commands & ECHO_FUNCTIONS))
              stmt.echo_code ();
          }

        try
          {
            if (cmd)
              cmd->accept (*this);
            else
              {
                if (debug_mode)
                  do_breakpoint (expr->is_breakpoint (true));

                // FIXME: maybe all of this should be packaged in
                // one virtual function that returns a flag saying whether
                // or not the expression will take care of binding ans and
                // printing the result.

                // FIXME: it seems that we should just have to
                // evaluate the expression and that should take care of
                // everything, binding ans as necessary?

                bool do_bind_ans = false;

                if (expr->is_identifier ())
                  {
                    tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

                    do_bind_ans = (! id->is_variable ());
                  }
                else
                  do_bind_ans = (! expr->is_assignment_expression ());

                octave_value tmp_result = evaluate (expr, 0);

                if (do_bind_ans && tmp_result.is_defined ())
                  bind_ans (tmp_result, expr->print_result ()
                            && statement_printing_enabled ());

                //              if (tmp_result.is_defined ())
                //                result_values(0) = tmp_result;
              }
          }
        catch (const std::bad_alloc&)
          {
            // FIXME: We want to use error_with_id here so that give users
            // control over this error message but error_with_id will
            // require some memory allocations.  Is there anything we can
            // do to make those more likely to succeed?

            error_with_id ("Octave:bad-alloc",
                           "out of memory or dimension too large for Octave's index type");
          }
      }
  }

  void
  tree_evaluator::visit_statement_list (tree_statement_list& lst)
  {
    // FIXME: commented out along with else clause below.
    // static octave_value_list empty_list;

    tree_statement_list::iterator p = lst.begin ();

    if (p != lst.end ())
      {
        while (true)
          {
            tree_statement *elt = *p++;

            if (! elt)
              error ("invalid statement found in statement list!");

            octave_quit ();

            elt->accept (*this);

            if (tree_break_command::breaking
                || tree_continue_command::continuing)
              break;

            if (tree_return_command::returning)
              break;

            if (p == lst.end ())
              break;
            else
              {
                // Clear previous values before next statement is
                // evaluated so that we aren't holding an extra
                // reference to a value that may be used next.  For
                // example, in code like this:
                //
                //   X = rand (N);  # refcount for X should be 1
                //                  # after this statement
                //
                //   X(idx) = val;  # no extra copy of X should be
                //                  # needed, but we will be faked
                //                  # out if retval is not cleared
                //                  # between statements here

                //              result_values = empty_list;
              }
          }
      }
  }

  void
  tree_evaluator::visit_switch_case (tree_switch_case&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_switch_case_list (tree_switch_case_list&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_switch_command (tree_switch_command& cmd)
  {
    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    tree_expression *expr = cmd.switch_value ();

    if (! expr)
      error ("missing value in switch command near line %d, column %d",
             cmd.line (), cmd.column ());

    octave_value val = evaluate (expr);

    tree_switch_case_list *lst = cmd.case_list ();

    if (lst)
      {
        for (tree_switch_case *t : *lst)
          {
            if (t->is_default_case () || switch_case_label_matches (t, val))
              {
                tree_statement_list *stmt_lst = t->commands ();

                if (stmt_lst)
                  stmt_lst->accept (*this);

                break;
              }
          }
      }
  }

  void
  tree_evaluator::visit_try_catch_command (tree_try_catch_command& cmd)
  {
    bool execution_error = false;

    {
      // unwind frame before catch block
      octave::unwind_protect frame;

      frame.protect_var (buffer_error_messages);
      frame.protect_var (Vdebug_on_error);
      frame.protect_var (Vdebug_on_warning);

      buffer_error_messages++;
      Vdebug_on_error = false;
      Vdebug_on_warning = false;

      // The catch code is *not* added to unwind_protect stack;
      // it doesn't need to be run on interrupts.

      tree_statement_list *try_code = cmd.body ();

      if (try_code)
        {
          try
            {
              in_try_catch++;
              try_code->accept (*this);
              in_try_catch--;
            }
          catch (const octave::execution_exception&)
            {
              octave::interpreter::recover_from_exception ();

              in_try_catch--;          // must be restored before "catch" block
              execution_error = true;
            }
        }
      // Unwind to let the user print any messages from
      // errors that occurred in the body of the try_catch statement,
      // or throw further errors.
    }

    if (execution_error)
      {
        tree_statement_list *catch_code = cmd.cleanup ();
        if (catch_code)
          {
            tree_identifier *expr_id = cmd.identifier ();
            octave_lvalue ult;

            if (expr_id)
              {
                ult = expr_id->lvalue (this);

                octave_scalar_map err;

                err.assign ("message", last_error_message ());
                err.assign ("identifier", last_error_id ());
                err.assign ("stack", last_error_stack ());

                ult.assign (octave_value::op_asn_eq, err);
              }

            // perform actual "catch" block
            if (catch_code)
              catch_code->accept (*this);
          }
      }
  }

  void
  tree_evaluator::do_unwind_protect_cleanup_code (tree_statement_list *list)
  {
    octave::unwind_protect frame;

    frame.protect_var (octave_interrupt_state);
    octave_interrupt_state = 0;

    // We want to preserve the last location info for possible
    // backtracking.

    frame.add_method (m_call_stack, &octave::call_stack::set_line,
                      m_call_stack.current_line ());
    frame.add_method (m_call_stack, &octave::call_stack::set_column,
                      m_call_stack.current_column ());

    // Similarly, if we have seen a return or break statement, allow all
    // the cleanup code to run before returning or handling the break.
    // We don't have to worry about continue statements because they can
    // only occur in loops.

    frame.protect_var (tree_return_command::returning);
    tree_return_command::returning = 0;

    frame.protect_var (tree_break_command::breaking);
    tree_break_command::breaking = 0;

    try
      {
        if (list)
          list->accept (*this);
      }
    catch (const octave::execution_exception&)
      {
        octave::interpreter::recover_from_exception ();

        if (tree_break_command::breaking || tree_return_command::returning)
          frame.discard (2);
        else
          frame.run (2);

        frame.discard (2);

        throw;
      }

    // The unwind_protects are popped off the stack in the reverse of
    // the order they are pushed on.

    // FIXME: these statements say that if we see a break or
    // return statement in the cleanup block, that we want to use the
    // new value of the breaking or returning flag instead of restoring
    // the previous value.  Is that the right thing to do?  I think so.
    // Consider the case of
    //
    //   function foo ()
    //     unwind_protect
    //       fprintf (stderr, "1: this should always be executed\n");
    //       break;
    //       fprintf (stderr, "1: this should never be executed\n");
    //     unwind_protect_cleanup
    //       fprintf (stderr, "2: this should always be executed\n");
    //       return;
    //       fprintf (stderr, "2: this should never be executed\n");
    //     end_unwind_protect
    //   endfunction
    //
    // If we reset the value of the breaking flag, both the returning
    // flag and the breaking flag will be set, and we shouldn't have
    // both.  So, use the most recent one.  If there is no return or
    // break in the cleanup block, the values should be reset to
    // whatever they were when the cleanup block was entered.

    if (tree_break_command::breaking || tree_return_command::returning)
      frame.discard (2);
    else
      frame.run (2);
  }

  void
  tree_evaluator::visit_unwind_protect_command (tree_unwind_protect_command& cmd)
  {
    tree_statement_list *cleanup_code = cmd.cleanup ();

    tree_statement_list *unwind_protect_code = cmd.body ();

    if (unwind_protect_code)
      {
        try
          {
            unwind_protect_code->accept (*this);
          }
        catch (const octave::execution_exception&)
          {
            // FIXME: Maybe we should be able to temporarily set the
            // interpreter's exception handling state to something "safe"
            // while the cleanup block runs instead of just resetting it
            // here?
            octave::interpreter::recover_from_exception ();

            // Run the cleanup code on exceptions, so that it is run even
            // in case of interrupt or out-of-memory.
            do_unwind_protect_cleanup_code (cleanup_code);

            // If an error occurs inside the cleanup code, a new
            // exception will be thrown instead of the original.
            throw;
          }

        // Also execute the unwind_protect_cleanump code if the
        // unwind_protect block runs without error.
        do_unwind_protect_cleanup_code (cleanup_code);
      }
  }

  void
  tree_evaluator::visit_while_command (tree_while_command& cmd)
  {
#if defined (HAVE_LLVM)
    if (tree_jit::execute (cmd))
      return;
#endif

    octave::unwind_protect frame;

    frame.protect_var (in_loop_command);

    in_loop_command = true;

    tree_expression *expr = cmd.condition ();

    if (! expr)
      panic_impossible ();

    for (;;)
      {
        if (debug_mode)
          do_breakpoint (cmd.is_breakpoint (true));

        if (is_logically_true (expr, "while"))
          {
            tree_statement_list *loop_body = cmd.body ();

            if (loop_body)
              loop_body->accept (*this);

            if (quit_loop_now ())
              break;
          }
        else
          break;
      }
  }

  void
  tree_evaluator::visit_do_until_command (tree_do_until_command& cmd)
  {
#if defined (HAVE_LLVM)
    if (tree_jit::execute (cmd))
      return;
#endif

    octave::unwind_protect frame;

    frame.protect_var (in_loop_command);

    in_loop_command = true;

    tree_expression *expr = cmd.condition ();
    int until_line = cmd.line ();
    int until_column = cmd.column ();

    if (! expr)
      panic_impossible ();

    for (;;)
      {
        tree_statement_list *loop_body = cmd.body ();

        if (loop_body)
          loop_body->accept (*this);

        if (quit_loop_now ())
          break;

        if (debug_mode)
          do_breakpoint (cmd.is_breakpoint (true));

        m_call_stack.set_location (until_line, until_column);

        if (is_logically_true (expr, "do-until"))
          break;
      }
  }

  void
  tree_evaluator::do_breakpoint (tree_statement& stmt) const
  {
    do_breakpoint (stmt.is_breakpoint (true), stmt.is_end_of_fcn_or_script ());
  }

  void
  tree_evaluator::do_breakpoint (bool is_breakpoint,
                                 bool is_end_of_fcn_or_script) const
  {
    bool break_on_this_statement = false;

    if (octave_debug_on_interrupt_state)
      {
        break_on_this_statement = true;

        octave_debug_on_interrupt_state = false;

        current_frame = m_call_stack.current_frame ();
      }
    else if (is_breakpoint)
      {
        break_on_this_statement = true;

        dbstep_flag = 0;

        current_frame = m_call_stack.current_frame ();
      }
    else if (dbstep_flag > 0)
      {
        if (m_call_stack.current_frame () == current_frame)
          {
            if (dbstep_flag == 1 || is_end_of_fcn_or_script)
              {
                // We get here if we are doing a "dbstep" or a "dbstep N" and the
                // count has reached 1 so that we must stop and return to debug
                // prompt.  Alternatively, "dbstep N" has been used but the end
                // of the frame has been reached so we stop at the last line and
                // return to prompt.

                break_on_this_statement = true;

                dbstep_flag = 0;
              }
            else
              {
                // Executing "dbstep N".  Decrease N by one and continue.

                dbstep_flag--;
              }

          }
        else if (dbstep_flag == 1
                 && m_call_stack.current_frame () < current_frame)
          {
            // We stepped out from the end of a function.

            current_frame = m_call_stack.current_frame ();

            break_on_this_statement = true;

            dbstep_flag = 0;
          }
      }
    else if (dbstep_flag == -1)
      {
        // We get here if we are doing a "dbstep in".

        break_on_this_statement = true;

        dbstep_flag = 0;

        current_frame = m_call_stack.current_frame ();
      }
    else if (dbstep_flag == -2)
      {
        // We get here if we are doing a "dbstep out".  Check for end of
        // function and whether the current frame is the same as the
        // cached value because we want to step out from the frame where
        // "dbstep out" was evaluated, not from any functions called from
        // that frame.

        if (is_end_of_fcn_or_script
            && m_call_stack.current_frame () == current_frame)
          dbstep_flag = -1;
      }

    if (break_on_this_statement)
      do_keyboard ();

  }

  // ARGS is currently unused, but since the do_keyboard function in
  // input.cc accepts an argument list, we preserve it here so that the
  // interface won't have to change if we decide to use it in the future.

  octave_value
  tree_evaluator::do_keyboard (const octave_value_list& args) const
  {
    return ::do_keyboard (args);
  }

  bool
  tree_evaluator::is_logically_true (tree_expression *expr,
                                     const char *warn_for)
  {
    bool expr_value = false;

    octave_value t1 = evaluate (expr);

    if (t1.is_defined ())
      return t1.is_true ();
    else
      error ("%s: undefined value used in conditional expression", warn_for);

    return expr_value;
  }

  octave_value_list
  tree_evaluator::make_value_list (octave::tree_argument_list *args,
                                   const string_vector& arg_nm,
                                   const octave_value *object, bool rvalue)
  {
    octave_value_list retval;

    if (args)
      {
        // Function calls inside an argument list can't have ignored
        // output arguments.

        unwind_protect frame;

        m_lvalue_list_stack.push (0);

        frame.add_method (m_lvalue_list_stack,
                          &value_stack<const std::list<octave_lvalue>*>::pop);

        if (rvalue && object && args->has_magic_end ()
            && object->is_undefined ())
          err_invalid_inquiry_subscript ();

        retval = args->convert_to_const_vector (this, object);
      }

    octave_idx_type n = retval.length ();

    if (n > 0)
      retval.stash_name_tags (arg_nm);

    return retval;
  }

  std::list<octave_lvalue>
  tree_evaluator::make_lvalue_list (tree_argument_list *lhs)
  {
    std::list<octave_lvalue> retval;

    for (tree_expression *elt : *lhs)
      retval.push_back (elt->lvalue (this));

    return retval;
  }
}

DEFUN (max_recursion_depth, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} max_recursion_depth ()
@deftypefnx {} {@var{old_val} =} max_recursion_depth (@var{new_val})
@deftypefnx {} {} max_recursion_depth (@var{new_val}, "local")
Query or set the internal limit on the number of times a function may
be called recursively.

If the limit is exceeded, an error message is printed and control returns to
the top level.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (max_recursion_depth);
}

/*
%!test
%! orig_val = max_recursion_depth ();
%! old_val = max_recursion_depth (2*orig_val);
%! assert (orig_val, old_val);
%! assert (max_recursion_depth (), 2*orig_val);
%! max_recursion_depth (orig_val);
%! assert (max_recursion_depth (), orig_val);

%!error (max_recursion_depth (1, 2))
*/

DEFUN (silent_functions, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} silent_functions ()
@deftypefnx {} {@var{old_val} =} silent_functions (@var{new_val})
@deftypefnx {} {} silent_functions (@var{new_val}, "local")
Query or set the internal variable that controls whether internal
output from a function is suppressed.

If this option is disabled, Octave will display the results produced by
evaluating expressions within a function body that are not terminated with
a semicolon.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (silent_functions);
}

/*
%!test
%! orig_val = silent_functions ();
%! old_val = silent_functions (! orig_val);
%! assert (orig_val, old_val);
%! assert (silent_functions (), ! orig_val);
%! silent_functions (orig_val);
%! assert (silent_functions (), orig_val);

%!error (silent_functions (1, 2))
*/
