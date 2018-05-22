/*

Copyright (C) 2009-2018 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>

#include <iostream>

#include <fstream>
#include <typeinfo>

#include "cmd-edit.h"
#include "file-ops.h"
#include "oct-env.h"

#include "bp-table.h"
#include "call-stack.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "interpreter-private.h"
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
#include "utils.h"
#include "variables.h"

//FIXME: This should be part of tree_evaluator
#include "pt-jit.h"

namespace octave
{
  size_t tree_evaluator::current_frame = 0;

  bool tree_evaluator::debug_mode = false;

  bool tree_evaluator::quiet_breakpoint_flag = false;

  // Normal evaluator.

  void
  tree_evaluator::reset (void)
  {
    m_statement_context = SC_OTHER;
    m_result_type = RT_UNDEFINED;
    m_expr_result_value = octave_value ();
    m_expr_result_value_list = octave_value_list ();
    m_lvalue_list_stack.clear ();
    m_nargout_stack.clear ();
  }

  void
  tree_evaluator::visit_anon_fcn_handle (tree_anon_fcn_handle& anon_fh)
  {
    // FIXME: should CMD_LIST be limited to a single expression?
    // I think that is what Matlab does.

    tree_parameter_list *param_list = anon_fh.parameter_list ();
    tree_expression *expr = anon_fh.expression ();

    symbol_scope af_scope = anon_fh.scope ();

    symbol_table& symtab = m_interpreter.get_symbol_table ();

    symbol_scope af_parent_scope;
    if (anon_fh.has_parent_scope ())
      af_parent_scope = symtab.current_scope ();

    symbol_scope new_scope;
    if (af_scope)
      new_scope = af_scope.dup ();

    if (new_scope && af_parent_scope)
      new_scope.inherit (af_parent_scope);

    tree_parameter_list *param_list_dup
      = param_list ? param_list->dup (new_scope) : nullptr;

    tree_parameter_list *ret_list = nullptr;

    tree_statement_list *stmt_list = nullptr;

    if (expr)
      {
        tree_expression *expr_dup = expr->dup (new_scope);
        tree_statement *stmt = new tree_statement (expr_dup, nullptr);
        stmt_list = new tree_statement_list (stmt);
      }

    octave_user_function *af
      = new octave_user_function (new_scope, param_list_dup, ret_list,
                                  stmt_list);

    new_scope.set_parent (af_parent_scope);

    octave_function *curr_fcn = m_call_stack.current ();

    if (curr_fcn)
      {
        // FIXME: maybe it would be better to just stash curr_fcn
        // instead of individual bits of info about it?

        af->stash_parent_fcn_name (curr_fcn->name ());
        af->stash_dir_name (curr_fcn->dir_name ());

        if (curr_fcn->is_class_method () || curr_fcn->is_class_constructor ())
          af->stash_dispatch_class (curr_fcn->dispatch_class ());
      }

    af->mark_as_anonymous_function ();

    // FIXME: these should probably come from ANON_FH.
    //    af->stash_fcn_file_name (expr.file_name ());
    //    af->stash_fcn_location (expr.line (), expr.column ());

    octave_value ov_fcn (af);

    octave_value fh (octave_fcn_binder::maybe_binder (ov_fcn, *this));

    push_result (fh);
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
                        push_result (octave_value (true));
                        return;
                      }
                  }
                else
                  {
                    if (etype == octave_value::op_el_and)
                      {
                        expr.matlab_style_short_circuit_warning ("&");
                        push_result (octave_value (false));
                        return;
                      }
                  }

                if (op_rhs)
                  {
                    octave_value b = evaluate (op_rhs);

                    result = b.is_true ();
                  }

                push_result (octave_value (result));
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
                profiler::enter<tree_binary_expression>
                  block (m_profiler, expr);

                // Note: The profiler does not catch the braindead
                // short-circuit evaluation code above, but that should be
                // ok.  The evaluation of operands and the operator itself
                // is entangled and it's not clear where to start/stop
                // timing the operator to make it reasonable.

                type_info& ti = m_interpreter.get_type_info ();

                val = ::do_binary_op (ti, etype, a, b);
              }
          }
      }

    push_result (val);
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
                push_result (octave_value (true));
                return;
              }
          }
        else
          {
            if (etype == tree_boolean_expression::bool_and)
              {
                push_result (octave_value (false));
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

    push_result (val);
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

                type_info& ti = m_interpreter.get_type_info ();

                val = ::do_binary_op (ti, etype, a, b);
              }
          }
      }

    push_result (val);
  }

  void
  tree_evaluator::visit_break_command (tree_break_command& cmd)
  {
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    if (m_in_loop_command)
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
        push_result (octave_value (val));
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

        symbol_table& symtab = m_interpreter.get_symbol_table ();

        octave_value fcn = symtab.find_function ("colon", tmp1);

        if (! fcn.is_defined ())
          error ("can not find overloaded colon function");

        octave_value_list tmp2 = feval (fcn, tmp1, 1);

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

    push_result (val);
  }

  void
  tree_evaluator::visit_continue_command (tree_continue_command& cmd)
  {
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    if (m_in_loop_command)
      tree_continue_command::continuing = 1;
  }

  bool
  tree_evaluator::statement_printing_enabled (void)
  {
    return ! (m_silent_functions && (m_statement_context == SC_FUNCTION
                                     || m_statement_context == SC_SCRIPT));
  }

  void
  tree_evaluator::reset_debug_state (void)
  {
    debug_mode = m_bp_table.have_breakpoints () || Vdebugging;

    m_dbstep_flag = 0;
  }

  void
  tree_evaluator::reset_debug_state (bool mode)
  {
    debug_mode = mode;

    m_dbstep_flag = 0;
  }

  Matrix
  tree_evaluator::ignored_fcn_outputs (void) const
  {
    Matrix retval;

    const std::list<octave_lvalue> *lvalues = lvalue_list ();

    if (! lvalues)
      return retval;

    octave_idx_type nbh = 0;

    for (const auto& lval : *lvalues)
      nbh += lval.is_black_hole ();

    if (nbh > 0)
      {
        retval.resize (1, nbh);

        octave_idx_type k = 0;
        octave_idx_type l = 0;

        for (const auto& lval : *lvalues)
          {
            if (lval.is_black_hole ())
              retval(l++) = k+1;

            k += lval.numel ();
          }
      }

    return retval;
  }

  bool
  tree_evaluator::isargout (int nargout, int iout) const
  {
    const std::list<octave_lvalue> *lvalues = lvalue_list ();

    if (iout >= std::max (nargout, 1))
      return false;
    else if (lvalues)
      {
        int k = 0;
        for (const auto& lval : *lvalues)
          {
            if (k == iout)
              return ! lval.is_black_hole ();
            k += lval.numel ();
            if (k > iout)
              break;
          }

        return true;
      }
    else
      return true;
  }

  void
  tree_evaluator::isargout (int nargout, int nout, bool *isargout) const
  {
    const std::list<octave_lvalue> *lvalues = lvalue_list ();

    if (lvalues)
      {
        int k = 0;
        for (const auto& lval : *lvalues)
          {
            if (lval.is_black_hole ())
              isargout[k++] = false;
            else
              {
                int l = std::min (k + lval.numel (),
                                  static_cast<octave_idx_type> (nout));
                while (k < l)
                  isargout[k++] = true;
              }
          }
      }
    else
      for (int i = 0; i < nout; i++)
        isargout[i] = true;

    for (int i = std::max (nargout, 1); i < nout; i++)
      isargout[i] = false;
  }

  octave_value
  tree_evaluator::evaluate (tree_decl_elt *elt)
  {
    // Do not allow functions to return null values.

    tree_identifier *id = elt->ident ();

    return id ? evaluate (id).storable_value () : octave_value ();
  }

  void
  tree_evaluator::define_parameter_list_from_arg_vector
    (tree_parameter_list *param_list, const octave_value_list& args)
  {
    int i = -1;

    for (tree_decl_elt *elt : *param_list)
      {
        i++;

        octave_lvalue ref = elt->lvalue (*this);

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
        octave_lvalue ref = elt->lvalue (*this);

        ref.assign (octave_value::op_asn_eq, octave_value ());
      }
  }
}

// END is documented in op-kw-docs.
DEFCONSTMETHOD (end, interp, , ,
                doc: /* -*- texinfo -*-
@deftypefn {} {} end
Last element of an array or the end of any @code{for}, @code{parfor},
@code{if}, @code{do}, @code{while}, @code{function}, @code{switch},
@code{try}, or @code{unwind_protect} block.

As an index of an array, the magic index @qcode{"end"} refers to the
last valid entry in an indexing operation.

Example:

@example
@group
@var{x} = [ 1 2 3; 4 5 6 ];
@var{x}(1,end)
   @result{} 3
@var{x}(end,1)
   @result{} 4
@var{x}(end,end)
   @result{} 6
@end group
@end example
@seealso{for, parfor, if, do, while, function, switch, try, unwind_protect}
@end deftypefn */)
{
  octave_value retval;

  octave::tree_evaluator& tw = interp.get_evaluator ();

  const octave_value *indexed_object = tw.indexed_object ();
  int index_position = tw.index_position ();
  int num_indices = tw.num_indices ();

  if (! indexed_object)
    error ("invalid use of end");

  if (indexed_object->isobject ())
    {
      octave_value_list args;

      args(2) = num_indices;
      args(1) = index_position + 1;
      args(0) = *indexed_object;

      std::string class_name = indexed_object->class_name ();

      octave::symbol_table& symtab = interp.get_symbol_table ();

      octave_value meth = symtab.find_method ("end", class_name);

      if (meth.is_defined ())
        return octave::feval (meth.function_value (), args, 1);
    }

  dim_vector dv = indexed_object->dims ();
  int ndims = dv.ndims ();

  if (num_indices < ndims)
    {
      for (int i = num_indices; i < ndims; i++)
        dv(num_indices-1) *= dv(i);

      if (num_indices == 1)
        {
          ndims = 2;
          dv.resize (ndims);
          dv(1) = 1;
        }
      else
        {
          ndims = num_indices;
          dv.resize (ndims);
        }
    }

  if (index_position < ndims)
    retval = dv(index_position);
  else
    retval = 1;

  return retval;
}

namespace octave
{
  octave_value_list
  tree_evaluator::convert_to_const_vector (tree_argument_list *arg_list,
                                           const octave_value *object)
  {
    // END doesn't make sense as a direct argument for a function (i.e.,
    // "fcn (end)" is invalid but "fcn (array (end))" is OK).  Maybe we
    // need a different way of asking an octave_value object this
    // question?

    bool stash_object = (arg_list->includes_magic_end ()
                         && object
                         && ! (object->is_function ()
                               || object->is_function_handle ()));

    unwind_protect frame;

    if (stash_object)
      {
        frame.protect_var (m_indexed_object);

        m_indexed_object = object;
      }

    int len = arg_list->length ();

    std::list<octave_value_list> args;

    auto p = arg_list->begin ();
    for (int k = 0; k < len; k++)
      {
        if (stash_object)
          {
            frame.protect_var (m_index_position);
            frame.protect_var (m_num_indices);

            m_index_position = k;
            m_num_indices = len;
          }

        tree_expression *elt = *p++;

        if (elt)
          {
            octave_value tmp = evaluate (elt);

            if (tmp.is_cs_list ())
              args.push_back (tmp.list_value ());
            else if (tmp.is_defined ())
              args.push_back (tmp);
          }
        else
          {
            args.push_back (octave_value ());
            break;
          }
      }

    return args;
  }

  octave_value_list
  tree_evaluator::convert_return_list_to_const_vector
    (tree_parameter_list *ret_list, int nargout, const Cell& varargout)
  {
    octave_idx_type vlen = varargout.numel ();
    int len = ret_list->length ();

    // Special case.  Will do a shallow copy.
    if (len == 0)
      return varargout;
    else if (nargout <= len)
      {
        symbol_scope scope = get_current_scope ();

        symbol_record::context_id context = scope.current_context ();

        octave_value_list retval (nargout);

        int i = 0;

        for (tree_decl_elt *elt : *ret_list)
          {
            if (elt->is_defined (context))
              {
                octave_value tmp = evaluate (elt);
                retval(i) = tmp;
              }

            i++;
          }

        return retval;
      }
    else
      {
        octave_value_list retval (len + vlen);

        int i = 0;

        for (tree_decl_elt *elt : *ret_list)
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
        octave_lvalue ult = id->lvalue (*this);

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

  symbol_scope
  tree_evaluator::get_current_scope (void)
  {
    symbol_table& symtab = m_interpreter.get_symbol_table ();

    return symtab.current_scope ();
  }

  // Return a pointer to the user-defined function FNAME.  If FNAME is empty,
  // search backward for the first user-defined function in the
  // current call stack.

  octave_user_code *
  tree_evaluator::get_user_code (const std::string& fname)
  {
    octave_user_code *user_code = nullptr;

    if (fname.empty ())
      user_code = m_call_stack.debug_user_code ();
    else
      {
        std::string name = fname;

        if (sys::file_ops::dir_sep_char () != '/' && name[0] == '@')
          {
            auto beg = name.begin () + 2;  // never have @/method
            auto end = name.end () - 1;    // never have trailing '/'
            std::replace (beg, end, '/', sys::file_ops::dir_sep_char ());
          }

        size_t name_len = name.length ();

        if (name_len > 2 && name.substr (name_len-2) == ".m")
          name = name.substr (0, name_len-2);

        symbol_table& symtab = m_interpreter.get_symbol_table ();

        octave_value fcn = symtab.find_function (name);

        if (fcn.is_defined () && fcn.is_user_code ())
          user_code = fcn.user_code_value ();
      }

    return user_code;
  }

  void
  tree_evaluator::visit_decl_command (tree_decl_command& cmd)
  {
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

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
    tree_identifier *id = elt.ident ();

    if (id)
      {
        if (elt.is_global ())
          {
            symbol_table& symtab = m_interpreter.get_symbol_table ();

            symbol_record global_sym = symtab.find_global_symbol (id->name ());

            id->link_to_global (symtab.global_scope (), global_sym);
          }
        else if (elt.is_persistent ())
          id->mark_persistent ();
        else
          error ("declaration list element not global or persistent");

        octave_lvalue ult = id->lvalue (*this);

        if (ult.is_undefined ())
          {
            tree_expression *expr = elt.expression ();

            octave_value init_val;

            if (expr)
              init_val = evaluate (expr);
            else
              init_val = Matrix ();

            ult.assign (octave_value::op_asn_eq, init_val);
          }
      }
  }

  // Decide if it's time to quit a for or while loop.
  static inline bool
  quit_loop_now (void)
  {
    octave_quit ();

    // Maybe handle 'continue N' someday...

    if (tree_continue_command::continuing)
      tree_continue_command::continuing--;

    bool quit = (tree_return_command::returning
                 || tree_break_command::breaking
                 || tree_continue_command::continuing);

    if (tree_break_command::breaking)
      tree_break_command::breaking--;

    return quit;
  }

  void
  tree_evaluator::visit_simple_for_command (tree_simple_for_command& cmd)
  {
    size_t line = cmd.line ();

    if (m_echo_state)
      {
        echo_code (line);
        line++;
      }

    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    // FIXME: need to handle PARFOR loops here using cmd.in_parallel ()
    // and cmd.maxproc_expr ();

    unwind_protect frame;

    frame.protect_var (m_in_loop_command);

    m_in_loop_command = true;

    tree_expression *expr = cmd.control_expr ();

    octave_value rhs = evaluate (expr);

#if defined (HAVE_LLVM)
    if (tree_jit::execute (cmd, rhs))
      return;
#endif

    if (rhs.is_undefined ())
      return;

    tree_expression *lhs = cmd.left_hand_side ();

    octave_lvalue ult = lhs->lvalue (*this);

    tree_statement_list *loop_body = cmd.body ();

    if (rhs.is_range ())
      {
        Range rng = rhs.range_value ();

        octave_idx_type steps = rng.numel ();

        for (octave_idx_type i = 0; i < steps; i++)
          {
            if (m_echo_state)
              m_echo_file_pos = line;

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
        if (m_echo_state)
          m_echo_file_pos = line;

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
                if (m_echo_state)
                  m_echo_file_pos = line;

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
    size_t line = cmd.line ();

    if (m_echo_state)
      {
        echo_code (line);
        line++;
      }

    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    unwind_protect frame;

    frame.protect_var (m_in_loop_command);

    m_in_loop_command = true;

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

    auto p = lhs->begin ();

    tree_expression *elt = *p++;

    octave_lvalue val_ref = elt->lvalue (*this);

    elt = *p;

    octave_lvalue key_ref = elt->lvalue (*this);

    const octave_map tmp_val = rhs.map_value ();

    tree_statement_list *loop_body = cmd.body ();

    string_vector keys = tmp_val.keys ();

    octave_idx_type nel = keys.numel ();

    for (octave_idx_type i = 0; i < nel; i++)
      {
        if (m_echo_state)
          m_echo_file_pos = line;

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

  octave_value_list
  tree_evaluator::execute_user_script (octave_user_script& user_script,
                                       int nargout,
                                       const octave_value_list& args)
  {
    octave_value_list retval;

    std::string file_name = user_script.fcn_file_name ();

    if (args.length () != 0 || nargout != 0)
      error ("invalid call to script %s", file_name.c_str ());

    tree_statement_list *cmd_list = user_script.body ();

    if (! cmd_list)
      return retval;

    unwind_protect frame;

    // XXX FIXME
    frame.add_method (user_script, &octave_user_script::set_call_depth,
                      user_script.call_depth ());
    user_script.increment_call_depth ();

    if (user_script.call_depth () >= max_recursion_depth ())
      error ("max_recursion_depth exceeded");

    m_call_stack.push (&user_script, &frame);

    // Set pointer to the current unwind_protect frame to allow
    // certain builtins register simple cleanup in a very optimized manner.
    // This is *not* intended as a general-purpose on-cleanup mechanism,

    frame.add_method (m_call_stack, &call_stack::pop);

    // Update line number even if debugging.
    frame.protect_var (Vtrack_line_num);
    Vtrack_line_num = true;

    frame.protect_var (m_statement_context);
    m_statement_context = SC_SCRIPT;

    profiler::enter<octave_user_script> block (m_profiler, user_script);

    symbol_scope script_scope = user_script.scope ();
    frame.add_method (script_scope, &symbol_scope::unbind_script_symbols);
    script_scope.bind_script_symbols (get_current_scope ());

    if (echo ())
      push_echo_state (frame, tree_evaluator::ECHO_SCRIPTS, file_name);

    cmd_list->accept (*this);

    if (tree_return_command::returning)
      tree_return_command::returning = 0;

    if (tree_break_command::breaking)
      tree_break_command::breaking--;

    return retval;
  }

  void
  tree_evaluator::visit_octave_user_function (octave_user_function&)
  {
    // ??
    panic_impossible ();
  }

  octave_value_list
  tree_evaluator::execute_user_function (octave_user_function& user_function,
                                         int nargout,
                                         const octave_value_list& xargs)
  {
    octave_value_list retval;

    tree_statement_list *cmd_list = user_function.body ();

    if (! cmd_list)
      return retval;

    // If this function is a classdef constructor, extract the first input
    // argument, which must be the partially constructed object instance.

    octave_value_list args (xargs);
    octave_value_list ret_args;

    if (user_function.is_classdef_constructor ())
      {
        if (args.length () > 0)
          {
            ret_args = args.slice (0, 1, true);
            args = args.slice (1, args.length () - 1, true);
          }
        else
          panic_impossible ();
      }

#if defined (HAVE_LLVM)
    if (user_function.is_special_expr ()
        && tree_jit::execute (*this, args, retval))
      return retval;
#endif

    unwind_protect frame;

    // XXX FIXME
    frame.add_method (user_function, &octave_user_function::set_call_depth,
                      user_function.call_depth ());
    user_function.increment_call_depth ();

    if (user_function.call_depth () >= max_recursion_depth ())
      error ("max_recursion_depth exceeded");

    // Save old and set current symbol table context, for
    // eval_undefined_error().

    symbol_scope fcn_scope = user_function.scope ();

    symbol_record::context_id context = user_function.active_context ();

    m_call_stack.push (&user_function, &frame, fcn_scope, context);

    frame.protect_var (Vtrack_line_num);
    // update source line numbers, even if debugging
    Vtrack_line_num = true;
    frame.add_method (m_call_stack, &call_stack::pop);

    if (user_function.call_depth () > 0
        && ! user_function.is_anonymous_function ())
      {
        fcn_scope.push_context ();

#if 0
        std::cerr << name () << " scope: " << fcn_scope
                  << " call depth: " << user_function.call_depth ()
                  << " context: " << fcn_scope.current_context () << std::endl;
#endif

        frame.add_method (fcn_scope, &symbol_scope::pop_context);
      }

    string_vector arg_names = xargs.name_tags ();

    tree_parameter_list *param_list = user_function.parameter_list ();

    if (param_list && ! param_list->varargs_only ())
      {
#if 0
        std::cerr << "defining param list, scope: " << fcn_scope
                  << ", context: " << fcn_scope.current_context () << std::endl;
#endif
        define_parameter_list_from_arg_vector (param_list, args);
      }

    // For classdef constructor, pre-populate the output arguments
    // with the pre-initialized object instance, extracted above.

    tree_parameter_list *ret_list = user_function.return_list ();

    if (user_function.is_classdef_constructor ())
      {
        if (! ret_list)
          error ("%s: invalid classdef constructor, no output argument defined",
                 user_function.dispatch_class ().c_str ());

        define_parameter_list_from_arg_vector (ret_list, ret_args);
      }

    // Force parameter list to be undefined when this function exits.
    // Doing so decrements the reference counts on the values of local
    // variables that are also named function parameters.

    if (param_list)
      frame.add_method (this, &tree_evaluator::undefine_parameter_list,
                        param_list);

    // Force return list to be undefined when this function exits.
    // Doing so decrements the reference counts on the values of local
    // variables that are also named values returned by this function.

    if (ret_list)
      frame.add_method (this, &tree_evaluator::undefine_parameter_list,
                        ret_list);

    if (user_function.call_depth () == 0)
      {
        // Force symbols to be undefined again when this function
        // exits.
        //
        // This cleanup function is added to the unwind_protect stack
        // after the calls to clear the parameter lists so that local
        // variables will be cleared before the parameter lists are
        // cleared.  That way, any function parameters that have been
        // declared global will be unmarked as global before they are
        // undefined by the clear_param_list cleanup function.

        frame.add_method (fcn_scope, &symbol_scope::refresh);
      }

    user_function.bind_automatic_vars (*this, arg_names, args.length (),
                                       nargout,
                                       user_function.all_va_args (args));

    frame.add_method (&user_function,
                      &octave_user_function::restore_warning_states);

    // Evaluate the commands that make up the function.

    frame.protect_var (m_statement_context);
    m_statement_context = SC_FUNCTION;

    {
      profiler::enter<octave_user_function> block (m_profiler, user_function);

      if (echo ())
        push_echo_state (frame, tree_evaluator::ECHO_FUNCTIONS,
                         user_function.fcn_file_name ());

      if (user_function.is_special_expr ())
        {
          assert (cmd_list->length () == 1);

          tree_statement *stmt = cmd_list->front ();

          tree_expression *expr = stmt->expression ();

          if (expr)
            {
              m_call_stack.set_location (stmt->line (), stmt->column ());

              retval = evaluate_n (expr, nargout);
            }
        }
      else
        cmd_list->accept (*this);
    }

    if (tree_return_command::returning)
      tree_return_command::returning = 0;

    if (tree_break_command::breaking)
      tree_break_command::breaking--;

    // Copy return values out.

    if (ret_list && ! user_function.is_special_expr ())
      {
        Cell varargout;

        if (ret_list->takes_varargs ())
          {
            octave_value varargout_varval = fcn_scope.varval ("varargout");

            if (varargout_varval.is_defined ())
              varargout = varargout_varval.xcell_value ("varargout must be a cell array object");
          }

        retval = convert_return_list_to_const_vector (ret_list, nargout,
                                                      varargout);
      }

    return retval;
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

        symbol_table& symtab = m_interpreter.get_symbol_table ();

        symtab.install_cmdline_function (nm, fcn);

        // Make sure that any variable with the same name as the new
        // function is cleared.

        symbol_scope scope = symtab.current_scope ();

        if (scope)
          scope.assign (nm);
      }
  }

  void
  tree_evaluator::visit_identifier (tree_identifier& expr)
  {
    octave_value_list retval;

    symbol_scope scope = get_current_scope ();

    symbol_record::context_id context = scope.current_context ();

    symbol_record sym = expr.symbol ();

    octave_value val = sym.find (context);

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
                && statement_printing_enabled ())
              {
                octave_value_list args = ovl (val);
                args.stash_name_tags (string_vector (expr.name ()));
                feval ("display", args);
              }

            push_result (val);
            return;
          }
      }
    else if (sym.is_added_static ())
      expr.static_workspace_error ();
    else
      expr.eval_undefined_error ();

    push_result (retval);
  }

  void
  tree_evaluator::visit_if_clause (tree_if_clause&)
  {
    panic_impossible ();
  }

  void
  tree_evaluator::visit_if_command (tree_if_command& cmd)
  {
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

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

        if (m_statement_context == SC_FUNCTION
            || m_statement_context == SC_SCRIPT)
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

    auto p_args = args.begin ();
    auto p_arg_nm = arg_nm.begin ();
    auto p_dyn_field = dyn_field.begin ();

    int n = args.size ();
    int beg = 0;

    octave_value base_expr_val;

    tree_expression *expr = idx_expr.expression ();

    if (expr->is_identifier () && type[beg] == '(')
      {
        tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

        symbol_scope scope = get_current_scope ();

        symbol_record::context_id context = scope.current_context ();

        if (! id->is_variable (context))
          {
            octave_value_list first_args;

            tree_argument_list *al = *p_args;

            if (al && al->length () > 0)
              {
                // Function calls inside an argument list can't have
                // ignored output arguments.

                unwind_protect frame;

                m_lvalue_list_stack.push (nullptr);

                frame.add_method (m_lvalue_list_stack,
                                  &value_stack<const std::list<octave_lvalue>*>::pop);

                string_vector anm = *p_arg_nm;
                first_args = convert_to_const_vector (al);
                first_args.stash_name_tags (anm);
              }

            octave_function *fcn = nullptr;

            octave_value val = id->do_lookup (context, first_args);

            if (val.is_function ())
              fcn = val.function_value (true);

            if (fcn)
              {
                try
                  {
                    retval = fcn->call (*this, nargout, first_args);
                  }
                catch (index_exception& e)
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

                    push_result (retval);
                    return;
                  }
              }
          }
      }

    if (base_expr_val.is_undefined ())
      base_expr_val = evaluate (expr);

    // If we are indexing an object or looking at something like
    //
    //   classname.static_function (args, ...);
    //
    // then we'll just build a complete index list for one big subsref
    // call.  If the expression we are indexing is a classname then
    // base_expr_val will be an octave_classdef_meta object.  If we have
    // files in a +packagename folder, they will also be an
    // octave_classdef_meta object, but we don't want to index them.

    bool indexing_object = (base_expr_val.isobject ()
                            || base_expr_val.isjava ()
                            || (base_expr_val.is_classdef_meta ()
                                && ! base_expr_val.is_package ()));

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
                      = base_expr_val.subsref (type.substr (beg, i-beg),
                                               idx, nargout);

                    partial_expr_val
                      = tmp_list.length () ? tmp_list(0) : octave_value ();

                    if (! indexing_object)
                      {
                        base_expr_val = partial_expr_val;

                        if (partial_expr_val.is_cs_list ())
                          err_indexed_cs_list ();

                        retval = partial_expr_val;

                        beg = i;
                        idx.clear ();

                        if (partial_expr_val.isobject ()
                            || partial_expr_val.isjava ()
                            || (partial_expr_val.is_classdef_meta ()
                                && ! partial_expr_val.is_package ()))
                          {
                            // Found an object, so now we'll build up
                            // complete index list for one big subsref
                            // call from this point on.

                            // FIXME: is is also possible to have a
                            // static method call buried somewhere in
                            // the depths of a complex indexing
                            // expression so that we would also need to
                            // check for an octave_classdef_meta object
                            // here?

                            indexing_object = true;
                          }
                      }
                  }
                catch (index_exception& e)
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
                           (idx_expr.get_struct_index (*this, p_arg_nm, p_dyn_field)));
            break;

          default:
            panic_impossible ();
          }

        p_args++;
        p_arg_nm++;
        p_dyn_field++;
      }


    // If ! idx.empty () that means we still have stuff to index otherwise
    // they would have been dealt with and idx would have been emptied.
    if (! idx.empty ())
      {
        // This is for +package and other classdef_meta objects
        if (! base_expr_val.is_function ()
            || base_expr_val.is_classdef_meta ())
          {
            try
              {
                retval = base_expr_val.subsref (type.substr (beg, n-beg),
                                                idx, nargout);
                beg = n;
                idx.clear ();
              }
            catch (index_exception& e)
              {
                final_index_error (e, expr);
              }
          }
        else
          {
            // FIXME: we want this to only be a superclass constructor
            // call Should we actually make a check for this or are all
            // other types of calls already dealt with?

            octave_function *fcn = base_expr_val.function_value ();

            if (fcn)
              {
                try
                  {
                    retval = fcn->call (*this, nargout, idx);
                  }
                catch (index_exception& e)
                  {
                    final_index_error (e, expr);
                  }
              }
          }
      }

    // FIXME: when can the following happen?  In what case does indexing
    // result in a value that is a function?  Classdef method calls?
    // Something else?

    octave_value val = (retval.length () ? retval(0) : octave_value ());

    if (val.is_function ())
      {
        octave_function *fcn = val.function_value (true);

        if (fcn)
          {
            octave_value_list final_args;

            if (! idx.empty ())
              {
                if (n - beg != 1)
                  error ("unexpected extra index at end of expression");

                if (type[beg] != '(')
                  error ("invalid index type '%c' for function call",
                         type[beg]);

                final_args = idx.front ();
              }

            retval = fcn->call (*this, nargout, final_args);
          }
      }

    push_result (retval);
  }

  void
  tree_evaluator::visit_matrix (tree_matrix& expr)
  {
    tm_const tmp (expr, *this);

    push_result (tmp.concat (m_string_fill_char));
  }

  void
  tree_evaluator::visit_cell (tree_cell& expr)
  {
    octave_value retval;

    // Function calls inside an argument list can't have ignored
    // output arguments.

    unwind_protect frame;

    m_lvalue_list_stack.push (nullptr);

    frame.add_method (m_lvalue_list_stack,
                      &value_stack<const std::list<octave_lvalue>*>::pop);

    octave_idx_type nr = expr.length ();
    octave_idx_type nc = -1;

    Cell val;

    octave_idx_type i = 0;

    for (tree_argument_list *elt : expr)
      {
        octave_value_list row = convert_to_const_vector (elt);

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

    push_result (retval);
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

        auto q = lhs->begin ();

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
                    if (ult.is_black_hole ())
                      {
                        k++;
                        continue;
                      }
                    else
                      {
                        octave_value tmp = rhs_val(k);

                        if (tmp.is_undefined ())
                          error ("element number %d undefined in return list",
                                 k+1);

                        ult.assign (octave_value::op_asn_eq, tmp);

                        retval_list.push_back (tmp);

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

            if (expr.print_result () && statement_printing_enabled ())
              {
                // We clear any index here so that we can get
                // the new value of the referenced object below,
                // instead of the indexed value (which should be
                // the same as the right hand side value).

                ult.clear_index ();

                octave_value lhs_val = ult.value ();

                octave_value_list args = ovl (lhs_val);
                args.stash_name_tags (string_vector (lhs_elt->name ()));
                feval ("display", args);
              }
          }

        // Concatenate return values.
        val = retval_list;
      }

    push_result (val);
  }

  void
  tree_evaluator::visit_no_op_command (tree_no_op_command& cmd)
  {
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

    if (debug_mode && cmd.is_end_of_fcn_or_script ())
      do_breakpoint (cmd.is_breakpoint (true), true);
  }

  void
  tree_evaluator::visit_constant (tree_constant& expr)
  {
    int nargout = m_nargout_stack.top ();

    if (nargout > 1)
      error ("invalid number of output arguments for constant expression");

    push_result (expr.value ());
  }

  void
  tree_evaluator::visit_fcn_handle (tree_fcn_handle& expr)
  {
    std::string nm = expr.name ();

    octave_value fh = make_fcn_handle (nm);

    push_result (fh);
  }

  void
  tree_evaluator::visit_funcall (tree_funcall& expr)
  {
    octave_value_list retval;

    octave_value fcn = expr.function ();

    octave_value_list args = expr.arguments ();

    int nargout = m_nargout_stack.top ();

    retval = feval (fcn.function_value (), args, nargout);

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

    push_result (retval);
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
            octave_lvalue ref = op->lvalue (*this);

            val = ref.value ();

            profiler::enter<tree_postfix_expression> block (m_profiler, expr);

            ref.do_unary_op (etype);
          }
        else
          {
            octave_value op_val = evaluate (op);

            if (op_val.is_defined ())
              {
                profiler::enter<tree_postfix_expression>
                  block (m_profiler, expr);

                type_info& ti = m_interpreter.get_type_info ();

                val = ::do_unary_op (ti, etype, op_val);
              }
          }
      }

    push_result (val);
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
            octave_lvalue op_ref = op->lvalue (*this);

            profiler::enter<tree_prefix_expression> block (m_profiler, expr);

            op_ref.do_unary_op (etype);

            val = op_ref.value ();
          }
        else
          {
            octave_value op_val = evaluate (op);

            if (op_val.is_defined ())
              {
                profiler::enter<tree_prefix_expression>
                  block (m_profiler, expr);

                // Attempt to do the operation in-place if it is unshared
                // (a temporary expression).
                if (op_val.get_count () == 1)
                  val = op_val.do_non_const_unary_op (etype);
                else
                  {
                    type_info& ti = m_interpreter.get_type_info ();

                    val = ::do_unary_op (ti, etype, op_val);
                  }
              }
          }
      }

    push_result (val);
  }

  void
  tree_evaluator::visit_return_command (tree_return_command& cmd)
  {
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

    if (debug_mode)
      do_breakpoint (cmd.is_breakpoint (true));

    // Act like dbcont.

    if (Vdebugging && m_call_stack.current_frame () == current_frame)
      {
        Vdebugging = false;

        reset_debug_state ();
      }
    else if (m_statement_context == SC_FUNCTION
             || m_statement_context == SC_SCRIPT
             || m_in_loop_command)
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
            unwind_protect frame;

            octave_lvalue ult = lhs->lvalue (*this);

            std::list<octave_lvalue> lvalue_list;
            lvalue_list.push_back (ult);

            m_lvalue_list_stack.push (&lvalue_list);

            frame.add_method (m_lvalue_list_stack,
                              &value_stack<const std::list<octave_lvalue>*>::pop);

            if (ult.numel () != 1)
              err_invalid_structure_assignment ();

            octave_value::assign_op etype = expr.op_type ();

            ult.assign (etype, rhs_val);

            if (etype == octave_value::op_asn_eq)
              val = rhs_val;
            else
              val = ult.value ();

            if (expr.print_result () && statement_printing_enabled ())
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
                feval ("display", args);
              }
          }
        catch (index_exception& e)
          {
            e.set_var (lhs->name ());
            std::string msg = e.message ();
            error_with_id (e.err_id (), msg.c_str ());
          }
      }

    push_result (val);
  }

  void
  tree_evaluator::visit_statement (tree_statement& stmt)
  {
    tree_command *cmd = stmt.command ();
    tree_expression *expr = stmt.expression ();

    if (cmd || expr)
      {
        if (m_statement_context == SC_FUNCTION
            || m_statement_context == SC_SCRIPT)
          {
            // Skip commands issued at a debug> prompt to avoid disturbing
            // the state of the program we are debugging.

            if (Vtrack_line_num)
              m_call_stack.set_location (stmt.line (), stmt.column ());
          }

        try
          {
            if (cmd)
              cmd->accept (*this);
            else
              {
                if (m_echo_state)
                  {
                    size_t line = stmt.line ();
                    echo_code (line);
                    m_echo_file_pos = line + 1;
                  }

                if (debug_mode)
                  do_breakpoint (expr->is_breakpoint (true));

                // FIXME: maybe all of this should be packaged in
                // one virtual function that returns a flag saying whether
                // or not the expression will take care of binding ans and
                // printing the result.

                // FIXME: it seems that we should just have to
                // evaluate the expression and that should take care of
                // everything, binding ans as necessary?

                octave_value tmp_result = evaluate (expr, 0);

                if (tmp_result.is_defined ())
                  {
                    bool do_bind_ans = false;

                    if (expr->is_identifier ())
                      {
                        symbol_scope scope = get_current_scope ();

                        symbol_record::context_id context
                          = scope.current_context ();

                        tree_identifier *id
                          = dynamic_cast<tree_identifier *> (expr);

                        do_bind_ans = (! id->is_variable (context));
                      }
                    else
                      do_bind_ans = (! expr->is_assignment_expression ());

                    if (do_bind_ans)
                      bind_ans (tmp_result, expr->print_result ()
                                && statement_printing_enabled ());
                  }
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
        catch (const interrupt_exception&)
          {
            // If we are debugging, then continue with next statement.
            // Otherwise, jump out of here.

            if (debug_mode)
              interpreter::recover_from_exception ();
            else
              throw;
          }
      }
  }

  void
  tree_evaluator::visit_statement_list (tree_statement_list& lst)
  {
    // FIXME: commented out along with else clause below.
    // static octave_value_list empty_list;

    auto p = lst.begin ();

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
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

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
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

    bool execution_error = false;

    {
      // unwind frame before catch block
      unwind_protect frame;

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
          catch (const execution_exception&)
            {
              interpreter::recover_from_exception ();

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

            if (expr_id)
              {
                octave_lvalue ult = expr_id->lvalue (*this);

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
    unwind_protect frame;

    frame.protect_var (octave_interrupt_state);
    octave_interrupt_state = 0;

    // We want to preserve the last location info for possible
    // backtracking.

    frame.add_method (m_call_stack, &call_stack::set_line,
                      m_call_stack.current_line ());
    frame.add_method (m_call_stack, &call_stack::set_column,
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
    catch (const execution_exception&)
      {
        interpreter::recover_from_exception ();

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
    if (m_echo_state)
      {
        size_t line = cmd.line ();
        echo_code (line);
        m_echo_file_pos = line + 1;
      }

    tree_statement_list *cleanup_code = cmd.cleanup ();

    tree_statement_list *unwind_protect_code = cmd.body ();

    if (unwind_protect_code)
      {
        try
          {
            unwind_protect_code->accept (*this);
          }
        catch (const execution_exception&)
          {
            // FIXME: Maybe we should be able to temporarily set the
            // interpreter's exception handling state to something "safe"
            // while the cleanup block runs instead of just resetting it
            // here?
            interpreter::recover_from_exception ();

            // Run the cleanup code on exceptions, so that it is run even
            // in case of interrupt or out-of-memory.
            do_unwind_protect_cleanup_code (cleanup_code);

            // If an error occurs inside the cleanup code, a new
            // exception will be thrown instead of the original.
            throw;
          }
        catch (const interrupt_exception&)
          {
            // The comments above apply here as well.
            interpreter::recover_from_exception ();
            do_unwind_protect_cleanup_code (cleanup_code);
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
    size_t line = cmd.line ();

    if (m_echo_state)
      {
        echo_code (line);
        line++;
      }

#if defined (HAVE_LLVM)
    if (tree_jit::execute (cmd))
      return;
#endif

    unwind_protect frame;

    frame.protect_var (m_in_loop_command);

    m_in_loop_command = true;

    tree_expression *expr = cmd.condition ();

    if (! expr)
      panic_impossible ();

    for (;;)
      {
        if (m_echo_state)
          m_echo_file_pos = line;

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
    size_t line = cmd.line ();

    if (m_echo_state)
      {
        echo_code (line);
        line++;
      }

#if defined (HAVE_LLVM)
    if (tree_jit::execute (cmd))
      return;
#endif

    unwind_protect frame;

    frame.protect_var (m_in_loop_command);

    m_in_loop_command = true;

    tree_expression *expr = cmd.condition ();
    int until_line = cmd.line ();
    int until_column = cmd.column ();

    if (! expr)
      panic_impossible ();

    for (;;)
      {
        if (m_echo_state)
          m_echo_file_pos = line;

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

  void tree_evaluator::bind_ans (const octave_value& val, bool print)
  {
    static std::string ans = "ans";

    if (val.is_defined ())
      {
        if (val.is_cs_list ())
          {
            octave_value_list lst = val.list_value ();

            for (octave_idx_type i = 0; i < lst.length (); i++)
              bind_ans (lst(i), print);
          }
        else
          {
            symbol_scope scope = get_current_scope ();

            scope.force_assign (ans, val);

            if (print)
              {
                octave_value_list args = ovl (val);
                args.stash_name_tags (string_vector (ans));
                feval ("display", args);
              }
          }
      }
  }

  void
  tree_evaluator::do_breakpoint (tree_statement& stmt)
  {
    do_breakpoint (stmt.is_breakpoint (true), stmt.is_end_of_fcn_or_script ());
  }

  void
  tree_evaluator::do_breakpoint (bool is_breakpoint,
                                 bool is_end_of_fcn_or_script)
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

        m_dbstep_flag = 0;

        current_frame = m_call_stack.current_frame ();
      }
    else if (m_dbstep_flag > 0)
      {
        if (m_call_stack.current_frame () == current_frame)
          {
            if (m_dbstep_flag == 1 || is_end_of_fcn_or_script)
              {
                // We get here if we are doing a "dbstep" or a "dbstep N" and the
                // count has reached 1 so that we must stop and return to debug
                // prompt.  Alternatively, "dbstep N" has been used but the end
                // of the frame has been reached so we stop at the last line and
                // return to prompt.

                break_on_this_statement = true;

                m_dbstep_flag = 0;
              }
            else
              {
                // Executing "dbstep N".  Decrease N by one and continue.

                m_dbstep_flag--;
              }

          }
        else if (m_dbstep_flag == 1
                 && m_call_stack.current_frame () < current_frame)
          {
            // We stepped out from the end of a function.

            current_frame = m_call_stack.current_frame ();

            break_on_this_statement = true;

            m_dbstep_flag = 0;
          }
      }
    else if (m_dbstep_flag == -1)
      {
        // We get here if we are doing a "dbstep in".

        break_on_this_statement = true;

        m_dbstep_flag = 0;

        current_frame = m_call_stack.current_frame ();
      }
    else if (m_dbstep_flag == -2)
      {
        // We get here if we are doing a "dbstep out".  Check for end of
        // function and whether the current frame is the same as the
        // cached value because we want to step out from the frame where
        // "dbstep out" was evaluated, not from any functions called from
        // that frame.

        if (is_end_of_fcn_or_script
            && m_call_stack.current_frame () == current_frame)
          m_dbstep_flag = -1;
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
  tree_evaluator::make_value_list (tree_argument_list *args,
                                   const string_vector& arg_nm,
                                   const octave_value *object, bool rvalue)
  {
    octave_value_list retval;

    if (args)
      {
        // Function calls inside an argument list can't have ignored
        // output arguments.

        unwind_protect frame;

        m_lvalue_list_stack.push (nullptr);

        frame.add_method (m_lvalue_list_stack,
                          &value_stack<const std::list<octave_lvalue>*>::pop);

        if (rvalue && object && args->has_magic_end ()
            && object->is_undefined ())
          err_invalid_inquiry_subscript ();

        retval = convert_to_const_vector (args, object);
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
      retval.push_back (elt->lvalue (*this));

    return retval;
  }

  octave_value
  tree_evaluator::max_recursion_depth (const octave_value_list& args,
                                       int nargout)
  {
    return set_internal_variable (m_max_recursion_depth, args, nargout,
                                  "max_recursion_depth", 0);
  }

  octave_value
  tree_evaluator::silent_functions (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_silent_functions, args, nargout,
                                  "silent_functions");
  }

  octave_value
  tree_evaluator::string_fill_char (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_string_fill_char, args, nargout,
                                  "string_fill_char");
  }

  void
  tree_evaluator::push_echo_state (unwind_protect& frame, int type,
                                   const std::string& file_name,
                                   size_t pos)
  {
    push_echo_state_cleanup (frame);

    set_echo_state (type, file_name, pos);
  }

  void
  tree_evaluator::set_echo_state (int type, const std::string& file_name,
                                  size_t pos)
  {
    m_echo_state = echo_this_file (file_name, type);
    m_echo_file_name = file_name;
    m_echo_file_pos = pos;
  }

  void
  tree_evaluator::maybe_set_echo_state (void)
  {
    octave_function *caller = m_call_stack.caller ();

    if (caller && caller->is_user_code ())
      {
        octave_user_code *fcn = dynamic_cast<octave_user_code *> (caller);

        int type = fcn->is_user_function () ? ECHO_FUNCTIONS : ECHO_SCRIPTS;

        std::string file_name = fcn->fcn_file_name ();

        size_t pos = m_call_stack.current_line ();

        set_echo_state (type, file_name, pos);
      }
  }

  void
  tree_evaluator::push_echo_state_cleanup (unwind_protect& frame)
  {
    frame.add_method (*this, &tree_evaluator::set_echo_state,
                      m_echo_state);

    frame.add_method (*this, &tree_evaluator::set_echo_file_name,
                      m_echo_file_name);

    frame.add_method (*this, &tree_evaluator::set_echo_file_pos,
                      m_echo_file_pos);
  }

  bool tree_evaluator::maybe_push_echo_state_cleanup (void)
  {
    // This function is expected to be called from ECHO, which would be
    // the top of the call stack.  If the caller of ECHO is a
    // user-defined function or script, then set up unwind-protect
    // elements to restore echo state.

    unwind_protect *frame = m_call_stack.curr_fcn_unwind_protect_frame ();

    if (frame)
      {
        push_echo_state_cleanup (*frame);
        return true;
      }

    return false;
  }


  octave_value
  tree_evaluator::echo (const octave_value_list& args, int)
  {
    bool cleanup_pushed = maybe_push_echo_state_cleanup ();

    string_vector argv = args.make_argv ();

    switch (args.length ())
      {
      case 0:
        if ((m_echo & ECHO_SCRIPTS) || (m_echo & ECHO_FUNCTIONS))
          {
            m_echo = ECHO_OFF;
            m_echo_files.clear ();
          }
        else
          m_echo = ECHO_SCRIPTS;
        break;

      case 1:
        {
          std::string arg0 = argv[0];

          if (arg0 == "on")
            m_echo = ECHO_SCRIPTS;
          else if (arg0 == "off")
            m_echo = ECHO_OFF;
          else
            {
              std::string file = fcn_file_in_path (arg0);
              file = sys::env::make_absolute (file);

              if (file.empty ())
                error ("echo: no such file %s", arg0.c_str ());

              if (m_echo & ECHO_ALL)
                {
                  // Echo is enabled for all functions, so turn it off
                  // for this one.

                  m_echo_files[file] = false;
                }
              else
                {
                  // Echo may be enabled for specific functions.

                  auto p = m_echo_files.find (file);

                  if (p == m_echo_files.end ())
                    {
                      // Not this one, so enable it.

                      m_echo |= ECHO_FUNCTIONS;
                      m_echo_files[file] = true;
                    }
                  else
                    {
                      // This one is already in the list.  Flip the
                      // status for it.

                      p->second = ! p->second;
                    }
                }
            }
        }
        break;

      case 2:
        {
          std::string arg0 = argv[0];
          std::string arg1 = argv[1];

          if (arg1 == "on" || arg1 == "off")
            std::swap (arg0, arg1);

          if (arg0 == "on")
            {
              if (arg1 == "all")
                {
                  m_echo = (ECHO_SCRIPTS | ECHO_FUNCTIONS | ECHO_ALL);
                  m_echo_files.clear ();
                }
              else
                {
                  std::string file = fcn_file_in_path (arg1);
                  file = sys::env::make_absolute (file);

                  if (file.empty ())
                    error ("echo: no such file %s", arg1.c_str ());

                  m_echo |= ECHO_FUNCTIONS;
                  m_echo_files[file] = true;
                }
            }
          else if (arg0 == "off")
            {
              if (arg1 == "all")
                {
                  m_echo = ECHO_OFF;
                  m_echo_files.clear ();
                }
              else
                {
                  std::string file = fcn_file_in_path (arg1);
                  file = sys::env::make_absolute (file);

                  if (file.empty ())
                    error ("echo: no such file %s", arg1.c_str ());

                  m_echo_files[file] = false;
                }
            }
          else
            print_usage ();
        }
        break;

      default:
        print_usage ();
        break;
      }

    if (cleanup_pushed)
      maybe_set_echo_state ();

    return octave_value ();
  }

  octave_value
  tree_evaluator::PS4 (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_PS4, args, nargout, "PS4");
  }

  bool tree_evaluator::echo_this_file (const std::string& file, int type) const
  {
    if ((type & m_echo) == ECHO_SCRIPTS)
      {
        // Asking about scripts and echo is enabled for them.
        return true;
      }

    if ((type & m_echo) == ECHO_FUNCTIONS)
      {
        // Asking about functions and echo is enabled for functions.
        // Now, which ones?

        auto p = m_echo_files.find (file);

        if (m_echo & ECHO_ALL)
          {
            // Return true ulness echo was turned off for a specific
            // file.

            return (p == m_echo_files.end () || p->second);
          }
        else
          {
            // Return true if echo is specifically enabled for this file.

            return p != m_echo_files.end () && p->second;
          }
      }

    return false;
  }

  void tree_evaluator::echo_code (size_t line)
  {
    std::string prefix = command_editor::decode_prompt_string (m_PS4);

    octave_function *curr_fcn = m_call_stack.current ();

    if (curr_fcn && curr_fcn->is_user_code ())
      {
        octave_user_code *code = dynamic_cast<octave_user_code *> (curr_fcn);

        size_t num_lines = line - m_echo_file_pos + 1;

        std::deque<std::string> lines
          = code->get_code_lines (m_echo_file_pos, num_lines);

        for (auto& elt : lines)
          octave_stdout << prefix << elt << std::endl;
      }
  }

  // Final step of processing an indexing error.  Add the name of the
  // variable being indexed, if any, then issue an error.  (Will this also
  // be needed by pt-lvalue, which calls subsref?)

  void tree_evaluator::final_index_error (index_exception& e,
                                          const tree_expression *expr)
  {
    std::string extra_message;

    symbol_scope scope = get_current_scope ();

    symbol_record::context_id ctxt = scope.current_context ();

    if (expr->is_identifier ()
        && dynamic_cast<const tree_identifier *> (expr)->is_variable (ctxt))
      {
        std::string var = expr->name ();

        e.set_var (var);

        symbol_table& symtab = m_interpreter.get_symbol_table ();

        octave_value fcn = symtab.find_function (var);

        if (fcn.is_function ())
          {
            octave_function *fp = fcn.function_value ();

            if (fp && fp->name () == var)
              extra_message
                = " (note: variable '" + var + "' shadows function)";
          }
      }

    std::string msg = e.message () + extra_message;

    error_with_id (e.err_id (), msg.c_str ());
  }
}

DEFMETHOD (max_recursion_depth, interp, args, nargout,
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

@seealso{max_stack_depth}
@end deftypefn */)
{
  octave::tree_evaluator& tw = interp.get_evaluator ();

  return tw.max_recursion_depth (args, nargout);
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

DEFMETHOD (silent_functions, interp, args, nargout,
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
  octave::tree_evaluator& tw = interp.get_evaluator ();

  return tw.silent_functions (args, nargout);
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

DEFMETHOD (string_fill_char, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} string_fill_char ()
@deftypefnx {} {@var{old_val} =} string_fill_char (@var{new_val})
@deftypefnx {} {} string_fill_char (@var{new_val}, "local")
Query or set the internal variable used to pad all rows of a character
matrix to the same length.

The value must be a single character and the default is @qcode{" "} (a
single space).  For example:

@example
@group
string_fill_char ("X");
[ "these"; "are"; "strings" ]
      @result{}  "theseXX"
          "areXXXX"
          "strings"
@end group
@end example

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  octave::tree_evaluator& tw = interp.get_evaluator ();

  return tw.string_fill_char (args, nargout);
}

/*
## string_fill_char() function call must be outside of %!test block
## due to the way a %!test block is wrapped inside a function
%!shared orig_val, old_val
%! orig_val = string_fill_char ();
%! old_val  = string_fill_char ("X");
%!test
%! assert (orig_val, old_val);
%! assert (string_fill_char (), "X");
%! assert (["these"; "are"; "strings"], ["theseXX"; "areXXXX"; "strings"]);
%! string_fill_char (orig_val);
%! assert (string_fill_char (), orig_val);

%!assert ( [ [], {1} ], {1} )

%!error (string_fill_char (1, 2))
*/

DEFMETHOD (PS4, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PS4 ()
@deftypefnx {} {@var{old_val} =} PS4 (@var{new_val})
@deftypefnx {} {} PS4 (@var{new_val}, "local")
Query or set the character string used to prefix output produced
when echoing commands is enabled.

The default value is @qcode{"+ "}.
@xref{Diary and Echo Commands}, for a description of echoing commands.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{echo, PS1, PS2}
@end deftypefn */)
{
  octave::tree_evaluator& tw = interp.get_evaluator ();

  return tw.PS4 (args, nargout);
}

DEFMETHOD (echo, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} echo
@deftypefnx {} {} echo on
@deftypefnx {} {} echo off
@deftypefnx {} {} echo on all
@deftypefnx {} {} echo off all
@deftypefnx {} {} echo @var{function} on
@deftypefnx {} {} echo @var{function} off
Control whether commands are displayed as they are executed.

Valid options are:

@table @code
@item on
Enable echoing of commands as they are executed in script files.

@item off
Disable echoing of commands as they are executed in script files.

@item on all
Enable echoing of commands as they are executed in script files and
functions.

@item off all
Disable echoing of commands as they are executed in script files and
functions.

@item @var{function} on
Enable echoing of commands as they are executed in the named function.

@item @var{function} off
Disable echoing of commands as they are executed in the named function.
@end table

@noindent
With no arguments, @code{echo} toggles the current echo state.

@seealso{PS4}
@end deftypefn */)
{
  octave::tree_evaluator& tw = interp.get_evaluator ();

  return tw.echo (args, nargout);
}

/*
%!error echo ([])
%!error echo (0)
%!error echo ("")
%!error echo ("Octave")
%!error echo ("off", "invalid")
%!error echo ("on", "invalid")
%!error echo ("on", "all", "all")
*/
