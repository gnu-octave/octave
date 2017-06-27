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

#if ! defined (octave_pt_eval_h)
#define octave_pt_eval_h 1

#include "octave-config.h"

#include <list>
#include <stack>
#include <string>

#include "call-stack.h"
#include "comment-list.h"
#include "ovl.h"
#include "pt-exp.h"
#include "pt-walk.h"

namespace octave
{
  class tree_decl_elt;
  class tree_expression;

  class interpreter;

  // How to evaluate the code that the parse trees represent.

  class OCTINTERP_API tree_evaluator : public tree_walker
  {
  public:

    template <typename T>
    class value_stack
    {
    public:

      value_stack (void) = default;

      value_stack (const value_stack&) = default;

      value_stack& operator = (const value_stack&) = default;

      ~value_stack (void) = default;

      void push (const T& val) { m_stack.push (val); }

      void pop (void)
      {
        m_stack.pop ();
      }

      T val_pop (void)
      {
        T retval = m_stack.top ();
        m_stack.pop ();
        return retval;
      }

      T top (void) const
      {
        return m_stack.top ();
      }

      size_t size (void) const
      {
        return m_stack.size ();
      }

      bool empty (void) const
      {
        return m_stack.empty ();
      }

      void clear (void)
      {
        while (! m_stack.empty ())
          m_stack.pop ();
      }

    private:

      std::stack<T> m_stack;
    };

    typedef void (*decl_elt_init_fcn) (tree_decl_elt&);

    tree_evaluator (interpreter& interp)
      : m_interpreter (interp), m_value_stack (), m_lvalue_list_stack (),
        m_nargout_stack (), m_call_stack (interp),
        m_max_recursion_depth (256), m_silent_functions (false),
        m_string_fill_char (' ')
    { }

    // No copying!

    tree_evaluator (const tree_evaluator&) = delete;

    tree_evaluator& operator = (const tree_evaluator&) = delete;

    ~tree_evaluator (void) = default;

    void reset (void);

    void visit_anon_fcn_handle (tree_anon_fcn_handle&);

    void visit_argument_list (tree_argument_list&);

    void visit_binary_expression (tree_binary_expression&);

    void visit_boolean_expression (tree_boolean_expression&);

    void visit_compound_binary_expression (tree_compound_binary_expression&);

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

    void visit_funcall (tree_funcall&);

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

    void do_unwind_protect_cleanup_code (tree_statement_list *list);

    void visit_unwind_protect_command (tree_unwind_protect_command&);

    void visit_while_command (tree_while_command&);
    void visit_do_until_command (tree_do_until_command&);

    void bind_ans (const octave_value& val, bool print);

    bool statement_printing_enabled (void);

    static void reset_debug_state (void);

    // If > 0, stop executing at the (N-1)th stopping point, counting
    //         from the the current execution point in the current frame.
    //
    // If < 0, stop executing at the next possible stopping point.
    static int dbstep_flag;

    // The number of the stack frame we are currently debugging.
    static size_t current_frame;

    static bool debug_mode;

    static bool quiet_breakpoint_flag;

    // Possible types of evaluation contexts.
    enum stmt_list_type
    {
      function,  // function body
      script,    // script file
      other      // command-line input or eval string
    };

    // The context for the current evaluation.
    static stmt_list_type statement_context;

    // TRUE means we are evaluating some kind of looping construct.
    static bool in_loop_command;

    Matrix ignored_fcn_outputs (void) const;

    const std::list<octave_lvalue> * lvalue_list (void)
    {
      return m_lvalue_list_stack.empty () ? 0 : m_lvalue_list_stack.top ();
    }

    octave_value evaluate (tree_expression *expr, int nargout = 1)
    {
      m_nargout_stack.push (nargout);

      expr->accept (*this);

      m_nargout_stack.pop ();

      octave_value_list tmp = m_value_stack.val_pop ();

      return tmp.empty () ? octave_value () : tmp(0);
    }

    octave_value_list evaluate_n (tree_expression *expr, int nargout = 1)
    {
      m_nargout_stack.push (nargout);

      expr->accept (*this);

      m_nargout_stack.pop ();

      return m_value_stack.val_pop ();
    }

    octave_value evaluate (tree_decl_elt *);

    void
    initialize_undefined_parameter_list_elements
      (tree_parameter_list *param_list, const std::string& warnfor,
       int nargout, const octave_value& val);

    void define_parameter_list_from_arg_vector
      (tree_parameter_list *param_list, const octave_value_list& args);

    void undefine_parameter_list (tree_parameter_list *param_list);

    octave_value_list
    convert_parameter_list_to_const_vector
      (tree_parameter_list *param_list, int nargout, const Cell& varargout);

    bool eval_decl_elt (tree_decl_elt *elt);

    bool switch_case_label_matches (tree_switch_case *expr,
                                    const octave_value& val);

    call_stack& get_call_stack (void) { return m_call_stack; }

    int max_recursion_depth (void) const { return m_max_recursion_depth; }

    int max_recursion_depth (int n)
    {
      int val = m_max_recursion_depth;
      m_max_recursion_depth = n;
      return val;
    }

    octave_value
    max_recursion_depth (const octave_value_list& args, int nargout);

    bool silent_functions (void) const { return m_silent_functions; }

    bool silent_functions (bool b)
    {
      int val = m_silent_functions;
      m_silent_functions = b;
      return val;
    }

    octave_value
    silent_functions (const octave_value_list& args, int nargout);

    char string_fill_char (void) const { return m_string_fill_char; }

    char string_fill_char (char c)
    {
      int val = m_string_fill_char;
      m_string_fill_char = c;
      return val;
    }

    octave_value
    string_fill_char (const octave_value_list& args, int nargout);

  private:

    void do_breakpoint (tree_statement& stmt) const;

    void do_breakpoint (bool is_breakpoint,
                        bool is_end_of_fcn_or_script = false) const;

    virtual octave_value
    do_keyboard (const octave_value_list& args = octave_value_list ()) const;

    bool is_logically_true (tree_expression *expr, const char *warn_for);

    octave_value_list
    make_value_list (tree_argument_list *args,
                     const string_vector& arg_nm,
                     const octave_value *object, bool rvalue = true);

    std::list<octave_lvalue> make_lvalue_list (tree_argument_list *);

    interpreter& m_interpreter;

    value_stack<octave_value_list> m_value_stack;

    value_stack<const std::list<octave_lvalue>*> m_lvalue_list_stack;

    value_stack<int> m_nargout_stack;

    call_stack m_call_stack;

    // Maximum nesting level for functions, scripts, or sourced files
    // called recursively.
    int m_max_recursion_depth;

    // If TRUE, turn off printing of results in functions (as if a
    // semicolon has been appended to each statement).
    bool m_silent_functions;

    // The character to fill with when creating string arrays.
    char m_string_fill_char;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_evaluator' instead")
typedef octave::tree_evaluator tree_evaluator;

#endif

#endif
