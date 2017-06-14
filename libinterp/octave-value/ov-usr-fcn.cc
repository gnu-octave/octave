/*

Copyright (C) 1996-2017 John W. Eaton

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

#include <sstream>

#include "str-vec.h"

#include "builtin-defun-decls.h"
#include "call-stack.h"
#include "defaults.h"
#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "ovl.h"
#include "ov-usr-fcn.h"
#include "ov.h"
#include "pager.h"
#include "pt-eval.h"
#include "pt-jit.h"
#include "pt-jump.h"
#include "pt-misc.h"
#include "pt-pr-code.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "symtab.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "unwind-prot.h"
#include "utils.h"
#include "parse.h"
#include "profiler.h"
#include "variables.h"
#include "ov-fcn-handle.h"

// Whether to optimize subsasgn method calls.
static bool Voptimize_subsasgn_calls = true;

// The character to fill with when creating string arrays.
extern char Vstring_fill_char;   // see pt-mat.cc

std::map<std::string, octave_value>
octave_user_code::subfunctions (void) const
{
  return std::map<std::string, octave_value> ();
}

// User defined scripts.

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_user_script,
                                     "user-defined script",
                                     "user-defined script");

octave_user_script::octave_user_script (void)
  : octave_user_code (), cmd_list (0), file_name (),
    t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    call_depth (-1)
{ }

octave_user_script::octave_user_script (const std::string& fnm,
                                        const std::string& nm,
                                        octave::tree_statement_list *cmds,
                                        const std::string& ds)
  : octave_user_code (nm, ds), cmd_list (cmds), file_name (fnm),
    t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    call_depth (-1)
{
  if (cmd_list)
    cmd_list->mark_as_script_body ();
}

octave_user_script::octave_user_script (const std::string& fnm,
                                        const std::string& nm,
                                        const std::string& ds)
  : octave_user_code (nm, ds), cmd_list (0), file_name (fnm),
    t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    call_depth (-1)
{ }

octave_user_script::~octave_user_script (void)
{
  if (cmd_list)
    cmd_list->remove_all_breakpoints (file_name);

  delete cmd_list;
}

octave_value_list
octave_user_script::call (octave::tree_evaluator& tw, int nargout,
                          const octave_value_list& args)
{
  octave_value_list retval;

  octave::unwind_protect frame;

  if (args.length () != 0 || nargout != 0)
    error ("invalid call to script %s", file_name.c_str ());

  if (cmd_list)
    {
      frame.protect_var (call_depth);
      call_depth++;

      if (call_depth >= Vmax_recursion_depth)
        error ("max_recursion_depth exceeded");

      octave::call_stack& cs
        = octave::__get_call_stack__ ("octave_user_script::call");

      cs.push (this);

      frame.add_method (cs, &octave::call_stack::pop);

      // Update line number even if debugging.
      frame.protect_var (Vtrack_line_num);
      Vtrack_line_num = true;

      frame.protect_var (octave::tree_evaluator::statement_context);
      octave::tree_evaluator::statement_context = octave::tree_evaluator::script;

      profile_data_accumulator::enter<octave_user_script>
        block (profiler, *this);

      cmd_list->accept (tw);

      if (octave::tree_return_command::returning)
        octave::tree_return_command::returning = 0;

      if (octave::tree_break_command::breaking)
        octave::tree_break_command::breaking--;
    }

  return retval;
}

void
octave_user_script::accept (octave::tree_walker& tw)
{
  tw.visit_octave_user_script (*this);
}

// User defined functions.

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_user_function,
                                     "user-defined function",
                                     "user-defined function");

// Ugh.  This really needs to be simplified (code/data?
// extrinsic/intrinsic state?).

octave_user_function::octave_user_function
  (symbol_table::scope *scope, octave::tree_parameter_list *pl,
   octave::tree_parameter_list *rl, octave::tree_statement_list *cl)
  : octave_user_code ("", ""), m_scope (scope),
    param_list (pl), ret_list (rl), cmd_list (cl),
    lead_comm (), trail_comm (), file_name (),
    location_line (0), location_column (0),
    parent_name (), t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    system_fcn_file (false), call_depth (-1),
    num_named_args (param_list ? param_list->length () : 0),
    subfunction (false), inline_function (false),
    anonymous_function (false), nested_function (false),
    class_constructor (none), class_method (false),
    parent_scope (0), curr_unwind_protect_frame (0)
#if defined (HAVE_LLVM)
    , jit_info (0)
#endif
{
  if (cmd_list)
    cmd_list->mark_as_function_body ();

  if (m_scope)
    m_scope->set_function (this);
}

octave_user_function::~octave_user_function (void)
{
  // FIXME: shouldn't this happen automatically when deleting cmd_list?
  if (cmd_list)
    cmd_list->remove_all_breakpoints (file_name);

  delete m_scope;

  delete param_list;
  delete ret_list;
  delete cmd_list;
  delete lead_comm;
  delete trail_comm;

#if defined (HAVE_LLVM)
  delete jit_info;
#endif
}

octave_user_function *
octave_user_function::define_ret_list (octave::tree_parameter_list *t)
{
  ret_list = t;

  return this;
}

void
octave_user_function::stash_fcn_file_name (const std::string& nm)
{
  file_name = nm;
}

// If there is no explicit end statement at the end of the function,
// relocate the no_op that was generated for the end of file condition
// to appear on the next line after the last statement in the file, or
// the next line after the function keyword if there are no statements.
// More precisely, the new location should probably be on the next line
// after the end of the parameter list, but we aren't tracking that
// information (yet).

void
octave_user_function::maybe_relocate_end_internal (void)
{
  if (cmd_list && ! cmd_list->empty ())
    {
      octave::tree_statement *last_stmt = cmd_list->back ();

      if (last_stmt && last_stmt->is_end_of_fcn_or_script ()
          && last_stmt->is_end_of_file ())
        {
          octave::tree_statement_list::reverse_iterator
            next_to_last_elt = cmd_list->rbegin ();

          next_to_last_elt++;

          int new_eof_line;
          int new_eof_col;

          if (next_to_last_elt == cmd_list->rend ())
            {
              new_eof_line = beginning_line ();
              new_eof_col = beginning_column ();
            }
          else
            {
              octave::tree_statement *next_to_last_stmt = *next_to_last_elt;

              new_eof_line = next_to_last_stmt->line ();
              new_eof_col = next_to_last_stmt->column ();
            }

          last_stmt->set_location (new_eof_line + 1, new_eof_col);
        }
    }
}

void
octave_user_function::maybe_relocate_end (void)
{
  std::map<std::string, octave_value> fcns = subfunctions ();

  if (! fcns.empty ())
    {
      for (auto& nm_fnval : fcns)
        {
          octave_user_function *f = nm_fnval.second.user_function_value ();

          if (f)
            f->maybe_relocate_end_internal ();
        }
    }

  maybe_relocate_end_internal ();
}

void
octave_user_function::stash_parent_fcn_scope (symbol_table::scope *ps)
{
  parent_scope = ps;
}

std::string
octave_user_function::profiler_name (void) const
{
  std::ostringstream result;

  if (is_anonymous_function ())
    result << "anonymous@" << fcn_file_name ()
           << ":" << location_line << ":" << location_column;
  else if (is_subfunction ())
    result << parent_fcn_name () << ">" << name ();
  else if (is_class_method ())
    result << "@" << dispatch_class () << "/" << name ();
  else if (is_class_constructor () || is_classdef_constructor ())
    result << "@" << name ();
  else if (is_inline_function ())
    result << "inline@" << fcn_file_name ()
           << ":" << location_line << ":" << location_column;
  else
    result << name ();

  return result.str ();
}

void
octave_user_function::mark_as_system_fcn_file (void)
{
  if (! file_name.empty ())
    {
      // We really should stash the whole path to the file we found,
      // when we looked it up, to avoid possible race conditions...
      // FIXME
      //
      // We probably also don't need to get the library directory
      // every time, but since this function is only called when the
      // function file is parsed, it probably doesn't matter that
      // much.

      std::string ff_name = fcn_file_in_path (file_name);

      if (Vfcn_file_dir == ff_name.substr (0, Vfcn_file_dir.length ()))
        system_fcn_file = true;
    }
  else
    system_fcn_file = false;
}

void
octave_user_function::erase_subfunctions (void)
{
  m_scope->erase_subfunctions ();
}

bool
octave_user_function::takes_varargs (void) const
{
  return (param_list && param_list->takes_varargs ());
}

bool
octave_user_function::takes_var_return (void) const
{
  return (ret_list && ret_list->takes_varargs ());
}

void
octave_user_function::mark_as_private_function (const std::string& cname)
{
  m_scope->mark_subfunctions_in_scope_as_private (cname);

  octave_function::mark_as_private_function (cname);
}

void
octave_user_function::lock_subfunctions (void)
{
  m_scope->lock_subfunctions ();
}

void
octave_user_function::unlock_subfunctions (void)
{
  m_scope->unlock_subfunctions ();
}

std::map<std::string, octave_value>
octave_user_function::subfunctions (void) const
{
  return m_scope->subfunctions ();
}

bool
octave_user_function::has_subfunctions (void) const
{
  return ! subfcn_names.empty ();
}

void
octave_user_function::stash_subfunction_names
  (const std::list<std::string>& names)
{
  subfcn_names = names;
}

octave_value_list
octave_user_function::all_va_args (const octave_value_list& args)
{
  octave_value_list retval;

  octave_idx_type n = args.length () - num_named_args;

  if (n > 0)
    retval = args.slice (num_named_args, n);

  return retval;
}

octave_value_list
octave_user_function::call (octave::tree_evaluator& tw, int nargout,
                            const octave_value_list& _args)
{
  octave_value_list retval;

  if (! cmd_list)
    return retval;

  // If this function is a classdef constructor, extract the first input
  // argument, which must be the partially constructed object instance.

  octave_value_list args (_args);
  octave_value_list ret_args;

  if (is_classdef_constructor ())
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
  if (is_special_expr ()
      && octave::tree_jit::execute (*this, args, retval))
    return retval;
#endif

  octave::unwind_protect frame;

  frame.protect_var (call_depth);
  call_depth++;

  if (call_depth >= Vmax_recursion_depth)
    error ("max_recursion_depth exceeded");

  // Save old and set current symbol table context, for
  // eval_undefined_error().

  octave::call_stack& cs
    = octave::__get_call_stack__ ("octave_user_function::call");

  symbol_table::context_id context = anonymous_function ? 0 : call_depth;
  cs.push (this, m_scope, context);

  frame.protect_var (Vtrack_line_num);
  Vtrack_line_num = true;    // update source line numbers, even if debugging
  frame.add_method (cs, &octave::call_stack::pop);

  if (call_depth > 0 && ! is_anonymous_function ())
    {
      m_scope->push_context ();

#if 0
      std::cerr << name () << " scope: " << m_scope
                << " call depth: " << call_depth
                << " context: " << m_scope->current_context () << std::endl;
#endif

      frame.add_method (m_scope, &symbol_table::scope::pop_context);
    }

  string_vector arg_names = args.name_tags ();

  if (param_list && ! param_list->varargs_only ())
    {
#if 0
      std::cerr << "defining param list, scope: " << m_scope
                << ", context: " << m_scope->current_context () << std::endl;
#endif
      tw.define_parameter_list_from_arg_vector (param_list, args);
    }

  // For classdef constructor, pre-populate the output arguments
  // with the pre-initialized object instance, extracted above.

  if (is_classdef_constructor ())
    {
      if (! ret_list)
        error ("%s: invalid classdef constructor, no output argument defined",
               dispatch_class ().c_str ());

      tw.define_parameter_list_from_arg_vector (ret_list, ret_args);
    }

  // Force parameter list to be undefined when this function exits.
  // Doing so decrements the reference counts on the values of local
  // variables that are also named function parameters.

  if (param_list)
    frame.add_method (&tw, &octave::tree_evaluator::undefine_parameter_list,
                      param_list);

  // Force return list to be undefined when this function exits.
  // Doing so decrements the reference counts on the values of local
  // variables that are also named values returned by this function.

  if (ret_list)
    frame.add_method (&tw, &octave::tree_evaluator::undefine_parameter_list,
                      ret_list);

  if (call_depth == 0)
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

      frame.add_method (m_scope, &symbol_table::scope::clear_variables);
    }

  bind_automatic_vars (tw, arg_names, args.length (), nargout,
                       all_va_args (args));

  frame.add_method (this, &octave_user_function::restore_warning_states);

  bool echo_commands = (Vecho_executing_commands & ECHO_FUNCTIONS);

  if (echo_commands)
    print_code_function_header ();

  // Set pointer to the current unwind_protect frame to allow
  // certain builtins register simple cleanup in a very optimized manner.
  // This is *not* intended as a general-purpose on-cleanup mechanism,
  frame.protect_var (curr_unwind_protect_frame);
  curr_unwind_protect_frame = &frame;

  // Evaluate the commands that make up the function.

  frame.protect_var (octave::tree_evaluator::statement_context);
  octave::tree_evaluator::statement_context = octave::tree_evaluator::function;

  {
    profile_data_accumulator::enter<octave_user_function>
      block (profiler, *this);

    if (is_special_expr ())
      {
        assert (cmd_list->length () == 1);

        octave::tree_statement *stmt = cmd_list->front ();

        octave::tree_expression *expr = stmt->expression ();

        if (expr)
          {
            cs.set_location (stmt->line (), stmt->column ());

            retval = tw.evaluate_n (expr, nargout);
          }
      }
    else
      cmd_list->accept (tw);
  }

  if (echo_commands)
    print_code_function_trailer ();

  if (octave::tree_return_command::returning)
    octave::tree_return_command::returning = 0;

  if (octave::tree_break_command::breaking)
    octave::tree_break_command::breaking--;

  // Copy return values out.

  if (ret_list && ! is_special_expr ())
    {
      tw.initialize_undefined_parameter_list_elements (ret_list, my_name,
                                                       nargout, Matrix ());

      Cell varargout;

      if (ret_list->takes_varargs ())
        {
          octave_value varargout_varval = m_scope->varval ("varargout");

          if (varargout_varval.is_defined ())
            varargout = varargout_varval.xcell_value ("varargout must be a cell array object");
        }

      retval = tw.convert_parameter_list_to_const_vector (ret_list, nargout, varargout);
    }

  return retval;
}

void
octave_user_function::accept (octave::tree_walker& tw)
{
  tw.visit_octave_user_function (*this);
}

octave::tree_expression *
octave_user_function::special_expr (void)
{
  assert (is_special_expr ());
  assert (cmd_list->length () == 1);

  octave::tree_statement *stmt = cmd_list->front ();
  return stmt->expression ();
}

bool
octave_user_function::subsasgn_optimization_ok (void)
{
  bool retval = false;
  if (Voptimize_subsasgn_calls
      && param_list && ret_list
      && param_list->length () > 0 && ! param_list->varargs_only ()
      && ret_list->length () == 1 && ! ret_list->takes_varargs ())
    {
      octave::tree_identifier *par1 = param_list->front ()->ident ();
      octave::tree_identifier *ret1 = ret_list->front ()->ident ();
      retval = par1->name () == ret1->name ();
    }

  return retval;
}

#if 0
void
octave_user_function::print_symtab_info (std::ostream& os) const
{
  symbol_table& symtab
    = octave::__get_symbol_table__ ("octave_user_function::print_symtab_info");

  symtab.print_info (os, m_scope);
}
#endif

void
octave_user_function::print_code_function_header (void)
{
  octave::tree_print_code tpc (octave_stdout, VPS4);

  tpc.visit_octave_user_function_header (*this);
}

void
octave_user_function::print_code_function_trailer (void)
{
  octave::tree_print_code tpc (octave_stdout, VPS4);

  tpc.visit_octave_user_function_trailer (*this);
}

void
octave_user_function::bind_automatic_vars
  (octave::tree_evaluator& tw, const string_vector& arg_names,
   int nargin, int nargout, const octave_value_list& va_args)
{
  if (! arg_names.empty ())
    {
      // It is better to save this in the hidden variable .argn. and
      // then use that in the inputname function instead of using argn,
      // which might be redefined in a function.  Keep the old argn name
      // for backward compatibility of functions that use it directly.

      m_scope->force_assign ("argn", charMatrix (arg_names, Vstring_fill_char));
      m_scope->force_assign (".argn.", Cell (arg_names));

      m_scope->mark_hidden (".argn.");

      m_scope->mark_automatic ("argn");
      m_scope->mark_automatic (".argn.");
    }

  m_scope->force_assign (".nargin.", nargin);
  m_scope->force_assign (".nargout.", nargout);

  m_scope->mark_hidden (".nargin.");
  m_scope->mark_hidden (".nargout.");

  m_scope->mark_automatic (".nargin.");
  m_scope->mark_automatic (".nargout.");

  m_scope->assign (".saved_warning_states.");

  m_scope->mark_automatic (".saved_warning_states.");
  m_scope->mark_automatic (".saved_warning_states.");

  if (takes_varargs ())
    m_scope->assign ("varargin", va_args.cell_value ());

  Matrix ignored_fcn_outputs = tw.ignored_fcn_outputs ();

  m_scope->assign (".ignored.", ignored_fcn_outputs);

  m_scope->mark_hidden (".ignored.");
  m_scope->mark_automatic (".ignored.");
}

void
octave_user_function::restore_warning_states (void)
{
  octave_value val = m_scope->varval (".saved_warning_states.");

  if (val.is_defined ())
    {
      // Fail spectacularly if .saved_warning_states. is not an
      // octave_map (or octave_scalar_map) object.

      if (! val.isstruct ())
        panic_impossible ();

      octave_map m = val.map_value ();

      Cell ids = m.contents ("identifier");
      Cell states = m.contents ("state");

      octave::interpreter& interp
        = octave::__get_interpreter__ ("octave_user_function::restore_warning_states");

      for (octave_idx_type i = 0; i < m.numel (); i++)
        Fwarning (interp, ovl (states(i), ids(i)));
    }
}

DEFMETHOD (nargin, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} nargin ()
@deftypefnx {} {} nargin (@var{fcn})
Report the number of input arguments to a function.

Called from within a function, return the number of arguments passed to the
function.  At the top level, return the number of command line arguments
passed to Octave.

If called with the optional argument @var{fcn}---a function name or
handle---return the declared number of arguments that the function can
accept.

If the last argument to @var{fcn} is @var{varargin} the returned value is
negative.  For example, the function @code{union} for sets is declared as

@example
@group
function [y, ia, ib] = union (a, b, varargin)

and

nargin ("union")
@result{} -3
@end group
@end example

Programming Note: @code{nargin} does not work on compiled functions
(@file{.oct} files) such as built-in or dynamically loaded functions.
@seealso{nargout, narginchk, varargin, inputname}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value retval;

  symbol_table& symtab = interp.get_symbol_table ();

  if (nargin == 1)
    {
      octave_value func = args(0);

      if (func.is_string ())
        {
          std::string name = func.string_value ();
          func = symtab.find_function (name);
          if (func.is_undefined ())
            error ("nargin: invalid function name: %s", name.c_str ());
        }

      octave_function *fcn_val = func.function_value (true);
      if (! fcn_val)
        error ("nargin: FCN must be a string or function handle");

      octave_user_function *fcn = fcn_val->user_function_value (true);

      if (! fcn)
        {
          // Matlab gives up for histc, so maybe it's ok that we
          // give up sometimes too?

          std::string type = fcn_val->type_name ();
          error ("nargin: number of input arguments unavailable for %s objects",
                 type.c_str ());
        }

      octave::tree_parameter_list *param_list = fcn->parameter_list ();

      retval = (param_list ? param_list->length () : 0);
      if (fcn->takes_varargs ())
        retval = -1 - retval;
    }
  else
    {
      retval = symtab.varval (".nargin.");

      if (retval.is_undefined ())
        retval = 0;
    }

  return retval;
}

DEFMETHOD (nargout, interp,args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} nargout ()
@deftypefnx {} {} nargout (@var{fcn})
Report the number of output arguments from a function.

Called from within a function, return the number of values the caller
expects to receive.  At the top level, @code{nargout} with no argument is
undefined and will produce an error.

If called with the optional argument @var{fcn}---a function name or
handle---return the number of declared output values that the function can
produce.

If the final output argument is @var{varargout} the returned value is
negative.

For example,

@example
f ()
@end example

@noindent
will cause @code{nargout} to return 0 inside the function @code{f} and

@example
[s, t] = f ()
@end example

@noindent
will cause @code{nargout} to return 2 inside the function @code{f}.

In the second usage,

@example
nargout (@@histc)   # or nargout ("histc") using a string input
@end example

@noindent
will return 2, because @code{histc} has two outputs, whereas

@example
nargout (@@imread)
@end example

@noindent
will return -2, because @code{imread} has two outputs and the second is
@var{varargout}.

Programming Note.  @code{nargout} does not work for built-in functions and
returns -1 for all anonymous functions.
@seealso{nargin, varargout, isargout, nthargout}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value retval;

  symbol_table& symtab = interp.get_symbol_table ();

  if (nargin == 1)
    {
      octave_value func = args(0);

      if (func.is_string ())
        {
          std::string name = func.string_value ();
          func = symtab.find_function (name);
          if (func.is_undefined ())
            error ("nargout: invalid function name: %s", name.c_str ());
        }

      if (func.is_inline_function ())
        return ovl (1);

      if (func.is_function_handle ())
        {
          octave_fcn_handle *fh = func.fcn_handle_value ();
          std::string fh_nm = fh->fcn_name ();

          if (fh_nm == octave_fcn_handle::anonymous)
            return ovl (-1);
        }

      octave_function *fcn_val = func.function_value (true);
      if (! fcn_val)
        error ("nargout: FCN must be a string or function handle");

      octave_user_function *fcn = fcn_val->user_function_value (true);

      if (! fcn)
        {
          // Matlab gives up for histc, so maybe it's ok that we
          // give up sometimes too?

          std::string type = fcn_val->type_name ();
          error ("nargout: number of output arguments unavailable for %s objects",
                 type.c_str ());
        }

      octave::tree_parameter_list *ret_list = fcn->return_list ();

      retval = (ret_list ? ret_list->length () : 0);

      if (fcn->takes_var_return ())
        retval = -1 - retval;
    }
  else
    {
      if (symtab.at_top_level ())
        error ("nargout: invalid call at top level");

      retval = symtab.varval (".nargout.");

      if (retval.is_undefined ())
        retval = 0;
    }

  return retval;
}

DEFUN (optimize_subsasgn_calls, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} optimize_subsasgn_calls ()
@deftypefnx {} {@var{old_val} =} optimize_subsasgn_calls (@var{new_val})
@deftypefnx {} {} optimize_subsasgn_calls (@var{new_val}, "local")
Query or set the internal flag for @code{subsasgn} method call
optimizations.

If true, Octave will attempt to eliminate the redundant copying when calling
the @code{subsasgn} method of a user-defined class.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{subsasgn}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (optimize_subsasgn_calls);
}

static bool val_in_table (const Matrix& table, double val)
{
  if (table.isempty ())
    return false;

  octave_idx_type i = table.lookup (val, ASCENDING);
  return (i > 0 && table(i-1) == val);
}

static bool isargout1 (int nargout, const Matrix& ignored, double k)
{
  if (k != octave::math::round (k) || k <= 0)
    error ("isargout: K must be a positive integer");

  return (k == 1 || k <= nargout) && ! val_in_table (ignored, k);
}

DEFMETHOD (isargout, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} isargout (@var{k})
Within a function, return a logical value indicating whether the argument
@var{k} will be assigned to a variable on output.

If the result is false, the argument has been ignored during the function
call through the use of the tilde (~) special output argument.  Functions
can use @code{isargout} to avoid performing unnecessary calculations for
outputs which are unwanted.

If @var{k} is outside the range @code{1:max (nargout)}, the function returns
false.  @var{k} can also be an array, in which case the function works
element-by-element and a logical array is returned.  At the top level,
@code{isargout} returns an error.
@seealso{nargout, varargout, nthargout}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  symbol_table& symtab = interp.get_symbol_table ();

  if (symtab.at_top_level ())
    error ("isargout: invalid call at top level");

  int nargout1 = symtab.varval (".nargout.").int_value ();

  Matrix ignored;
  octave_value tmp = symtab.varval (".ignored.");
  if (tmp.is_defined ())
    ignored = tmp.matrix_value ();

  if (args(0).is_scalar_type ())
    {
      double k = args(0).double_value ();

      return ovl (isargout1 (nargout1, ignored, k));
    }
  else if (args(0).isnumeric ())
    {
      const NDArray ka = args(0).array_value ();

      boolNDArray r (ka.dims ());
      for (octave_idx_type i = 0; i < ka.numel (); i++)
        r(i) = isargout1 (nargout1, ignored, ka(i));

      return ovl (r);
    }
  else
    err_wrong_type_arg ("isargout", args(0));

  return ovl ();
}

/*
%!function [x, y] = try_isargout ()
%!  if (isargout (1))
%!    if (isargout (2))
%!      x = 1; y = 2;
%!    else
%!      x = -1;
%!    endif
%!  else
%!    if (isargout (2))
%!      y = -2;
%!    else
%!      error ("no outputs requested");
%!    endif
%!  endif
%!endfunction
%!
%!function [a, b] = try_isargout2 (x, y)
%!  a = y;
%!  b = {isargout(1), isargout(2), x};
%!endfunction
%!
%!test
%! [x, y] = try_isargout ();
%! assert ([x, y], [1, 2]);
%!
%!test
%! [x, ~] = try_isargout ();
%! assert (x, -1);
%!
%!test
%! [~, y] = try_isargout ();
%! assert (y, -2);
%!
%!error [~, ~] = try_isargout ()
%!
## Check to see that isargout isn't sticky:
%!test
%! [x, y] = try_isargout ();
%! assert ([x, y], [1, 2]);
%!
## It should work without ():
%!test
%! [~, y] = try_isargout;
%! assert (y, -2);
%!
## It should work in function handles, anonymous functions, and cell
## arrays of handles or anonymous functions.
%!test
%! fh = @try_isargout;
%! af = @() try_isargout;
%! c = {fh, af};
%! [~, y] = fh ();
%! assert (y, -2);
%! [~, y] = af ();
%! assert (y, -2);
%! [~, y] = c{1}();
%! assert (y, -2);
%! [~, y] = c{2}();
%! assert (y, -2);
%!
## Nesting, anyone?
%!test
%! [~, b] = try_isargout2 (try_isargout, rand);
%! assert (b, {0, 1, -1});
%!test
%! [~, b] = try_isargout2 ({try_isargout, try_isargout}, rand);
%! assert (b, {0, 1, {-1, -1}});
*/
