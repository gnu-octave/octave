/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2006, 2007 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "str-vec.h"

#include <defaults.h>
#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "input.h"
#include "oct-obj.h"
#include "ov-usr-fcn.h"
#include "ov.h"
#include "pager.h"
#include "pt-jump.h"
#include "pt-misc.h"
#include "pt-pr-code.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "symtab.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "parse.h"
#include "variables.h"

// Maximum nesting level for functions called recursively.
static int Vmax_recursion_depth = 256;

// User defined scripts.

DEFINE_OCTAVE_ALLOCATOR (octave_user_script);

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
					tree_statement_list *cmds,
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
  delete cmd_list;
}

octave_value_list
octave_user_script::subsref (const std::string&,
			     const std::list<octave_value_list>&, int)
{
  octave_value_list retval;

  ::error ("invalid use of script in index expression");

  return retval;
}

octave_value_list
octave_user_script::do_multi_index_op (int nargout,
				       const octave_value_list& args)
{
  octave_value_list retval;

  unwind_protect::begin_frame ("user_script_eval");

  if (! error_state)
    {
      if (args.length () == 0 && nargout == 0)
	{
	  if (cmd_list)
	    {
	      unwind_protect_int (call_depth);
	      call_depth++;

	      if (call_depth < Vmax_recursion_depth)
		{
		  octave_call_stack::push (this);

		  unwind_protect::add (octave_call_stack::unwind_pop, 0);

		  cmd_list->eval ();

		  if (tree_return_command::returning)
		    tree_return_command::returning = 0;

		  if (tree_break_command::breaking)
		    tree_break_command::breaking--;
		}
	      else
		::error ("max_recursion_limit exceeded");
    	    }
	}
      else
	error ("invalid call to script");
    }

  unwind_protect::run_frame ("user_script_eval");

  return retval;
}

void
octave_user_script::accept (tree_walker& tw)
{
  tw.visit_octave_user_script (*this);
}

// User defined functions.

DEFINE_OCTAVE_ALLOCATOR (octave_user_function);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_user_function,
				     "user-defined function",
				     "user-defined function");

// Ugh.  This really needs to be simplified (code/data?
// extrinsic/intrinsic state?).

octave_user_function::octave_user_function
  (symbol_table::scope_id sid, tree_parameter_list *pl,
   tree_parameter_list *rl, tree_statement_list *cl)
  : octave_user_code (std::string (), std::string ()),
    param_list (pl), ret_list (rl), cmd_list (cl),
    lead_comm (), trail_comm (), file_name (),
    parent_name (), t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    system_fcn_file (false), call_depth (-1),
    num_named_args (param_list ? param_list->length () : 0),
    nested_function (false), inline_function (false),
    class_constructor (false), class_method (false), xdispatch_class (),
    args_passed (), num_args_passed (0), parent_scope (-1), local_scope (sid)
{
  if (cmd_list)
    cmd_list->mark_as_function_body ();
}

octave_user_function::~octave_user_function (void)
{
  delete param_list;
  delete ret_list;
  delete cmd_list;
  delete lead_comm;
  delete trail_comm;

  symbol_table::erase_scope (local_scope);
}

octave_user_function *
octave_user_function::define_ret_list (tree_parameter_list *t)
{
  ret_list = t;

  return this;
}

void
octave_user_function::stash_fcn_file_name (const std::string& nm)
{
  file_name = nm;
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
	system_fcn_file = 1;
    }
  else
    system_fcn_file = 0;
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
octave_user_function::lock_subfunctions (void)
{
  symbol_table::lock_subfunctions (local_scope);
}

void
octave_user_function::unlock_subfunctions (void)
{
  symbol_table::unlock_subfunctions (local_scope);
}

octave_value_list
octave_user_function::octave_all_va_args (void)
{
  octave_value_list retval;

  int n = num_args_passed - num_named_args;

  if (n > 0)
    {
      retval.resize (n);

      int k = 0;
      for (int i = num_named_args; i < num_args_passed; i++)
	retval(k++) = args_passed(i);
    }

  return retval;
}

static void
clear_param_list (void *lst)
{
  tree_parameter_list *tmp = static_cast<tree_parameter_list *> (lst);

  if (tmp)
    tmp->undefine ();
}

static void
restore_args_passed (void *fcn)
{
  octave_user_function *tmp = static_cast<octave_user_function *> (fcn);

  if (tmp)
    tmp->restore_args_passed ();
}

octave_value_list
octave_user_function::subsref (const std::string& type,
			       const std::list<octave_value_list>& idx,
			       int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
	int tmp_nargout = (type.length () > 1 && nargout == 0) ? 1 : nargout;

	retval = do_multi_index_op (tmp_nargout, idx.front ());
      }
      break;

    case '{':
    case '.':
      {
	std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // FIXME -- perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

octave_value_list
octave_user_function::do_multi_index_op (int nargout,
					 const octave_value_list& args)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  if (! cmd_list)
    return retval;

  int nargin = args.length ();

  unwind_protect::begin_frame ("user_func_eval");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth >= Vmax_recursion_depth)
    {
      ::error ("max_recursion_limit exceeded");
      unwind_protect::run_frame ("user_func_eval");
      return retval;
    }

  // Save old and set current symbol table context, for
  // eval_undefined_error().

  symbol_table::push_scope (local_scope);
  unwind_protect::add (symbol_table::pop_scope);

  octave_call_stack::push (this, local_scope, call_depth);
  unwind_protect::add (octave_call_stack::unwind_pop, 0);

  if (call_depth > 0)
    {
      symbol_table::push_context ();

      unwind_protect::add (symbol_table::pop_context);
    }
  else
    {
      // Force symbols to be undefined again when this function exits.
      unwind_protect::add (symbol_table::clear_variables);
    }

  // Save and restore args passed for recursive calls.

  save_args_passed (args);

  unwind_protect::add (::restore_args_passed, this);

  string_vector arg_names = args.name_tags ();

  unwind_protect_int (num_args_passed);
  num_args_passed = nargin;

  if (param_list && ! param_list->varargs_only ())
    {
      param_list->define_from_arg_vector (args);
      if (error_state)
	goto abort;
    }

  // Force parameter list to be undefined when this function exits.
  // Doing so decrements the reference counts on the values of local
  // variables that are also named function parameters.

  unwind_protect::add (clear_param_list, param_list);

  // Force return list to be undefined when this function exits.
  // Doing so decrements the reference counts on the values of local
  // variables that are also named values returned by this function.

  unwind_protect::add (clear_param_list, ret_list);

  // The following code is in a separate scope to avoid warnings from
  // G++ about `goto abort' crossing the initialization of some
  // variables.

  {
    bind_automatic_vars (arg_names, nargin, nargout, octave_all_va_args ());

    bool echo_commands = (Vecho_executing_commands & ECHO_FUNCTIONS);

    if (echo_commands)
      print_code_function_header ();

    // Evaluate the commands that make up the function.

    unwind_protect_bool (evaluating_function_body);
    evaluating_function_body = true;

    if (is_inline_function ())
      {
	assert (cmd_list->length () == 1);

	retval = cmd_list->eval (false, nargout);
      }
    else
      cmd_list->eval ();

    if (echo_commands)
      print_code_function_trailer ();

    if (tree_return_command::returning)
      tree_return_command::returning = 0;

    if (tree_break_command::breaking)
      tree_break_command::breaking--;

    if (error_state)
      goto abort;
    
    // Copy return values out.

    if (ret_list && ! is_inline_function ())
      {
	ret_list->initialize_undefined_elements (my_name, nargout, Matrix ());

	Cell varargout;

	if (ret_list->takes_varargs ())
	  {
	    octave_value varargout_varval = symbol_table::varval ("varargout");

	    if (varargout_varval.is_defined ())
	      {
		varargout = varargout_varval.cell_value ();

		if (error_state)
		  error ("expecting varargout to be a cell array object");
	      }
	  }

	if (! error_state)
	  retval = ret_list->convert_to_const_vector (varargout);
      }
  }

 abort:
  unwind_protect::run_frame ("user_func_eval");

  return retval;
}

void
octave_user_function::accept (tree_walker& tw)
{
  tw.visit_octave_user_function (*this);
}

#if 0
void
octave_user_function::print_symtab_info (std::ostream& os) const
{
  symbol_table::print_info (os, local_scope);
}
#endif

void
octave_user_function::print_code_function_header (void)
{
  tree_print_code tpc (octave_stdout, VPS4);

  tpc.visit_octave_user_function_header (*this);
}

void
octave_user_function::print_code_function_trailer (void)
{
  tree_print_code tpc (octave_stdout, VPS4);

  tpc.visit_octave_user_function_trailer (*this);
}

void
octave_user_function::bind_automatic_vars
  (const string_vector& arg_names, int nargin, int nargout,
   const octave_value_list& va_args)
{
  if (! arg_names.empty ())
    symbol_table::varref ("argn") = arg_names;

  symbol_table::varref (".nargin.") = nargin;
  symbol_table::varref (".nargout.") = nargout;

  symbol_table::mark_hidden (".nargin.");
  symbol_table::mark_hidden (".nargout.");

  if (takes_varargs ())
    {
      int n = va_args.length ();

      Cell varargin (1, n);

      for (int i = 0; i < n; i++)
	varargin(0,i) = va_args(i);

      symbol_table::varref ("varargin") = varargin;
    }
}

DEFUN (nargin, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} nargin ()\n\
@deftypefnx {Built-in Function} {} nargin (@var{fcn_name})\n\
Within a function, return the number of arguments passed to the function.\n\
At the top level, return the number of command line arguments passed to\n\
Octave.  If called with the optional argument @var{fcn_name}, return the\n\
maximum number of arguments the named function can accept, or -1 if the\n\
function accepts a variable number of arguments.\n\
@seealso{nargout, varargin, varargout}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string fname = args(0).string_value ();

      if (! error_state)
	{
	  octave_value fcn_val = symbol_table::find_user_function (fname);

	  octave_user_function *fcn = fcn_val.user_function_value (true);

	  if (fcn)
	    {
	      if (fcn->takes_varargs ())
		retval = -1;
	      else
		{
		  tree_parameter_list *param_list = fcn->parameter_list ();

		  retval = param_list ? param_list->length () : 0;
		}
	    }
	  else
	    error ("nargin: invalid function");
	}
      else
	error ("nargin: expecting string as first argument");
    }
  else if (nargin == 0)
    {
      retval = symbol_table::varval (".nargin.");

      if (retval.is_undefined ())
	retval = 0;
    }
  else
    print_usage ();

  return retval;
}

DEFUN (nargout, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} nargout ()\n\
@deftypefnx {Built-in Function} {} nargout (@var{fcn_name})\n\
Within a function, return the number of values the caller expects to\n\
receive.  If called with the optional argument @var{fcn_name}, return the\n\
maximum number of values the named function can produce, or -1 if the\n\
function can produce a variable number of values.\n\
\n\
For example,\n\
\n\
@example\n\
f ()\n\
@end example\n\
\n\
@noindent\n\
will cause @code{nargout} to return 0 inside the function @code{f} and\n\
\n\
@example\n\
[s, t] = f ()\n\
@end example\n\
\n\
@noindent\n\
will cause @code{nargout} to return 2 inside the function\n\
@code{f}.\n\
\n\
At the top level, @code{nargout} is undefined.\n\
@seealso{nargin, varargin, varargout}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string fname = args(0).string_value ();

      if (! error_state)
	{
	  octave_value fcn_val = symbol_table::find_user_function (fname);

	  octave_user_function *fcn = fcn_val.user_function_value (true);

	  if (fcn)
	    {
	      if (fcn->takes_var_return ())
		retval = -1;
	      else
		{
		  tree_parameter_list *ret_list = fcn->return_list ();

		  retval = ret_list ? ret_list->length () : 0;
		}
	    }
	  else
	    error ("nargout: invalid function");
	}
      else
	error ("nargout: expecting string as first argument");
    }
  else if (nargin == 0)
    {
      if (! symbol_table::at_top_level ())
	{
	  retval = symbol_table::varval (".nargout.");

	  if (retval.is_undefined ())
	    retval = 0;
	}
      else
	error ("nargout: invalid call at top level");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (max_recursion_depth, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} max_recursion_depth ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} max_recursion_depth (@var{new_val})\n\
Query or set the internal limit on the number of times a function may\n\
be called recursively.  If the limit is exceeded, an error message is\n\
printed and control returns to the top level.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (max_recursion_depth);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
