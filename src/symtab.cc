/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2003, 2004, 2005, 2006, 2007 John W. Eaton
  
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

#include "oct-env.h"
#include "oct-time.h"
#include "file-ops.h"
#include "file-stat.h"

#include "defun.h"
#include "dirfns.h"
#include "input.h"
#include "load-path.h"
#include "symtab.h"
#include "ov-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pt-arg-list.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"

symbol_table *symbol_table::instance = 0;

std::map<symbol_table::scope_id, symbol_table*> symbol_table::all_instances;

std::map<std::string, symbol_table::fcn_info> symbol_table::fcn_table;

const symbol_table::scope_id symbol_table::xglobal_scope = 0;
const symbol_table::scope_id symbol_table::xtop_scope = 1;

symbol_table::scope_id symbol_table::xcurrent_scope = 1;
symbol_table::scope_id symbol_table::xcurrent_caller_scope = -1;

symbol_table::scope_id symbol_table::xparent_scope = -1;

std::deque<symbol_table::scope_id> symbol_table::scope_stack;

symbol_table::scope_id symbol_table::next_available_scope = 2;
std::set<symbol_table::scope_id> symbol_table::scope_ids_in_use;
std::set<symbol_table::scope_id> symbol_table::scope_ids_free_list;

// Should Octave always check to see if function files have changed
// since they were last compiled?
static int Vignore_function_time_stamp = 1;

octave_value
symbol_table::symbol_record::find (tree_argument_list *args,
				   const string_vector& arg_names,
				   octave_value_list& evaluated_args,
				   bool& args_evaluated) const
{
  octave_value retval;

  if (is_global ())
    return symbol_table::varref (name (), symbol_table::xglobal_scope);
  else
    {
      octave_value val = varval ();

      if (val.is_defined ())
	return val;
    }

  return symbol_table::find_function (name (), args, arg_names,
				      evaluated_args, args_evaluated);
}

// Check the load path to see if file that defined this is still
// visible.  If the file is no longer visible, then erase the
// definition and move on.  If the file is visible, then we also
// need to check to see whether the file has changed since the the
// function was loaded/parsed.  However, this check should only
// happen once per prompt (for files found from relative path
// elements, we also check if the working directory has changed
// since the last time the function was loaded/parsed).
//
// FIXME -- perhaps this should be done for all loaded functions when
// the prompt is printed or the directory has changed, and then we
// would not check for it when finding symbol definitions.

static inline bool
out_of_date_check_internal (octave_value& function)
{
  bool retval = false;

  octave_function *fcn = function.function_value (true);

  if (fcn)
    {
      // FIXME -- we need to handle nested functions properly here.

      if (! fcn->is_nested_function ())
	{
	  std::string ff = fcn->fcn_file_name ();

	  if (! ff.empty ())
	    {
	      octave_time tc = fcn->time_checked ();

	      bool relative = fcn->is_relative ();

	      if (tc < Vlast_prompt_time
		  || (relative && tc < Vlast_chdir_time))
		{
		  octave_time ottp = fcn->time_parsed ();
		  time_t tp = ottp.unix_time ();

		  std::string nm = fcn->name ();

		  int nm_len = nm.length ();

		  std::string file;
		  std::string dir_name;

		  if (octave_env::absolute_pathname (nm)
		      && ((nm_len > 4 && (nm.substr (nm_len-4) == ".oct"
					  || nm.substr (nm_len-4) == ".mex"))
			  || (nm_len > 2 && nm.substr (nm_len-4) == ".m")))
		    file = nm;
		  else
		    // FIXME -- this lookup is not right since it doesn't
		    // account for dispatch type.
		    file = octave_env::make_absolute (load_path::find_fcn (nm, dir_name),
						      octave_env::getcwd ());

		  if (file.empty ())
		    {
		      // Can't see this function from current
		      // directory, so we should clear it.

		      function = octave_value ();
		    }
		  else if (same_file (file, ff))
		    {
		      fcn->mark_fcn_file_up_to_date (octave_time ());

		      if (! (Vignore_function_time_stamp == 2
			     || (Vignore_function_time_stamp
				 && fcn->is_system_fcn_file ())))
			{
			  file_stat fs (ff);

			  if (fs)
			    {
			      if (fs.is_newer (tp))
				{
				  fcn = load_fcn_from_file (ff, dir_name);

				  if (fcn)
				    {
				      retval = true;

				      function = octave_value (fcn);
				    }
				  else
				    function = octave_value ();
				}				
			    }
			  else
			    function = octave_value ();
			}
		    }
		}
	    }
	}
    }

  return retval;
}

bool
out_of_date_check (octave_value& function)
{
  return out_of_date_check_internal (function);
}

octave_value
symbol_table::fcn_info::fcn_info_rep::load_private_function
  (const std::string& dir_name)
{
  octave_value retval;

  std::string file_name = load_path::find_private_fcn (dir_name, name);

  if (! file_name.empty ())
    {
      octave_function *fcn = load_fcn_from_file (file_name);

      if (fcn)
	{
	  retval = octave_value (fcn);

	  private_functions[dir_name] = retval;
	}
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::load_class_constructor (void)
{
  octave_value retval;

  std::string dir_name;

  std::string file_name = load_path::find_method (name, name, dir_name);

  if (! file_name.empty ())
    {
      octave_function *fcn = load_fcn_from_file (file_name, dir_name, name);

      if (fcn)
	{
	  retval = octave_value (fcn);

	  class_constructors[name] = retval;
	}
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::load_class_method
  (const std::string& dispatch_type)
{
  octave_value retval;

  std::string dir_name;

  std::string file_name = load_path::find_method (dispatch_type, name, dir_name);

  if (! file_name.empty ())
    {
      octave_function *fcn = load_fcn_from_file (file_name, dir_name,
						 dispatch_type);

      if (fcn)
	{
	  retval = octave_value (fcn);

	  class_methods[dispatch_type] = retval;
	}
    }

  return retval;
}

void
symbol_table::fcn_info::fcn_info_rep::print_dispatch (std::ostream& os) const
{
  if (dispatch_map.empty ())
    os << "dispatch: " << name << " is not overloaded" << std::endl;
  else
    {
      os << "Overloaded function " << name << ":\n\n";

      for (const_dispatch_map_iterator p = dispatch_map.begin ();
	   p != dispatch_map.end (); p++)
	os << "  " << name << " (" << p->first << ", ...) -> " 
	   << p->second << " (" << p->first << ", ...)\n";

      os << std::endl;
    }
}

std::string
symbol_table::fcn_info::fcn_info_rep::help_for_dispatch (void) const
{
  std::string retval;

  if (! dispatch_map.empty ())
    {
      retval = "Overloaded function:\n\n";

      for (const_dispatch_map_iterator p = dispatch_map.begin ();
	   p != dispatch_map.end (); p++)
	retval += "  " + p->second + " (" + p->first + ", ...)\n\n";
    }

  return retval;
}

// Find the definition of NAME according to the following precedence
// list:
//
//   variable
//   subfunction
//   private function
//   class constructor
//   class method
//   legacy dispatch
//   command-line function
//   autoload function
//   function on the path
//   built-in function

// Notes:
//
// FIXME -- we need to evaluate the argument list to determine the
// dispatch type.  The method used here works (pass in the args, pass
// out the evaluated args and a flag saying whether the evaluation was
// needed), but it seems a bit inelegant.  We do need to save the
// evaluated args in some way to avoid evaluating them multiple times.
//  Maybe evaluated args could be attached to the tree_argument_list
// object?  Then the argument list could be evaluated outside of this
// function and we could elimnate the arg_names, evaluated_args, and
// args_evaluated arguments.  We would still want to avoid computing
// the dispatch type unless it is needed, so the args should be passed
// rather than the dispatch type.  But the arguments will need to be
// evaluated no matter what, so evaluating them beforehand should be
// OK.  If the evaluated arguments are attached to args, then we would
// need to determine the appropriate place(s) to clear them (for
// example, before returning from tree_index_expression::rvalue).

octave_value
symbol_table::fcn_info::fcn_info_rep::find
  (tree_argument_list *args, const string_vector& arg_names,
   octave_value_list& evaluated_args, bool& args_evaluated,
   scope_id scope)
{
  static bool deja_vu = false;

  // Subfunction.  I think it only makes sense to check for
  // subfunctions if we are currently executing a function defined
  // from a .m file.

  scope_val_iterator r = subfunctions.find (scope);

  if (r != subfunctions.end ())
    {
      // FIXME -- out-of-date check here.

      return r->second;
    }
  else if (curr_parent_function)
    {
      scope_id pscope = curr_parent_function->scope ();

      r = subfunctions.find (pscope);

      if (r != subfunctions.end ())
	{
	  // FIXME -- out-of-date check here.

	  return r->second;
	}
    }

  // Private function.

  octave_function *curr_fcn = octave_call_stack::current ();

  if (curr_fcn)
    {
      std::string dir_name = curr_fcn->dir_name ();

      if (! dir_name.empty ())
	{
	  str_val_iterator q = private_functions.find (dir_name);

	  if (q == private_functions.end ())
	    {
	      octave_value val = load_private_function (dir_name);

	      if (val.is_defined ())
		return val;
	    }
	  else
	    {
	      octave_value& fval = q->second;

	      if (fval.is_defined ())
		out_of_date_check_internal (fval);

	      if (fval.is_defined ())
		return fval;
	      else
		{
		  octave_value val = load_private_function (dir_name);

		  if (val.is_defined ())
		    return val;
		}
	    }
	}
    }

  // Class constructors.  The class name and function name are the same.

  str_val_iterator q = class_constructors.find (name);

  if (q == class_constructors.end ())
    {
      octave_value val = load_class_constructor ();

      if (val.is_defined ())
	return val;
    }
  else
    {
      octave_value& fval = q->second;

      if (fval.is_defined ())
	out_of_date_check_internal (fval);

      if (fval.is_defined ())
	return fval;
      else
	{
	  octave_value val = load_class_constructor ();

	  if (val.is_defined ())
	    return val;
	}
    }

  // Class methods.

  if (args_evaluated || (args && args->length () > 0))
    {
      if (! args_evaluated)
	evaluated_args = args->convert_to_const_vector ();

      if (! error_state)
	{
	  int n = evaluated_args.length ();

	  if (n > 0 && ! args_evaluated)
	    evaluated_args.stash_name_tags (arg_names);

	  args_evaluated = true;

	  // FIXME -- need to handle precedence.

	  std::string dispatch_type = evaluated_args(0).class_name ();

	  for (int i = 1; i < n; i++)
	    {
	      octave_value arg = evaluated_args(i);

	      if (arg.is_object ())
		{
		  dispatch_type = arg.class_name ();
		  break;
		}
	    }

	  octave_value fcn = find_method (dispatch_type);

	  if (fcn.is_defined ())
	    return fcn;
	}
      else
	return octave_value ();
    }

  // Legacy dispatch.  We just check args_evaluated here because the
  // actual evaluation will have happened already when searching for
  // class methods.

  if (args_evaluated && ! dispatch_map.empty ())
    {
      std::string dispatch_type = evaluated_args(0).type_name ();

      std::string fname;

      dispatch_map_iterator p = dispatch_map.find (dispatch_type);

      if (p == dispatch_map.end ())
	p = dispatch_map.find ("any");

      if (p != dispatch_map.end ())
	{
	  fname = p->second;

	  octave_value fcn
	    = symbol_table::find_function (fname, evaluated_args, scope);

	  if (fcn.is_defined ())
	    return fcn;
	}
    }

  // Command-line function.

  if (cmdline_function.is_defined ())
    return cmdline_function;

  // Autoload?

  octave_value fcn = find_autoload ();

  if (fcn.is_defined ())
    return fcn;

  // Function on the path.

  fcn = find_user_function ();

  if (fcn.is_defined ())
    return fcn;

  // Built-in function.

  if (built_in_function.is_defined ())
    return built_in_function;

  // At this point, we failed to find anything.  It is possible that
  // the user created a file on the fly since the last prompt or
  // chdir, so try updating the load path and searching again.

  octave_value retval;

  if (! deja_vu)
    {
      load_path::update ();

      deja_vu = true;

      retval = find (args, arg_names, evaluated_args, args_evaluated, scope);
    }

  deja_vu = false;

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::find_method (const std::string& dispatch_type)
{
  octave_value retval;

  str_val_iterator q = class_methods.find (dispatch_type);

  if (q == class_methods.end ())
    {
      octave_value val = load_class_method (dispatch_type);

      if (val.is_defined ())
	return val;
    }
  else
    {
      octave_value& fval = q->second;

      if (fval.is_defined ())
	out_of_date_check_internal (fval);

      if (fval.is_defined ())
	return fval;
      else
	{
	  octave_value val = load_class_method (dispatch_type);

	  if (val.is_defined ())
	    return val;
	}
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::find_autoload (void)
{
  octave_value retval;

  // Autoloaded function.

  if (autoload_function.is_defined ())
    out_of_date_check_internal (autoload_function);

  if (! autoload_function.is_defined ())
    {
      std::string file_name = lookup_autoload (name);

      if (! file_name.empty ())
	{
	  size_t pos = file_name.find_last_of (file_ops::dir_sep_chars);

	  std::string dir_name = file_name.substr (0, pos);

	  octave_function *fcn = load_fcn_from_file (file_name, dir_name,
						     "", name, true);

	  if (fcn)
	    autoload_function = octave_value (fcn);
	}
    }

  return autoload_function;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::find_user_function (void)
{
  // Function on the path.

  if (function_on_path.is_defined ())
    out_of_date_check_internal (function_on_path);

  if (! function_on_path.is_defined ())
    {
      std::string dir_name;

      std::string file_name = load_path::find_fcn (name, dir_name);

      if (! file_name.empty ())
	{
	  octave_function *fcn = load_fcn_from_file (file_name, dir_name);

	  if (fcn)
	    function_on_path = octave_value (fcn);
	}
    }

  return function_on_path;
}

octave_value
symbol_table::fcn_info::find (tree_argument_list *args,
			      const string_vector& arg_names,
			      octave_value_list& evaluated_args,
			      bool& args_evaluated, scope_id scope)
{
  return rep->find (args, arg_names, evaluated_args, args_evaluated, scope);
}

octave_value
symbol_table::find (const std::string& name, tree_argument_list *args,
		    const string_vector& arg_names,
		    octave_value_list& evaluated_args, bool& args_evaluated,
		    symbol_table::scope_id scope, bool skip_variables)
{
  symbol_table *inst = get_instance (scope);

  return inst
    ? inst->do_find (name, args, arg_names, evaluated_args,
		       args_evaluated, scope, skip_variables)
    : octave_value ();
}

octave_value
symbol_table::find_function (const std::string& name, tree_argument_list *args,
			     const string_vector& arg_names,
			     octave_value_list& evaluated_args,
			     bool& args_evaluated, scope_id scope)
{
  return find (name, args, arg_names, evaluated_args, args_evaluated,
	       scope, true);
}

octave_value
symbol_table::do_find (const std::string& name, tree_argument_list *args,
		       const string_vector& arg_names,
		       octave_value_list& evaluated_args,
		       bool& args_evaluated, scope_id scope,
		       bool skip_variables)
{
  octave_value retval;

  // Variable.

  if (! skip_variables)
    {
      table_iterator p = table.find (name);

      if (p != table.end ())
	{
	  symbol_record& sr = p->second;

	  // FIXME -- should we be using something other than varref here?

	  if (sr.is_global ())
	    return symbol_table::varref (name, xglobal_scope);
	  else
	    {
	      octave_value& val = sr.varref ();

	      if (val.is_defined ())
		return val;
	    }
	}
    }

  fcn_table_iterator p = fcn_table.find (name);

  if (p != fcn_table.end ())
    {
      evaluated_args = octave_value_list ();
      args_evaluated = false;

      return p->second.find (args, arg_names, evaluated_args, args_evaluated,
			     scope);
    }
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find (args, arg_names, evaluated_args,
				     args_evaluated, scope);

      if (fcn.is_defined ())
	fcn_table[name] = finfo;

      return fcn;
    }

  return retval;
}

DEFUN (ignore_function_time_stamp, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} ignore_function_time_stamp ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} ignore_function_time_stamp (@var{new_val})\n\
Query or set the internal variable that controls whether Octave checks\n\
the time stamp on files each time it looks up functions defined in\n\
function files.  If the internal variable is set to @code{\"system\"},\n\
Octave will not automatically recompile function files in subdirectories of\n\
@file{@var{octave-home}/lib/@var{version}} if they have changed since\n\
they were last compiled, but will recompile other function files in the\n\
search path if they change.  If set to @code{\"all\"}, Octave will not\n\
recompile any function files unless their definitions are removed with\n\
@code{clear}.  If set to \"none\", Octave will always check time stamps\n\
on files to determine whether functions defined in function files\n\
need to recompiled.\n\
@end deftypefn")
{
  octave_value retval;

  if (nargout > 0)
    {
      switch (Vignore_function_time_stamp)
	{
	case 1:
	  retval = "system";
	  break;

	case 2:
	  retval = "all";
	  break;

	default:
	  retval = "none";
	  break;
	}
    }

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string sval = args(0).string_value ();

      if (! error_state)
	{
	  if (sval == "all")
	    Vignore_function_time_stamp = 2;
	  else if (sval == "system")
	    Vignore_function_time_stamp = 1;
	  else if (sval == "none")
	    Vignore_function_time_stamp = 0;
	  else
	    error ("ignore_function_time_stamp: expecting argument to be \"all\", \"system\", or \"none\"");
	}
      else
	error ("ignore_function_time_stamp: expecting argument to be character string");
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
