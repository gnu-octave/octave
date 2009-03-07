/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
              2005, 2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_user_function_h)
#define octave_user_function_h 1

#include <ctime>

#include <string>
#include <stack>

#include "comment-list.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "symtab.h"

class string_vector;

class octave_value;
class tree_parameter_list;
class tree_statement_list;
class tree_va_return_list;
class tree_walker;

class 
octave_user_code : public octave_function
{
public:
  octave_user_code (void)
    : octave_function () { }

  ~octave_user_code (void) { }

  bool is_user_code (void) const { return true; }

  virtual tree_statement_list *body (void) = 0;

protected:

  octave_user_code (const std::string& nm,
		    const std::string& ds = std::string ())
    : octave_function (nm, ds) { }

private:

  // No copying!

  octave_user_code (const octave_user_code& f);

  octave_user_code& operator = (const octave_user_code& f);
};

// Scripts.

class
octave_user_script : public octave_user_code
{
public:

  octave_user_script (void);

  octave_user_script (const std::string& fnm, const std::string& nm,
		      tree_statement_list *cmds,
		      const std::string& ds = std::string ());

  octave_user_script (const std::string& fnm, const std::string& nm,
		      const std::string& ds = std::string ());

  ~octave_user_script (void);

  octave_function *function_value (bool = false) { return this; }

  const octave_function *function_value (bool = false) const { return this; }

  octave_user_script *user_script_value (bool = false) { return this; }

  octave_user_code *user_code_value (bool = false) { return this; }

  // Scripts and user functions are both considered "scripts" because
  // they are written in Octave's scripting language.

  bool is_user_script (void) const { return true; }

  void stash_fcn_file_name (const std::string& nm) { file_name = nm; }

  void mark_fcn_file_up_to_date (const octave_time& t) { t_checked = t; }

  void stash_fcn_file_time (const octave_time& t)
    {
      t_parsed = t;
      mark_fcn_file_up_to_date (t);
    }

  std::string fcn_file_name (void) const { return file_name; }

  octave_time time_parsed (void) const { return t_parsed; }

  octave_time time_checked (void) const { return t_checked; }

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx)
    {
      octave_value_list tmp = subsref (type, idx, 1);
      return tmp.length () > 0 ? tmp(0) : octave_value ();
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout);

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& args);

  tree_statement_list *body (void) { return cmd_list; }

  void accept (tree_walker& tw);

private:

  // The list of commands that make up the body of this function.
  tree_statement_list *cmd_list;

  // The name of the file we parsed.
  std::string file_name;

  // The time the file was parsed.
  octave_time t_parsed;

  // The time the file was last checked to see if it needs to be
  // parsed again.
  octave_time t_checked;

  // Used to keep track of recursion depth.
  int call_depth;

  // No copying!

  octave_user_script (const octave_user_script& f);

  octave_user_script& operator = (const octave_user_script& f);

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

// User-defined functions.

class
octave_user_function : public octave_user_code
{
public:

  octave_user_function (symbol_table::scope_id sid = -1,
			tree_parameter_list *pl = 0,
			tree_parameter_list *rl = 0,
			tree_statement_list *cl = 0);

  ~octave_user_function (void);

  octave_function *function_value (bool = false) { return this; }

  const octave_function *function_value (bool = false) const { return this; }

  octave_user_function *user_function_value (bool = false) { return this; }

  octave_user_code *user_code_value (bool = false) { return this; }

  octave_user_function *define_param_list (tree_parameter_list *t);

  octave_user_function *define_ret_list (tree_parameter_list *t);

  void stash_fcn_file_name (const std::string& nm);

  void stash_parent_fcn_name (const std::string& p) { parent_name = p; }

  void stash_parent_fcn_scope (symbol_table::scope_id ps) { parent_scope = ps; }

  void stash_leading_comment (octave_comment_list *lc) { lead_comm = lc; }

  void stash_trailing_comment (octave_comment_list *tc) { trail_comm = tc; }

  void mark_fcn_file_up_to_date (const octave_time& t) { t_checked = t; }

  void stash_fcn_file_time (const octave_time& t)
    {
      t_parsed = t;
      mark_fcn_file_up_to_date (t);
    }

  std::string fcn_file_name (void) const { return file_name; }

  std::string parent_fcn_name (void) const { return parent_name; }

  symbol_table::scope_id parent_fcn_scope (void) const { return parent_scope; }

  symbol_table::scope_id scope (void) { return local_scope; }

  octave_time time_parsed (void) const { return t_parsed; }

  octave_time time_checked (void) const { return t_checked; }

  void mark_as_system_fcn_file (void);

  bool is_system_fcn_file (void) const { return system_fcn_file; }

  bool is_user_function (void) const { return true; }

  void erase_subfunctions (void)
    {
      symbol_table::erase_subfunctions_in_scope (local_scope);
    }

  bool takes_varargs (void) const;

  bool takes_var_return (void) const;

  void lock_subfunctions (void);

  void unlock_subfunctions (void);

  octave_value_list octave_all_va_args (void);

  void stash_function_name (const std::string& s) { my_name = s; }

  void mark_as_nested_function (void) { nested_function = true; }

  bool is_nested_function (void) const { return nested_function; }

  void mark_as_inline_function (void) { inline_function = true; }

  bool is_inline_function (void) const { return inline_function; }

  void mark_as_class_constructor (void) { class_constructor = true; }

  bool is_class_constructor (void) const { return class_constructor; }

  void mark_as_class_method (void) { class_method = true; }

  bool is_class_method (void) const { return class_method; }

  void save_args_passed (const octave_value_list& args)
    {
      if (call_depth > 0)
	saved_args.push (args_passed);

      args_passed = args;
    }

  void restore_args_passed (void)
    {
      if (saved_args.empty ())
	args_passed = octave_value_list ();
      else
	{
	  args_passed = saved_args.top ();
	  saved_args.pop ();
	}
    }

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx)
    {
      octave_value_list tmp = subsref (type, idx, 1);
      return tmp.length () > 0 ? tmp(0) : octave_value ();
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout);

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& args);

  tree_parameter_list *parameter_list (void) { return param_list; }

  tree_parameter_list *return_list (void) { return ret_list; }

  tree_statement_list *body (void) { return cmd_list; }

  octave_comment_list *leading_comment (void) { return lead_comm; }

  octave_comment_list *trailing_comment (void) { return trail_comm; }

  void accept (tree_walker& tw);

#if 0
  void print_symtab_info (std::ostream& os) const;
#endif

private:

  // List of arguments for this function.  These are local variables.
  tree_parameter_list *param_list;

  // List of parameters we return.  These are also local variables in
  // this function.
  tree_parameter_list *ret_list;

  // The list of commands that make up the body of this function.
  tree_statement_list *cmd_list;

  // The comments preceding the FUNCTION token.
  octave_comment_list *lead_comm;

  // The comments preceding the ENDFUNCTION token.
  octave_comment_list *trail_comm;

  // The name of the file we parsed.
  std::string file_name;

  // The name of the parent function, if any.
  std::string parent_name;

  // The time the file was parsed.
  octave_time t_parsed;

  // The time the file was last checked to see if it needs to be
  // parsed again.
  octave_time t_checked;

  // True if this function came from a file that is considered to be a
  // system function.  This affects whether we check the time stamp
  // on the file to see if it has changed.
  bool system_fcn_file;

  // Used to keep track of recursion depth.
  int call_depth;

  // The number of arguments that have names.
  int num_named_args;

  // TRUE means this is a nested function.
  bool nested_function;

  // TRUE means this is an inline function.
  bool inline_function;

  // TRUE means this function is the constructor for class object.
  bool class_constructor;

  // TRUE means this function is a method for a class.
  bool class_method;

  // The values that were passed as arguments.
  octave_value_list args_passed;

  // A place to store the passed args for recursive calls.
  std::stack<octave_value_list> saved_args;

  // The number of arguments passed in.
  int num_args_passed;

  // The scope of the parent function, if any.
  symbol_table::scope_id parent_scope;

  symbol_table::scope_id local_scope;

#if 0
  // The symbol record for argn in the local symbol table.
  octave_value& argn_varref;

  // The symbol record for nargin in the local symbol table.
  octave_value& nargin_varref;

  // The symbol record for nargout in the local symbol table.
  octave_value& nargout_varref;

  // The symbol record for varargin in the local symbol table.
  octave_value& varargin_varref;
#endif

  void print_code_function_header (void);

  void print_code_function_trailer (void);

  void bind_automatic_vars (const string_vector& arg_names, int nargin,
			    int nargout, const octave_value_list& va_args);

  // No copying!

  octave_user_function (const octave_user_function& fn);

  octave_user_function& operator = (const octave_user_function& fn);

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
