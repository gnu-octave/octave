/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_tree_fcn_h)
#define octave_tree_fcn_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <ctime>

class ostream;

#include <string>

class symbol_table;
class tree_parameter_list;
class tree_statement_list;
class tree_va_return_list;

class tree_walker;

#include "oct-obj.h"
#include "pt-fvc.h"

// User defined functions.

class
tree_function : public tree_fvc
{
public:

  tree_function (int l = -1, int c = -1) : tree_fvc (l, c)
    { init (); }

  tree_function (tree_statement_list *cl, symbol_table *st,
		 int l = -1, int c = -1)
     : tree_fvc (l, c)
       {
	 init ();
	 sym_tab = st;
	 cmd_list = cl;
	 install_nargin_and_nargout ();
       }

  ~tree_function (void);

//  tree_function *define (tree_statement_list *t);
  tree_function *define_param_list (tree_parameter_list *t);
  tree_function *define_ret_list (tree_parameter_list *t);

  void stash_fcn_file_name (void);

  void stash_fcn_file_time (time_t t)
    { t_parsed = t; }

  void stash_symtab_ptr (symbol_record *sr)
    { symtab_entry = sr; }

  string fcn_file_name (void)
    { return file_name; }

  time_t time_parsed (void)
    { return t_parsed; }

  void mark_as_system_fcn_file (void);

  bool is_system_fcn_file (void) const
    { return system_fcn_file; }

  bool takes_varargs (void) const;

  void octave_va_start (void)
    { curr_va_arg_number = num_named_args; }

  octave_value octave_va_arg (void);

  octave_value_list octave_all_va_args (void);

  bool takes_var_return (void) const;

  void octave_vr_val (const octave_value& val);

  void stash_function_name (const string& s);

  string function_name (void)
    { return fcn_name; }

  octave_value eval (bool print = false);

  octave_value_list eval (bool print, int nargout,
			  const octave_value_list& args);

  void traceback_error (void);

  tree_parameter_list *parameter_list (void) { return param_list; }

  tree_parameter_list *return_list (void) { return ret_list; }

  tree_statement_list *body (void) { return cmd_list; }

  void accept (tree_walker& tw);

private:

  // List of arguments for this function.  These are local variables.
  tree_parameter_list *param_list;

  // List of parameters we return.  These are also local variables in
  // this function.
  tree_parameter_list *ret_list;

  // The list of commands that make up the body of this function.
  tree_statement_list *cmd_list;

  // The local symbol table for this function.
  symbol_table *sym_tab;

  // Used to keep track of recursion depth.
  int call_depth;

  // The name of the file we parsed
  string file_name;

  // The name of the function.
  string fcn_name;

  // The time the file was parsed.
  time_t t_parsed;

  // True if this function came from a file that is considered to be a
  // system function.  This affects whether we check the time stamp
  // on the file to see if it has changed.
  bool system_fcn_file;

  // The number of arguments that have names.
  int num_named_args;

  // The values that were passed as arguments.
  octave_value_list args_passed;

  // The number of arguments passed in.
  int num_args_passed;

  // Used to keep track of the current offset into the list of va_args.
  int curr_va_arg_number;

  // The list of return values when an unspecified number can be
  // returned.
  tree_va_return_list *vr_list;

  // The symbol record for this function.
  symbol_record *symtab_entry;

  // The symbol record for nargin in the local symbol table.
  symbol_record *nargin_sr;

  // The symbol record for nargout in the local symbol table.
  symbol_record *nargout_sr;

  void print_code_function_header (void);
  void print_code_function_trailer (void);

  void install_nargin_and_nargout (void);

  void bind_nargin_and_nargout (int nargin, int nargout);

  void init (void)
    {
      call_depth = 0;
      param_list = 0;
      ret_list = 0;
      sym_tab = 0;
      cmd_list = 0;
      t_parsed = 0;
      system_fcn_file = 0;
      num_named_args = 0;
      num_args_passed = 0;
      curr_va_arg_number = 0;
      vr_list = 0;
      symtab_entry = 0;
    }
};

extern void symbols_of_pt_fcn (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
