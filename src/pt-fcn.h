// pt-fcn.h                                      -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

class tree_parameter_list;
class tree_statement_list;
class tree_va_return_list;

#include "oct-obj.h"
#include "symtab.h"
#include "pt-fvc.h"

// User defined functions.

class
tree_function : public tree_fvc
{
private:
  void install_nargin_and_nargout (void);

  void bind_nargin_and_nargout (int nargin, int nargout);

  void init (void)
    {
      call_depth = 0;
      param_list = 0;
      ret_list = 0;
      sym_tab = 0;
      cmd_list = 0;
      file_name = 0;
      fcn_name = 0;
      t_parsed = 0;
      system_fcn_file = 0;
      num_named_args = 0;
      num_args_passed = 0;
      curr_va_arg_number = 0;
      vr_list = 0;
    }

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

  char *fcn_file_name (void)
    { return file_name; }

  time_t time_parsed (void)
    { return t_parsed; }

  void mark_as_system_fcn_file (void);

  int is_system_fcn_file (void) const
    { return system_fcn_file; }

  int takes_varargs (void) const;

  void octave_va_start (void)
    { curr_va_arg_number = num_named_args; }

  tree_constant octave_va_arg (void);

  Octave_object octave_all_va_args (void);

  int takes_var_return (void) const;

  void octave_vr_val (const tree_constant& val);

  void stash_function_name (char *s);

  char *function_name (void)
    { return fcn_name; }

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  void traceback_error (void);

  void print_code (ostream& os);

private:
  int call_depth;
  tree_parameter_list *param_list;
  tree_parameter_list *ret_list;
  symbol_table *sym_tab;
  tree_statement_list *cmd_list;
  char *file_name;
  char *fcn_name;
  time_t t_parsed;
  int system_fcn_file;
  int num_named_args;
  Octave_object args_passed;
  int num_args_passed;
  int curr_va_arg_number;
  tree_va_return_list *vr_list;
  symbol_record *nargin_sr;
  symbol_record *nargout_sr;

  void print_code_function_header (void);
  void print_code_function_header (ostream& os);

  void print_code_function_trailer (void);
  void print_code_function_trailer (ostream& os);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
