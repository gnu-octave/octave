// user-prefs.cc                                              -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "error.h"
#include "gripes.h"
#include "mappers.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"

// The list of user preferences.  Values change when global variables
// change, so we don't have to do a variable look up every time we
// need to check a preference.
user_preferences user_pref;

// Initialize global user_pref structure.

void
init_user_prefs (void)
{
  user_pref.automatic_replot = 0;
  user_pref.beep_on_error = 0;
  user_pref.define_all_return_values = 0;
  user_pref.do_fortran_indexing = 0;
  user_pref.empty_list_elements_ok = 0;
  user_pref.gnuplot_has_multiplot = 0;
  user_pref.history_size = 0;
  user_pref.ignore_function_time_stamp = 0;
  user_pref.implicit_str_to_num_ok = 0;
  user_pref.ok_to_lose_imaginary_part = 0;
  user_pref.output_max_field_width = 0;
  user_pref.output_precision = 0;
  user_pref.page_screen_output = 0;
  user_pref.prefer_column_vectors = 0;
  user_pref.prefer_zero_one_indexing = 0;
  user_pref.print_answer_id_name = 0;
  user_pref.print_empty_dimensions = 0;
  user_pref.propagate_empty_matrices = 0;
  user_pref.read_only_constants = 1;
  user_pref.resize_on_range_error = 0;
  user_pref.return_last_computed_value = 0;
  user_pref.save_precision = 0;
  user_pref.saving_history = 0;
  user_pref.silent_functions = 0;
  user_pref.split_long_rows = 0;
  user_pref.struct_levels_to_print = 0;
  user_pref.suppress_verbose_help_message = 0;
  user_pref.treat_neg_dim_as_zero = 0;
  user_pref.warn_assign_as_truth_value = 0;
  user_pref.warn_comma_in_global_decl = 0;
  user_pref.warn_divide_by_zero = 0;
  user_pref.warn_function_name_clash = 0;
  user_pref.whitespace_in_literal_matrix = 0;

  user_pref.completion_append_char = '\0';

  user_pref.default_save_format = string ();
  user_pref.editor = string ();
  user_pref.exec_path = string ();
  user_pref.gnuplot_binary = string ();
  user_pref.history_file = string ();
  user_pref.imagepath = string ();
  user_pref.info_file = string ();
  user_pref.info_prog = string ();
  user_pref.loadpath = string ();
  user_pref.pager_binary = string ();
  user_pref.ps1 = string ();
  user_pref.ps2 = string ();
  user_pref.ps4 = string ();
  user_pref.pwd = string ();
}

// Check the value of a string variable to see if it it's ok to do
// something.
//
//   return of  1 => always ok.
//   return of  0 => never ok.
//   return of -1 => ok, but give me warning (default).

// XXX FIXME XXX -- should also allow zero to mean "false" and nonzero
// to mean "true".

static int
check_preference (const string& var)
{
  int pref = -1;

  string val = builtin_string_variable (var);

  if (val.empty ())
    {
      double dval = 0;
      if (builtin_real_scalar_variable (var, dval))
	pref = NINT (dval);
    }
  else
    {
      if (val.compare ("yes", 0, 3) == 0
	  || val.compare ("true", 0, 4) == 0)
	pref = 1;
      else if (val.compare ("never", 0, 5) == 0
	       || val.compare ("no", 0, 2) == 0
	       || val.compare ("false", 0, 5) == 0)
	pref = 0;
    }

  return pref;
}

// XXX FIXME XXX -- some of these should do their own checking to be
// able to provide more meaningful warning or error messages.

// Should a replot command be generated automatically each time a plot
// changes in some way?

int
automatic_replot (void)
{
  user_pref.automatic_replot = check_preference ("automatic_replot");

  return 0;
}


// Should we beep obnoxiously before printing error messages?

int
beep_on_error (void)
{
  user_pref.beep_on_error = check_preference ("beep_on_error");

  return 0;
}


// Should variables returned from functions have default values if
// they are otherwise uninitialized?

int
define_all_return_values (void)
{
  user_pref.define_all_return_values =
    check_preference ("define_all_return_values");

  return 0;
}


// Should we allow assignments like:
//
//   octave> A(1) = 3; A(2) = 5
//
// for A already defined and a matrix type?

int
do_fortran_indexing (void)
{
  user_pref.do_fortran_indexing =
    check_preference ("do_fortran_indexing"); 

  return 0;
}


// Echo commands as they are executed?
//
//   1  ==>  echo commands read from script files
//   2  ==>  echo commands from functions
//   4  ==>  echo commands read from command line
//
// more than one state can be active at once.

int
echo_executing_commands (void)
{
  user_pref.echo_executing_commands =
    check_preference ("echo_executing_commands"); 

  return 0;
}


// Should ignore empty elements in a matrix list (i.e., is an
//  expression like `[[], 1]' ok?

int
empty_list_elements_ok (void)
{
  user_pref.empty_list_elements_ok =
    check_preference ("empty_list_elements_ok");

  return 0;
}


// Does gnuplot appear to support multiplot?

int
gnuplot_has_multiplot (void)
{
  user_pref.gnuplot_has_multiplot =
    check_preference ("gnuplot_has_multiplot");

  return 0;
}


// How many lines of command history should we save?

int
history_size (void)
{
  double val;
  if (builtin_real_scalar_variable ("history_size", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && (double) ival == val)
	{
	  user_pref.history_size = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("history_size");
  return -1;
}


// Should Octave always check to see if function files have changed
// since they were last compiled?

int
ignore_function_time_stamp (void)
{
  int pref = 0;

  string val = builtin_string_variable ("ignore_function_time_stamp");

  if (! val.empty ())
    {
      if (val.compare ("all", 0, 3) == 0)
	pref = 2;
      if (val.compare ("system", 0, 6) == 0)
	pref = 1;
    }

  user_pref.ignore_function_time_stamp = pref;

  return 0;
}


// Should we allow things like:
//
//   octave> 'abc' + 0
//   97 98 99
//
// to happen?

int
implicit_str_to_num_ok (void)
{
  user_pref.implicit_str_to_num_ok =
    check_preference ("implicit_str_to_num_ok");

  return 0;
}


// Should we allow silent conversion of complex to real when a real
// type is what we're really looking for?

int
ok_to_lose_imaginary_part (void)
{
  user_pref.ok_to_lose_imaginary_part =
    check_preference ("ok_to_lose_imaginary_part");

  return 0;
}


// If possible, send all output intended for the screen through the
// pager. 

int
page_screen_output (void)
{
  user_pref.page_screen_output = check_preference ("page_screen_output");

  return 0;
}


// When doing assignments like:
//
//   octave> A(1) = 3; A(2) = 5
//
// (for A undefined) should we build column vectors?  Returning true
// only matters when resize_on_range_error is also true.

int
prefer_column_vectors (void)
{
  user_pref.prefer_column_vectors =
    check_preference ("prefer_column_vectors");

  return 0;
}


// For things like
//
//   a = [2,3]; a([1,1])
//
// return [2 3] instead of [2 2].

int
prefer_zero_one_indexing (void)
{
  user_pref.prefer_zero_one_indexing =
    check_preference ("prefer_zero_one_indexing");

  return 0;
}


// Should we print things like
//
//   octave> a = [1,2;3,4]
//   a = 
//
//      1  2
//      3  4

int
print_answer_id_name (void)
{
  user_pref.print_answer_id_name =
    check_preference ("print_answer_id_name");

  return 0;
}


// Should we also print the dimensions of empty matrices?

int
print_empty_dimensions (void)
{
  user_pref.print_empty_dimensions =
    check_preference ("print_empty_dimensions");

  return 0;
}


// Should operations on empty matrices return empty matrices or an
// error?

int
propagate_empty_matrices (void)
{
  user_pref.propagate_empty_matrices =
    check_preference ("propagate_empty_matrices");

  return 0;
}

// Should built-in constants always be read only?

int
read_only_constants (void)
{
  user_pref.read_only_constants = check_preference ("read_only_constants");

  return 0;
}

// When doing assignments, should we resize matrices if the indices
// are outside the current bounds?

int
resize_on_range_error (void)
{
  user_pref.resize_on_range_error =
    check_preference ("resize_on_range_error");

  return 0;
}


// If a function does not return any values explicitly, return the
// last computed value.

int
return_last_computed_value (void)
{
  user_pref.return_last_computed_value =
    check_preference ("return_last_computed_value");

  return 0;
}


// Should we save command history?

int
saving_history (void)
{
  user_pref.saving_history = check_preference ("saving_history");

  return 0;
}


// Suppress printing results in called functions.

int
silent_functions (void)
{
  user_pref.silent_functions =
    check_preference ("silent_functions");

  return 0;
}


// Should should big matrices be split into smaller slices for output?

int
split_long_rows (void)
{
  user_pref.split_long_rows = check_preference ("split_long_rows");

  return 0;
}


// How many levels of structure elements should we print?

int
struct_levels_to_print (void)
{
  double val;
  if (builtin_real_scalar_variable ("struct_levels_to_print", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && (double) ival == val)
	{
	  user_pref.struct_levels_to_print = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("struct_levels_to_print");
  return -1;
}


// Suppress printing of additional help message in help and usage
// functions?

int
suppress_verbose_help_message (void)
{
  user_pref.suppress_verbose_help_message =
    check_preference ("suppress_verbose_help_message");

  return 0;
}


// Should things like:
//
//   octave> ones (-1, 5)
//
// result in an empty matrix or an error?

int
treat_neg_dim_as_zero (void)
{
  user_pref.treat_neg_dim_as_zero =
    check_preference ("treat_neg_dim_as_zero");

  return 0;
}


// Generate a warning for the assignment in things like
//
//   octave> if (a = 2 < n)
//
// but not
//
//   octave> if ((a = 2) < n)

int
warn_assign_as_truth_value (void)
{
  user_pref.warn_assign_as_truth_value =
    check_preference ("warn_assign_as_truth_value");

  return 0;
}


// Generate a warning for the comma in things like
//
//   octave> global a, b = 2

int
warn_comma_in_global_decl (void)
{
  user_pref.warn_comma_in_global_decl =
    check_preference ("warn_comma_in_global_decl");

  return 0;
}


// On IEEE machines, allow divide by zero errors to be suppressed.

int
warn_divide_by_zero (void)
{
  user_pref.warn_divide_by_zero = check_preference ("warn_divide_by_zero");

  return 0;
}

// Generate warning if declared function name disagrees with the name
// of the file in which it is defined.

int
warn_function_name_clash (void)
{
  user_pref.warn_function_name_clash =
    check_preference ("warn_function_name_clash");

  return 0;
}


// Generate warning if a statement in a function is not terminated
// with a semicolon.  Useful for checking functions that should only
// produce output using explicit printing statements.

int
warn_missing_semicolon (void)
{
  user_pref.warn_missing_semicolon = 
    check_preference ("warn_missing_semicolon");

  return 0;
}


// Should whitespace in a literal matrix list be automatically
// converted to commas and semicolons?
//
//   user specifies   value of pref
//   --------------   -------------
//   "ignore"               2
//   "traditional"          1
//   anything else          0
//
// Octave will never insert a comma in a literal matrix list if the
// user specifies "ignore".  For example, the statement [1 2] will
// result in an error instead of being treated the same as [1, 2], and
// the statement
//
//   [ 1, 2,
//     3, 4 ]
//
// will result in the vector [1 2 3 4] instead of a matrix.
//
// Traditional behavior makes Octave convert spaces to a comma between
// identifiers and `('.  For example, the statement
//
//   [eye (2)]
//
// will be parsed as
//
//   [eye, (2)]
//
// and will result in an error since the `eye' function will be
// called with no arguments.  To get around this, you would have to
// omit the space between `eye' and the `('.
//
// The default value is 0, which results in behavior that is the same
// as traditional, except that Octave does not convert spaces to a
// comma between identifiers and `('.  For example, the statement
//
//   [eye (2)]
//
// will result in a call to `eye' with the argument `2'. 

int
whitespace_in_literal_matrix (void)
{
  int pref = 0;
  string val = builtin_string_variable ("whitespace_in_literal_matrix");
  if (! val.empty ())
    {
      if (val.compare ("ignore", 0, 6) == 0)
	pref = 2;
      else if (val.compare ("traditional", 0, 11) == 0)
	pref = 1;
    }
  user_pref.whitespace_in_literal_matrix = pref;
  return 0;
}


int
set_output_max_field_width (void)
{
  double val;
  if (builtin_real_scalar_variable ("output_max_field_width", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival > 0 && (double) ival == val)
	{
	  user_pref.output_max_field_width = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("output_max_field_width");
  return -1;
}

int
set_output_precision (void)
{
  double val;
  if (builtin_real_scalar_variable ("output_precision", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && (double) ival == val)
	{
	  user_pref.output_precision = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("output_precision");
  return -1;
}

int
set_save_precision (void)
{
  double val;
  if (builtin_real_scalar_variable ("save_precision", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && (double) ival == val)
	{
	  user_pref.save_precision = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("save_precision");
  return -1;
}

int
sv_completion_append_char (void)
{
  int status = 0;

  string s = builtin_string_variable ("completion_append_char");

  switch (s.length ())
    {
    case 1:
      user_pref.completion_append_char = s[0];
      break;

    case 0:
      user_pref.completion_append_char = '\0';
      break;

    default:
      warning ("completion_append_char must be a single character");
      status = -1;
      break;
    }

  return status;
}

int
sv_default_save_format (void)
{
  int status = 0;

  string s = builtin_string_variable ("default_save_format");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("default_save_format");
      status = -1;
    }
  else
    user_pref.default_save_format = s;

  return status;
}

int
sv_editor (void)
{
  int status = 0;

  string s = builtin_string_variable ("EDITOR");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("EDITOR");
      status = -1;
    }
  else
    user_pref.editor = s;

  return status;
}

int
sv_exec_path (void)
{
  int status = 0;

  string exec_path = builtin_string_variable ("EXEC_PATH");

  if (exec_path.empty ())
    {
      gripe_invalid_value_specified ("EXEC_PATH");
      status = -1;
    }
  else
    {
      string arch_dir = octave_arch_lib_dir ();
      string bin_dir = octave_bin_dir ();

      int len = arch_dir.length () + bin_dir.length () + strlen (SEPCHAR_STR);

      static char *putenv_cmd = 0;

      delete [] putenv_cmd;

      putenv_cmd = 0;

      int eplen = exec_path.length ();

      if (eplen > 0)
	{
	  int prepend = (exec_path[0] == ':');
	  int append = (eplen > 1 && exec_path[eplen-1] == ':');

	  if (prepend)
	    {
	      if (append)
		{
		  putenv_cmd = new char [2 * len + eplen + 6];
		  sprintf (putenv_cmd,
			   "PATH=%s" SEPCHAR_STR "%s%s%s" SEPCHAR_STR "%s",
			   arch_dir.c_str (), bin_dir.c_str (),
			   exec_path.c_str (), arch_dir.c_str (),
			   bin_dir.c_str ());
		}
	      else
		{
		  putenv_cmd = new char [len + eplen + 6];
		  sprintf (putenv_cmd,
			   "PATH=%s" SEPCHAR_STR "%s%s",
			   arch_dir.c_str (), bin_dir.c_str (),
			   exec_path.c_str ());
		}
	    }
	  else
	    {
	      if (append)
		{
		  putenv_cmd = new char [len + eplen + 6];
		  sprintf (putenv_cmd,
			   "PATH=%s%s" SEPCHAR_STR "%s",
			   exec_path.c_str (), arch_dir.c_str (),
			   bin_dir.c_str ());
		}
	      else
		{
		  putenv_cmd = new char [len + eplen + 6];
		  sprintf (putenv_cmd, "PATH=%s", exec_path.c_str ());
		}
	    }
	}
      else
	{
	  putenv_cmd = new char [len+6];
	  sprintf (putenv_cmd, "PATH=%s" SEPCHAR_STR "%s",
		   arch_dir.c_str (), bin_dir.c_str ());
	}

      putenv (putenv_cmd);
    }

  return status;
}

int
sv_gnuplot_binary (void)
{
  int status = 0;

  string s = builtin_string_variable ("gnuplot_binary");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("gnuplot_binary");
      status = -1;
    }
  else
    user_pref.gnuplot_binary = s;

  return status;
}

int
sv_history_file (void)
{
  int status = 0;

  string s = builtin_string_variable ("history_file");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("history_file");
      status = -1;
    }
  else
    user_pref.history_file = s;

  return status;
}

int
sv_imagepath (void)
{
  int status = 0;

  string s = builtin_string_variable ("IMAGEPATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("IMAGEPATH");
      status = -1;
    }
  else
    user_pref.imagepath = s;

  return status;
}

int
sv_info_file (void)
{
  int status = 0;

  string s = builtin_string_variable ("INFO_FILE");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("INFO_FILE");
      status = -1;
    }
  else
    user_pref.info_file = s;

  return status;
}

int
sv_info_prog (void)
{
  int status = 0;

  string s = builtin_string_variable ("INFO_PROGRAM");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("INFO_PROGRAM");
      status = -1;
    }
  else
    user_pref.info_prog = s;

  return status;
}

int
sv_loadpath (void)
{
  int status = 0;

  string s = builtin_string_variable ("LOADPATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("LOADPATH");
      status = -1;
    }
  else
    user_pref.loadpath = maybe_add_default_load_path (s);

  return status;
}

int
sv_pager_binary (void)
{
  int status = 0;

  string s = builtin_string_variable ("PAGER");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("PAGER");
      status = -1;
    }
  else
    user_pref.pager_binary = s;

  return status;
}

int
sv_ps1 (void)
{
  int status = 0;

  string s = builtin_string_variable ("PS1");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("PS1");
      status = -1;
    }
  else
    user_pref.ps1 = s;

  return status;
}

int
sv_ps2 (void)
{
  int status = 0;

  string s = builtin_string_variable ("PS2");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("PS2");
      status = -1;
    }
  else
    user_pref.ps2 = s;

  return status;
}

int
sv_ps4 (void)
{
  int status = 0;

  string s = builtin_string_variable ("PS4");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("PS4");
      status = -1;
    }
  else
    user_pref.ps4 = s;

  return status;
}

int
sv_pwd (void)
{
  int status = 0;

  string s = builtin_string_variable ("PWD");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("PWD");
      status = -1;
    }
  else
    user_pref.pwd = s;

  return status;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
