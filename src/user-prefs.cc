// user-prefs.cc                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>

#include "user-prefs.h"
#include "error.h"
#include "variables.h"
#include "utils.h"

// The list of user preferences.  Values change when global variables
// change, so we don\'t have to do a variable look up every time we
// need to check a preference.
user_preferences user_pref;

/*
 * Check the value of a string variable to see if it it\'s ok to do
 * something.
 *
 *   return of -1 => ok, but give me warning (default).
 *   return of  0 => always ok.
 *   return of  1 => never ok.
 */
static int
check_str_pref (char *var)
{
  char *val = builtin_string_variable (var);
  int pref = -1;
  if (val != (char *) NULL)
    {
      if (strncmp (val, "yes", 3) == 0
	  || strncmp (val, "true", 4) == 0)
	pref = 1;
      else if (strncmp (val, "never", 5) == 0
	       || strncmp (val, "no", 2) == 0
	       || strncmp (val, "false", 5) == 0)
	pref = 0;
    }
  return pref;
}

/*
 * Should commas be required to separate elements in a literal matrix
 * list?
 *
 *   user specifies   value of pref
 *   --------------   -------------
 *   "required"             2
 *   "traditional"          1
 *   anything else          0
 *
 * Octave will never insert a comma in a literal matrix list if the
 * user specifies "required".  For example, the statement [1 2] will
 * result in an error instead of being treated the same as [1, 2].
 *
 * Traditional behavior makes Octave convert spaces to a comma between
 * identifiers and `('.  For example, the statement
 *
 *   [eye (2)]
 *
 * will be parsed as
 *
 *   [eye, (2)]
 *
 * and will result in an error since the eye function will be
 * called with no arguments.  To get around this, you would have to
 * omit the space between `eye' and the `('.
 *
 * The default value is 0, which results in behavior that is the same
 * as traditional, except that Octave does not convert spaces to a
 * comma between identifiers and `('.  For example, the statement
 *
 *   [eye (2)]
 *
 * will result in a call to linspace with the argument `2'. 
 */
int
commas_in_literal_matrix (void)
{
  int pref = 0;
  char *val = builtin_string_variable ("commas_in_literal_matrix");
  if (val != (char *) NULL)
    {
      if (strncmp (val, "required", 8) == 0)
	pref = 2;
      else if (strncmp (val, "traditional", 11) == 0)
	pref = 1;
    }
  user_pref.commas_in_literal_matrix = pref;
  return 0;
}

/*
 * Should we allow assignments like:
 *
 *   octave> A(1) = 3; A(2) = 5
 *
 * for A already defined and a matrix type?
 */
int
do_fortran_indexing (void)
{
  user_pref.do_fortran_indexing =
    check_str_pref ("do_fortran_indexing"); 

  return 0;
}

/*
 * Should ignore empty elements in a matrix list (i.e., is an
 *  expression like `[[], 1]' ok?
 */
int
empty_list_elements_ok (void)
{
  user_pref.empty_list_elements_ok =
    check_str_pref ("empty_list_elements_ok");

  return 0;
}

/*
 * Should Octave always check to see if function files have changed
 * since they were last compiled?
 */
int
ignore_function_time_stamp (void)
{
  int pref = 0;

  char *val = builtin_string_variable ("ignore_function_time_stamp");

  if (val != (char *) NULL)
    {
      if (strncmp (val, "all", 3) == 0)
	pref = 2;
      if (strncmp (val, "system", 6) == 0)
	pref = 1;
    }

  user_pref.ignore_function_time_stamp = pref;

  return 0;
}

/*
 * Should we allow things like:
 *
 *   octave> 'abc' + 0
 *   97 98 99
 *
 * to happen?
 */
int
implicit_str_to_num_ok (void)
{
  user_pref.implicit_str_to_num_ok =
    check_str_pref ("implicit_str_to_num_ok");

  return 0;
}

/*
 * Should we allow silent conversion of complex to real when a real
 * type is what we\'re really looking for?
 */
int
ok_to_lose_imaginary_part (void)
{
  user_pref.ok_to_lose_imaginary_part =
    check_str_pref ("ok_to_lose_imaginary_part");

  return 0;
}

/*
 * If possible, send all output intended for the screen through the
 * pager. 
 */
int
page_screen_output (void)
{
  user_pref.page_screen_output = check_str_pref ("page_screen_output");

  return 0;
}

/*
 * When doing assignments like:
 *
 *   octave> A(1) = 3; A(2) = 5
 *
 * (for A undefined) should we build column vectors?  Returning true
 * only matters when resize_on_range_error is also true.
 */
int
prefer_column_vectors (void)
{
  user_pref.prefer_column_vectors =
    check_str_pref ("prefer_column_vectors");

  return 0;
}

/*
 * For things like
 *
 *   a = [2,3]; a([1,1])
 *
 * return [2 3] instead of [2 2].
 */
int
prefer_zero_one_indexing (void)
{
  user_pref.prefer_zero_one_indexing =
    check_str_pref ("prefer_zero_one_indexing");

  return 0;
}

/*
 * Should we print things like
 *
 *   octave> a = [1,2;3,4]
 *   a = 
 *
 *      1  2
 *      3  4
 */
int
print_answer_id_name (void)
{
  user_pref.print_answer_id_name =
    check_str_pref ("print_answer_id_name");

  return 0;
}

/*
 * Should we also print the dimensions of empty matrices?
 */
int
print_empty_dimensions (void)
{
  user_pref.print_empty_dimensions =
    check_str_pref ("print_empty_dimensions");

  return 0;
}

/*
 * Should operations on empty matrices return empty matrices or an
 * error?
 */
int
propagate_empty_matrices (void)
{
  user_pref.propagate_empty_matrices =
    check_str_pref ("propagate_empty_matrices");

  return 0;
}

/*
 * When doing assignments, should we resize matrices if the indices
 * are outside the current bounds?
 */
int
resize_on_range_error (void)
{
  user_pref.resize_on_range_error =
    check_str_pref ("resize_on_range_error");

  return 0;
}

/*
 * If a function does not return any values explicitly, return the
 * last computed value.
 */
int
return_last_computed_value (void)
{
  user_pref.return_last_computed_value =
    check_str_pref ("return_last_computed_value");

  return 0;
}

/*
 * Suppress printing results in called functions.
 */
int
silent_functions (void)
{
  user_pref.silent_functions =
    check_str_pref ("silent_functions");

  return 0;
}

/*
 * Should should big matrices be split into smaller slices for output?
 */
int
split_long_rows (void)
{
  user_pref.split_long_rows = check_str_pref ("split_long_rows");

  return 0;
}

/*
 * Should things like:
 *
 *   octave> ones (-1, 5)
 *
 * result in an empty matrix or an error?
 */
int
treat_neg_dim_as_zero (void)
{
  user_pref.treat_neg_dim_as_zero =
    check_str_pref ("treat_neg_dim_as_zero");

  return 0;
}

/*
 * Generate a warning for the assignment in things like
 *
 *   octave> if (a = 2 < n)
 *
 * but not
 *
 *   octave> if ((a = 2) < n)
 */
int
warn_assign_as_truth_value (void)
{
  user_pref.warn_assign_as_truth_value =
    check_str_pref ("warn_assign_as_truth_value");

  return 0;
}

/*
 * Generate a warning for the comma in things like
 *
 *   octave> global a, b = 2
 */
int
warn_comma_in_global_decl (void)
{
  user_pref.warn_comma_in_global_decl =
    check_str_pref ("warn_comma_in_global_decl");

  return 0;
}

/*
 * On IEEE machines, allow divide by zero errors to be suppressed.
 */
int
warn_divide_by_zero (void)
{
  user_pref.warn_divide_by_zero = check_str_pref ("warn_divide_by_zero");

  return 0;
}

int
set_output_max_field_width (void)
{
  int status = 0;

  static int kludge = 0;

  double val;
  if (builtin_real_scalar_variable ("output_max_field_width", val) == 0)
    {
      int ival = NINT (val);
      if (ival > 0 && (double) ival == val)
	{
	  user_pref.output_max_field_width= ival;
	  return status;
	}
    }

  if (kludge == 0)
    kludge++;
  else
    {
      warning ("invalid value specified for output_max_field_width");
      status = -1;
    }

  return status;
}

int
set_output_precision (void)
{
  int status = 0;

  static int kludge = 0;

  double val;
  if (builtin_real_scalar_variable ("output_precision", val) == 0)
    {
      int ival = NINT (val);
      if (ival >= 0 && (double) ival == val)
	{
	  user_pref.output_precision = ival;
	  return status;
	}
    }

  if (kludge == 0)
    kludge++;
  else
    {
      warning ("invalid value specified for output_precision");
      status = -1;
    }

  return status;
}

int
set_save_precision (void)
{
  int status = 0;

  static int kludge = 0;

  double val;
  if (builtin_real_scalar_variable ("save_precision", val) == 0)
    {
      int ival = NINT (val);
      if (ival >= 0 && (double) ival == val)
	{
	  user_pref.save_precision = ival;
	  return status;
	}
    }

  if (kludge == 0)
    kludge++;
  else
    {
      warning ("invalid value specified for save_precision");
      status = -1;
    }

  return status;
}

int
sv_editor (void)
{
  int status = 0;

  char *s = builtin_string_variable ("EDITOR");
  if (s != (char *) NULL)
    {
      delete [] user_pref.editor;
      user_pref.editor = s;
    }
  else
    {
      warning ("invalid value specified for EDITOR");
      status = -1;
    }

  return status;
}

int
sv_gnuplot_binary (void)
{
  int status = 0;

  char *s = builtin_string_variable ("gnuplot_binary");
  if (s != (char *) NULL)
    {
      delete [] user_pref.gnuplot_binary;
      user_pref.gnuplot_binary = s;
    }
  else
    {
      warning ("invalid value specified for gnuplot_binary");
      status = -1;
    }

  return status;
}

int
sv_info_file (void)
{
  int status = 0;

  char *s = builtin_string_variable ("INFO_FILE");
  if (s != (char *) NULL)
    {
      delete [] user_pref.info_file;
      user_pref.info_file = s;
    }
  else
    {
      warning ("invalid value specified for INFO_FILE");
      status = -1;
    }

  return status;
}

int
sv_loadpath (void)
{
  int status = 0;

  char *s = builtin_string_variable ("LOADPATH");
  if (s != (char *) NULL)
    {
      delete [] user_pref.loadpath;
      user_pref.loadpath = s;
    }
  else
    {
      warning ("invalid value specified for LOADPATH");
      status = -1;
    }

  return status;
}

int
sv_pager_binary (void)
{
  int status = 0;

  char *s = builtin_string_variable ("PAGER");
  if (s != (char *) NULL)
    {
      delete [] user_pref.pager_binary;
      user_pref.pager_binary = s;
    }
  else
    {
      warning ("invalid value specified for PAGER");
      status = -1;
    }

  return status;
}

int
sv_ps1 (void)
{
  int status = 0;

  char *s = builtin_string_variable ("PS1");
  if (s != (char *) NULL)
    {
      delete [] user_pref.ps1;
      user_pref.ps1 = s;
    }
  else
    {
      warning ("invalid value specified for PS1");
      status = -1;
    }

  return status;
}

int
sv_ps2 (void)
{
  int status = 0;

  char *s = builtin_string_variable ("PS2");
  if (s != (char *) NULL)
    {
      delete [] user_pref.ps2;
      user_pref.ps2 = s;
    }
  else
    {
      warning ("invalid value specified for PS2");
      status = -1;
    }

  return status;
}

int
sv_pwd (void)
{
  int status = 0;

  char *s = builtin_string_variable ("PWD");
  if (s != (char *) NULL)
    {
      delete [] user_pref.pwd;
      user_pref.pwd = s;
    }
  else
    {
      warning ("invalid value specified for PWD");
      status = -1;
    }

  return status;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
