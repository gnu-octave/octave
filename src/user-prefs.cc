/*

Copyright (C) 1996 John W. Eaton

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
#include "oct-hist.h"
#include "sysdep.h"
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
  user_pref.history_size = 0;
  user_pref.ignore_function_time_stamp = 0;
  user_pref.print_answer_id_name = 0;
  user_pref.read_only_constants = 1;
  user_pref.save_precision = 0;
  user_pref.saving_history = 0;
  user_pref.suppress_verbose_help_message = 0;
  user_pref.treat_neg_dim_as_zero = 0;
  user_pref.warn_divide_by_zero = 0;

  user_pref.default_save_format = string ();
  user_pref.editor = string ();
  user_pref.exec_path = string ();
  user_pref.history_file = string ();
  user_pref.imagepath = string ();
  user_pref.info_file = string ();
  user_pref.info_prog = string ();
  user_pref.loadpath = string ();
  user_pref.pwd = string ();
}

// Check the value of a string variable to see if it it's ok to do
// something.
//
//   return of  1 => always ok.
//   return of  0 => never ok.
//   return of -1 => ok, but give me warning (default).

int
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
  user_pref.echo_executing_commands
    = check_preference ("echo_executing_commands"); 

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
	  octave_command_history.set_size (ival);
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
  user_pref.print_answer_id_name = check_preference ("print_answer_id_name");

  return 0;
}


// Should built-in constants always be read only?

int
read_only_constants (void)
{
  user_pref.read_only_constants = check_preference ("read_only_constants");

  return 0;
}


// Should we save command history?

int
saving_history (void)
{
  user_pref.saving_history = check_preference ("saving_history");
  octave_command_history.ignore_entries (! user_pref.saving_history);
  return 0;
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
  user_pref.treat_neg_dim_as_zero
    = check_preference ("treat_neg_dim_as_zero");

  return 0;
}


// On IEEE machines, allow divide by zero errors to be suppressed.

int
warn_divide_by_zero (void)
{
  user_pref.warn_divide_by_zero = check_preference ("warn_divide_by_zero");

  return 0;
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
    {
      user_pref.history_file = s;
      octave_command_history.set_file (oct_tilde_expand (s));
    }

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
;;; End: ***
*/
