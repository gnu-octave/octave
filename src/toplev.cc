// toplev.cc                                            -*- C++ -*-
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

#include <cassert>
#include <csetjmp>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <string>

#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include "lo-error.h"
#include "str-vec.h"

#include "builtins.h"
#include "defaults.h"
#include "defun.h"
#include "dynamic-ld.h"
#include "error.h"
#include "file-io.h"
#include "help.h"
#include "input.h"
#include "lex.h"
#include "oct-hist.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "pt-const.h"
#include "pt-misc.h"
#include "pt-plot.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

// argv[0] for this program.
string raw_prog_name;

// Cleaned-up name of this program, not including path information.
string prog_name;

// Login name for user running this program.
string user_name;

// Name of the host we are running on.
string host_name;

// User's home directory.
string home_directory;

// Guess what?
string the_current_working_directory;

// The path that will be searched for programs that we execute.
// (--exec-path path)
string exec_path;

// Load path specified on command line.
// (--path path; -p path)
string load_path;

// Name of the info file specified on command line.
// (--info-file file)
string info_file;

// Name of the info reader we'd like to use.
// (--info-program program)
string info_prog;

// Name of the editor to be invoked by the edit_history command.
string editor;

// If nonzero, don't do fancy line editing.
int no_line_editing = 0;

// If nonzero, print verbose info in some cases.
// (--verbose; -V)
int verbose_flag = 0;

// Nonzero means we printed messages about reading startup files.
int reading_startup_message_printed = 0;

// Command number, counting from the beginning of this session.
int current_command_number = 1;

// Nonzero means we are exiting via the builtin exit or quit functions.
int quitting_gracefully = 0;

// Current command to execute.
tree_statement_list *global_command = 0;

// Pointer to function that is currently being evaluated.
tree_function *curr_function = 0;

// Nonzero means input is coming from startup file.
int input_from_startup_file = 0;

// The command-line options.
charMatrix octave_argv;

// Nonzero means that input is coming from a file that was named on
// the command line.
int input_from_command_line_file = 1;

void
parse_and_execute (FILE *f, int print)
{
  begin_unwind_frame ("parse_and_execute");
  
  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer (f);

  add_unwind_protect (restore_input_buffer, (void *) old_buf);
  add_unwind_protect (delete_input_buffer, (void *) new_buf);

  switch_to_buffer (new_buf);

  unwind_protect_int (using_readline);
  unwind_protect_int (input_from_command_line_file);

  using_readline = 0;
  input_from_command_line_file = 0;

  unwind_protect_ptr (curr_sym_tab);

  int retval;
  do
    {
      reset_parser ();

      retval = yyparse ();

      if (retval == 0 && global_command)
	{
	  global_command->eval (print);
	  delete global_command;
	}
    }
  while (retval == 0);

  run_unwind_frame ("parse_and_execute");
}

void
parse_and_execute (const string& s, int print, int verbose,
		   const char *warn_for)
{
  begin_unwind_frame ("parse_and_execute_2");

  unwind_protect_int (reading_script_file);
  unwind_protect_str (curr_fcn_file_full_name);

  reading_script_file = 1;
  curr_fcn_file_full_name = s;

  FILE *f = get_input_from_file (s, 0);

  if (f)
    {
      unwind_protect_int (input_line_number);
      unwind_protect_int (current_input_column);

      input_line_number = 0;
      current_input_column = 1;

      if (verbose)
	{
	  cout << "reading commands from " << s << " ... ";
	  reading_startup_message_printed = 1;
	  cout.flush ();
	}

      parse_and_execute (f, print);

      fclose (f);

      if (verbose)
	cout << "done." << endl;
    }
  else if (warn_for)
    error ("%s: unable to open file `%s'", warn_for, s.c_str ());

  run_unwind_frame ("parse_and_execute_2");
}

DEFUN ("source", Fsource, Ssource, 10,
  "source (FILE)\n\
\n\
Parse and execute the contents of FILE.  Like executing commands in a\n\
script file but without requiring the file to be named `FILE.m'.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      string file = args(0).string_value ();

      if (! error_state)
	{
	  file = oct_tilde_expand (file);

	  parse_and_execute (file, 1, 0, "source");

	  if (error_state)
	    error ("source: error sourcing file `%s'", file.c_str ());
	}
      else
	error ("source: expecting file name as argument");
    }
  else
    print_usage ("source");

  return retval;
}

// Fix up things before exiting.

void
clean_up_and_exit (int retval)
{
  raw_mode (0);

  clean_up_history ();

  close_plot_stream ();

  close_diary_file ();

  close_files ();

  cleanup_tmp_files ();

  if (!quitting_gracefully && (interactive || forced_interactive))
    cout << "\n";

  if (retval == EOF)
    retval = 0;

  exit (retval);

  // This is bogus but should prevent g++ from giving a warning saying
  // that this volatile function does return.

  panic_impossible ();
}

DEFUN_TEXT ("casesen", Fcasesen, Scasesen, 10,
  "casesen [on|off]")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = make_argv (args, "casesen");

  if (error_state)
    return retval;

  if (argc == 1 || (argc > 1 && argv[1] == "off"))
    warning ("casesen: sorry, Octave is always case sensitive");
  else if (argc > 1 && argv[1] == "on")
    ; // ok.
  else
    print_usage ("casesen");

  return retval;
}

DEFUN ("computer", Fcomputer, Scomputer, 11,
  "computer ():\n\
\n\
Have Octave ask the system, \"What kind of computer are you?\"")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 0)
    warning ("computer: ignoring extra arguments");

  ostrstream output_buf;

  if (strcmp (TARGET_HOST_TYPE, "unknown") == 0)
    output_buf << "Hi Dave, I'm a HAL-9000";
  else
    output_buf << TARGET_HOST_TYPE;

  if (nargout == 0)
    {
      output_buf << "\n" << ends;
      maybe_page_output (output_buf);
    }
  else
    {
      output_buf << ends;
      char *msg = output_buf.str ();
      retval = msg;
      delete [] msg;
    }

  return retval;
}

DEFUN ("flops", Fflops, Sflops, 10,
  "flops (): count floating point operations")
{
  int nargin = args.length ();

  if (nargin > 0)
    print_usage ("flops");

  warning ("flops is a flop, always returning zero");

  return 0.0;
}

DEFUN ("quit", Fquit, Squit, 00,
  "quit (): exit Octave gracefully")
{
  Octave_object retval;
  quitting_gracefully = 1;
  clean_up_and_exit (0);
  return retval;
}

DEFALIAS (exit, quit);

DEFUN ("warranty", Fwarranty, Swarranty, 00,
  "warranty (): describe copying conditions")
{
  Octave_object retval;

  ostrstream output_buf;
  output_buf << "\n" OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n\n\
This program is free software; you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 2 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program. If not, write to the Free Software\n\
Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.\n\
\n";

  output_buf << ends;
  maybe_page_output (output_buf);

  return retval;
}

// XXX FIXME XXX -- this may not be the best place for these...

Octave_object
feval (const Octave_object& args, int nargout)
{
  Octave_object retval;

  tree_fvc *fcn = is_valid_function (args(0), "feval", 1);
  if (fcn)
    {
      int tmp_nargin = args.length () - 1;
      Octave_object tmp_args;
      tmp_args.resize (tmp_nargin);
      for (int i = 0; i < tmp_nargin; i++)
	tmp_args(i) = args(i+1);
      retval = fcn->eval (0, nargout, tmp_args);
    }

  return retval;
}

DEFUN ("feval", Ffeval, Sfeval, 11,
  "feval (NAME, ARGS, ...)\n\
\n\
evaluate NAME as a function, passing ARGS as its arguments")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin > 0)
    retval = feval (args, nargout);
  else
    print_usage ("feval");

  return retval;
}

static Octave_object
eval_string (const string& s, int print, int& parse_status,
	     int nargout) 
{
  begin_unwind_frame ("eval_string");

  unwind_protect_int (get_input_from_eval_string);
  unwind_protect_int (input_from_command_line_file);
  unwind_protect_ptr (global_command);
  unwind_protect_str (current_eval_string);

  get_input_from_eval_string = 1;
  input_from_command_line_file = 0;
  current_eval_string = s;

  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer (0);

  add_unwind_protect (restore_input_buffer, (void *) old_buf);
  add_unwind_protect (delete_input_buffer, (void *) new_buf);

  switch_to_buffer (new_buf);

  unwind_protect_ptr (curr_sym_tab);

  reset_parser ();

  parse_status = yyparse ();

  // Important to reset the idea of where input is coming from before
  // trying to eval the command we just parsed -- it might contain the
  // name of an function file that still needs to be parsed!

  tree_statement_list *command = global_command;

  run_unwind_frame ("eval_string");

  Octave_object retval;

  if (parse_status == 0 && command)
    {
      retval = command->eval (print, nargout);
      delete command;
    }

  return retval;
}

tree_constant
eval_string (const string& s, int print, int& parse_status)
{
  tree_constant retval;

  Octave_object tmp = eval_string (s, print, parse_status, 1);

  retval = tmp(0);

  return retval;
}

static Octave_object
eval_string (const tree_constant& arg, int& parse_status, int nargout)
{
  string s = arg.string_value ();

  if (error_state)
    {
      error ("eval: expecting string argument");
      return -1.0;
    }

  // Yes Virginia, we always print here...

  return eval_string (s, 1, parse_status, nargout);
}

DEFUN ("eval", Feval, Seval, 11,
  "eval (TRY, CATCH)\n\
\n\
Evaluate the string TRY as octave code.  If that fails, evaluate the\n\
string CATCH.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      begin_unwind_frame ("Feval");

      if (nargin > 1)
	{
	  unwind_protect_int (buffer_error_messages);
	  buffer_error_messages = 1;
	}

      int parse_status = 0;

      retval = eval_string (args(0), parse_status, nargout);

      if (nargin > 1 && (parse_status != 0 || error_state))
	{
	  error_state = 0;

	  // Set up for letting the user print any messages from
	  // errors that occurred in the first part of this eval().

	  buffer_error_messages = 0;
	  bind_global_error_variable ();
	  add_unwind_protect (clear_global_error_variable, 0);

	  eval_string (args(1), parse_status, nargout);

	  retval = Octave_object ();
	}

      run_unwind_frame ("Feval");
    }
  else
    print_usage ("eval");

  return retval;
}

// Execute a shell command.

DEFUN ("system", Fsystem, Ssystem, 11,
  "system (string [, return_output]): execute shell commands")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ("system");
      return retval;
    }

  tree_constant tc_command = args(0);

  string tmp = tc_command.string_value ();

  if (error_state)
    {
      error ("system: expecting string as first argument");
    }
  else
    {
      iprocstream *cmd = new iprocstream (tmp.c_str ());

      add_unwind_protect (cleanup_iprocstream, cmd);

      int status = 127;

      if (cmd && *cmd)
	{
	  ostrstream output_buf;

	  char ch;
	  while (cmd->get (ch))
	    output_buf.put (ch);

	  output_buf << ends;

	  status = cmd->close ();

	  // The value in status is as returned by waitpid.  If the
	  // process exited normally, extract the actual exit status of
	  // the command.  Otherwise, return 127 as a failure code.

	  if ((status & 0xff) == 0)
	    status = (status & 0xff00) >> 8;

	  if (nargout > 0 || nargin > 1)
	    {
	      char *msg = output_buf.str ();

	      retval(1) = (double) status;
	      retval(0) = msg;

	      delete [] msg;
	    }
	  else
	    maybe_page_output (output_buf);
	}
      else
	error ("unable to start subprocess for `%s'", tmp.c_str ());

      run_unwind_protect ();
    }

  return retval;
}

DEFALIAS (shell_cmd, system);

#if defined (__GNUG__) && defined (DEBUG_NEW_DELETE)
int debug_new_delete = 0;

typedef void (*vfp)(void);
extern vfp __new_handler;

void *
__builtin_new (size_t sz)
{
  void *p;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;
  p = (void *) malloc (sz);
  while (p == 0)
    {
      (*__new_handler) ();
      p = (void *) malloc (sz);
    }

  if (debug_new_delete)
    cout << "__builtin_new: " << p << endl;

  return p;
}

void
__builtin_delete (void *ptr)
{
  if (debug_new_delete)
    cout << "__builtin_delete: " << ptr << endl;

  if (ptr)
    free (ptr);
}
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
