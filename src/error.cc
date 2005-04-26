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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdarg>
#include <cstring>

#include <string>

#include "lo-sstream.h"

#include "defun.h"
#include "error.h"
#include "input.h"
#include "pager.h"
#include "oct-obj.h"
#include "utils.h"
#include "ov.h"
#include "ov-usr-fcn.h"
#include "pt-pr-code.h"
#include "pt-stmt.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "variables.h"

// TRUE means that Octave will try to beep obnoxiously before printing
// error messages.
static bool Vbeep_on_error;

// TRUE means that Octave will try to enter the debugger when an error
// is encountered.  This will also inhibit printing of the normal
// traceback message (you will only see the top-level error message).
static bool Vdebug_on_error;

// TRUE means that Octave will try to enter the debugger when a warning
// is encountered.
static bool Vdebug_on_warning;

// The text of the last error message.
static std::string Vlast_error_message;

// The text of the last warning message.
static std::string Vlast_warning_message;

// The warning frequency for Matlab handle graphics backwards
// compatibility warnings (currently not used).
static std::string Vwarning_frequency = "once";

// The current warning state.  Valid values are "on", "off",
// "backtrace", or "debug".
std::string Vwarning_option = "backtrace";

// Current error state.
//
// Valid values:
//
//   -2: an error has occurred, but don't print any messages.
//   -1: an error has occurred, we are printing a traceback
//    0: no error
//    1: an error has occurred
//
int error_state = 0;

// Current warning state.
//
//  Valid values:
//
//    0: no warning
//    1: a warning has occurred
//
int warning_state = 0;

// Tell the error handler whether to print messages, or just store
// them for later.  Used for handling errors in eval() and
// the `unwind_protect' statement.
int buffer_error_messages = 0;

// TRUE means error messages are turned off.
bool discard_error_messages = false;

// TRUE means warning messages are turned off.
bool discard_warning_messages = false;

// The message buffer.
static OSSTREAM *error_message_buffer = 0;

void
reset_error_handler (void)
{
  error_state = 0;
  warning_state = 0;
  buffer_error_messages = 0;
  discard_error_messages = false;
}

// Warning messages are never buffered.

static void
vwarning (const char *name, const char *fmt, va_list args)
{
  if (discard_warning_messages)
    return;

  flush_octave_stdout ();

  OSSTREAM output_buf;

  if (name)
    output_buf << name << ": ";

  octave_vformat (output_buf, fmt, args);

  output_buf << std::endl << OSSTREAM_ENDS;

  // XXX FIXME XXX -- we really want to capture the message before it
  // has all the formatting goop attached to it.  We probably also
  // want just the message, not the traceback information.

  std::string msg_string = OSSTREAM_STR (output_buf);

  OSSTREAM_FREEZE (output_buf);

  if (! warning_state)
    {
      // This is the first warning in a possible series.
      Vlast_warning_message = msg_string;
    }

  octave_diary << msg_string;

  std::cerr << msg_string;
}

static void
verror (bool save_last_error, std::ostream& os,
	const char *name, const char *fmt, va_list args)
{
  if (discard_error_messages)
    return;

  if (! buffer_error_messages)
    flush_octave_stdout ();

  bool to_beep_or_not_to_beep_p = Vbeep_on_error && ! error_state;

  OSSTREAM output_buf;

  if (to_beep_or_not_to_beep_p)
    output_buf << "\a";

  if (name)
    output_buf << name << ": ";

  octave_vformat (output_buf, fmt, args);

  output_buf << std::endl << OSSTREAM_ENDS;

  // XXX FIXME XXX -- we really want to capture the message before it
  // has all the formatting goop attached to it.  We probably also
  // want just the message, not the traceback information.

  std::string msg_string = OSSTREAM_STR (output_buf);

  OSSTREAM_FREEZE (output_buf);

  if (! error_state && save_last_error)
    {
      // This is the first error in a possible series.
      Vlast_error_message = msg_string;
    }

  if (buffer_error_messages)
    {
      std::string tmp = msg_string;

      if (! error_message_buffer)
	{
	  error_message_buffer = new OSSTREAM;

	  // XXX FIXME XXX -- this is ugly, but it prevents
	  //
	  //   eval ("error (\"msg\")", "error (lasterr ())");
	  //
	  // from printing `error: ' twice.  Assumes that the NAME we
	  // have been given doesn't contain `:'.

	  size_t pos = msg_string.find (':');

	  if (pos != NPOS && pos < Vlast_error_message.length () - 2)
	    tmp = msg_string.substr (pos+2);
	}

      *error_message_buffer << tmp;
    }
  else
    {
      octave_diary << msg_string;
      os << msg_string;
    }
}

// Note that we don't actually print any message if the error string
// is just "" or "\n".  This allows error ("") and error ("\n") to
// just set the error state.

static void
error_1 (std::ostream& os, const char *name, const char *fmt, va_list args)
{
  if (error_state != -2)
    {
      if (fmt)
	{
	  if (*fmt)
	    {
	      int len = strlen (fmt);
	      if (fmt[len - 1] == '\n')
		{
		  if (len > 1)
		    {
		      char *tmp_fmt = strsave (fmt);
		      tmp_fmt[len - 1] = '\0';
		      verror (true, os, name, tmp_fmt, args);
		      delete [] tmp_fmt;
		    }

		  error_state = -2;
		}
	      else
		verror (true, os, name, fmt, args);
	    }
	}
      else
	panic ("error_1: invalid format");

      if (! error_state)
	error_state = 1;
    }
}

void
message (const char *name, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror (false, std::cerr, name, fmt, args);
  va_end (args);
}

void
usage (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror (true, std::cerr, "usage", fmt, args);
  error_state = -1;
  va_end (args);
}

static void
pr_where_2 (const char *fmt, va_list args)
{
  if (fmt)
    {
      if (*fmt)
	{
	  int len = strlen (fmt);
	  if (fmt[len - 1] == '\n')
	    {
	      if (len > 1)
		{
		  char *tmp_fmt = strsave (fmt);
		  tmp_fmt[len - 1] = '\0';
		  verror (false, std::cerr, 0, tmp_fmt, args);
		  delete [] tmp_fmt;
		}
	    }
	  else
	    verror (false, std::cerr, 0, fmt, args);
	}
    }
  else
    panic ("pr_where_2: invalid format");
}

static void
pr_where_1 (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  pr_where_2 (fmt, args);
  va_end (args);
}

static void
pr_where (const char *name, bool print_code = true)
{
  if (curr_statement)
    {
      std::string nm;

      int l = -1;
      int c = -1;

      octave_function *fcn = curr_function;

      if (fcn)
	{
	  nm = fcn->name ();

	  if (nm == "error" || nm == "warning")
	    fcn = curr_caller_function;

	  if (fcn)
	    {
	      nm = fcn->fcn_file_name ();

	      if (nm.empty ())
		nm = fcn->name ();

	      if (curr_statement)
		{
		  l = curr_statement->line ();
		  c = curr_statement->column ();
		}
	    }
	}

      if (nm.empty ())
	{
	  if (l > 0 && c > 0)
	    pr_where_1 ("%s: near line %d, column %d:", name, l, c);
	}
      else
	{
	  if (l > 0 && c > 0)
	    pr_where_1 ("%s: in %s near line %d, column %d:",
			name, nm.c_str (), l, c);
	  else
	    pr_where_1 ("%s: in %s", name, nm.c_str ());
	}

      if (print_code)
	{
	  // XXX FIXME XXX -- Note that the column number is probably
	  // not going to mean much here since the code is being
	  // reproduced from the parse tree, and we are only showing
	  // one statement even if there were multiple statements on
	  // the original source line.

	  OSSTREAM output_buf;

	  output_buf << std::endl;

	  tree_print_code tpc (output_buf, ">>> ");

	  curr_statement->accept (tpc);

	  output_buf << std::endl << OSSTREAM_ENDS;

	  pr_where_1 ("%s", OSSTREAM_C_STR (output_buf));

	  OSSTREAM_FREEZE (output_buf);
	}
    }
}

void
warning (const char *fmt, ...)
{
  if (Vwarning_option != "off")
    {
      if (curr_sym_tab != top_level_sym_tab
	  && Vwarning_option == "backtrace"
	  && ! warning_state
	  && ! discard_warning_messages)
	pr_where ("warning", false);

      va_list args;
      va_start (args, fmt);
      vwarning ("warning", fmt, args);
      va_end (args);

      warning_state = 1;

      if ((interactive || forced_interactive)
	  && Vdebug_on_warning && curr_function)
	{
	  unwind_protect_bool (Vdebug_on_warning);
	  Vdebug_on_warning = false;

	  do_keyboard (octave_value_list ());

	  unwind_protect::run ();
	}
    }
}

void
error (const char *fmt, ...)
{
  int init_state = error_state;

  va_list args;
  va_start (args, fmt);
  error_1 (std::cerr, "error", fmt, args);
  va_end (args);

  if ((interactive || forced_interactive)
      && Vdebug_on_error && init_state == 0 && curr_function)
    {
      unwind_protect_bool (Vdebug_on_error);
      Vdebug_on_error = false;

      pr_where ("error");

      error_state = 0;

      do_keyboard (octave_value_list ());

      unwind_protect::run ();
    }
}

void
parse_error (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 (std::cerr, 0, fmt, args);
  va_end (args);
}

void
panic (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  buffer_error_messages = 0;
  discard_error_messages = false;
  verror (false, std::cerr, "panic", fmt, args);
  va_end (args);
  abort ();
}

static void
defun_usage_message_1 (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 (octave_stdout, 0, fmt, args);
  va_end (args);
}

void
defun_usage_message (const std::string& msg)
{
  defun_usage_message_1 ("%s", msg.c_str ());
}

typedef void (*error_fun)(const char *, ...);

extern octave_value_list Fsprintf (const octave_value_list&, int);

static std::string
handle_message (error_fun f, const char *msg, const octave_value_list& args)
{
  std::string retval;

  std::string tstr;

  int nargin = args.length ();

  if (nargin > 0)
    {
      octave_value arg;

      if (nargin > 1)
	{
	  octave_value_list tmp = Fsprintf (args, 1);
	  arg = tmp(0);
	}
      else
	arg = args(0);

      if (arg.is_defined ())
	{
	  if (arg.is_string ())
	    {
	      tstr = arg.string_value ();
	      msg = tstr.c_str ();
	      
	      if (! msg)
		return retval;
	    }
	  else if (arg.is_empty ())
	    return retval;
	}
    }

// Ugh.

  int len = strlen (msg);
  if (msg[len - 1] == '\n')
    {
      if (len > 1)
	{
	  char *tmp_msg = strsave (msg);
	  tmp_msg[len - 1] = '\0';
	  f ("%s\n", tmp_msg);
	  retval = tmp_msg;
	  delete [] tmp_msg;
	}
    }
  else
    {
      f ("%s", msg);
      retval = msg;
    }

  return retval;
}

DEFUN (error, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} error (@var{template}, @dots{})\n\
The @code{error} function formats the optional arguments under the\n\
control of the template string @var{template} using the same rules as\n\
the @code{printf} family of functions (@pxref{Formatted Output}).\n\
The resulting message is prefixed by the string @samp{error: } and\n\
printed on the @code{stderr} stream.\n\
\n\
Calling @code{error} also sets Octave's internal error state such that\n\
control will return to the top level without evaluating any more\n\
commands.  This is useful for aborting from functions or scripts.\n\
\n\
If the error message does not end with a new line character, Octave will\n\
print a traceback of all the function calls leading to the error.  For\n\
example, given the following function definitions:\n\
\n\
@example\n\
@group\n\
function f () g () end\n\
function g () h () end\n\
function h () nargin == 1 || error (\"nargin != 1\"); end\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
calling the function @code{f} will result in a list of messages that\n\
can help you to quickly locate the exact location of the error:\n\
\n\
@example\n\
@group\n\
f ()\n\
error: nargin != 1\n\
error: evaluating index expression near line 1, column 30\n\
error: evaluating binary operator `||' near line 1, column 27\n\
error: called from `h'\n\
error: called from `g'\n\
error: called from `f'\n\
@end group\n\
@end example\n\
\n\
If the error message ends in a new line character, Octave will print the\n\
message but will not display any traceback messages as it returns\n\
control to the top level.  For example, modifying the error message\n\
in the previous example to end in a new line causes Octave to only print\n\
a single message:\n\
\n\
@example\n\
@group\n\
function h () nargin == 1 || error (\"nargin != 1\\n\"); end\n\
f ()\n\
error: nargin != 1\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;
  handle_message (error, "unspecified error", args);
  return retval;
}

static inline octave_value_list
set_warning_option (const std::string& state,
		    const std::string& frequency, int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    retval(1) = Vwarning_frequency;

  if (nargout >= 0)
    retval(0) = Vwarning_option;

  if (! state.empty ())
    Vwarning_option = state;
    
  if (! frequency.empty ())
    Vwarning_frequency = frequency;
    
  return retval;
}

DEFUN (warning, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} warning (@var{msg})\n\
Print a warning message @var{msg} prefixed by the string @samp{warning: }.  \n\
After printing the warning message, Octave will continue to execute\n\
commands.  You should use this function when you want to notify the user\n\
of an unusual condition, but only when it makes sense for your program\n\
to go on.\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  bool done = false;

  if (args.all_strings_p ())
    {
      string_vector argv = args.make_argv ("warning");

      if (! error_state)
	{
	  if (argc == 1)
	    {
	      retval = set_warning_option ("", "", nargout);
	      done = true;
	    }
	  else if (argc == 2)
	    {
	      std::string arg = argv(1);

	      if (arg == "on" || arg == "off" || arg == "backtrace")
		{
		  retval = set_warning_option (arg, "", nargout);
		  done = true;
		}
	      else if (arg == "once" || arg == "always")
		{
		  retval = set_warning_option ("", arg, nargout);
		  done = true;
		}
	      else if (arg == "debug")
		{
		  bind_builtin_variable ("debug_on_warning", true);
		  retval = set_warning_option ("", "", nargout);
		  done = true;
		}
	    }
	}
    }

  if (! done)
    {
      std::string prev_msg = Vlast_warning_message;

      std::string curr_msg
	= handle_message (warning, "unspecified warning", args);

      if (nargout > 0)
	retval(0) = Vlast_warning_message;
    }

  return retval;
}

DEFUN (lasterr, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} lasterr ()\n\
@deftypefnx {Built-in Function} {} lasterr (@var{msg})\n\
Without any arguments, return the last error message.  With one\n\
argument, set the last error message to @var{msg}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("lasterr");

  if (argc == 1)
    retval(0) = Vlast_error_message;
  else if (argc == 2)
    Vlast_error_message = argv(1);
  else
    print_usage ("lasterr");

  return retval;  
}

// For backward compatibility.
DEFALIAS (error_text, lasterr);
DEFALIAS (__error_text__, lasterr);

DEFUN (lastwarn, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} lastwarn ()\n\
@deftypefnx {Built-in Function} {} lastwarn (@var{msg})\n\
Without any arguments, return the last warning message.  With one\n\
argument, set the last warning message to @var{msg}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("lastwarn");

  if (argc == 1)
    retval(0) = Vlast_warning_message;
  else if (argc == 2)
    Vlast_warning_message = argv(1);
  else
    print_usage ("lastwarn");

  return retval;  
}

DEFUN (usage, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} usage (@var{msg})\n\
Print the message @var{msg}, prefixed by the string @samp{usage: }, and\n\
set Octave's internal error state such that control will return to the\n\
top level without evaluating any more commands.  This is useful for\n\
aborting from functions.\n\
\n\
After @code{usage} is evaluated, Octave will print a traceback of all\n\
the function calls leading to the usage message.\n\
\n\
You should use this function for reporting problems errors that result\n\
from an improper call to a function, such as calling a function with an\n\
incorrect number of arguments, or with arguments of the wrong type.  For\n\
example, most functions distributed with Octave begin with code like\n\
this\n\
\n\
@example\n\
@group\n\
if (nargin != 2)\n\
  usage (\"foo (a, b)\");\n\
endif\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
to check for the proper number of arguments.\n\
@end deftypefn")
{
  octave_value_list retval;
  handle_message (usage, "unknown", args);
  return retval;
}

static int
beep_on_error (void)
{
  Vbeep_on_error = check_preference ("beep_on_error");

  return 0;
}

static int
debug_on_error (void)
{
  Vdebug_on_error = check_preference ("debug_on_error");

  return 0;
}

static int
debug_on_warning (void)
{
  Vdebug_on_warning = check_preference ("debug_on_warning");

  return 0;
}

void
symbols_of_error (void)
{
  DEFVAR (beep_on_error, false, beep_on_error,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} beep_on_error\n\
If the value of @code{beep_on_error} is nonzero, Octave will try\n\
to ring your terminal's bell before printing an error message.  The\n\
default value is 0.\n\
@end defvr");

  DEFVAR (debug_on_error, false, debug_on_error,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} debug_on_error\n\
If the value of @code{debug_on_error} is nonzero, Octave will try\n\
to enter the debugger when an error is encountered.  This will also\n\
inhibit printing of the normal traceback message (you will only see\n\
the top-level error message).  The default value is 0.\n\
@end defvr");

  DEFVAR (debug_on_warning, false, debug_on_warning,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} debug_on_warning\n\
If the value of @code{debug_on_warning} is nonzero, Octave will try\n\
to enter the debugger when a warning is encountered.  The default\n\
value is 0.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
