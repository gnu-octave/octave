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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdarg>
#include <cstring>

#include <strstream>
#include <string>

#include "defun.h"
#include "error.h"
#include "pager.h"
#include "oct-obj.h"
#include "utils.h"
#include "ov.h"
#include "variables.h"

// TRUE means that Octave will try to beep obnoxiously before printing
// error messages.
static bool Vbeep_on_error;

// Current error state.
int error_state = 0;

// Current warning state.
int warning_state = 0;

// Tell the error handler whether to print messages, or just store
// them for later.  Used for handling errors in eval() and
// the `unwind_protect' statement.
bool buffer_error_messages = false;

// The message buffer
std::ostrstream *error_message_buffer = 0;

// Warning messages are never buffered.
// XXX FIXME XXX -- we should provide another way to turn them off...

static void
vwarning (const char *name, const char *fmt, va_list args)
{
  flush_octave_stdout ();

  std::ostrstream output_buf;

  if (name)
    {
      octave_diary << name << ": ";
      std::cerr << name << ": ";
    }

  octave_diary.vform (fmt, args);
  std::cerr.vform (fmt, args);

  octave_diary << endl;
  std::cerr << endl;
}

static void
verror (const char *name, const char *fmt, va_list args)
{
  flush_octave_stdout ();

  bool to_beep_or_not_to_beep_p = Vbeep_on_error && ! error_state;

  std::ostrstream output_buf;

  if (to_beep_or_not_to_beep_p)
    output_buf << "\a";
  if (name)
    output_buf << name << ": ";
  output_buf.vform (fmt, args);
  output_buf << endl << ends;

  char *msg = output_buf.str ();

  if (buffer_error_messages)
    {
      char *ptr = msg;

      if (! error_message_buffer)
	{
	  error_message_buffer = new ostrstream;

	  // XXX FIXME XXX -- this is ugly, but it prevents
	  //
	  //   eval ("error (\"msg\")", "error (__error_text__)");
	  //
	  // from printing `error: ' twice.  Assumes that the NAME we
	  // have been given doesn't contain `:'.

	  ptr = strchr (msg, ':');

	  if (ptr)
	    {
	      if (*++ptr != '\0')
		++ptr;
	    }
	  else
	    ptr = msg;
	}

      *error_message_buffer << ptr;
    }
  else
    {
      octave_diary << msg;
      std::cerr << msg;
    }

  delete [] msg;
}

// Note that we don't actually print any message if the error string
// is just "" or "\n".  This allows error ("") and error ("\n") to
// just set the error state.

static void
error_1 (const char *name, const char *fmt, va_list args)
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
		      verror (name, tmp_fmt, args);
		      delete [] tmp_fmt;
		    }

		  error_state = -2;
		}
	      else
		verror (name, fmt, args);
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
  verror (name, fmt, args);
  va_end (args);
}

void
usage (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_state = -1;
  verror ("usage", fmt, args);
  va_end (args);
}

void
warning (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  warning_state = 1;
  vwarning ("warning", fmt, args);
  va_end (args);
}

void
error (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 ("error", fmt, args);
  va_end (args);
}

void
parse_error (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 (0, fmt, args);
  va_end (args);
}

void
panic (const char *fmt, ...)
{
  flush_octave_stdout ();

  va_list args;
  va_start (args, fmt);
  verror ("panic", fmt, args);
  va_end (args);
  abort ();
}

typedef void (*error_fun)(const char *, ...);

extern octave_value_list Fsprintf (const octave_value_list&, int);

static octave_value_list
handle_message (error_fun f, const char *msg, const octave_value_list& args)
{
  octave_value_list retval;

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
	  delete [] tmp_msg;
	}
    }
  else
    f ("%s", msg);

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
  return handle_message (error, "unspecified error", args);
}

DEFUN (warning, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} warning (@var{msg})\n\
Print a warning message @var{msg} prefixed by the string @samp{warning: }.  \n\
After printing the warning message, Octave will continue to execute\n\
commands.  You should use this function should when you want to notify\n\
the user of an unusual condition, but only when it makes sense for your\n\
program to go on.\n\
@end deftypefn")
{
  return handle_message (warning, "unspecified warning", args);
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
  return handle_message (usage, "unknown", args);
}

void
bind_global_error_variable (void)
{
  if (error_message_buffer)
    {
      *error_message_buffer << ends;

      char *error_text = error_message_buffer->str ();

      bind_builtin_constant ("__error_text__", error_text, true);

      delete [] error_text;

      delete error_message_buffer;

      error_message_buffer = 0;
    }
  else
    bind_builtin_constant ("__error_text__", "", true);
}

void
clear_global_error_variable (void *)
{
  delete error_message_buffer;
  error_message_buffer = 0;

  bind_builtin_constant ("__error_text__", "", true);
}

static int
beep_on_error (void)
{
  Vbeep_on_error = check_preference ("beep_on_error");

  return 0;
}

void
symbols_of_error (void)
{
  DEFVAR (beep_on_error, 0.0, beep_on_error,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} beep_on_error\n\
If the value of @code{beep_on_error} is nonzero, Octave will try\n\
to ring your terminal's bell before printing an error message.  The\n\
default value is 0.\n\
@end defvr");

  DEFCONST (error_text, "",
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} error_text\n\
This variable contains the text of error messages that would have\n\
been printed in the body of the most recent @code{unwind_protect} or\n\
@code{try} statement or the @var{try} part of the most recent call to\n\
the @code{eval} function.  Outside of the @code{unwind_protect} and\n\
@code{try} statements or the @code{eval} function, or if no error has\n\
occurred within them, the value of @code{error_text} is guaranteed to be\n\
the empty string.\n\
\n\
Note that the message does not include the first @samp{error: } prefix,\n\
so that it may easily be passed to the @code{error} function without\n\
additional processing@footnote{Yes, it's a kluge, but it seems to be a\n\
reasonably useful one.}.\n\
\n\
@xref{The try Statement}, and @ref{The unwind_protect Statement}.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
