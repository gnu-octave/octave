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

// Get command input interactively or from files.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>

#include <iostream>
#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-edit.h"
#include "file-ops.h"
#include "quit.h"
#include "str-vec.h"

#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "oct-map.h"
#include "oct-hist.h"
#include "toplev.h"
#include "oct-obj.h"
#include "pager.h"
#include "parse.h"
#include "pathlen.h"
#include "pt.h"
#include "pt-const.h"
#include "pt-stmt.h"
#include "sighandlers.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Primary prompt string.
static std::string Vps1;

// Secondary prompt string.
static std::string Vps2;

// String printed before echoed input (enabled by --echo-input).
std::string Vps4;

// Echo commands as they are executed?
//
//   1  ==>  echo commands read from script files
//   2  ==>  echo commands from functions
//   4  ==>  echo commands read from command line
//
// more than one state can be active at once.
int Vecho_executing_commands;

// The time we last printed a prompt.
octave_time Vlast_prompt_time;

// Character to append after successful command-line completion attempts.
static char Vcompletion_append_char;

// Global pointer for eval().
std::string current_eval_string;

// TRUE means get input from current_eval_string.
bool get_input_from_eval_string = false;

// TRUE means we haven't been asked for the input from
// current_eval_string yet.
bool input_from_eval_string_pending = false;

// TRUE means that input is coming from a file that was named on
// the command line.
bool input_from_command_line_file = false;

// TRUE means we're parsing a function file.
bool reading_fcn_file = false;

// Simple name of function file we are reading.
std::string curr_fcn_file_name;

// Full name of file we are reading.
std::string curr_fcn_file_full_name;

// TRUE means we're parsing a script file.
bool reading_script_file = false;

// If we are reading from an M-file, this is it.
FILE *ff_instream = 0;

// TRUE means this is an interactive shell.
bool interactive = false;

// TRUE means the user forced this shell to be interactive (-i).
bool forced_interactive = false;

// Should we issue a prompt?
int promptflag = 1;

// The current line of input, from wherever.
std::string current_input_line;

// TRUE after a call to completion_matches.
bool octave_completion_matches_called = false;

static void
do_input_echo (const std::string& input_string)
{
  int do_echo = reading_script_file ?
    (Vecho_executing_commands & ECHO_SCRIPTS)
      : (Vecho_executing_commands & ECHO_CMD_LINE) && ! forced_interactive;

  if (do_echo)
    {
      if (forced_interactive)
	{
	  if (promptflag > 0)
	    octave_stdout << command_editor::decode_prompt_string (Vps1);
	  else
	    octave_stdout << command_editor::decode_prompt_string (Vps2);
	}
      else
	octave_stdout << command_editor::decode_prompt_string (Vps4);

      if (! input_string.empty ())
	{
	  octave_stdout << input_string;

	  if (input_string[input_string.length () - 1] != '\n')
	    octave_stdout << "\n";
	}
    }
}

std::string
gnu_readline (const std::string& s, bool force_readline)
{
  OCTAVE_QUIT;

  std::string retval;

  if (line_editing || force_readline)
    {
      bool eof;

      retval = command_editor::readline (s, eof);

      if (! eof && retval.empty ())
	retval = "\n";
    }
  else
    {
      if (! s.empty () && (interactive || forced_interactive))
	{
	  FILE *stream = command_editor::get_output_stream ();

	  fprintf (stream, s.c_str ());
	  fflush (stream);
	}

      FILE *curr_stream = command_editor::get_input_stream ();

      if (reading_fcn_file || reading_script_file)
	curr_stream = ff_instream;

      retval = octave_fgets (curr_stream);
    }

  return retval;
}

static std::string
octave_gets (void)
{
  OCTAVE_QUIT;

  std::string retval;

  Vlast_prompt_time.stamp ();

  if ((interactive || forced_interactive)
      && (! (reading_fcn_file || reading_script_file)))
    {
      std::string ps = (promptflag > 0) ? Vps1 : Vps2;

      std::string prompt = command_editor::decode_prompt_string (ps);

      pipe_handler_error_count = 0;

      flush_octave_stdout ();

      octave_diary << prompt;

      retval = gnu_readline (prompt);
    }
  else
    retval = gnu_readline ("");

  current_input_line = retval;

  if (! current_input_line.empty ())
    {
      if (! (input_from_startup_file || input_from_command_line_file))
	command_history::add (current_input_line);

      if (! (reading_fcn_file || reading_script_file))
	{
	  octave_diary << current_input_line;

	  if (current_input_line[current_input_line.length () - 1] != '\n')
	    octave_diary << "\n";
	}

      do_input_echo (current_input_line);
    }
  else if (! (reading_fcn_file || reading_script_file))
    octave_diary << "\n";
  
  return retval;
}

// Read a line from the input stream.

static std::string
get_user_input (void)
{
  OCTAVE_QUIT;

  std::string retval;

  if (get_input_from_eval_string)
    {
      if (input_from_eval_string_pending)
	{
	  input_from_eval_string_pending = false;

	  retval = current_eval_string;

	  size_t len = retval.length ();

	  if (retval[len-1] != '\n')
	    retval.append ("\n");
	}
    }
  else
    retval = octave_gets ();

  current_input_line = retval;

  if (! get_input_from_eval_string)
    input_line_number++;

  return retval;
}

int
octave_read (char *buf, unsigned max_size)
{
  // XXX FIXME XXX -- is this a safe way to buffer the input?

  static const char * const eol = "\n";
  static std::string input_buf;
  static const char *pos = 0;
  static size_t chars_left = 0;

  int status = 0;
  if (chars_left == 0)
    {
      pos = 0;

      input_buf = get_user_input ();

      chars_left = input_buf.length ();

      pos = input_buf.c_str ();
    }

  if (chars_left > 0)
    {
      size_t len = max_size > chars_left ? chars_left : max_size;
      assert (len > 0);

      memcpy (buf, pos, len);

      chars_left -= len;
      pos += len;

      // Make sure input ends with a new line character.
      if (chars_left == 0 && buf[len-1] != '\n')
	{
	  if (len < max_size) 
	    {
	      // There is enough room to plug the newline character in
	      // the buffer.
	      buf[len++] = '\n';
	    }
	  else
	    {
	      // There isn't enough room to plug the newline character
	      // in the buffer so make sure it is returned on the next
	      // octave_read call.
	      pos = eol;
	      chars_left = 1;
	    }
	}

      status = len;

    }
  else if (chars_left == 0)
    {
      status = 0;
    }
  else    
    status = -1;

  return status;
}

// Fix things up so that input can come from file `name', printing a
// warning if the file doesn't exist.

FILE *
get_input_from_file (const std::string& name, int warn)
{
  FILE *instream = 0;

  if (name.length () > 0)
    instream = fopen (name.c_str (), "r");

  if (! instream && warn)
    warning ("%s: no such file or directory", name.c_str ());

  if (reading_fcn_file || reading_script_file)
    ff_instream = instream;
  else
    command_editor::set_input_stream (instream);

  return instream;
}

// Fix things up so that input can come from the standard input.  This
// may need to become much more complicated, which is why it's in a
// separate function.

FILE *
get_input_from_stdin (void)
{
  command_editor::set_input_stream (stdin);
  return command_editor::get_input_stream ();
}

// XXX FIXME XXX -- make this generate file names when appropriate.

static string_vector
generate_possible_completions (const std::string& text, std::string& prefix,
			       std::string& hint)
{
  string_vector names;

  prefix = "";

  if (looks_like_struct (text))
    names = generate_struct_completions (text, prefix, hint);
  else
    names = make_name_list ();

  // Sort and remove duplicates.

  names.qsort (true);

  return names;
}

static std::string
generate_completion (const std::string& text, int state)
{
  std::string retval;

  static std::string prefix;
  static std::string hint;

  static size_t hint_len = 0;

  static int list_index = 0;
  static int name_list_len = 0;
  static int name_list_total_len = 0;
  static string_vector name_list;
  static string_vector file_name_list;

  static int matches = 0;

  if (state == 0)
    {
      list_index = 0;

      prefix = "";

      hint = text;

      name_list = generate_possible_completions (text, prefix, hint);

      name_list_len = name_list.length ();

      file_name_list = command_editor::generate_filename_completions (text);

      name_list.append (file_name_list);

      name_list_total_len = name_list.length ();

      hint_len = hint.length ();

      matches = 0;

      for (int i = 0; i < name_list_len; i++)
	if (hint == name_list[i].substr (0, hint_len))
	  matches++;
    }

  if (name_list_total_len > 0 && matches > 0)
    {
      while (list_index < name_list_total_len)
	{
	  std::string name = name_list[list_index];

	  list_index++;

	  if (hint == name.substr (0, hint_len))
	    {
	      if (list_index <= name_list_len && ! prefix.empty ())
		retval = prefix + "." + name;
	      else
		retval = name;

	      // XXX FIXME XXX -- looks_like_struct is broken for now,
	      // so it always returns false.

 	      if (matches == 1 && looks_like_struct (retval))
 		{
 		  // Don't append anything, since we don't know
 		  // whether it should be '(' or '.'.

 		  command_editor::set_completion_append_character ('\0');
 		}
 	      else
 		command_editor::set_completion_append_character
 		  (Vcompletion_append_char);

	      break;
	    }
	}
    }

  return retval;
}

void
initialize_command_input (void)
{
  // If we are using readline, this allows conditional parsing of the
  // .inputrc file.

  command_editor::set_name ("Octave");

  // XX FIXME XXX -- this needs to include a comma too, but that
  // causes trouble for the new struct element completion code.

  static const char *s = "\t\n !\"\'*+-/:;<=>(){}[\\]^`~";

  command_editor::set_basic_word_break_characters (s);

  command_editor::set_completer_word_break_characters (s);

  command_editor::set_basic_quote_characters ("\"");

  command_editor::set_completion_function (generate_completion);
}

static bool
match_sans_spaces_semi (const std::string& standard, const std::string& test)
{
  size_t beg = test.find_first_not_of (" \t");

  if (beg != NPOS)
    {
      size_t end = test.find_last_not_of ("; \t");

      size_t len = end == NPOS ? NPOS : end - beg + 1;

      return (test.substr (beg, len) == standard);
    }

  return false;
}

// If the user simply hits return, this will produce an empty matrix.

static octave_value_list
get_user_input (const octave_value_list& args, bool debug, int nargout)
{
  octave_value_list retval;

  int nargin = args.length ();

  int read_as_string = 0;

  if (nargin == 2)
    read_as_string++;

  std::string nm;
  int line = -1;

  // We look at curr_caller_function because curr_function is always
  // "keyboard".

  if (debug && curr_caller_function)
    {
      nm = curr_caller_function->fcn_file_name ();

      if (nm.empty ())
	nm = curr_caller_function->name ();

      if (curr_statement)
	line = curr_statement->line ();
    }

  OSSTREAM buf;

  if (! nm.empty ())
    {
      buf << "stopped in " << nm;

      if (line > 0)
	buf << " at line " << line;
    }
    
  buf << OSSTREAM_ENDS;

  std::string msg = OSSTREAM_STR (buf);

  OSSTREAM_FREEZE (buf);

  if (! msg.empty ())
    message ("keyboard", msg.c_str ());

  std::string prompt = "debug> ";

  if (nargin > 0)
    {
      prompt = args(0).string_value ();

      if (error_state)
	{
	  error ("input: unrecognized argument");
	  return retval;
	}
    }

 again:

  flush_octave_stdout ();

  octave_diary << prompt;

  std::string input_buf = gnu_readline (prompt.c_str (), true);

  if (! input_buf.empty ())
    {
      if (! input_from_startup_file)
	command_history::add (input_buf);

      size_t len = input_buf.length ();

      octave_diary << input_buf;

      if (input_buf[len - 1] != '\n')
	octave_diary << "\n";

      if (len < 1)
	{
	  if (debug)
	    goto again;
	  else
	    return read_as_string ? octave_value ("") : octave_value (Matrix ());
	}

      if (debug)
	{
	  if (match_sans_spaces_semi ("exit", input_buf)
	      || match_sans_spaces_semi ("quit", input_buf)
	      || match_sans_spaces_semi ("return", input_buf)
	      || match_sans_spaces_semi ("dbcont", input_buf))
	    {
	      return retval;
	    }
	  else if (match_sans_spaces_semi ("dbstep", input_buf))
	    {
	      tree::break_next = true;

	      tree::last_line = 0;

	      tree::break_function = curr_function;

	      return retval;
	    }
	  else if (match_sans_spaces_semi ("dbnext", input_buf))
	    {
	      tree::break_next = true;

	      tree::last_line = curr_statement->line ();

	      tree::break_function = curr_function;

	      return retval;
	    }
	}

      if (read_as_string)
	{
	  // XXX FIXME XXX -- fix gnu_readline and octave_gets instead!
	  if (input_buf.length () == 1 && input_buf[0] == '\n')
	    retval(0) = "";
	  else
	    retval(0) = input_buf;
	}
      else
	{
	  int parse_status = 0;

	  bool silent = ! debug;

	  retval = eval_string (input_buf, silent, parse_status, nargout);

	  if (! debug && retval.length () == 0)
	    retval(0) = Matrix ();
	}
    }
  else
    error ("input: reading user-input failed!");

  if (debug)
    {
      // Clear error_state so that if errors were encountered while
      // evaluating user input, extra error messages will not be
      // printed after we return.

      error_state = 0;

      retval = octave_value_list ();

      goto again;
    }

  return retval;
}

DEFUN (input, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} input (@var{prompt})\n\
@deftypefnx {Built-in Function} {} input (@var{prompt}, \"s\")\n\
Print a prompt and wait for user input.  For example,\n\
\n\
@example\n\
input (\"Pick a number, any number! \")\n\
@end example\n\
\n\
@noindent\n\
prints the prompt\n\
\n\
@example\n\
Pick a number, any number!\n\
@end example\n\
\n\
@noindent\n\
and waits for the user to enter a value.  The string entered by the user\n\
is evaluated as an expression, so it may be a literal constant, a\n\
variable name, or any other valid expression.\n\
\n\
Currently, @code{input} only returns one value, regardless of the number\n\
of values produced by the evaluation of the expression.\n\
\n\
If you are only interested in getting a literal string value, you can\n\
call @code{input} with the character string @code{\"s\"} as the second\n\
argument.  This tells Octave to return the string entered by the user\n\
directly, without evaluating it first.\n\
\n\
Because there may be output waiting to be displayed by the pager, it is\n\
a good idea to always call @code{fflush (stdout)} before calling\n\
@code{input}.  This will ensure that all pending output is written to\n\
the screen before your prompt.  @xref{Input and Output}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    retval = get_user_input (args, false, nargout);
  else
    print_usage ("input");

  return retval;
}

bool
octave_yes_or_no (const std::string& prompt)
{
  std::string prompt_string = prompt + "(yes or no) ";

  while (1)
    {
      std::string input_buf = gnu_readline (prompt_string);

      if (input_buf == "yes")
	return true;
      else if (input_buf == "no")
	return false;
      else
	message (0, "Please answer yes or no.");
    }
}

DEFUN (yes_or_no, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} yes_or_no (@var{prompt})\n\
Ask the user a yes-or-no question.  Return 1 if the answer is yes.\n\
Takes one argument, which is the string to display to ask the\n\
question.  It should end in a space; @samp{yes-or-no-p} adds\n\
@samp{(yes or no) } to it.  The user must confirm the answer with\n\
RET and can edit it until it has been confirmed.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0 || nargin == 1)
    {
      std::string prompt;

      if (nargin == 1)
	{
	  prompt = args(0).string_value ();

	  if (error_state)
	    {
	      error ("yes_or_no: expecting argument to be character string");
	      return retval;
	    }
	}

      retval = octave_yes_or_no (prompt);
    }
  else
    print_usage ("yes_or_no");

  return retval;
}

static void
restore_command_history (void *)
{
  command_history::ignore_entries (! Vsaving_history);
}

octave_value
do_keyboard (const octave_value_list& args)
{
  octave_value retval;

  int nargin = args.length ();

  assert (nargin == 0 || nargin == 1);

  unwind_protect::begin_frame ("do_keyboard");

  // XXX FIXME XXX -- we shouldn't need both the
  // command_history object and the
  // Vsaving_history variable...
  command_history::ignore_entries (false);

  unwind_protect::add (restore_command_history, 0);

  unwind_protect_bool (Vsaving_history);

  Vsaving_history = true;

  octave_value_list tmp = get_user_input (args, true, 0);

  retval = tmp(0);

  unwind_protect::run_frame ("do_keyboard");

  return retval;
}

DEFUN (keyboard, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} keyboard (@var{prompt})\n\
This function is normally used for simple debugging.  When the\n\
@code{keyboard} function is executed, Octave prints a prompt and waits\n\
for user input.  The input strings are then evaluated and the results\n\
are printed.  This makes it possible to examine the values of variables\n\
within a function, and to assign new values to variables.  No value is\n\
returned from the @code{keyboard} function, and it continues to prompt\n\
for input until the user types @samp{quit}, or @samp{exit}.\n\
\n\
If @code{keyboard} is invoked without any arguments, a default prompt of\n\
@samp{debug> } is used.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0 || nargin == 1)
    do_keyboard (args);
  else
    print_usage ("keyboard");

  return retval;
}

DEFCMD (echo, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} echo options\n\
Control whether commands are displayed as they are executed.  Valid\n\
options are:\n\
\n\
@table @code\n\
@item on\n\
Enable echoing of commands as they are executed in script files.\n\
\n\
@item off\n\
Disable echoing of commands as they are executed in script files.\n\
\n\
@item on all\n\
Enable echoing of commands as they are executed in script files and\n\
functions.\n\
\n\
@item off all\n\
Disable echoing of commands as they are executed in script files and\n\
functions.\n\
@end table\n\
\n\
@noindent\n\
If invoked without any arguments, @code{echo} toggles the current echo\n\
state.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("echo");

  if (error_state)
    return retval;

  switch (argc)
    {
    case 1:
      {
	if ((Vecho_executing_commands & ECHO_SCRIPTS)
	    || (Vecho_executing_commands & ECHO_FUNCTIONS))
	  bind_builtin_variable ("echo_executing_commands", ECHO_OFF);
	else
	  bind_builtin_variable ("echo_executing_commands", ECHO_SCRIPTS);
      }
      break;

    case 2:
      {
	std::string arg = argv[1];

	if (arg == "on")
	  bind_builtin_variable ("echo_executing_commands", ECHO_SCRIPTS);
	else if (arg == "off")
	  bind_builtin_variable ("echo_executing_commands", ECHO_OFF);
	else
	  print_usage ("echo");
      }
      break;

    case 3:
      {
	std::string arg = argv[1];

	if (arg == "on" && argv[2] == "all")
	  {
	    int tmp = (ECHO_SCRIPTS | ECHO_FUNCTIONS);
	    bind_builtin_variable ("echo_executing_commands", tmp);
	  }
	else if (arg == "off" && argv[2] == "all")
	  bind_builtin_variable ("echo_executing_commands", ECHO_OFF);
	else
	  print_usage ("echo");
      }
      break;

    default:
      print_usage ("echo");
      break;
    }

  return retval;
}

DEFUN (completion_matches, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} completion_matches (@var{hint})\n\
Generate possible completions given @var{hint}.\n\
\n\
This function is provided for the benefit of programs like Emacs which\n\
might be controlling Octave and handling user input.  The current\n\
command number is not incremented when this function is called.  This is\n\
a feature, not a bug.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string hint = args(0).string_value ();

      if (! error_state)
	{
	  int n = 32;

	  string_vector list (n);

	  int k = 0;

	  for (;;)
	    {
	      std::string cmd = generate_completion (hint, k);

	      if (! cmd.empty ())
		{
		  if (k == n)
		    {
		      n *= 2;
		      list.resize (n);
		    }

		  list[k++] = cmd;
		}
	      else
		{
		  list.resize (k);
		  break;
		}
	    }

	  if (nargout > 0)
	    {
	      if (! list.empty ())
		retval = list;
	      else
		retval = "";
	    }
	  else
	    {
	      // We don't use string_vector::list_in_columns here
	      // because it will be easier for Emacs if the names
	      // appear in a single column.

	      int len = list.length ();

	      for (int i = 0; i < len; i++)
		octave_stdout << list[i] << "\n";
	    }

	  octave_completion_matches_called = true;
	}
    }
  else
    print_usage ("completion_matches");

  return retval;
}

DEFUN (read_readline_init_file, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} read_readline_init_file (@var{file})\n\
Read the readline library initialiazation file @var{file}.  If\n\
@var{file} is omitted, read the default initialization file (normally\n\
@file{~/.inputrc}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    command_editor::read_init_file ();
  else if (nargin == 1)
    {
      std::string file = file_ops::tilde_expand (args(0).string_value ());

      if (! error_state)
	command_editor::read_init_file (file);
    }
  else
    print_usage ("read_readline_init_file");

  return retval;
}

static std::string hook_fcn;
static octave_value user_data;

static int
input_event_hook (void)
{
  if (is_valid_function (hook_fcn))
    {
      if (user_data.is_defined ())
	feval (hook_fcn, user_data, 0);
      else
	feval (hook_fcn, octave_value_list (), 0);
    }
  else
    {
      hook_fcn = std::string ();
      user_data = octave_value ();

      command_editor::set_event_hook (0);
    }

  return 0;
}

DEFUN (input_event_hook, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{ofcn}, @var{odata}] =} input_event_hook (@var{fcn}, @var{data})\n\
Given the name of a function as a string and any Octave value object,\n\
install @var{fcn} as a function to call periodically, when Octave is\n\
waiting for input.  The function should have the form\n\
@example\n\
@var{fcn} (@var{data})\n\
@end example\n\
\n\
If @var{data} is omitted, Octave calls the function without any\n\
arguments.  If both @var{fcn} and @var{data} are omitted, Octave\n\
clears the hook.  In all cases, the name of the previous hook function\n\
and the user data are returned.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 2)
    print_usage ("input_event_hook");
  else
    {
      octave_value tmp_user_data;

      std::string tmp_hook_fcn;

      if (nargin > 1)
	tmp_user_data = args(1);

      if (nargin > 0)
	{
	  tmp_hook_fcn = args(0).string_value ();

	  if (error_state)
	    {
	      error ("input_event_hook: expecting string as first arg");
	      return retval;
	    }

	  command_editor::set_event_hook (input_event_hook);
	}

      if (nargin == 0)
	command_editor::set_event_hook (0);

      retval(1) = user_data;
      retval(0) = hook_fcn;

      hook_fcn = tmp_hook_fcn;
      user_data = tmp_user_data;
    }

  return retval;
}

static int
ps1 (void)
{
  int status = 0;

  Vps1 = builtin_string_variable ("PS1");

  return status;
}

static int
ps2 (void)
{
  int status = 0;

  Vps2 = builtin_string_variable ("PS2");

  return status;
}

static int
ps4 (void)
{
  int status = 0;

  Vps4 = builtin_string_variable ("PS4");

  return status;
}

static int
completion_append_char (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("completion_append_char");

  switch (s.length ())
    {
    case 1:
      Vcompletion_append_char = s[0];
      break;

    case 0:
      Vcompletion_append_char = '\0';
      break;

    default:
      warning ("completion_append_char must be a single character");
      status = -1;
      break;
    }

  return status;
}

static int
echo_executing_commands (void)
{
  Vecho_executing_commands = check_preference ("echo_executing_commands"); 

  return 0;
}

void
symbols_of_input (void)
{
  DEFVAR (PS1, "\\s:\\#> ", ps1,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} PS1\n\
The primary prompt string.  When executing interactively, Octave\n\
displays the primary prompt @code{PS1} when it is ready to read a\n\
command.\n\
\n\
The default value of @code{PS1} is @code{\"\\s:\\#> \"}.  To change it, use a\n\
command like\n\
\n\
@example\n\
octave:13> PS1 = \"\\\\u@@\\\\H> \"\n\
@end example\n\
\n\
@noindent\n\
which will result in the prompt @samp{boris@@kremvax> } for the user\n\
@samp{boris} logged in on the host @samp{kremvax.kgb.su}.  Note that two\n\
backslashes are required to enter a backslash into a string.\n\
@xref{Strings}.\n\
@end defvr");

  DEFVAR (PS2, "> ", ps2,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} PS2\n\
The secondary prompt string, which is printed when Octave is\n\
expecting additional input to complete a command.  For example, when\n\
defining a function over several lines, Octave will print the value of\n\
@code{PS1} at the beginning of each line after the first.  The default\n\
value of @code{PS2} is @code{\"> \"}.\n\
@end defvr");

  DEFVAR (PS4, "+ ", ps4,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} PS4\n\
If Octave is invoked with the @code{--echo-commands} option, the value of\n\
@code{PS4} is printed before each line of input that is echoed.  The\n\
default value of @code{PS4} is @code{\"+ \"}.  @xref{Invoking Octave}, for\n\
a description of @code{--echo-commands}.\n\
@end defvr");

  DEFVAR (completion_append_char, " ", completion_append_char,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} completion_append_char\n\
The value of @code{completion_append_char} is used as the character to\n\
append to successful command-line completion attempts.  The default\n\
value is @code{\" \"} (a single space).\n\
@end defvr");

  DEFVAR (echo_executing_commands, ECHO_OFF, echo_executing_commands,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} echo_executing_commands\n\
This variable may also be used to control the echo state.  It may be\n\
the sum of the following values:\n\
\n\
@table @asis\n\
@item 1\n\
Echo commands read from script files.\n\
\n\
@item 2\n\
Echo commands from functions.\n\
\n\
@item 4\n\
Echo commands read from command line.\n\
@end table\n\
\n\
More than one state can be active at once.  For example, a value of 3 is\n\
equivalent to the command @kbd{echo on all}.\n\
\n\
The value of @code{echo_executing_commands} is set by the @kbd{echo}\n\
command and the command line option @code{--echo-input}.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
