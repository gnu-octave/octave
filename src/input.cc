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

// Get command input interactively or from files.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <csignal>

#include <string>

#include <iostream.h>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-edit.h"
#include "file-ops.h"
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
#include "pt-const.h"
#include "sighandlers.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Primary prompt string.
static string Vps1;

// Secondary prompt string.
static string Vps2;

// String printed before echoed input (enabled by --echo-input).
string Vps4;

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
string current_eval_string;

// TRUE means get input from current_eval_string.
bool get_input_from_eval_string = false;

// TRUE means we're parsing a function file.
bool reading_fcn_file = false;

// Simple name of function file we are reading.
string curr_fcn_file_name;

// Full name of file we are reading.
string curr_fcn_file_full_name;

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
string current_input_line;

// TRUE after a call to completion_matches().
bool octave_completion_matches_called = false;

static void
do_input_echo (const string& input_string)
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

string
gnu_readline (const string& s, bool force_readline)
{
  string retval;

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

static string
octave_gets (void)
{
  string retval;

  Vlast_prompt_time.stamp ();

  if ((interactive || forced_interactive)
      && (! (reading_fcn_file || reading_script_file)))
    {
      string ps = (promptflag > 0) ? Vps1 : Vps2;

      string prompt = command_editor::decode_prompt_string (ps);

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
      if (! input_from_startup_file)
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

static string
get_user_input (void)
{
  string retval;

  if (get_input_from_eval_string)
    {
      retval = current_eval_string;

      size_t len = retval.length ();

      if (retval[len-1] != '\n')
	retval.append ("\n");
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

  static string input_buf;
  static const char *pos = 0;
  static size_t chars_left = 0;

  int status = 0;

  if (input_buf.empty ())
    {
      pos = 0;

      input_buf = get_user_input ();

      chars_left = input_buf.length ();

      pos = input_buf.c_str ();
    }

  if (chars_left > 0)
    {
      buf[0] = '\0';

      size_t len = max_size > 2 ? max_size - 2 : 0;

      assert (len > 0);

      strncpy (buf, pos, len);

      if (chars_left > len)
	{
	  chars_left -= len;

	  pos += len;

	  buf[len] = '\0';

	  status = len;
	}
      else
	{
	  input_buf = "";

	  len = chars_left;

	  if (buf[len-1] != '\n')
	    buf[len++] = '\n';

	  buf[len] = '\0';

	  status = len;
	}
    }
  else if (chars_left == 0)
    {
      input_buf = "";

      status = 0;
    }
  else    
    status = -1;

  return status;
}

// Fix things up so that input can come from file `name', printing a
// warning if the file doesn't exist.

FILE *
get_input_from_file (const string& name, int warn)
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
generate_possible_completions (const string& text, string& prefix,
			       string& hint)
{
  string_vector names;

  prefix = "";

  if (! text.empty () && text != "." && text.rfind ('.') != NPOS)
    names = generate_struct_completions (text, prefix, hint);
  else
    names = make_name_list ();

  // Sort and remove duplicates.

  names.qsort (true);

  return names;
}

static string
generate_completion (const string& text, int state)
{
  string retval;

  static string prefix;
  static string hint;

  static size_t prefix_len = 0;
  static size_t hint_len = 0;

  static int list_index = 0;
  static int name_list_len = 0;
  static string_vector name_list;

  static int matches = 0;

  if (state == 0)
    {
      list_index = 0;

      prefix = "";

      hint = text;

      name_list = generate_possible_completions (text, prefix, hint);

      name_list_len = name_list.length ();

      prefix_len = prefix.length ();
	
      hint_len = hint.length ();

      matches = 0;

      for (int i = 0; i < name_list_len; i++)
	if (! name_list[i].compare (hint, 0, hint_len))
	  matches++;
    }

  if (name_list_len > 0 && matches > 0)
    {
      while (list_index < name_list_len)
	{
	  string name = name_list[list_index];

	  list_index++;

	  if (! name.compare (hint, 0, hint_len))
	    {
	      if (! prefix.empty ())
		retval = prefix + "." + name;
	      else
		retval = name;

	      if (matches == 1 && looks_like_struct (retval))
		command_editor::set_completion_append_character ('.');
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

  command_editor::set_basic_quote_characters ("\"");

  command_editor::set_completion_function (generate_completion);
}

static bool
match_sans_spaces (const string& standard, const string& test)
{
  size_t beg = test.find_first_not_of (" \t");

  if (beg != NPOS)
    {
      size_t end = test.find_last_not_of (" \t");

      size_t len = end == NPOS ? NPOS : end - beg + 1;

      return test.compare (standard, beg, len) == 0;
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

  string prompt ("debug> ");

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

  string input_buf = gnu_readline (prompt.c_str (), true);

  if (! input_buf.empty ())
    {
      if (! input_from_startup_file)
	command_history::add (input_buf);

      size_t len = input_buf.length ();

      if (len < 1)
	{
	  if (debug)
	    goto again;
	  else
	    {
	      if (read_as_string)
		return "";
	      else
		return Matrix ();
	    }
	}

      if (debug
	  && (match_sans_spaces ("exit", input_buf)
	      || match_sans_spaces ("quit", input_buf)
	      || match_sans_spaces ("return", input_buf)))
	{
	  return retval;
	}
      else if (read_as_string)
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
  "input (PROMPT [, S])\n\
\n\
Prompt user for input.  If the second argument is present, return\n\
value as a string.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    retval = get_user_input (args, false, nargout);
  else
    print_usage ("input");

  return retval;
}

static void
restore_command_history (void *)
{
  command_history::ignore_entries (! Vsaving_history);
}

DEFUN (keyboard, args, ,
  "keyboard (PROMPT)\n\
\n\
maybe help in debugging function files")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0 || nargin == 1)
    {
      unwind_protect::begin_frame ("keyboard");

      // XXX FIXME XXX -- we shouldn't need both the
      // command_history object and the
      // Vsaving_history variable...
      command_history::ignore_entries (false);

      unwind_protect::add (restore_command_history, 0);

      unwind_protect_bool (Vsaving_history);

      Vsaving_history = true;

      retval = get_user_input (args, true, 0);

      unwind_protect::run_frame ("keyboard");
    }
  else
    print_usage ("keyboard");

  return retval;
}

DEFUN_TEXT(echo, args, ,
  "echo [options]\n\
\n\
  echo [on|off]         -- enable or disable echoing of commands as\n\
                           they are executed in script files\n\
\n\
  echo [on all|off all] -- enable or disable echoing of commands as they\n\
                           are executed in script files and functions\n\
\n\
Without any arguments, toggle the current echo state.")
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
	  bind_builtin_variable ("echo_executing_commands",
				 static_cast<double> (ECHO_OFF));
	else
	  bind_builtin_variable ("echo_executing_commands",
				 static_cast<double> (ECHO_SCRIPTS));
      }
      break;

    case 2:
      {
	string arg = argv[1];

	if (arg == "on")
	  bind_builtin_variable ("echo_executing_commands",
				 static_cast<double> (ECHO_SCRIPTS));
	else if (arg == "off")
	  bind_builtin_variable ("echo_executing_commands",
				 static_cast<double> (ECHO_OFF));
	else
	  print_usage ("echo");
      }
      break;

    case 3:
      {
	string arg = argv[1];

	if (arg == "on" && argv[2] == "all")
	  {
	    int tmp = (ECHO_SCRIPTS | ECHO_FUNCTIONS);
	    bind_builtin_variable ("echo_executing_commands",
				   static_cast<double> (tmp));
	  }
	else if (arg == "off" && argv[2] == "all")
	  bind_builtin_variable ("echo_executing_commands",
				 static_cast<double> (ECHO_OFF));
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
  "completion_matches (HINT): generate possible completions given HINT\n\
\n\
This function is provided for the benefit of programs like Emacs which\n\
might be controlling Octave and handling user input.  The current command\n\
number is not incremented when this function is called.  This is a feature,\n\
not a bug.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      string hint = args(0).string_value ();

      if (! error_state)
	{
	  int n = 32;

	  string_vector list (n);

	  int k = 0;

	  for (;;)
	    {
	      string cmd = generate_completion (hint, k);

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
  "read_readline_init_file (FILE)")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    command_editor::read_init_file ();
  else if (nargin == 1)
    {
      string file = file_ops::tilde_expand (args(0).string_value ());

      if (! error_state)
	command_editor::read_init_file (file);
    }
  else
    print_usage ("read_readline_init_file");

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

  string s = builtin_string_variable ("completion_append_char");

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
    "primary prompt string");

  DEFVAR (PS2, "> ", ps2,
    "secondary prompt string");

  DEFVAR (PS4, "+ ", ps4,
    "string printed before echoed input (enabled by --echo-input)");

  DEFVAR (completion_append_char, " ", completion_append_char,
    "the string to append after successful command-line completion attempts");

  DEFVAR (echo_executing_commands, static_cast<double> (ECHO_OFF),
	  echo_executing_commands,
    "echo commands as they are executed");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
