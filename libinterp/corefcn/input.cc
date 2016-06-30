/*

Copyright (C) 1993-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Get command input interactively or from files.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>

#include <iostream>
#include <sstream>
#include <string>

#include "cmd-edit.h"
#include "file-ops.h"
#include "quit.h"
#include "str-vec.h"

#include "call-stack.h"
#include "debug.h"
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "errwarn.h"
#include "help.h"
#include "hook-fcn.h"
#include "input.h"
#include "interpreter.h"
#include "lex.h"
#include "load-path.h"
#include "octave.h"
#include "octave-link.h"
#include "oct-map.h"
#include "oct-hist.h"
#include "interpreter.h"
#include "octave-link.h"
#include "ovl.h"
#include "ov-fcn-handle.h"
#include "pager.h"
#include "parse.h"
#include "pt.h"
#include "pt-const.h"
#include "pt-eval.h"
#include "pt-stmt.h"
#include "sighandlers.h"
#include "symtab.h"
#include "sysdep.h"
#include "interpreter.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Primary prompt string.
static std::string VPS1;

// Secondary prompt string.
static std::string VPS2;

// String printed before echoed input (enabled by --echo-input).
std::string VPS4 = "+ ";

// Echo commands as they are executed?
//
//   1  ==>  echo commands read from script files
//   2  ==>  echo commands from functions
//   4  ==>  echo commands read from command line
//
// more than one state can be active at once.
int Vecho_executing_commands = ECHO_OFF;

// The time we last printed a prompt.
octave::sys::time Vlast_prompt_time = 0.0;

// Character to append after successful command-line completion attempts.
static char Vcompletion_append_char = ' ';

// TRUE after a call to completion_matches.
bool octave_completion_matches_called = false;

// TRUE if the plotting system has requested a call to drawnow at
// the next user prompt.
bool Vdrawnow_requested = false;

// TRUE if we are in debugging mode.
bool Vdebugging = false;

// TRUE if we are recording line numbers in a source file.
// Always true except when debugging and taking input directly from
// the terminal.
bool Vtrack_line_num = true;

// If we are in debugging mode, this is the last command entered, so
// that we can repeat the previous command if the user just types RET.
static std::string last_debugging_command = "\n";

// TRUE if we are running in the Emacs GUD mode.
static bool Vgud_mode = false;

// The filemarker used to separate filenames from subfunction names
char Vfilemarker = '>';

static hook_function_list input_event_hook_functions;

// For octave_quit.
void
remove_input_event_hook_functions (void)
{
  input_event_hook_functions.clear ();
}

void
set_default_prompts (void)
{
  // Use literal "octave" instead of "\\s" to avoid setting the prompt
  // to "octave.exe" or "octave-gui", etc.

  VPS1 = "octave:\\#> ";
  VPS2 = "> ";
  VPS4 = "+ ";

  octave_link::set_default_prompts (VPS1, VPS2, VPS4);
}

void
octave_base_reader::do_input_echo (const std::string& input_string) const
{
  bool forced_interactive = octave::application::forced_interactive ();

  int do_echo = reading_script_file ()
    ? (Vecho_executing_commands & ECHO_SCRIPTS)
    : ((Vecho_executing_commands & ECHO_CMD_LINE) && ! forced_interactive);

  if (do_echo)
    {
      if (forced_interactive)
        {
          if (pflag > 0)
            octave_stdout << octave::command_editor::decode_prompt_string (VPS1);
          else
            octave_stdout << octave::command_editor::decode_prompt_string (VPS2);
        }
      else
        octave_stdout << octave::command_editor::decode_prompt_string (VPS4);

      if (! input_string.empty ())
        {
          octave_stdout << input_string;

          if (input_string[input_string.length () - 1] != '\n')
            octave_stdout << "\n";
        }
    }
}

static std::string
gnu_readline (const std::string& s, bool& eof)
{
  octave_quit ();

  eof = false;

  std::string retval = octave::command_editor::readline (s, eof);

  if (! eof && retval.empty ())
    retval = "\n";

  return retval;
}

static inline std::string
interactive_input (const std::string& s, bool& eof)
{
  Vlast_prompt_time.stamp ();

  if (Vdrawnow_requested && octave::application::interactive ())
    {
      bool eval_error = false;

      try
        {
          feval ("drawnow");
        }
      catch (const octave_execution_exception& e)
        {
          eval_error = true;

          std::string stack_trace = e.info ();

          if (! stack_trace.empty ())
            std::cerr << stack_trace;

          if (octave::application::interactive ())
            recover_from_exception ();
        }

      flush_octave_stdout ();

      // We set Vdrawnow_requested to false even if there is an error in
      // drawnow so that the error doesn't reappear at every prompt.

      Vdrawnow_requested = false;

      if (eval_error)
        return "\n";
    }

  return gnu_readline (s, eof);
}

std::string
octave_base_reader::octave_gets (bool& eof)
{
  octave_quit ();

  eof = false;

  std::string retval;

  // Process pre input event hook function prior to flushing output and
  // printing the prompt.

  if (octave::application::interactive ())
    {
      if (! Vdebugging)
        octave_link::exit_debugger_event ();

      octave_link::pre_input_event ();

      octave_link::set_workspace ();
    }

  bool history_skip_auto_repeated_debugging_command = false;

  std::string ps = (pflag > 0) ? VPS1 : VPS2;

  std::string prompt = octave::command_editor::decode_prompt_string (ps);

  octave::pipe_handler_error_count = 0;

  flush_octave_stdout ();

  octave_pager_stream::reset ();
  octave_diary_stream::reset ();

  octave_diary << prompt;

  retval = interactive_input (prompt, eof);

  // There is no need to update the load_path cache if there is no
  // user input.
  if (retval != "\n"
      && retval.find_first_not_of (" \t\n\r") != std::string::npos)
    {
      load_path::update ();

      if (Vdebugging)
        last_debugging_command = retval;
      else
        last_debugging_command = "\n";
    }
  else if (Vdebugging)
    {
      retval = last_debugging_command;
      history_skip_auto_repeated_debugging_command = true;
    }

  if (retval != "\n")
    {
      if (! history_skip_auto_repeated_debugging_command)
        {
          if (octave::command_history::add (retval))
            octave_link::append_history (retval);
        }

      octave_diary << retval;

      if (retval[retval.length () - 1] != '\n')
        octave_diary << "\n";

      do_input_echo (retval);
    }
  else
    octave_diary << "\n";

  // Process post input event hook function after the internal history
  // list has been updated.

  if (octave::application::interactive ())
    octave_link::post_input_event ();

  return retval;
}

bool
octave_base_reader::reading_fcn_file (void) const
{
  return lexer ? lexer->reading_fcn_file : false;
}

bool
octave_base_reader::reading_classdef_file (void) const
{
  return lexer ? lexer->reading_classdef_file : false;
}

bool
octave_base_reader::reading_script_file (void) const
{
  return lexer ? lexer->reading_script_file : false;
}

// Fix things up so that input can come from the standard input.  This
// may need to become much more complicated, which is why it's in a
// separate function.

FILE *
get_input_from_stdin (void)
{
  octave::command_editor::set_input_stream (stdin);
  return octave::command_editor::get_input_stream ();
}

// FIXME: make this generate filenames when appropriate.

static string_vector
generate_possible_completions (const std::string& text, std::string& prefix,
                               std::string& hint, bool& deemed_struct)
{
  string_vector names;

  prefix = "";

  char prev_char = octave::command_editor::get_prev_char (text.length ());
  deemed_struct = looks_like_struct (text, prev_char);

  if (deemed_struct)
    names = generate_struct_completions (text, prefix, hint);
  else
    names = make_name_list ();

  // Sort and remove duplicates.

  names.sort (true);

  return names;
}

static bool
is_completing_dirfns (void)
{
  static std::string dirfns_commands[] = {"cd", "ls"};
  static const size_t dirfns_commands_length = 2;

  bool retval = false;

  std::string line = octave::command_editor::get_line_buffer ();

  for (size_t i = 0; i < dirfns_commands_length; i++)
    {
      int index = line.find (dirfns_commands[i] + " ");

      if (index == 0)
        {
          retval = true;
          break;
        }
    }

  return retval;
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

      // No reason to display symbols while completing a
      // file/directory operation.

      bool deemed_struct = false;

      if (is_completing_dirfns ())
        name_list = string_vector ();
      else
        name_list = generate_possible_completions (text, prefix, hint,
                                                   deemed_struct);

      name_list_len = name_list.numel ();

      // If the line was something like "a{1}." then text = "." but
      // we don't want to expand all the . files.
      if (! deemed_struct)
        {

          file_name_list = octave::command_editor::generate_filename_completions (text);

          name_list.append (file_name_list);

        }

      name_list_total_len = name_list.numel ();

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
                    // Special case: array reference forces prefix="."
                    //               in generate_struct_completions ()
              if (list_index <= name_list_len && ! prefix.empty ())
                retval = (prefix == "." ? "" : prefix) + "." + name;
              else
                retval = name;

              char prev_char = octave::command_editor::get_prev_char
                                                       (text.length ());
              if (matches == 1 && looks_like_struct (retval, prev_char))
                {
                  // Don't append anything, since we don't know
                  // whether it should be '(' or '.'.

                  octave::command_editor::set_completion_append_character ('\0');
                }
              else
                octave::command_editor::set_completion_append_character
                  (Vcompletion_append_char);

              break;
            }
        }
    }

  return retval;
}

static std::string
quoting_filename (const std::string &text, int, char quote)
{
  if (quote)
    return text;
  else
    return (std::string ("'") + text);
}

// Try to parse a partial command line in reverse, excluding trailing TEXT.
// If it appears a variable has been indexed by () or {},
// return that expression,
// to allow autocomplete of field names of arrays of structures.
std::string
find_indexed_expression (const std::string& text)
{
  std::string line = octave::command_editor::get_line_buffer ();

  int pos = line.length () - text.length ();
  int curly_count = 0;
  int paren_count = 0;

  int last = --pos;

  while (pos >= 0 && (line[pos] == ')' || line[pos] == '}'))
    {
      if (line[pos] == ')')
        paren_count++;
      else if (line[pos] == '}')
        curly_count++;

      while (curly_count + paren_count > 0 && --pos >= 0)
        {
          if (line[pos] == ')')
            paren_count++;
          else if (line[pos] == '(')
            paren_count--;
          else if (line[pos] == '}')
            curly_count++;
          else if (line[pos] == '{')
            curly_count--;
        }

      while (--pos >= 0 && line[pos] == ' ')
        ;
    }

  while (pos >= 0 && (isalnum (line[pos]) || line[pos] == '_'))
    pos--;

  if (++pos >= 0)
    return (line.substr (pos, last + 1 - pos));
  else
    return std::string ();
}

void
initialize_command_input (void)
{
  // If we are using readline, this allows conditional parsing of the
  // .inputrc file.

  octave::command_editor::set_name ("Octave");

  // FIXME: this needs to include a comma too, but that
  // causes trouble for the new struct element completion code.

  static const char *s = "\t\n !\"\'*+-/:;<=>(){}[\\]^`~";

  octave::command_editor::set_basic_word_break_characters (s);

  octave::command_editor::set_completer_word_break_characters (s);

  octave::command_editor::set_basic_quote_characters ("\"");

  octave::command_editor::set_filename_quote_characters (" \t\n\\\"'@<>=;|&()#$`?*[!:{");
  octave::command_editor::set_completer_quote_characters ("'\"");

  octave::command_editor::set_completion_function (generate_completion);

  octave::command_editor::set_quoting_function (quoting_filename);
}

static void
execute_in_debugger_handler (const std::pair<std::string, int>& arg)
{
  octave_link::execute_in_debugger_event (arg.first, arg.second);
}

static void
get_debug_input (const std::string& prompt)
{
  octave::unwind_protect frame;

  bool silent = tree_evaluator::quiet_breakpoint_flag;
  tree_evaluator::quiet_breakpoint_flag = false;

  octave_user_code *caller = octave_call_stack::caller_user_code ();
  std::string nm;
  int curr_debug_line;

  bool have_file = false;

  if (caller)
    {
      nm = caller->fcn_file_name ();

      if (nm.empty ())
        nm = caller->name ();
      else
        have_file = true;

      curr_debug_line = octave_call_stack::caller_user_code_line ();
    }
  else
    curr_debug_line = octave_call_stack::current_line ();

  std::ostringstream buf;

  if (! nm.empty ())
    {
      if (Vgud_mode)
        {
          static char ctrl_z = 'Z' & 0x1f;

          buf << ctrl_z << ctrl_z << nm << ":" << curr_debug_line;
        }
      else
        {
          // FIXME: we should come up with a clean way to detect
          // that we are stopped on the no-op command that marks the
          // end of a function or script.

          if (! silent)
            {
              buf << "stopped in " << nm;

              if (curr_debug_line > 0)
                buf << " at line " << curr_debug_line;
            }

          if (have_file)
            {
              octave_link::enter_debugger_event (nm, curr_debug_line);

              octave_link::set_workspace ();

              frame.add_fcn (execute_in_debugger_handler,
                             std::pair<std::string, int> (nm, curr_debug_line));

              if (! silent)
                {
                  std::string line_buf
                    = get_file_line (nm, curr_debug_line);

                  if (! line_buf.empty ())
                    buf << "\n" << curr_debug_line << ": " << line_buf;
                }
            }
        }
    }

  if (silent)
    octave::command_editor::erase_empty_line (true);

  std::string msg = buf.str ();

  if (! msg.empty ())
    std::cerr << msg << std::endl;

  frame.protect_var (VPS1);
  VPS1 = prompt;

  octave::application *app = octave::application::app ();

  if (! app->interactive ())
    {

      frame.add_method (app, &octave::application::interactive,
                        app->interactive ());

      frame.add_method (app, &octave::application::forced_interactive,
                        app->forced_interactive ());

      app->interactive (true);

      app->forced_interactive (true);
    }

  octave_parser curr_parser;

  while (Vdebugging)
    {
      try
        {
          Vtrack_line_num = false;

          reset_error_handler ();

          curr_parser.reset ();

          int retval = curr_parser.run ();

          if (octave::command_editor::interrupt (false))
            break;
          else
            {
              if (retval == 0 && curr_parser.stmt_list)
                {
                  curr_parser.stmt_list->accept (*current_evaluator);

                  if (octave_completion_matches_called)
                    octave_completion_matches_called = false;
                }

              octave_quit ();
            }
        }
      catch (const octave_execution_exception& e)
        {
          std::string stack_trace = e.info ();

          if (! stack_trace.empty ())
            std::cerr << stack_trace;

          // Ignore errors when in debugging mode;
          recover_from_exception ();
        }
    }
}

const std::string octave_base_reader::in_src ("invalid");

const std::string octave_terminal_reader::in_src ("terminal");

std::string
octave_terminal_reader::get_input (bool& eof)
{
  octave_quit ();

  eof = false;

  return octave_gets (eof);
}

const std::string octave_file_reader::in_src ("file");

std::string
octave_file_reader::get_input (bool& eof)
{
  octave_quit ();

  eof = false;

  return octave_fgets (file, eof);
}

const std::string octave_eval_string_reader::in_src ("eval_string");

std::string
octave_eval_string_reader::get_input (bool& eof)
{
  octave_quit ();

  eof = false;

  std::string retval;

  retval = eval_string;

  // Clear the eval string so that the next call will return
  // an empty character string with EOF = true.
  eval_string = "";

  if (retval.empty ())
    eof = true;

  return retval;
}

// If the user simply hits return, this will produce an empty matrix.

static octave_value_list
get_user_input (const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  int read_as_string = 0;

  if (args.length () == 2)
    read_as_string++;

  std::string prompt = args(0).xstring_value ("input: unrecognized argument");

  flush_octave_stdout ();

  octave_pager_stream::reset ();
  octave_diary_stream::reset ();

  octave_diary << prompt;

  bool eof = false;

  std::string input_buf = interactive_input (prompt.c_str (), eof);

  if (input_buf.empty ())
    error ("input: reading user-input failed!");

  size_t len = input_buf.length ();

  octave_diary << input_buf;

  if (input_buf[len - 1] != '\n')
    octave_diary << "\n";

  if (len < 1)
    return read_as_string ? octave_value ("") : octave_value (Matrix ());

  if (read_as_string)
    {
      // FIXME: fix gnu_readline and octave_gets instead!
      if (input_buf.length () == 1 && input_buf[0] == '\n')
        retval(0) = "";
      else
        retval(0) = input_buf;
    }
  else
    {
      int parse_status = 0;

      retval = eval_string (input_buf, true, parse_status, nargout);

      if (! Vdebugging && retval.empty ())
        retval(0) = Matrix ();
    }

  return retval;
}

DEFUN (input, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{ans} =} input (@var{prompt})
@deftypefnx {} {@var{ans} =} input (@var{prompt}, "s")
Print @var{prompt} and wait for user input.

For example,

@example
input ("Pick a number, any number! ")
@end example

@noindent
prints the prompt

@example
Pick a number, any number!
@end example

@noindent
and waits for the user to enter a value.  The string entered by the user
is evaluated as an expression, so it may be a literal constant, a variable
name, or any other valid Octave code.

The number of return arguments, their size, and their class depend on the
expression entered.

If you are only interested in getting a literal string value, you can call
@code{input} with the character string @qcode{"s"} as the second argument.
This tells Octave to return the string entered by the user directly, without
evaluating it first.

Because there may be output waiting to be displayed by the pager, it is a
good idea to always call @code{fflush (stdout)} before calling @code{input}.
 This will ensure that all pending output is written to the screen before
your prompt.
@seealso{yes_or_no, kbhit, pause, menu, listdlg}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  return get_user_input (args, std::max (nargout, 1));
}

bool
octave_yes_or_no (const std::string& prompt)
{
  std::string prompt_string = prompt + "(yes or no) ";

  while (1)
    {
      bool eof = false;

      std::string input_buf = interactive_input (prompt_string, eof);

      if (input_buf == "yes")
        return true;
      else if (input_buf == "no")
        return false;
      else
        message (0, "Please answer yes or no.");
    }
}

DEFUN (yes_or_no, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{ans} =} yes_or_no ("@var{prompt}")
Ask the user a yes-or-no question.

Return logical true if the answer is yes or false if the answer is no.

Takes one argument, @var{prompt}, which is the string to display when asking
the question.  @var{prompt} should end in a space; @code{yes-or-no} adds the
string @samp{(yes or no) } to it.  The user must confirm the answer with
@key{RET} and can edit it until it has been confirmed.
@seealso{input}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  std::string prompt;

  if (nargin == 1)
    prompt = args(0).xstring_value ("yes_or_no: PROMPT must be a string");

  return ovl (octave_yes_or_no (prompt));
}

octave_value
do_keyboard (const octave_value_list& args)
{
  octave_value retval;

  int nargin = args.length ();

  assert (nargin == 0 || nargin == 1);

  octave::unwind_protect frame;

  frame.add_fcn (octave::command_history::ignore_entries,
                 octave::command_history::ignoring_entries ());

  octave::command_history::ignore_entries (false);

  frame.protect_var (Vdebugging);

  frame.add_fcn (octave_call_stack::restore_frame,
                 octave_call_stack::current_frame ());

  // FIXME: probably we just want to print one line, not the
  // entire statement, which might span many lines...
  //
  // tree_print_code tpc (octave_stdout);
  // stmt.accept (tpc);

  Vdebugging = true;
  Vtrack_line_num = false;

  std::string prompt = "debug> ";
  if (nargin > 0)
    prompt = args(0).string_value ();

  get_debug_input (prompt);

  return retval;
}

DEFUN (keyboard, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} keyboard ()
@deftypefnx {} {} keyboard ("@var{prompt}")
Stop m-file execution and enter debug mode.

When the @code{keyboard} function is executed, Octave prints a prompt and
waits for user input.  The input strings are then evaluated and the results
are printed.  This makes it possible to examine the values of variables
within a function, and to assign new values if necessary.  To leave the
prompt and return to normal execution type @samp{return} or @samp{dbcont}.
The @code{keyboard} function does not return an exit status.

If @code{keyboard} is invoked without arguments, a default prompt of
@samp{debug> } is used.
@seealso{dbstop, dbcont, dbquit}
@end deftypefn */)
{
  if (args.length () > 1)
    print_usage ();

  octave::unwind_protect frame;

  frame.add_fcn (octave_call_stack::restore_frame,
                 octave_call_stack::current_frame ());

  // Skip the frame assigned to the keyboard function.
  octave_call_stack::goto_frame_relative (0);

  tree_evaluator::debug_mode = true;
  tree_evaluator::quiet_breakpoint_flag = false;

  tree_evaluator::current_frame = octave_call_stack::current_frame ();

  do_keyboard (args);

  return ovl ();
}

DEFUN (echo, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} echo
@deftypefnx {} {} echo on
@deftypefnx {} {} echo off
@deftypefnx {} {} echo on all
@deftypefnx {} {} echo off all
Control whether commands are displayed as they are executed.

Valid options are:

@table @code
@item on
Enable echoing of commands as they are executed in script files.

@item off
Disable echoing of commands as they are executed in script files.

@item on all
Enable echoing of commands as they are executed in script files and
functions.

@item off all
Disable echoing of commands as they are executed in script files and
functions.
@end table

@noindent
With no arguments, @code{echo} toggles the current echo state.
@end deftypefn */)
{
  string_vector argv = args.make_argv ();

  switch (args.length ())
    {
    case 0:
      {
        if ((Vecho_executing_commands & ECHO_SCRIPTS)
            || (Vecho_executing_commands & ECHO_FUNCTIONS))
          Vecho_executing_commands = ECHO_OFF;
        else
          Vecho_executing_commands = ECHO_SCRIPTS;
      }
      break;

    case 1:
      {
        std::string arg = argv[0];

        if (arg == "on")
          Vecho_executing_commands = ECHO_SCRIPTS;
        else if (arg == "off")
          Vecho_executing_commands = ECHO_OFF;
        else
          print_usage ();
      }
      break;

    case 2:
      {
        std::string arg = argv[0];

        if (arg == "on" && argv[1] == "all")
          {
            int tmp = (ECHO_SCRIPTS | ECHO_FUNCTIONS);
            Vecho_executing_commands = tmp;
          }
        else if (arg == "off" && argv[1] == "all")
          Vecho_executing_commands = ECHO_OFF;
        else
          print_usage ();
      }
      break;

    default:
      print_usage ();
      break;
    }

  return ovl ();
}

/*
%!test
%! state = echo_executing_commands ();
%! unwind_protect
%!   echo ();
%!   s1 = echo_executing_commands ();
%!   assert (s1 != state);
%!   echo ();
%!   s2 = echo_executing_commands ();
%!   assert (s2 != s1);
%! unwind_protect_cleanup
%!   echo_executing_commands (state);
%! end_unwind_protect

%!test
%! state = echo_executing_commands ();
%! unwind_protect
%!   echo ("off");
%!   assert (echo_executing_commands () == 0);
%!   echo ("on");
%!   assert (echo_executing_commands () != 0);
%!   echo ("off");
%!   assert (echo_executing_commands () == 0);
%! unwind_protect_cleanup
%!   echo_executing_commands (state);
%! end_unwind_protect

%!#test  # FIXME: This passes, but produces a lot of onscreen output
%! state = echo_executing_commands ();
%! unwind_protect
%!   echo ("on", "all");
%!   assert (echo_executing_commands () != 0);
%!   echo ("off", "all");
%!   assert (echo_executing_commands () == 0);
%! unwind_protect_cleanup
%!   echo_executing_commands (state);
%! end_unwind_protect

%!error echo ([])
%!error echo (0)
%!error echo ("")
%!error echo ("Octave")
%!error echo ("off", "invalid")
%!error echo ("on", "invalid")
%!error echo ("on", "all", "all")
*/

DEFUN (__echostate__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{state} =} __echostate__ ()
Undocumented internal function
@end deftypefn */)
{
  return ovl (Vecho_executing_commands == ECHO_SCRIPTS);
}

DEFUN (completion_matches, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {} completion_matches (@var{hint})
Generate possible completions given @var{hint}.

This function is provided for the benefit of programs like Emacs which
might be controlling Octave and handling user input.  The current
command number is not incremented when this function is called.  This is
a feature, not a bug.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval;

  std::string hint = args(0).string_value ();

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

      int len = list.numel ();

      for (int i = 0; i < len; i++)
        octave_stdout << list[i] << "\n";
    }

  octave_completion_matches_called = true;

  return retval;
}

/*
%!assert (ischar (completion_matches ("")))
%!assert (ischar (completion_matches ("a")))
%!assert (ischar (completion_matches (" ")))
%!assert (isempty (completion_matches (" ")))
%!assert (any (strcmp ("abs", deblank (cellstr (completion_matches (""))))))
%!assert (any (strcmp ("abs", deblank (cellstr (completion_matches ("a"))))))
%!assert (any (strcmp ("abs", deblank (cellstr (completion_matches ("ab"))))))
%!assert (any (strcmp ("abs", deblank (cellstr (completion_matches ("abs"))))))
%!assert (! any (strcmp ("abs", deblank (cellstr (completion_matches ("absa"))))))

%!error completion_matches ()
%!error completion_matches (1, 2)
*/

DEFUN (readline_read_init_file, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} readline_read_init_file (@var{file})
Read the readline library initialization file @var{file}.

If @var{file} is omitted, read the default initialization file
(normally @file{~/.inputrc}).

@xref{Readline Init File, , , readline, GNU Readline Library},
for details.
@seealso{readline_re_read_init_file}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin == 0)
    octave::command_editor::read_init_file ();
  else
    {
      std::string file = args(0).string_value ();

      octave::command_editor::read_init_file (file);
    }

  return ovl ();
}

DEFUN (readline_re_read_init_file, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} readline_re_read_init_file ()
Re-read the last readline library initialization file that was read.

@xref{Readline Init File, , , readline, GNU Readline Library},
for details.
@seealso{readline_read_init_file}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  octave::command_editor::re_read_init_file ();

  return ovl ();
}

static int
internal_input_event_hook_fcn (void)
{
  input_event_hook_functions.run ();

  if (input_event_hook_functions.empty ())
    octave::command_editor::remove_event_hook (internal_input_event_hook_fcn);

  return 0;
}

DEFUN (add_input_event_hook, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{id} =} add_input_event_hook (@var{fcn})
@deftypefnx {} {@var{id} =} add_input_event_hook (@var{fcn}, @var{data})
Add the named function or function handle @var{fcn} to the list of functions
to call periodically when Octave is waiting for input.

The function should have the form

@example
@var{fcn} (@var{data})
@end example

If @var{data} is omitted, Octave calls the function without any arguments.

The returned identifier may be used to remove the function handle from the
list of input hook functions.
@seealso{remove_input_event_hook}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value user_data;

  if (nargin == 2)
    user_data = args(1);

  hook_function hook_fcn (args(0), user_data);

  if (input_event_hook_functions.empty ())
    octave::command_editor::add_event_hook (internal_input_event_hook_fcn);

  input_event_hook_functions.insert (hook_fcn.id (), hook_fcn);

  return ovl (hook_fcn.id ());
}

DEFUN (remove_input_event_hook, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} remove_input_event_hook (@var{name})
@deftypefnx {} {} remove_input_event_hook (@var{fcn_id})
Remove the named function or function handle with the given identifier
from the list of functions to call periodically when Octave is waiting
for input.
@seealso{add_input_event_hook}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string hook_fcn_id = args(0).string_value ("remove_input_event_hook: argument not valid as a hook function name or id");

  bool warn = (nargin < 2);

  hook_function_list::iterator p
    = input_event_hook_functions.find (hook_fcn_id);

  if (p != input_event_hook_functions.end ())
    input_event_hook_functions.erase (p);
  else if (warn)
    warning ("remove_input_event_hook: %s not found in list",
             hook_fcn_id.c_str ());

  if (input_event_hook_functions.empty ())
    octave::command_editor::remove_event_hook (internal_input_event_hook_fcn);

  return ovl ();
}

DEFUN (PS1, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PS1 ()
@deftypefnx {} {@var{old_val} =} PS1 (@var{new_val})
@deftypefnx {} {} PS1 (@var{new_val}, "local")
Query or set the primary prompt string.

When executing interactively, Octave displays the primary prompt when it is
ready to read a command.

The default value of the primary prompt string is @qcode{"octave:\#> "}.
To change it, use a command like

@example
PS1 ("\\u@@\\H> ")
@end example

@noindent
which will result in the prompt @samp{boris@@kremvax> } for the user
@samp{boris} logged in on the host @samp{kremvax.kgb.su}.  Note that two
backslashes are required to enter a backslash into a double-quoted
character string.  @xref{Strings}.

You can also use ANSI escape sequences if your terminal supports them.
This can be useful for coloring the prompt.  For example,

@example
PS1 ('\[\033[01;31m\]\s:\#> \[\033[0m\]')
@end example

@noindent
will give the default Octave prompt a red coloring.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{PS2, PS4}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (PS1);
}

DEFUN (PS2, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PS2 ()
@deftypefnx {} {@var{old_val} =} PS2 (@var{new_val})
@deftypefnx {} {} PS2 (@var{new_val}, "local")
Query or set the secondary prompt string.

The secondary prompt is printed when Octave is expecting additional input to
complete a command.  For example, if you are typing a @code{for} loop that
spans several lines, Octave will print the secondary prompt at the beginning
of each line after the first.  The default value of the secondary prompt
string is @qcode{"> "}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{PS1, PS4}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (PS2);
}

DEFUN (PS4, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PS4 ()
@deftypefnx {} {@var{old_val} =} PS4 (@var{new_val})
@deftypefnx {} {} PS4 (@var{new_val}, "local")
Query or set the character string used to prefix output produced
when echoing commands is enabled.

The default value is @qcode{"+ "}.
@xref{Diary and Echo Commands}, for a description of echoing commands.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{echo, echo_executing_commands, PS1, PS2}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (PS4);
}

DEFUN (completion_append_char, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} completion_append_char ()
@deftypefnx {} {@var{old_val} =} completion_append_char (@var{new_val})
@deftypefnx {} {} completion_append_char (@var{new_val}, "local")
Query or set the internal character variable that is appended to
successful command-line completion attempts.

The default value is @qcode{" "} (a single space).

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (completion_append_char);
}

DEFUN (echo_executing_commands, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} echo_executing_commands ()
@deftypefnx {} {@var{old_val} =} echo_executing_commands (@var{new_val})
@deftypefnx {} {} echo_executing_commands (@var{new_val}, "local")
Query or set the internal variable that controls the echo state.

It may be the sum of the following values:

@table @asis
@item 1
Echo commands read from script files.

@item 2
Echo commands from functions.

@item 4
Echo commands read from command line.
@end table

More than one state can be active at once.  For example, a value of 3 is
equivalent to the command @kbd{echo on all}.

The value of @code{echo_executing_commands} may be set by the @kbd{echo}
command or the command line option @option{--echo-commands}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (echo_executing_commands);
}

DEFUN (__request_drawnow__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} __request_drawnow__ ()
@deftypefnx {} {} __request_drawnow__ (@var{flag})
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin == 0)
    Vdrawnow_requested = true;
  else
    Vdrawnow_requested = args(0).bool_value ();

  return ovl ();
}

DEFUN (__gud_mode__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __gud_mode__ ()
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value_list retval;

  if (nargin == 0)
    retval = ovl (Vgud_mode);
  else
    Vgud_mode = args(0).bool_value ();

  return retval;
}

DEFUN (filemarker, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} filemarker ()
@deftypefnx {} {@var{old_val} =} filemarker (@var{new_val})
@deftypefnx {} {} filemarker (@var{new_val}, "local")
Query or set the character used to separate the filename from the
subfunction names contained within the file.

By default this is the character @samp{>}.
This can be used in a generic manner to interact with subfunctions.
For example,

@example
help (["myfunc", filemarker, "mysubfunc"])
@end example

@noindent
returns the help string associated with the subfunction @code{mysubfunc}
located in the file @file{myfunc.m}.

@code{filemarker} is also useful during debugging for placing breakpoints
within subfunctions or nested functions.
For example,

@example
dbstop (["myfunc", filemarker, "mysubfunc"])
@end example

@noindent
will set a breakpoint at the first line of the subfunction @code{mysubfunc}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  char tmp = Vfilemarker;
  octave_value retval = SET_INTERNAL_VARIABLE (filemarker);

  // The character passed must not be a legal character for a function name
  if (::isalnum (Vfilemarker) || Vfilemarker == '_')
    {
      Vfilemarker = tmp;
      error ("filemarker: character can not be a valid character for a function name");
    }

  return retval;
}
