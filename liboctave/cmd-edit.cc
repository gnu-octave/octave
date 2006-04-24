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

#include <cstdlib>
#include <cstring>

#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "quit.h"

#include "cmd-edit.h"
#include "cmd-hist.h"
#include "lo-error.h"
#include "lo-utils.h"
#include "oct-env.h"
#include "oct-time.h"

command_editor *command_editor::instance = 0;

#if defined (USE_READLINE)

#include <cstdio>
#include <cstdlib>

#include "oct-rl-edit.h"

class
gnu_readline : public command_editor
{
public:

  typedef command_editor::startup_hook_fcn startup_hook_fcn;

  typedef command_editor::event_hook_fcn event_hook_hook_fcn;

  typedef command_editor::completion_fcn completion_fcn;

  gnu_readline (void);

  ~gnu_readline (void) { }

  void do_set_name (const std::string& n);

  std::string do_readline (const std::string& prompt, bool& eof);

  void do_set_input_stream (FILE *f);

  FILE *do_get_input_stream (void);

  void do_set_output_stream (FILE *f);

  FILE *do_get_output_stream (void);

  int do_terminal_rows (void);

  int do_terminal_cols (void);

  void do_clear_screen (void);

  void do_resize_terminal (void);

  std::string newline_chars (void);

  void do_restore_terminal_state (void);

  void do_blink_matching_paren (bool flag);

  void do_set_basic_word_break_characters (const std::string& s);

  void do_set_completer_word_break_characters (const std::string& s);

  void do_set_basic_quote_characters (const std::string& s);

  void do_set_completion_append_character (char c);

  void do_set_completion_function (completion_fcn f);

  completion_fcn do_get_completion_function (void) const;

  string_vector
  do_generate_filename_completions (const std::string& text);

  void do_insert_text (const std::string& text);

  void do_newline (void);

  void do_clear_undo_list (void);

  void do_set_startup_hook (startup_hook_fcn f);

  void do_restore_startup_hook (void);

  void do_set_event_hook (event_hook_fcn f);

  void do_restore_event_hook (void);

  void do_read_init_file (const std::string& file);

  bool do_filename_completion_desired (bool);

  static int operate_and_get_next (int, int);

  static int history_search_backward (int, int);

  static int history_search_forward (int, int);

private:

  startup_hook_fcn previous_startup_hook;

  event_hook_fcn previous_event_hook;

  completion_fcn completion_function;

  static char *command_generator (const char *text, int state);

  static char **command_completer (const char *text, int start, int end);
};

gnu_readline::gnu_readline ()
  : command_editor (), previous_startup_hook (0),
    previous_event_hook (0), completion_function (0)
{
  // FIXME -- need interface to rl_add_defun, rl_initialize, and
  // a function to set rl_terminal_name

  std::string term = octave_env::getenv ("TERM");

  octave_rl_set_terminal_name (term.c_str ());

  octave_rl_initialize ();

  do_blink_matching_paren (true);

  // Bind operate-and-get-next.

  octave_rl_add_defun ("operate-and-get-next",
		       gnu_readline::operate_and_get_next,
		       octave_rl_ctrl ('O'));

  // And the history search functions.

  octave_rl_add_defun ("history-search-backward",
		       gnu_readline::history_search_backward,
		       octave_rl_meta ('P'));

  octave_rl_add_defun ("history-search-forward",
		       gnu_readline::history_search_forward,
		       octave_rl_meta ('N'));
}



void
gnu_readline::do_set_name (const std::string& nm)
{
  ::octave_rl_set_name (nm.c_str ());
}

std::string
gnu_readline::do_readline (const std::string& prompt, bool& eof)
{
  std::string retval;

  eof = false;

  char *line = 0;

  const char *p = prompt.c_str ();

  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  line = ::octave_rl_readline (p);

  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  if (line)
    {
      retval = line;

      free (line);
    }
  else
    eof = true;

  return retval;
}

void
gnu_readline::do_set_input_stream (FILE *f)
{
  ::octave_rl_set_input_stream (f);
}

FILE *
gnu_readline::do_get_input_stream (void)
{
  return ::octave_rl_get_input_stream ();
}

void
gnu_readline::do_set_output_stream (FILE *f)
{
  ::octave_rl_set_output_stream (f);
}

FILE *
gnu_readline::do_get_output_stream (void)
{
  return ::octave_rl_get_output_stream ();
}

// GNU readline handles SIGWINCH, so these values have a good chance
// of being correct even if the window changes size (they may be
// wrong if, for example, the luser changes the window size while the
// pager is running, and the signal is handled by the pager instead of
// us.

int
gnu_readline::do_terminal_rows (void)
{
  int sh = ::octave_rl_screen_height ();

  return sh > 0 ? sh : 24;
}

int
gnu_readline::do_terminal_cols (void)
{
  int sw = ::octave_rl_screen_width ();

  return sw > 0 ? sw : 80;
}

void
gnu_readline::do_clear_screen (void)
{
  ::octave_rl_clear_screen ();
}

void
gnu_readline::do_resize_terminal (void)
{
  ::octave_rl_resize_terminal ();
}

std::string
gnu_readline::newline_chars (void)
{
  return "\r\n";
}

void
gnu_readline::do_restore_terminal_state (void)
{
  ::octave_rl_restore_terminal_state ();
}

void
gnu_readline::do_blink_matching_paren (bool flag)
{
  ::octave_rl_enable_paren_matching (flag ? 1 : 0);
}

void
gnu_readline::do_set_basic_word_break_characters (const std::string& s)
{
  ::octave_rl_set_basic_word_break_characters (s.c_str ());
}

void
gnu_readline::do_set_completer_word_break_characters (const std::string& s)
{
  ::octave_rl_set_completer_word_break_characters (s.c_str ());
}

void
gnu_readline::do_set_basic_quote_characters (const std::string& s)
{
  ::octave_rl_set_basic_quote_characters (s.c_str ());
}

void
gnu_readline::do_set_completion_append_character (char c)
{
  ::octave_rl_set_completion_append_character (c);
}

void
gnu_readline::do_set_completion_function (completion_fcn f)
{
  completion_function = f;

  rl_attempted_completion_fcn_ptr fp
    = f ? gnu_readline::command_completer : 0;

  ::octave_rl_set_completion_function (fp);
}

gnu_readline::completion_fcn
gnu_readline::do_get_completion_function (void) const
{
  return completion_function;
}

string_vector
gnu_readline::do_generate_filename_completions (const std::string& text)
{
  string_vector retval;

  int n = 0;
  int count = 0;

  char *fn = 0;

  while (1)
    {
      fn = ::octave_rl_filename_completion_function (text.c_str (), count);

      if (fn)
	{
	  if (count == n)
	    {
	      // Famous last words:  Most large directories will not
	      // have more than a few hundred files, so we should not
	      // resize too many times even if the growth is linear...

	      n += 100;
	      retval.resize (n);
	    }

	  retval[count++] = fn;

	  free (fn);
	}
      else
	break;
    }

  retval.resize (count);

  return retval;
}

void
gnu_readline::do_insert_text (const std::string& text)
{
  ::octave_rl_insert_text (text.c_str ());
}

void
gnu_readline::do_newline (void)
{
  ::octave_rl_newline ();
}

void
gnu_readline::do_clear_undo_list ()
{
  ::octave_rl_clear_undo_list ();
}

void
gnu_readline::do_set_startup_hook (startup_hook_fcn f)
{
  previous_startup_hook = ::octave_rl_get_startup_hook ();

  ::octave_rl_set_startup_hook (f);
}

void
gnu_readline::do_restore_startup_hook (void)
{
  ::octave_rl_set_startup_hook (previous_startup_hook);
}

void
gnu_readline::do_set_event_hook (event_hook_fcn f)
{
  previous_event_hook = octave_rl_get_event_hook ();

  ::octave_rl_set_event_hook (f);
}

void
gnu_readline::do_restore_event_hook (void)
{
  ::octave_rl_set_event_hook (previous_event_hook);
}

void
gnu_readline::do_read_init_file (const std::string& file)
{
  ::octave_rl_read_init_file (file.c_str ());
}

bool
gnu_readline::do_filename_completion_desired (bool arg)
{
  return ::octave_rl_filename_completion_desired (arg);
}

int
gnu_readline::operate_and_get_next (int /* count */, int /* c */)
{
  // Accept the current line.

  command_editor::newline ();

  // Find the current line, and find the next line to use.

  int x_where = command_history::where ();

  int x_length = command_history::length ();

  if ((command_history::is_stifled ()
       && (x_length >= command_history::max_input_history ()))
      || (x_where >= x_length - 1))
    command_history::set_mark (x_where);
  else
    command_history::set_mark (x_where + 1);

  command_editor::set_startup_hook (command_history::goto_mark);

  return 0;
}

int
gnu_readline::history_search_backward (int count, int c)
{
  return octave_rl_history_search_backward (count, c);
}

int
gnu_readline::history_search_forward (int count, int c)
{
  return octave_rl_history_search_forward (count, c);
}

char *
gnu_readline::command_generator (const char *text, int state)
{
  char *retval = 0;

  completion_fcn f = command_editor::get_completion_function ();

  std::string tmp = f (text, state);

  size_t len = tmp.length ();

  if (len > 0)
    {
      retval = static_cast<char *> (malloc (len+1));

      strcpy (retval, tmp.c_str ());
    }

  return retval;
}

char **
gnu_readline::command_completer (const char *text, int, int)
{
  char **matches = 0;
  matches
    = ::octave_rl_completion_matches (text, gnu_readline::command_generator);
  return matches;
}

#endif

class
default_command_editor : public command_editor
{
public:

  default_command_editor (void)
    : command_editor (), input_stream (stdin), output_stream (stdout) { }

  ~default_command_editor (void) { }

  std::string do_readline (const std::string& prompt, bool& eof);

  void do_set_input_stream (FILE *f);

  FILE *do_get_input_stream (void);

  void do_set_output_stream (FILE *f);

  FILE *do_get_output_stream (void);

  string_vector do_generate_filename_completions (const std::string& text);

  void do_insert_text (const std::string&);

  void do_newline (void);

private:

  FILE *input_stream;

  FILE *output_stream;
};

std::string
default_command_editor::do_readline (const std::string& prompt, bool& eof)
{
  fprintf (output_stream, prompt.c_str ());
  fflush (output_stream);

  return octave_fgetl (input_stream, eof);
}

void
default_command_editor::do_set_input_stream (FILE *f)
{
  input_stream = f;
}

FILE *
default_command_editor::do_get_input_stream (void)
{
  return input_stream;
}

void
default_command_editor::do_set_output_stream (FILE *f)
{
  output_stream = f;
}

FILE *
default_command_editor::do_get_output_stream (void)
{
  return output_stream;
}

string_vector
default_command_editor::do_generate_filename_completions (const std::string&)
{
  // FIXME
  return string_vector ();
}

void
default_command_editor::do_insert_text (const std::string&)
{
  // FIXME
}

void
default_command_editor::do_newline (void)
{
  // FIXME
}

bool
command_editor::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    make_command_editor ();

  if (! instance)
    {
      current_liboctave_error_handler
	("unable to create command history object!");

      retval = false;
    }

  return retval;
}

void
command_editor::make_command_editor (void)
{
#if defined (USE_READLINE)
  instance = new gnu_readline ();
#else
  instance = new default_command_editor ();
#endif
}

void
command_editor::set_name (const std::string& n)
{
  if (instance_ok ())
    instance->do_set_name (n);
}

std::string
command_editor::readline (const std::string& prompt)
{
  bool eof;

  return readline (prompt, eof);
}

std::string
command_editor::readline (const std::string& prompt, bool& eof)
{
  return (instance_ok ())
    ? instance->do_readline (prompt, eof) : std::string ();
}

void
command_editor::set_input_stream (FILE *f)
{
  if (instance_ok ())
    instance->do_set_input_stream (f);
}

FILE *
command_editor::get_input_stream (void)
{
  return (instance_ok ())
    ? instance->do_get_input_stream () : 0;
}

void
command_editor::set_output_stream (FILE *f)
{
  if (instance_ok ())
    instance->do_set_output_stream (f);
}

FILE *
command_editor::get_output_stream (void)
{
  return (instance_ok ())
    ? instance->do_get_output_stream () : 0;
}

int
command_editor::terminal_rows (void)
{
  return (instance_ok ())
    ? instance->do_terminal_rows () : -1;
}

int
command_editor::terminal_cols (void)
{
  return (instance_ok ())
    ? instance->do_terminal_cols () : -1;
}

void
command_editor::clear_screen (void)
{
  if (instance_ok ())
    instance->do_clear_screen ();
}

void
command_editor::resize_terminal (void)
{
  if (instance_ok ())
    instance->do_resize_terminal ();
}

std::string
command_editor::decode_prompt_string (const std::string& s)
{
  return (instance_ok ())
    ? instance->do_decode_prompt_string (s) : std::string ();
}

int
command_editor::current_command_number (void)
{
  return (instance_ok ())
    ? instance->command_number : 0;
}

void
command_editor::reset_current_command_number (int n)
{
  if (instance_ok ())
    instance->command_number = n;
}

void
command_editor::increment_current_command_number (void)
{
  if (instance_ok ())
    instance->command_number++;
}

void
command_editor::restore_terminal_state (void)
{
  if (instance_ok ())
    instance->do_restore_terminal_state ();
}

void
command_editor::blink_matching_paren (bool flag)
{
  if (instance_ok ())
    instance->do_blink_matching_paren (flag);
}

void
command_editor::set_basic_word_break_characters (const std::string& s)
{
  if (instance_ok ())
    instance->do_set_basic_word_break_characters (s);
}

void
command_editor::set_completer_word_break_characters (const std::string& s)
{
  if (instance_ok ())
    instance->do_set_completer_word_break_characters (s);
}

void
command_editor::set_basic_quote_characters (const std::string& s)
{
  if (instance_ok ())
    instance->do_set_basic_quote_characters (s);
}

void
command_editor::set_completion_append_character (char c)
{
  if (instance_ok ())
    instance->do_set_completion_append_character (c);
}

void
command_editor::set_completion_function (completion_fcn f)
{
  if (instance_ok ())
    instance->do_set_completion_function (f);
}

command_editor::completion_fcn
command_editor::get_completion_function (void)
{
  return (instance_ok ())
    ? instance->do_get_completion_function () : 0;
}

string_vector
command_editor::generate_filename_completions (const std::string& text)
{
  return (instance_ok ())
    ? instance->do_generate_filename_completions (text) : string_vector ();
}

void
command_editor::insert_text (const std::string& text)
{
  if (instance_ok ())
    instance->do_insert_text (text);
}

void
command_editor::newline (void)
{
  if (instance_ok ())
    instance->do_newline ();
}

void
command_editor::clear_undo_list (void)
{
  if (instance_ok ())
    instance->do_clear_undo_list ();
}

void
command_editor::set_startup_hook (startup_hook_fcn f)
{
  if (instance_ok ())
    instance->do_set_startup_hook (f);
}

void
command_editor::restore_startup_hook (void)
{
  if (instance_ok ())
    instance->do_restore_startup_hook ();
}

void
command_editor::set_event_hook (event_hook_fcn f)
{
  if (instance_ok ())
    instance->do_set_event_hook (f);
}

void
command_editor::restore_event_hook (void)
{
  if (instance_ok ())
    instance->do_restore_event_hook ();
}

void
command_editor::read_init_file (const std::string& file)
{
  if (instance_ok ())
    instance->do_read_init_file (file);
}

bool
command_editor::filename_completion_desired (bool arg)
{
  return (instance_ok ())
    ? instance->do_filename_completion_desired (arg) : false;
}

// Return a string which will be printed as a prompt.  The string may
// contain special characters which are decoded as follows: 
//   
//	\a	bell (ascii 07)
//	\d	the date
//	\e	escape (ascii 033)
//	\h	the hostname up to the first `.'
//	\H	the hostname
//	\n	CRLF
//	\r	CR
//	\s	the name of the shell (program)
//	\t	the time
//	\T	the time in 12-hour hh:mm:ss format
//	\@	the time in 12-hour hh:mm am/pm format
//	\A	the time in 24-hour hh:mm format
//	\u	your username
//	\w	the current working directory
//	\W	the last element of PWD
//	\!	the history number of this command
//	\#	the command number of this command
//	\$	a $ or a # if you are root
//	\nnn    character code nnn in octal
//	\\	a backslash
//	\[	begin a sequence of non-printing chars
//	\]	end a sequence of non-printing chars

std::string
command_editor::do_decode_prompt_string (const std::string& s)
{
  std::string result;
  std::string temp;
  size_t i = 0;
  size_t slen = s.length ();
  int c;

  while (i < slen)
    {
      c = s[i];

      i++;

      if (c == '\\')
	{
	  c = s[i];

	  switch (c)
	    {
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	      // Maybe convert an octal number.
	      {
		int n = read_octal (s.substr (i, 3));

		temp = "\\";

		if (n != -1)
		  {
		    i += 3;
		    temp[0] = n;
		  }

		c = 0;
		goto add_string;
	      }

	    case 'a':
	      {
		temp = '\a';

		goto add_string;
	      }

	    case 'e':
	      {
		temp = '\033';

		goto add_string;
	      }

	    case 'r':
	      {
		temp = '\r';

		goto add_string;
	      }

	    case 'd':
	    case 't':
	    case 'T':
	    case '@':
	    case 'A':
	      // Make the current time/date into a string.
	      {
		octave_localtime now;

		if (c == 'd')
		  temp = now.strftime ("%a %b %d");
		else if (c == 't')
		  temp = now.strftime ("%H:%M:%S");
		else if (c == 'T')
		  temp = now.strftime ("%I:%M:%S");
		else if (c == '@')
		  temp = now.strftime ("%I:%M %p");
		else if (c == 'A')
		  temp = now.strftime ("%H:%M");

		goto add_string;
	      }

	    case 'n':
	      {
		temp = newline_chars ();

		goto add_string;
	      }

	    case 's':
	      {
		temp = octave_env::get_program_name ();
		temp = octave_env::base_pathname (temp);

		goto add_string;
	      }

	    case 'w':
	    case 'W':
	      {
		temp = octave_env::getcwd ();

		std::string home_dir = octave_env::get_home_directory ();

		if (c == 'W' && (home_dir.empty () || temp != home_dir))
		  {
		    if (temp != "/" && temp != "//")
		      {
			size_t pos = temp.rfind ('/');

			if (pos != NPOS && pos != 0)
			  temp = temp.substr (pos + 1);
		      }
		  }
		else
		  temp = octave_env::polite_directory_format (temp);

		goto add_string;
	      }

	    case 'u':
	      {
		temp = octave_env::get_user_name ();

		goto add_string;
	      }

	    case 'H':
	      {
		temp = octave_env::get_host_name ();

		goto add_string;
	      }

	    case 'h':
	      {
		temp = octave_env::get_host_name ();

		size_t pos = temp.find ('.');

		if (pos != NPOS)
		  temp.resize (pos);
		
		goto add_string;
	      }

	    case '#':
	      {
		char number_buffer[128];
		sprintf (number_buffer, "%d", command_number);
		temp = number_buffer;

		goto add_string;
	      }

	    case '!':
	      {
		char number_buffer[128];
		int num = command_history::current_number ();
		if (num > 0)
                  sprintf (number_buffer, "%d", num);
		else
		  strcpy (number_buffer, "!");
		temp = number_buffer;

		goto add_string;
	      }

	    case '$':
	      {
#if defined (HAVE_GETEUID)
		temp = (::geteuid () == 0 ? "#" : "$");
#else
		temp = "$";
#endif

		goto add_string;
	      }

#if defined (USE_READLINE)
	    case '[':
	    case ']':
	      {
		temp.resize (2);

		temp[0] = '\001';
		temp[1] = ((c == '[')
			   ? ::octave_rl_prompt_start_ignore ()
			   : ::octave_rl_prompt_end_ignore ());

		goto add_string;
	      }
#endif

	    case '\\':
	      {
		temp = "\\";

		goto add_string;
	      }

	    default:
	      {
		temp = "\\ ";
		temp[1] = c;

		goto add_string;
	      }

	    add_string:
	      {
		if (c)
		  i++;

		result.append (temp);

		break;
	      }
	    }
	}
      else
	result += c;
    }

  return result;
}

// Return the octal number parsed from STRING, or -1 to indicate that
// the string contained a bad number.

int
command_editor::read_octal (const std::string& s)
{
  int result = 0;
  int digits = 0;

  size_t i = 0;
  size_t slen = s.length ();

  while (i < slen && s[i] >= '0' && s[i] < '8')
    {
      digits++;
      result = (result * 8) + s[i] - '0';
      i++;
    }

  if (! digits || result > 0777 || i < slen)
    result = -1;

  return result;
}

void
command_editor::error (int err_num)
{
  current_liboctave_error_handler ("%s", strerror (err_num));
}

void
command_editor::error (const std::string& s)
{
  current_liboctave_error_handler ("%s", s.c_str ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
