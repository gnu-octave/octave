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

#include <cstring>
#include <ctime>

#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-edit.h"
#include "cmd-hist.h"
#include "lo-error.h"
#include "lo-utils.h"
#include "oct-env.h"

command_editor *command_editor::instance = 0;

#if defined (USE_READLINE)

#include <cstdio>
#include <cstdlib>

#include <readline/readline.h>

// It would be nice if readline.h declared these, I think.

extern int rl_blink_matching_paren;

extern int screenheight;

extern int screenwidth;

class
gnu_readline : public command_editor
{
public:

  typedef command_editor::fcn fcn;

  typedef command_editor::completion_fcn completion_fcn;

  gnu_readline (void);

  ~gnu_readline (void) { }

  void do_set_name (const string& n);

  string do_readline (const string& prompt);

  void do_set_input_stream (FILE *f);

  FILE *do_get_input_stream (void);

  void do_set_output_stream (FILE *f);

  FILE *do_get_output_stream (void);

  int do_terminal_rows (void);

  int do_terminal_cols (void);

  void do_clear_screen (void);

  string newline_chars (void);

  void do_restore_terminal_state (void);

  void do_blink_matching_paren (bool flag);

  void do_set_basic_quote_characters (const string& s);

  void do_set_completion_append_character (char c);

  void do_set_completion_function (completion_fcn f);

  completion_fcn do_get_completion_function (void) const;

  void do_insert_text (const string& text);

  void do_newline (void);

  void do_clear_undo_list (void);

  void do_set_startup_hook (fcn f);

  void do_restore_startup_hook (void);

  void do_set_event_hook (fcn f);

  void do_restore_event_hook (void);

  void do_read_init_file (const string& file);

  static void operate_and_get_next (int, int);

private:

  fcn previous_startup_hook;

  fcn previous_event_hook;

  completion_fcn completion_function;

  static char *command_generator (const char *text, int state);

  static char **command_completer (char *text, int start, int end);
};

gnu_readline::gnu_readline ()
  : command_editor (), previous_startup_hook (0),
    previous_event_hook (0), completion_function (0)
{
  rl_initialize ();

  do_blink_matching_paren (true);

  // Bind operate-and-get-next.

  rl_add_defun ("operate-and-get-next",
		gnu_readline::operate_and_get_next, CTRL ('O'));

  // And the history search functions.

  rl_add_defun ("history-search-backward",
		rl_history_search_backward, META ('p'));

  rl_add_defun ("history-search-forward",
		rl_history_search_forward, META ('n'));
}

void
gnu_readline::do_set_name (const string& n)
{
  static char *nm = 0;

  delete [] nm;

  nm = strsave (n.c_str ());

  rl_readline_name = nm;

  // Since we've already called rl_initialize, we need to re-read the
  // init file to take advantage of the conditional parsing feature
  // based on rl_readline_name;

  rl_re_read_init_file ();
}

string
gnu_readline::do_readline (const string& prompt)
{
  string retval;

  char *line = ::readline (prompt.c_str ());

  if (line)
    {
      retval = line;

      free (line);
    }

  return retval;
}

void
gnu_readline::do_set_input_stream (FILE *f)
{
  rl_instream = f;
}

FILE *
gnu_readline::do_get_input_stream (void)
{
  return rl_instream;
}

void
gnu_readline::do_set_output_stream (FILE *f)
{
  rl_outstream = f;
}

FILE *
gnu_readline::do_get_output_stream (void)
{
  return rl_outstream;
}

// GNU readline handles SIGWINCH, so these values have a good chance
// of being correct even if the window changes size (they may be
// wrong if, for example, the luser changes the window size while the
// pager is running, and the signal is handled by the pager instead of
// us.

int
gnu_readline::do_terminal_rows (void)
{
  return screenheight > 0 ? screenheight : 24;
}

int
gnu_readline::do_terminal_cols (void)
{
  return screenwidth > 0 ? screenwidth : 80;
}

void
gnu_readline::do_clear_screen (void)
{
  rl_clear_screen ();
}

string
gnu_readline::newline_chars (void)
{
  return "\r\n";
}

void
gnu_readline::do_restore_terminal_state (void)
{
  if (rl_deprep_term_function)
    rl_deprep_term_function ();
}

void
gnu_readline::do_blink_matching_paren (bool flag)
{
  rl_blink_matching_paren = flag ? 1 : 0;
}

void
gnu_readline::do_set_basic_quote_characters (const string& s)
{
  static char *ss = 0;

  delete [] ss;

  ss = strsave (s.c_str ());

  rl_basic_quote_characters = ss;
}

void
gnu_readline::do_set_completion_append_character (char c)
{
  rl_completion_append_character = c;
}

void
gnu_readline::do_set_completion_function (completion_fcn f)
{
  completion_function = f;

  typedef char** (*foo) (...);

  rl_attempted_completion_function
    = completion_function
    ? reinterpret_cast<foo> (gnu_readline::command_completer) : 0;
}

gnu_readline::completion_fcn
gnu_readline::do_get_completion_function (void) const
{
  return completion_function;
}

void
gnu_readline::do_insert_text (const string& text)
{
  rl_insert_text (text.c_str ());
}

void
gnu_readline::do_newline (void)
{
  rl_newline ();
}

void
gnu_readline::do_clear_undo_list ()
{
  if (rl_undo_list)
    {
      free_undo_list ();

      rl_undo_list = 0;
    }
}

void
gnu_readline::do_set_startup_hook (fcn f)
{
  previous_startup_hook = rl_startup_hook;

  rl_startup_hook = f;
}

void
gnu_readline::do_restore_startup_hook (void)
{
  rl_startup_hook = previous_startup_hook;
}

void
gnu_readline::do_set_event_hook (fcn f)
{
  previous_event_hook = rl_event_hook;

  rl_event_hook = f;
}

void
gnu_readline::do_restore_event_hook (void)
{
  rl_event_hook = previous_event_hook;
}

void
gnu_readline::do_read_init_file (const string& file)
{
  if (file.empty ())
    rl_re_read_init_file ();
  else
    rl_read_init_file (file.c_str ());
}

void
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
}

char *
gnu_readline::command_generator (const char *text, int state)
{
  char *retval = 0;

  completion_fcn f = command_editor::get_completion_function ();

  string tmp = f (text, state);

  size_t len = tmp.length ();

  if (len > 0)
    {
      retval = static_cast<char *> (malloc (len+1));

      strcpy (retval, tmp.c_str ());
    }

  return retval;
}

char **
gnu_readline::command_completer (char *text, int /* start */, int /* end */)
{
  char **matches = 0;
  matches = completion_matches (text, gnu_readline::command_generator);
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

  string do_readline (const string& prompt);

  void do_set_input_stream (FILE *f);

  FILE *do_get_input_stream (void);

  void do_set_output_stream (FILE *f);

  FILE *do_get_output_stream (void);

  void do_insert_text (const string&);

  void do_newline (void);

private:

  FILE *input_stream;

  FILE *output_stream;
};

string
default_command_editor::do_readline (const string& prompt)
{
  fprintf (output_stream, prompt.c_str ());
  fflush (output_stream);

  return octave_fgets (input_stream);
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

void
default_command_editor::do_insert_text (const string&)
{
  // XXX FIXME XXX
}

void
default_command_editor::do_newline (void)
{
  // XXX FIXME XXX
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
command_editor::set_name (const string& n)
{
  if (instance_ok ())
    instance->do_set_name (n);
}

string
command_editor::readline (const string& prompt)
{
  return (instance_ok ())
    ? instance->do_readline (prompt) : string ();
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

string
command_editor::decode_prompt_string (const string& s)
{
  return (instance_ok ())
    ? instance->do_decode_prompt_string (s) : string ();
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
command_editor::set_basic_quote_characters (const string& s)
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

void
command_editor::insert_text (const string& text)
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
command_editor::set_startup_hook (fcn f)
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
command_editor::set_event_hook (fcn f)
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
command_editor::read_init_file (const string& file)
{
  if (instance_ok ())
    instance->do_read_init_file (file);
}

// Return a string which will be printed as a prompt.  The string may
// contain special characters which are decoded as follows: 
//   
//	\t	the time
//	\d	the date
//	\n	CRLF
//	\s	the name of the shell (program)
//	\w	the current working directory
//	\W	the last element of PWD
//	\u	your username
//	\h	the hostname
//	\#	the command number of this command
//	\!	the history number of this command
//	\$	a $ or a # if you are root
//	\<octal> character code in octal
//	\\	a backslash

string
command_editor::do_decode_prompt_string (const string& s)
{
  string result;
  string temp;
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
	  
	    case 't':
	    case 'd':
	      // Make the current time/date into a string.
	      {
		time_t now = time (0);

		temp = ctime (&now);

		if (c == 't')
		  {
		    temp = temp.substr (11);
		    temp.resize (8);
		  }
		else
		  temp.resize (10);

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

		if (c == 'W')
		  {
		    size_t pos = temp.rfind ('/');

		    if (pos != NPOS && pos != 0)
		      temp = temp.substr (pos + 1);
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
		temp = (::geteuid () == 0 ? "#" : "$");

		goto add_string;
	      }

#if defined (USE_READLINE)
	    case '[':
	    case ']':
	      {
		temp.resize (2);

		temp[0] = '\001';
		temp[1] = ((c == '[')
			   ? RL_PROMPT_START_IGNORE
			   : RL_PROMPT_END_IGNORE);

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
command_editor::read_octal (const string& s)
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
command_editor::error (const string& s)
{
  current_liboctave_error_handler ("%s", s.c_str ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
