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

/*

The 2 functions listed below were adapted from similar functions
from GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991
Free Software Foundation, Inc.

  read_octal    decode_prompt_string

*/

// Use the GNU readline library for command line editing and hisory.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <ctime>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <csignal>

#include <string>

#include <iostream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

// This must come before anything that includes iostream.h...
// (This is apparently no longer true...)

#include "readline/readline.h"
#include "readline/history.h"

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
#include "utils.h"
#include "variables.h"

// Primary prompt string.
static string Vps1;

// Secondary prompt string.
static string Vps2;

// String printed before echoed input (enabled by --echo-input).
string Vps4;

// Character to append after successful command-line completion attempts.
static char Vcompletion_append_char;

// Global pointer for eval().
string current_eval_string;

// Nonzero means get input from current_eval_string.
int get_input_from_eval_string = 0;

// Nonzero means we're parsing a function file.
int reading_fcn_file = 0;

// Simple name of function file we are reading.
string curr_fcn_file_name;

// Full name of file we are reading.
string curr_fcn_file_full_name;

// Nonzero means we're parsing a script file.
int reading_script_file = 0;

// If we are reading from an M-file, this is it.
FILE *ff_instream = 0;

// Nonzero means this is an interactive shell.
int interactive = 0;

// Nonzero means the user forced this shell to be interactive (-i).
int forced_interactive = 0;

// Should we issue a prompt?
int promptflag = 1;

// The current line of input, from wherever.
string current_input_line;

// Return the octal number parsed from STRING, or -1 to indicate that
// the string contained a bad number.

static int
read_octal (const string& s)
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

static string
decode_prompt_string (const string& s)
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
		if (using_readline)
		  temp = "\r\n";
		else
		  temp = "\n";

		goto add_string;
	      }

	    case 's':
	      {
		temp = base_pathname (Vprogram_name);

		goto add_string;
	      }
	
	    case 'w':
	    case 'W':
	      {
#define EFFICIENT
#ifdef EFFICIENT
		// Use the value of PWD because it is much more
		// effecient.

		temp = Vcurrent_directory;

		if (temp.empty ())
		  temp = octave_getcwd ();
#else
		temp = octave_getcwd ();
#endif	/* EFFICIENT */

		if (c == 'W')
		  {
		    size_t pos = temp.rfind ('/');

		    if (pos != NPOS && pos != 0)
		      temp = temp.substr (pos + 1);
		  }
		else
		  temp = polite_directory_format (temp);

		goto add_string;
	      }
      
	    case 'u':
	      {
		temp = Vuser_name;

		goto add_string;
	      }

	    case 'h':
	      {
		temp = Vhost_name;

		size_t pos = temp.find ('.');

		if (pos != NPOS)
		  temp.resize (pos);
		
		goto add_string;
	      }

	    case '#':
	      {
		char number_buffer[128];
		sprintf (number_buffer, "%d", current_command_number);
		temp = number_buffer;

		goto add_string;
	      }

	    case '!':
	      {
		char number_buffer[128];
		int num = octave_command_history.current_number ();
		if (num > 0)
                  sprintf (number_buffer, "%d", num);
		else
		  strcpy (number_buffer, "!");
		temp = number_buffer;

		goto add_string;
	      }

	    case '$':
	      {
		temp = (geteuid () == 0 ? "#" : "$");

		goto add_string;
	      }

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

static void
do_input_echo (const string& input_string)
{
  int do_echo = reading_script_file ?
    (Vecho_executing_commands & ECHO_SCRIPTS)
      : (Vecho_executing_commands & ECHO_CMD_LINE);

  if (do_echo)
    {
      if (forced_interactive)
	{
	  if (promptflag > 0)
	    octave_stdout << decode_prompt_string (Vps1);
	  else
	    octave_stdout << decode_prompt_string (Vps2);
	}
      else
	octave_stdout << decode_prompt_string (Vps4);

      if (! input_string.empty ())
	{
	  octave_stdout << input_string;

	  if (input_string[input_string.length () - 1] != '\n')
	    octave_stdout << "\n";
	}
    }
}

char *
gnu_readline (const char *s)
{
  char *retval = 0;

  if (using_readline)
    {
      char *tmp = retval = ::readline (s);

      if (tmp && strlen (tmp) == 0)
	{
	  retval = (char *) malloc (2);
	  retval[0] = '\n';
	  retval[1] = '\0';
	}
    }
  else
    {
      if (s && *s && (interactive || forced_interactive))
	fprintf (rl_outstream, s);

      FILE *curr_stream = rl_instream;
      if (reading_fcn_file || reading_script_file)
	curr_stream = ff_instream;

      int grow_size = 1024;
      int max_size = grow_size;

      char *buf = (char *) malloc (max_size);
      char *bufptr = buf;

      do
	{
	  if (fgets (bufptr, grow_size, curr_stream))
	    {
	      int len = strlen (bufptr);

	      if (len == grow_size - 1)
		{
		  int tmp = bufptr - buf + grow_size - 1;
		  grow_size *= 2;
		  max_size += grow_size;
		  buf = (char *) realloc (buf, max_size);
		  bufptr = buf + tmp;

		  if (*(bufptr-1) == '\n')
		    {
		      *bufptr = '\0';
		      retval = buf;
		    }
		}
	      else if (bufptr[len-1] != '\n')
		{
		  bufptr[len++] = '\n';
		  bufptr[len] = '\0';
		  retval = buf;
		}
	      else
		retval = buf;
	    }
	  else
	    break;
	}
      while (! retval);
    }

  return retval;
}

static char *
octave_gets (void)
{
  char *retval = 0;

  if ((interactive || forced_interactive)
      && (! (reading_fcn_file || reading_script_file)))
    {
      const char *ps = (promptflag > 0) ? Vps1.c_str () :
	Vps2.c_str ();

      string prompt = decode_prompt_string (ps);

      if (interactive)
	{
	  pipe_handler_error_count = 0;
	  flush_octave_stdout ();
	}

      octave_diary << prompt;

      retval = gnu_readline (prompt.c_str ());
    }
  else
    retval = gnu_readline ("");

  if (retval)
    current_input_line = retval;
  else
    current_input_line = "";

  if (! current_input_line.empty ())
    {
      if (! input_from_startup_file)
	octave_command_history.add (current_input_line);

      octave_diary << current_input_line;

      do_input_echo (current_input_line);
    }

  octave_diary << "\n";
  
  return retval;
}

// Read a line from the input stream.

static char *
get_user_input (void)
{
  char *retval = 0;

  if (get_input_from_eval_string)
    {
      size_t len = current_eval_string.length ();

      retval = (char *) malloc (len + 2);

      strcpy (retval, current_eval_string.c_str ());

      retval[len++] = '\n';
      retval[len] = '\0';    // Paranoia.
    }
  else
    retval = octave_gets ();

  if (retval)
    current_input_line = retval;

  if (! get_input_from_eval_string)
    input_line_number++;

  return retval;
}

int
octave_read (char *buf, unsigned max_size)
{
  static char *input_buf = 0;
  static char *cur_pos = 0;
  static int chars_left = 0;

  int status = 0;

  if (! input_buf)
    {
      cur_pos = input_buf = get_user_input ();

      chars_left = input_buf ? strlen (input_buf) : 0;
    }

  if (chars_left > 0)
    {
      buf[0] = '\0';

      int len = max_size - 2;

      strncpy (buf, cur_pos, len);

      if (chars_left > len)
	{
	  chars_left -= len;

	  cur_pos += len;

	  buf[len] = '\0';

	  status = len;
	}
      else
	{
	  free (input_buf);
	  input_buf = 0;

	  len = chars_left;

	  if (buf[len-1] != '\n')
	    buf[len++] = '\n';

	  buf[len] = '\0';

	  status = len;
	}
    }
  else if (chars_left == 0)
    status = 0;
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
    rl_instream = instream;

  return instream;
}

// Fix things up so that input can come from the standard input.  This
// may need to become much more complicated, which is why it's in a
// separate function.

FILE *
get_input_from_stdin (void)
{
  rl_instream = stdin;
  return rl_instream;
}

static const char **
generate_struct_completions (const char *text, char *& prefix,
			     char *& hint)
{
  const char **names = 0;

  assert (text);

  char *id = strsave (text);
  char *ptr = strchr (id, '.');
  *ptr = '\0';

  char *elts = ptr + 1;
  ptr = strrchr (elts, '.');
  if (ptr)
    *ptr = '\0';
  else
    elts = 0;

  prefix = strsave (text);
  ptr = strrchr (prefix, '.');
  *ptr = '\0';

  delete [] hint;
  hint = strsave (ptr + 1);

  symbol_record *sr = curr_sym_tab->lookup (id, 0, 0);
  if (! sr)
    sr = global_sym_tab->lookup (id, 0, 0);

  if (sr && sr->is_defined ())
    {
      tree_fvc *tmp_fvc = sr->def ();

      octave_value *def = 0;
      if (tmp_fvc->is_constant ())
	def = (octave_value *) tmp_fvc;

      if (def && def->is_map ())
	{
	  string_vector tmp_names;

	  if (elts && *elts)
	    {
	      octave_value ult = def->lookup_map_element (elts, 0, 1);

	      if (ult.is_map ())
		{
		  Octave_map m = ult.map_value ();
		  tmp_names = m.make_name_list ();
		}
	    }
	  else
	    {
	      Octave_map m = def->map_value ();
	      tmp_names = m.make_name_list ();
	    }

	  int n = tmp_names.length ();

	  if (n > 0)
	    {
	      names = new const char * [n+1];
	      for (int i = 0; i < n; i++)
		names[i] = strsave (tmp_names[i].c_str ());
	      names[n] = 0;
	    }
	}
    }

  delete [] id;

  return names;
}

// XXX FIXME XXX -- make this generate file names when appropriate.

static const char **
generate_possible_completions (const char *text, char *& prefix,
			       char *& hint)
{
  const char **names = 0;

  prefix = 0;

  if (text && *text && *text != '.' && strrchr (text, '.'))
    names = generate_struct_completions (text, prefix, hint);
  else
    {
      string_vector tmp_names = make_name_list ();

      int n = tmp_names.length ();

      if (n > 0)
	{
	  names = new const char * [n+1];
	  for (int i = 0; i < n; i++)
	    names[i] = strsave (tmp_names[i].c_str ());
	  names[n] = 0;
	}
    }

  return names;
}

static int
looks_like_struct (const char *nm)
{
  int retval = 0;

  assert (nm);

  char *id = strsave (nm);
  char *elts = 0;
  char *ptr = strchr (id, '.');
  if (ptr)
    {
      *ptr = '\0';
      elts = ptr + 1;
    }

  symbol_record *sr = curr_sym_tab->lookup (id, 0, 0);
  if (! sr)
    sr = global_sym_tab->lookup (id, 0, 0);

  if (sr && sr->is_defined ())
    {
      tree_fvc *tmp_fvc = sr->def ();

      octave_value *def = 0;
      if (tmp_fvc->is_constant ())
	def = (octave_value *) tmp_fvc;

      if (def && def->is_map ())
	{
	  if (elts && *elts)
	    {
	      octave_value ult = def->lookup_map_element (elts, 0, 1);

	      if (ult.is_map ())
		retval = 1;
	    }
	  else
	    retval = 1;
	}
    }

  delete [] id;

  return retval;	
}

static char *
command_generator (const char *text, int state)
{
  static char *prefix = 0;
  static char *hint = 0;

  static int prefix_len = 0;
  static int hint_len = 0;

  static int list_index = 0;
  static const char **name_list = 0;

  static int matches = 0;

  if (state == 0)
    {
      list_index = 0;

      if (name_list)
	{
	  const char **ptr = name_list;

	  while (ptr && *ptr)
	    delete [] *ptr++;

	  delete [] name_list;

	  name_list = 0;
	}

      delete [] prefix;
      prefix = 0;

      delete [] hint;
      hint = strsave (text);

      name_list = generate_possible_completions (text, prefix, hint);

      prefix_len = 0;
      if (prefix)
	prefix_len = strlen (prefix);
	
      assert (hint);
      hint_len = strlen (hint);

      matches = 0;
      if (name_list)
	{
	  int i = 0;
	  while (name_list[i])
	    if (strncmp (name_list[i++], hint, hint_len) == 0)
	      matches++;
	}
    }

  if (name_list && matches)
    {
      const char *name;

      while ((name = name_list[list_index]) != 0)
	{
	  list_index++;
	  if (strncmp (name, hint, hint_len) == 0)
	    {
	      int len = 2 + prefix_len + strlen (name);
	      char *buf = (char *) malloc (len);

	      if (prefix)
		{
		  strcpy (buf, prefix);
		  strcat (buf, ".");
		  strcat (buf, name);
		}
	      else
		strcpy (buf, name);

	      if (matches == 1 && looks_like_struct (buf))
		rl_completion_append_character = '.';
	      else
		rl_completion_append_character
		  = Vcompletion_append_char;

	      return buf;
	    }
	}
    }

  return 0;
}

static char **
command_completer (char *text, int /* start */, int /* end */)
{
  char **matches = 0;
  matches = completion_matches (text, command_generator);
  return matches;
}

// The next two functions implement the equivalent of the K*rn shell
// C-o operate-and-get-next-history-line editing command.  Stolen from
// the GNU Bourne Again SHell.

// ??
static int saved_history_line_to_use = 0;

// ??
static Function *old_rl_startup_hook = 0;

static void
set_saved_history (void)
{
  HIST_ENTRY *h;

  if (saved_history_line_to_use)
    {
      if (history_set_pos (saved_history_line_to_use))
	{
	  h = current_history ();
	  if (h)
	    {
	      rl_insert_text (h->line);

	      // Get rid of any undo list created by the previous
	      // insert, so the line won't totally be erased when the
	      // edits are undone (they will be normally, because this
	      // is a history  line -- cf. readline.c: line 380 or
	      // so).

	      if (rl_undo_list)
		{
		  free_undo_list ();
		  rl_undo_list = 0;
		}
	    }
	}
    }
  saved_history_line_to_use = 0;
  rl_startup_hook = old_rl_startup_hook;
}

static void
operate_and_get_next (int /* count */, int /* c */)
{
  int where;

  // Accept the current line.

  rl_newline ();

  // Find the current line, and find the next line to use.

  where = where_history ();

  if ((history_is_stifled () && (history_length >= max_input_history))
      || (where >= history_length - 1))
    saved_history_line_to_use = where;
  else
    saved_history_line_to_use = where + 1;

  old_rl_startup_hook = rl_startup_hook;
  rl_startup_hook = (Function *) set_saved_history;
}

void
initialize_readline (void)
{
  // Set things up internally in case some function that uses readline
  // (currently Fclc(), maybe others) is called before readline().

  rl_initialize ();

  // Allow conditional parsing of the ~/.inputrc file

  rl_readline_name = "Octave";

  // Tell the completer that we want to try first.

  rl_attempted_completion_function = (CPPFunction *) command_completer;

  // Bind operate-and-get-next.

  rl_add_defun ("operate-and-get-next",
		(Function *) operate_and_get_next, CTRL ('O'));


  // And the history search functions.

  rl_add_defun ("history-search-backward",
		(Function *) rl_history_search_backward, META ('p'));

  rl_add_defun ("history-search-forward",
		(Function *) rl_history_search_forward, META ('n'));

  // Don't treat single quotes as string delimiters when doing paren
  // matching.

  rl_paren_string_delimiters = "\"";
}

static int
match_sans_spaces (const char *standard, const char *test)
{
  char *tmp = strsave (test);

  char *tp = tmp;
  while (*tp == ' ' || *tp == '\t')
    tp++;

  char *ep = tmp + strlen (tmp) - 1;
  while (*ep == ' ' || *ep == '\t')
    ep--;

  *(ep+1) = '\0';

  int retval = strcmp (standard, tp) == 0;

  delete [] tmp;

  return retval;

}

// If the user simply hits return, this will produce an empty matrix.

static octave_value_list
get_user_input (const octave_value_list& args, int debug = 0)
{
  octave_value retval;

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

  char *input_buf = gnu_readline (prompt.c_str ());

  if (input_buf)
    {
      if (! input_from_startup_file)
	octave_command_history.add (input_buf);

      int len = strlen (input_buf);

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

      if (match_sans_spaces ("exit", input_buf)
	  || match_sans_spaces ("quit", input_buf)
	  || match_sans_spaces ("return", input_buf))
	{
	  return retval;
	}
      else if (read_as_string)
	{
	  retval = input_buf;
	}
      else
	{
	  int parse_status = 0;
	  retval = eval_string (input_buf, 0, parse_status);
	  if (retval.is_defined ())
	    {
	      if (debug)
		retval.eval (1);
	    }
	  else
	    retval = Matrix ();
	}
    }
  else
    error ("input: reading user-input failed!");

  if (debug)
    goto again;

  return retval;
}

DEFUN (input, args, ,
  "input (PROMPT [, S])\n\
\n\
Prompt user for input.  If the second argument is present, return
value as a string.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    retval = get_user_input (args);
  else
    print_usage ("input");

  return retval;
}

DEFUN (keyboard, args, ,
  "keyboard (PROMPT)\n\
\n\
maybe help in debugging function files")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0 || nargin == 1)
    retval = get_user_input (args, 1);
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
	  bind_builtin_variable ("echo_executing_commands", ECHO_OFF);
	else
	  bind_builtin_variable ("echo_executing_commands", ECHO_SCRIPTS);
      }
      break;

    case 2:
      {
	string arg = argv[1];

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
	string arg = argv[1];

	if (arg == "on" && argv[2] == "all")
	  bind_builtin_variable ("echo_executing_commands",
				 (ECHO_SCRIPTS | ECHO_FUNCTIONS));
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

void
symbols_of_input (void)
{
  DEFVAR (PS1, "\\s:\\#> ", 0, ps1,
    "primary prompt string");

  DEFVAR (PS2, "> ", 0, ps2,
    "secondary prompt string");

  DEFVAR (PS4, "+ ", 0, ps4,
    "string printed before echoed input (enabled by --echo-input)");

  DEFVAR (completion_append_char, " ", 0, completion_append_char,
    "the string to append after successful command-line completion attempts");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
