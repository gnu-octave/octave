// input.cc                                             -*- C++ -*-
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

/*

The 3 functions listed below were adapted from similar functions
from GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991
Free Software Foundation, Inc.

  read_octal    sub_append_string    decode_prompt_string

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

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <iostream.h>
#include <strstream.h>

// This must come before anything that includes iostream.h...
extern "C"
{
#include "readline/readline.h"
#include "readline/history.h"

extern void free_undo_list ();

extern char *xmalloc ();

// Yes, this sucks, but it avoids a conflict with another readline
// function declared in iostream.h.

#if 0
#define LINE_SIZE 8192
static int no_line_editing = 0;
#endif

char *
gnu_readline (char *s)
{
#if 0
  static int state = 0;
  static char *line_from_stdin = 0;
  if (no_line_editing)
    {
      if (! state)
	{
	  line_from_stdin = (char *) malloc (LINE_SIZE);
	  state = 1;
	}
      fputs ("octave> ", stdout);
      fgets (line_from_stdin, LINE_SIZE, stdin);
      return line_from_stdin;
    }
  else
#endif
    return readline (s);
}
}

#include "help.h"
#include "error.h"
#include "utils.h"
#include "input.h"
#include "pager.h"
#include "parse.h"
#include "dirfns.h"
#include "octave.h"
#include "sysdep.h"
#include "variables.h"
#include "tree-const.h"
#include "octave-hist.h"
#include "sighandlers.h"
#include "user-prefs.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "symtab.h"
#include "defun.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

// The size that strings change by.
#ifndef DEFAULT_ARRAY_SIZE
#define DEFAULT_ARRAY_SIZE 512
#endif

// The growth rate for the prompt string.
#ifndef PROMPT_GROWTH
#define PROMPT_GROWTH 50
#endif

// Global pointer for eval().
const char *current_eval_string = 0;

// Nonzero means get input from current_eval_string.
int get_input_from_eval_string = 0;

// Nonzero means we're parsing a function file.
int reading_fcn_file = 0;

// Simple name of function file we are reading.
char *curr_fcn_file_name = 0;

// Nonzero means we're parsing a script file.
int reading_script_file = 0;

// If we are reading from an M-file, this is it.
FILE *ff_instream = 0;

// Nonzero means we are using readline.
int using_readline = 1;

// Nonzero means commands are echoed as they are executed.
// (--echo-commands; -x).
int echo_input = 0;

// Nonzero means this is an interactive shell.
int interactive = 0;

// Nonzero means the user forced this shell to be interactive (-i).
int forced_interactive = 0;

// Should we issue a prompt?
int promptflag = 1;

// The current line of input, from wherever.
char *current_input_line = 0;

// A line of input from readline.
static char *octave_gets_line = 0;

extern tree_constant eval_string (const char *string, int print,
				  int ans_assign, int& parse_status);

// Append SOURCE to TARGET at INDEX.  SIZE is the current amount of
// space allocated to TARGET.  SOURCE can be NULL, in which case
// nothing happens.  Gets rid of SOURCE by free ()ing it.  Returns
// TARGET in case the location has changed.

static char *
sub_append_string (char *source, char *target, int *index, int *size)
{
  if (source)
    {
      while ((int)strlen (source) >= (int)(*size - *index))
	{
	  char *tmp = new char [*size += DEFAULT_ARRAY_SIZE];
	  strcpy (tmp, target);
	  delete [] target;
	  target = tmp;
	}

      strcat (target, source);
      *index += strlen (source);

      delete [] source;
    }
  return target;
}

// Return the octal number parsed from STRING, or -1 to indicate that
// the string contained a bad number.

int
read_octal (const char *string)
{
  int result = 0;
  int digits = 0;

  while (*string && *string >= '0' && *string < '8')
    {
      digits++;
      result = (result * 8) + *string++ - '0';
    }

  if (! digits || result > 0777 || *string)
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

static char *
decode_prompt_string (const char *string)
{
  int result_size = PROMPT_GROWTH;
  int result_index = 0;
  char *result = new char [PROMPT_GROWTH];
  int c;
  char *temp = 0;

  result[0] = 0;
  while ((c = *string++))
    {
      if (c == '\\')
	{
	  c = *string;

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
	      {
		char octal_string[4];
		int n;

		strncpy (octal_string, string, 3);
		octal_string[3] = '\0';

		n = read_octal (octal_string);

		temp = strsave ("\\");
		if (n != -1)
		  {
		    string += 3;
		    temp[0] = n;
		  }

		c = 0;
		goto add_string;
	      }
	  
	    case 't':
	    case 'd':
	      /* Make the current time/date into a string. */
	      {
		time_t the_time = time (0);
		char *ttemp = ctime (&the_time);
		temp = strsave (ttemp);

		if (c == 't')
		  {
		    strcpy (temp, temp + 11);
		    temp[8] = '\0';
		  }
		else
		  temp[10] = '\0';

		goto add_string;
	      }

	    case 'n':
	      if (! no_line_editing)
		temp = strsave ("\r\n");
	      else
		temp = strsave ("\n");
	      goto add_string;

	    case 's':
	      {
		temp = base_pathname (prog_name);
		temp = strsave (temp);
		goto add_string;
	      }
	
	    case 'w':
	    case 'W':
	      {
		char t_string[MAXPATHLEN];
#define EFFICIENT
#ifdef EFFICIENT

// Use the value of PWD because it is much more effecient.

		temp = user_pref.pwd;

		if (! temp)
		  octave_getcwd (t_string, MAXPATHLEN);
		else
		  strcpy (t_string, temp);
#else
		octave_getcwd (t_string, MAXPATHLEN);
#endif	/* EFFICIENT */

		if (c == 'W')
		  {
		    char *dir = strrchr (t_string, '/');
		    if (dir && dir != t_string)
		      strcpy (t_string, dir + 1);
		    temp = strsave (t_string);
		  }
		else
		  temp = strsave (polite_directory_format (t_string));
		goto add_string;
	      }
      
	    case 'u':
	      {
		temp = strsave (user_name);

		goto add_string;
	      }

	    case 'h':
	      {
		char *t_string;

		temp = strsave (host_name);
		t_string = strchr (temp, '.');
		if (t_string);
		  *t_string = '\0';
		
		goto add_string;
	      }

	    case '#':
	      {
		char number_buffer[128];
		sprintf (number_buffer, "%d", current_command_number);
		temp = strsave (number_buffer);
		goto add_string;
	      }

	    case '!':
	      {
		char number_buffer[128];
		int num = current_history_number ();
		if (num > 0)
                  sprintf (number_buffer, "%d", num);
		else
		  strcpy (number_buffer, "!");
		temp = strsave (number_buffer);
		goto add_string;
	      }

	    case '$':
	      temp = strsave (geteuid () == 0 ? "#" : "$");
	      goto add_string;

	    case '\\':
	      temp = strsave ("\\");
	      goto add_string;

	    default:
	      temp = strsave ("\\ ");
	      temp[1] = c;

	    add_string:
	      if (c)
		string++;
	      result =
		(char *)sub_append_string (temp, result,
					   &result_index, &result_size);
	      temp = 0; // Free ()'ed in sub_append_string ().
	      result[result_index] = '\0';
	      break;
	    }
	}
      else
	{
	  while (3 + result_index > result_size)
	    {
	      char *tmp = new char [result_size += PROMPT_GROWTH];
	      strcpy (tmp, result);
	      delete [] result;
	      result = tmp;
	    }
	  result[result_index++] = c;
	  result[result_index] = '\0';
	}
    }

#if 0
// I don't really think that this is a good idea.  Do you?
  if (! find_variable ("NO_PROMPT_VARS"))
    {
      WORD_LIST *expand_string (), *list;
      char *string_list ();

      list = expand_string (result, 1);
      free (result);
      result = string_list (list);
      dispose_words (list);
    }
#endif

  return result;
}

static void
do_input_echo (const char *input_string)
{
  if (echo_input)
    {
      ostrstream buf;

      if (! forced_interactive)
	{
	  char *prefix = decode_prompt_string (user_pref.ps4);
	  buf << prefix;
	  delete [] prefix;
	}

      if (input_string)
	{
	  buf << input_string;
	  int len = strlen (input_string);
	  if (input_string[len-1] != '\n')
	    buf << "\n";
	}

      maybe_page_output (buf);
    }
}

// Use GNU readline to get an input line and store it in the history
// list.

static char *
octave_gets (void)
{
  if (octave_gets_line)
    {
      free (octave_gets_line);
      octave_gets_line = 0;
    }

  if (interactive || forced_interactive)
    {
      char *ps = (promptflag > 0) ? user_pref.ps1 : user_pref.ps2;
      char *prompt = decode_prompt_string (ps);

      if (interactive)
	{
	  pipe_handler_error_count = 0;
	  flush_output_to_pager ();
	}

      maybe_write_to_diary_file (prompt);

      octave_gets_line = gnu_readline (prompt);

      delete [] prompt;
    }
  else
    octave_gets_line = gnu_readline ("");

  current_input_line = octave_gets_line;

  if (octave_gets_line && *octave_gets_line)
    {
      maybe_save_history (octave_gets_line);

      maybe_write_to_diary_file (octave_gets_line);

      do_input_echo (octave_gets_line);
    }

  maybe_write_to_diary_file ("\n");
  
  return octave_gets_line;
}

// Read a line from the input stream.

int
octave_read (char *buf, int max_size)
{
  int status = 0;

  static char *stashed_line = 0;

  if (get_input_from_eval_string)
    {
      int len = strlen (current_eval_string);
      if (len < max_size - 1)
	{
	  strcpy (buf, current_eval_string);
	  buf[len++] = '\n';
	  buf[len] = '\0';    // Paranoia.
	  status = len;
	}
      else
	status = -1;

      if (stashed_line)
	delete [] stashed_line;

      stashed_line = strsave (buf);
      current_input_line = stashed_line;
    }
  else if (using_readline)
    {
      char *cp = octave_gets ();
      if (cp)
	{
	  int len = strlen (cp);
	  if (len >= max_size)
	    status = -1;
	  else
	    {
	      strcpy (buf, cp);
	      buf[len++] = '\n';
	      buf[len] = '\0';    // Paranoia.
	      status = len;
	    }
	}
      current_input_line = cp;
    }
  else
    {
      FILE *curr_stream = rl_instream;
      if (reading_fcn_file || reading_script_file)
	curr_stream = ff_instream;

      assert (curr_stream);

// Why is this required?
      buf[0] = '\0';

      if (fgets (buf, max_size, curr_stream))
	{
	  int len = strlen (buf);
	  if (len > max_size - 2)
	    status = -1;
	  else
	    {
	      if (buf[len-1] != '\n')
		{
		  buf[len++] = '\n';
		  buf[len] = '\0';
		}
	      status = len;
	    }
	}
      else
	status = 0; // Tell yylex that we found EOF.

      if (stashed_line)
	delete [] stashed_line;

      stashed_line = strsave (buf);

      current_input_line = stashed_line;

      do_input_echo (current_input_line);
    }

  input_line_number++;

  return status;
}

// Fix things up so that input can come from file `name', printing a
// warning if the file doesn't exist.

FILE *
get_input_from_file (char *name, int warn)
{
  FILE *instream = 0;

  if (name && *name)
    instream = fopen (name, "r");

  if (! instream && warn)
    warning ("%s: no such file or directory", name);

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

static char **
generate_struct_completions (const char *text, char *& prefix,
			     char *& hint)
{
  char **names = 0;

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

      tree_constant *def = 0;
      if (tmp_fvc->is_constant ())
	def = (tree_constant *) tmp_fvc;

      if (def && def->is_map ())
	{
	  if (elts && *elts)
	    {
	      tree_constant ult = def->lookup_map_element (elts, 0, 1);

	      if (ult.is_map ())
		{
		  Octave_map m = ult.map_value ();
		  names = m.make_name_list ();
		}
	    }
	  else
	    {
	      Octave_map m = def->map_value ();
	      names = m.make_name_list ();
	    }
	}
    }

  delete [] id;

  return names;
}

static char **
generate_possible_completions (const char *text, char *& prefix,
			       char *& hint)
{
  char **names = 0;

  prefix = 0;

  if (text && *text && *text != '.')
    {
      if (strrchr (text, '.'))
	names = generate_struct_completions (text, prefix, hint);
      else
	names = make_name_list ();
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

      tree_constant *def = 0;
      if (tmp_fvc->is_constant ())
	def = (tree_constant *) tmp_fvc;

      if (def && def->is_map ())
	{
	  if (elts && *elts)
	    {
	      tree_constant ult = def->lookup_map_element (elts, 0, 1);

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
  static char **name_list = 0;

  static int matches = 0;

  if (state == 0)
    {
      list_index = 0;

      if (name_list)
	{
	  char **ptr = name_list;
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
      char *name;
      while ((name = name_list[list_index]) != 0)
	{
	  list_index++;
	  if (strncmp (name, hint, hint_len) == 0)
	    {
	      int len = 2 + prefix_len + strlen (name);
	      char *buf = (char *) xmalloc (len);

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
		rl_completion_append_character = ' ';

	      return buf;
	    }
	}
    }

  return 0;
}

static char **
command_completer (char *text, int start, int end)
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

// Get rid of any undo list created by the previous insert, so the
// line won't totally be erased when the edits are undone (they will
// be normally, because this is a history  line -- cf. readline.c:
// line 380 or so).
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
operate_and_get_next (int count, int c)
{
  int where;
  extern int history_stifled, history_length, max_input_history;

// Accept the current line.
  rl_newline ();

// Find the current line, and find the next line to use.
  where = where_history ();

  if (history_stifled && (history_length >= max_input_history))
    saved_history_line_to_use = where;
  else
    saved_history_line_to_use = where + 1;

  old_rl_startup_hook = rl_startup_hook;
  rl_startup_hook = (Function *) set_saved_history;
}

void
initialize_readline (void)
{
// Allow conditional parsing of the ~/.inputrc file
  rl_readline_name = "Octave";

// Tell the completer that we want to try first.
  rl_attempted_completion_function = (CPPFunction *) command_completer;

// Bind operate-and-get-next.
  rl_add_defun ("operate-and-get-next",
		(Function *) operate_and_get_next, CTRL ('O'));
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

static Octave_object
get_user_input (const Octave_object& args, int nargout, int debug = 0)
{
  tree_constant retval;

  int nargin = args.length ();

  int read_as_string = 0;

  if (nargin == 2)
    read_as_string++;

  char *prompt = "debug> ";
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

  flush_output_to_pager ();

  char *input_buf = gnu_readline (prompt);

  if (input_buf)
    {
      maybe_save_history (input_buf);

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
	  retval = eval_string (input_buf, 0, 0, parse_status);
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

DEFUN ("input", Finput, Sinput, 2, 1,
  "input (PROMPT [, S])\n\
\n\
Prompt user for input.  If the second argument is present, return
value as a string.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    retval = get_user_input (args, nargout);
  else
    print_usage ("input");

  return retval;
}

DEFUN ("keyboard", Fkeyboard, Skeyboard, 1, 1,
  "keyboard (PROMPT)\n\
\n\
maybe help in debugging function files")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 0 || nargin == 1)
    retval = get_user_input (args, nargout, 1);
  else
    print_usage ("keyboard");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
