// input.cc                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Use the GNU readline library for command line editing and hisory.

#ifdef __GNUG__
#pragma implementation
#endif

#include <stdio.h>
#include <stdlib.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>

// This must come before anything that includes iostream.h...
extern "C"
{
#include "readline/readline.h"

/*
 * Yes, this sucks, but it avoids a conflict with another readline
 * function declared in iostream.h.
 */
#if 0
#define LINE_SIZE 8192
static int no_line_editing = 1;
#endif

char *
gnu_readline (char *s)
{
#if 0
  static int state = 0;
  static char *line_from_stdin = (char *) NULL;
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

#include "variables.h"
#include "error.h"
#include "utils.h"
#include "input.h"
#include "pager.h"
#include "help.h"
#include "octave-hist.h"
#include "sighandlers.h"
#include "parse.h"
#include "user-prefs.h"
#include "builtins.h"

// Global pointer for eval().
const char *current_eval_string = (char *) NULL;

// Nonzero means get input from current_eval_string.
int get_input_from_eval_string = 0;

// Nonzero means we're parsing an M-file.
int reading_m_file = 0;

// Simple name of M-file we are reading.
char *curr_m_file_name = (char *) NULL;

// Nonzero means we're parsing a script file.
int reading_script_file = 0;

// If we are reading from an M-file, this is it.
FILE *mf_instream = (FILE *) NULL;

// Nonzero means we are using readline.
int using_readline = 1;

// Nonzero means commands are echoed as they are executed (-x).
int echo_input = 0;

// Nonzero means this is an interactive shell.
int interactive = 0;

// Nonzero means the user forced this shell to be interactive (-i).
int forced_interactive = 0;

// Should we issue a prompt?
int promptflag = 1;

// The current line of input, from wherever.
char *current_input_line = (char *) NULL;

// A line of input from readline.
static char *octave_gets_line = (char *) NULL;

/*
 * Use GNU readline to get an input line and store it in the history
 * list.
 */
char *
octave_gets (void)
{
  if (octave_gets_line != NULL)
    {
      free (octave_gets_line);
      octave_gets_line = (char *) NULL;
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

      octave_gets_line = gnu_readline (prompt);
      delete [] prompt;
    }
  else
    octave_gets_line = gnu_readline ("");

  current_input_line = octave_gets_line;

  if (octave_gets_line && *octave_gets_line)
    {
      maybe_save_history (octave_gets_line);

      if (echo_input)
	{
	  if (!forced_interactive)
	    cout << "+ ";
	  if (octave_gets_line != (char *) NULL)
	    cout << octave_gets_line << "\n";
	}
    }
  return octave_gets_line;
}

/*
 * Read a line from the input stream.
 */
int 
octave_read (char *buf, int max_size)
{
  int status = 0;

  static char *stashed_line = (char *) NULL;

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
      if (cp != (char *) NULL)
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
      if (reading_m_file || reading_script_file)
	curr_stream = mf_instream;

      assert (curr_stream != (FILE *) NULL);

// Why is this required?
      buf[0] = '\0';

      if (fgets (buf, max_size, curr_stream) != (char *) NULL)
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
    }
  input_line_number++;
  return status;
}

/*
 * Fix things up so that input can come from file `name', printing a
 * warning if the file doesn't exist.
 */
FILE *
get_input_from_file (char *name, int warn = 1)
{
  FILE *instream = (FILE *) NULL;

  if (name && *name)
    instream = fopen (name, "r");

  if (instream == (FILE *) NULL && warn)
    message (name, "no such file or directory");

  if (reading_m_file || reading_script_file)
    mf_instream = instream;
  else
    rl_instream = instream;

  return instream;
}

/*
 * Fix things up so that input can come from the standard input.  This
 * may need to become much more complicated, which is why it's in a
 * separate function.
 */
FILE *
get_input_from_stdin (void)
{
  rl_instream = stdin;
  return rl_instream;
}

char *
command_generator (char *text, int state)
{
  static int len = 0;
  static int list_index = 0;

  static char **name_list = (char **) NULL;

  if (state == 0)
    {
      list_index = 0;
      len = strlen (text);

      if (name_list != (char **) NULL)
	delete [] name_list;

      name_list = make_name_list ();
    }

  char *name;
  while ((name = name_list[list_index]) != (char *) NULL)
    {
      list_index++;
      if (strncmp (name, text, len) == 0)
	return name;
    }

  return (char *) NULL;
}

char **
command_completer (char *text, int start, int end)
{
  char **matches = (char **) NULL;
  matches = completion_matches (text, command_generator);
  return matches;
}

void
initialize_readline (void)
{
// Allow conditional parsing of the ~/.inputrc file
  rl_readline_name = "Octave";

// Tell the completer that we want to try first.
  rl_attempted_completion_function = (Function *) command_completer;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
