// octave-hist.cc                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

The functions listed below were adapted from similar functions from
GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

  do_history         edit_history_readline
  do_edit_history    edit_history_add_hist

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <fstream.h>
#include <strstream.h>

#include "statdefs.h"
#include "utils.h"
#include "error.h"
#include "input.h"
#include "pager.h"
#include "octave.h"
#include "oct-obj.h"
#include "user-prefs.h"
#include "unwind-prot.h"
#include "octave-hist.h"
#include "sighandlers.h"
#include "defun.h"

extern "C"
{
#include <readline/history.h>
}

// Nonzero means input is coming from temporary history file.
int input_from_tmp_history_file = 0;

// Nonzero means we are saving history lines.
int saving_history = 1;

// The number of lines to save in the history file.
static int octave_hist_size = 1024;

// The name of the history file.
static char *octave_hist_file;

// The number of hisory lines we read from the history file.
static int history_lines_in_file = 0;

// The number of history lines we've saved so far.
static int history_lines_this_session = 0;

// Get some default values, possibly reading them from the
// environment.

static int
default_history_size (void)
{
  int size = 1024;
  char *env_size = getenv ("OCTAVE_HISTSIZE");
  if (env_size)
    {
      int val;
      if (sscanf (env_size, "%d", &val) == 1)
	size = val > 0 ? val : 0;
    }
  return size;
}

static char *
default_history_file (void)
{
  char *file = 0;

  char *env_file = getenv ("OCTAVE_HISTFILE");
  if (env_file)
    {
      fstream f (env_file, (ios::in | ios::out));
      if (f)
	{
	  file = strsave (env_file);
	  f.close ();
	}
    }

  if (! file && home_directory)
    file = strconcat (home_directory, "/.octave_hist");

  return file;
}

// Prime the history list.

void
initialize_history (void)
{
  octave_hist_file = default_history_file ();
  octave_hist_size = default_history_size ();

  read_history (octave_hist_file);
  using_history ();
  history_lines_in_file = where_history ();
}

void
clean_up_history (void)
{
  stifle_history (octave_hist_size);
  write_history (octave_hist_file);
}

void
maybe_save_history (char *s)
{
  if (saving_history)
    {
      add_history (s);
      history_lines_this_session++;
    }
}

// Display, save, or load history.  Stolen and modified from bash.
//
// Arg of -w FILENAME means write file, arg of -r FILENAME
// means read file, arg of -q means don't number lines.  Arg of N
// means only display that many items. 

void
do_history (int argc, char **argv)
{
  HIST_ENTRY **hlist;

  int numbered_output = 1;

  while (--argc > 0)
    {
      argv++;

      if (*argv[0] == '-' && strlen (*argv) == 2
	  && ((*argv)[1] == 'r' || (*argv)[1] == 'w'
	      || (*argv)[1] == 'a' || (*argv)[1] == 'n'))
	{
	  char *file;
	  int result = 0;

	  if (argc > 1)
	    file = *(argv+1);
	  else
	    file = octave_hist_file;

	  switch ((*argv)[1])
	    {
	    case 'a':		// Append `new' lines to file.
	      {
		if (history_lines_this_session)
		  {
		    if (history_lines_this_session < where_history ())
		      {
// If the filename was supplied, then create it if it doesn't already
// exist.
			if (file)
			  {
			    struct stat buf;

			    if (stat (file, &buf) == -1)
			      {
				int tem;

				tem = open (file, O_CREAT, 0666);
				close (tem);
			      }
			  }

			result =
			  append_history (history_lines_this_session, file);
			history_lines_in_file += history_lines_this_session;
			history_lines_this_session = 0;
		      }
		  }
	      }
	      break;

	    case 'w':		// Write entire history.
	      result = write_history (file);
	      break;

	    case 'r':		// Read entire file.
	      result = read_history (file);
	      break;

	    case 'n':		// Read `new' history from file.
// Read all of the lines in the file that we haven't already read.
	      using_history ();
	      result = read_history_range (file, history_lines_in_file, -1);
	      using_history ();
	      history_lines_in_file = where_history ();
	      break;
	    }
	  return;
	}
      else if (strcmp (*argv, "-q") == 0)
	numbered_output = 0;
      else if (strcmp (*argv, "--") == 0)
	{
	  argc--;
	  argv++;
	  break;
	}
      else
	break;
    }

  int limited = 0;
  int limit = 0;

  if (argc > 0)
    {
      limited = 1;
      if (sscanf (*argv, "%d", &limit) != 1)
        {
	  if (*argv[0] == '-')
	    error ("history: unrecognized option `%s'", *argv);
	  else
	    error ("history: bad non-numeric arg `%s'", *argv);
	  return;
        }
    }

  hlist = history_list ();

  if (hlist)
    {
      for (int i = 0; hlist[i]; i++)
	; // Do nothing.

      if (limit < 0)
	limit = -limit;

      if (!limited)
	i = 0;
      else
	if ((i -= limit) < 0)
	  i = 0;

      ostrstream output_buf;

      while (hlist[i])
	{
//	  QUIT;  // in bash: (interrupt_state) throw_to_top_level ();

	  if (numbered_output)
	    output_buf.form ("%5d%c", i + history_base,
			     hlist[i]->data ? '*' : ' '); 
	  output_buf << hlist[i]->line << "\n";
	  i++;
	}

      output_buf << ends;
      maybe_page_output (output_buf);
    }
}

// Read the edited history lines from STREAM and return them
// one at a time.  This can read unlimited length lines.  The
//  caller should free the storage.

static char *
edit_history_readline (fstream& stream)
{
  char c;
  int line_len = 128;
  int lindex = 0;
  char *line = new char [line_len];
  line[0] = '\0';

  while (stream.get (c))
    {
      if (lindex + 2 >= line_len)
	{
	  char *tmp_line = new char [line_len += 128];
	  strcpy (tmp_line, line);
	  delete [] line;
	  line = tmp_line;
	}

      if (c == '\n')
	{
	  line[lindex++] = '\n';
	  line[lindex++] = '\0';
	  return line;
	}
      else
	line[lindex++] = c;
    }

  if (! lindex)
    {
      delete [] line;
      return 0;
    }

  if (lindex + 2 >= line_len)
    {
      char *tmp_line = new char [lindex+3];
      strcpy (tmp_line, line);
      delete [] line;
      line = tmp_line;
    }

// Finish with newline if none in file.

  line[lindex++] = '\n';
  line[lindex++] = '\0';
  return line;
}

extern "C"
{
  HIST_ENTRY *history_get ();
}

// Use `command' to replace the last entry in the history list, which,
// by this time, is `run_history blah...'.  The intent is that the
// new command become the history entry, and that `fc' should never
// appear in the history list.  This way you can do `run_history' to
// your heart's content.

static void
edit_history_repl_hist (char *command)
{
  if (! command || ! *command)
    return;

  HIST_ENTRY **hlist = history_list ();

  if (! hlist)
    return;

  for (int i = 0; hlist[i]; i++)
    ; // Count 'em.
  i--;

// History_get () takes a parameter that should be offset by history_base.

// Don't free this.
  HIST_ENTRY *histent = history_get (history_base + i);
  if (! histent)
    return;

  char *data = 0;
  if (histent->data)
    {
      int len = strlen (histent->data);
      data = (char *) malloc (len);
      strcpy (data, histent->data);
    }

  int n = strlen (command);

  if (command[n - 1] == '\n')
    command[n - 1] = '\0';

  if (command && *command)
    {
      HIST_ENTRY *discard = replace_history_entry (i, command, data);
      if (discard)
	{
	  if (discard->line)
	    free (discard->line);

	  free ((char *) discard);
	}
    }
}

static void
edit_history_add_hist (char *line)
{
  if (line)
    {
      int len = strlen (line);
      if (len > 0 && line[len-1] == '\n')
	line[len-1] = '\0';

      if (line[0] != '\0')
	add_history (line);
    }
}

#define histline(i) (hlist[(i)]->line)

static char *
mk_tmp_hist_file (int argc, char **argv, int insert_curr, char *warn_for)
{
  HIST_ENTRY **hlist;

  hlist = history_list ();

  int hist_count = 0;

  while (hlist[hist_count++])
    ; // Find the number of items in the history list.

// The current command line is already part of the history list by the
// time we get to this point.  Delete it from the list.

  hist_count -= 2;
  if (! insert_curr)
    remove_history (hist_count);
  hist_count--;

// If no numbers have been specified, the default is to edit the last
// command in the history list.

  int hist_end = hist_count;
  int hist_beg = hist_count;
  int reverse = 0;

// Process options

  int usage_error = 0;
  if (argc == 3)
    {
      argv++;
      if (sscanf (*argv++, "%d", &hist_beg) != 1
	  || sscanf (*argv, "%d", &hist_end) != 1)
	usage_error = 1;
      else
	{
	  hist_beg--;
	  hist_end--;
	}
    }
  else if (argc == 2)
    {
      argv++;
      if (sscanf (*argv++, "%d", &hist_beg) != 1)
	usage_error = 1;
      else
	{
	  hist_beg--;
	  hist_end = hist_beg;
	}
    }

  if (hist_beg < 0 || hist_end < 0 || hist_beg > hist_count
      || hist_end > hist_count)
    {
      error ("%s: history specification out of range", warn_for);
      return 0;
    }

  if (usage_error)
    {
      usage ("%s [first] [last]", warn_for);
      return 0;
    }

  if (hist_end < hist_beg)
    {
      int t = hist_end;
      hist_end = hist_beg;
      hist_beg = t;
      reverse = 1;
    }

  char *name = octave_tmp_file_name ();

  fstream file (name, ios::out);

  if (! file)
    {
      error ("%s: couldn't open temporary file `%s'", warn_for, name);
      return 0;
    }

  if (reverse)
    {
      for (int i = hist_end; i >= hist_beg; i--)
	file << histline (i) << "\n";
    }
  else
    {
      for (int i = hist_beg; i <= hist_end; i++)
	file << histline (i) << "\n";
    }

  file.close ();

  return strsave (name);
}

void
do_edit_history (int argc, char **argv)
{
  char *name = mk_tmp_hist_file (argc, argv, 0, "edit_history");

  if (! name)
    return;

// Call up our favorite editor on the file of commands.

  ostrstream buf;
  buf << user_pref.editor << " " << name << ends;
  char *cmd = buf.str ();

// Ignore interrupts while we are off editing commands.  Should we
// maybe avoid using system()?

  volatile sig_handler *saved_sigint_handler = signal (SIGINT, SIG_IGN);
  system (cmd);
  signal (SIGINT, saved_sigint_handler);

// Write the commands to the history file since parse_and_execute
// disables command line history while it executes.

  fstream file (name, ios::in);

  char *line;
  int first = 1;
  while ((line = edit_history_readline (file)) != 0)
    {

// Skip blank lines

      if (line[0] == '\n')
	{
	  delete [] line;
	  continue;
	}

      if (first)
	{
	  first = 0;
	  edit_history_repl_hist (line);
	}
      else
	edit_history_add_hist (line);
    }

  file.close ();

// Turn on command echo, so the output from this will make better sense.

  begin_unwind_frame ("do_edit_history");
  unwind_protect_int (echo_input);
  unwind_protect_int (input_from_tmp_history_file);
  echo_input = 1;
  input_from_tmp_history_file = 1;

  parse_and_execute (name, 1);

  run_unwind_frame ("do_edit_history");

// Delete the temporary file.  Should probably be done with an
// unwind_protect.

  unlink (name);

  delete [] name;
}

void
do_run_history (int argc, char **argv)
{
  char *name = mk_tmp_hist_file (argc, argv, 1, "run_history");

  if (! name)
    return;

// Turn on command echo, so the output from this will make better sense.

  begin_unwind_frame ("do_run_history");
  unwind_protect_int (echo_input);
  unwind_protect_int (input_from_tmp_history_file);
  echo_input = 1;
  input_from_tmp_history_file = 1;

  parse_and_execute (name, 1);

  run_unwind_frame ("do_run_history");

// Delete the temporary file.  Should probably be done with an
// unwind_protect.

  unlink (name);

  delete [] name;
}

int
current_history_number (void)
{
  using_history ();

  if (octave_hist_size > 0)
    return history_base + where_history ();
  else
    return -1;

}

DEFUN_TEXT ("edit_history", Fedit_history, Sedit_history, -1, 1,
  "edit_history [first] [last]\n\
\n\
edit commands from the history list")
{
  Octave_object retval;

  DEFINE_ARGV("edit_history");

  do_edit_history (argc, argv);

  DELETE_ARGV;

  return retval;
}

DEFUN_TEXT ("history", Fhistory, Shistory, -1, 1,
  "history [N] [-w file] [-r file] [-q]\n\
\n\
display, save, or load command history")
{
  Octave_object retval;

  DEFINE_ARGV("history");

  do_history (argc, argv);

  DELETE_ARGV;

  return retval;
}

DEFUN_TEXT ("run_history", Frun_history, Srun_history, -1, 1,
  "run_history [first] [last]\n\
\n\
run commands from the history list")
{
  Octave_object retval;

  DEFINE_ARGV("run_history");

  do_run_history (argc, argv);

  DELETE_ARGV;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
