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

The functions listed below were adapted from similar functions from
GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

  do_history         edit_history_readline
  do_edit_history    edit_history_add_hist

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <csignal>
#include <cstdlib>
#include <cstring>

#include <string>

#include <fstream.h>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-hist.h"
#include "file-ops.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "error.h"
#include "input.h"
#include "oct-hist.h"
#include "oct-obj.h"
#include "pager.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Nonzero means input is coming from temporary history file.
int input_from_tmp_history_file = 0;

// Guess what?
command_history octave_command_history;

// Get some default values, possibly reading them from the
// environment.

int
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

string
default_history_file (void)
{
  string file;

  char *env_file = getenv ("OCTAVE_HISTFILE");

  if (env_file)
    {
      fstream f (env_file, (ios::in | ios::out));

      if (f)
	{
	  file = env_file;
	  f.close ();
	}
    }

  if (file.empty ())
    {
      if (! Vhome_directory.empty ())
	{
	  file = Vhome_directory;
	  file.append ("/.octave_hist");
	}
      else
	file = ".octave_hist";
    }

  return file;
}

// Display, save, or load history.  Stolen and modified from bash.
//
// Arg of -w FILENAME means write file, arg of -r FILENAME
// means read file, arg of -q means don't number lines.  Arg of N
// means only display that many items. 

static void
do_history (int argc, const string_vector& argv)
{
  int numbered_output = 1;

  int i;
  for (i = 1; i < argc; i++)
    {
      string option = argv[i];

      if (option == "-r" || option == "-w" || option == "-a"
	  || option == "-n")
	{
	  if (i < argc - 1)
	    {
	      string file = oct_tilde_expand (argv[i+1]);
	      octave_command_history.set_file (file);
	    }

	  if (option == "-a")
	    // Append `new' lines to file.
	    octave_command_history.append ();

	  else if (option == "-w")
	    // Write entire history.
	    octave_command_history.write ();

	  else if (option == "-r")
	    // Read entire file.
	    octave_command_history.read ();

	  else if (option == "-n")
	    // Read `new' history from file.
	    octave_command_history.read_range ();

	  else
	    panic_impossible ();

	  return;
	}
      else if (argv[i] == "-q")
	numbered_output = 0;
      else if (argv[i] == "--")
	{
	  i++;
	  break;
	}
      else
	break;
    }

  int limit = -1;

  if (i < argc)
    {
      if (sscanf (argv[i].c_str (), "%d", &limit) != 1)
        {
	  if (argv[i][0] == '-')
	    error ("history: unrecognized option `%s'", argv[i].c_str ());
	  else
	    error ("history: bad non-numeric arg `%s'", argv[i].c_str ());

	  return;
        }

      if (limit < 0)
	limit = -limit;
    }

  string_vector hlist = octave_command_history.list (limit, numbered_output);

  int len = hlist.length ();

  for (i = 0; i < len; i++)
    octave_stdout << hlist[i] << "\n";
}

// Read the edited history lines from STREAM and return them
// one at a time.  This can read unlimited length lines.  The
// caller should free the storage.

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

// Use `command' to replace the last entry in the history list, which,
// by this time, is `run_history blah...'.  The intent is that the
// new command becomes the history entry, and that `fc' should never
// appear in the history list.  This way you can do `run_history' to
// your heart's content.

static void
edit_history_repl_hist (const string& command)
{
  if (! command.empty ())
    {
      string_vector hlist = octave_command_history.list ();

      int len = hlist.length ();

      if (len > 0)
	{
	  int i = len - 1;

	  string histent = octave_command_history.get_entry (i);

	  if (! histent.empty ())
	    {
	      string cmd = command;

	      int cmd_len = cmd.length ();

	      if (cmd[cmd_len - 1] == '\n')
		cmd.resize (cmd_len - 1);

	      if (! cmd.empty ())
		octave_command_history.replace_entry (i, cmd);
	    }
	}
    }
}

static void
edit_history_add_hist (const string& line)
{
  if (! line.empty ())
    {
      string tmp = line;

      int len = tmp.length ();
	
      if (len > 0 && tmp[len-1] == '\n')
	tmp.resize (len - 1);

      if (! tmp.empty ())
	octave_command_history.add (tmp);
    }
}

static string
mk_tmp_hist_file (int argc, const string_vector& argv,
		  int insert_curr, char *warn_for) 
{
  string retval;

  string_vector hlist = octave_command_history.list ();

  int hist_count = hlist.length ();

  // The current command line is already part of the history list by
  // the time we get to this point.  Delete it from the list.

  hist_count -= 2;

  if (! insert_curr)
    octave_command_history.remove (hist_count);

  hist_count--;

  // If no numbers have been specified, the default is to edit the
  // last command in the history list.

  int hist_end = hist_count;
  int hist_beg = hist_count;
  int reverse = 0;

  // Process options.

  int usage_error = 0;
  if (argc == 3)
    {
      if (sscanf (argv[1].c_str (), "%d", &hist_beg) != 1
	  || sscanf (argv[2].c_str (), "%d", &hist_end) != 1)
	usage_error = 1;
      else
	{
	  hist_beg--;
	  hist_end--;
	}
    }
  else if (argc == 2)
    {
      if (sscanf (argv[1].c_str (), "%d", &hist_beg) != 1)
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
      return retval;
    }

  if (usage_error)
    {
      usage ("%s [first] [last]", warn_for);
      return retval;
    }

  if (hist_end < hist_beg)
    {
      int t = hist_end;
      hist_end = hist_beg;
      hist_beg = t;
      reverse = 1;
    }

  string name = oct_tempnam ();

  fstream file (name.c_str (), ios::out);

  if (! file)
    {
      error ("%s: couldn't open temporary file `%s'", warn_for,
	     name.c_str ());
      return retval;
    }

  if (reverse)
    {
      for (int i = hist_end; i >= hist_beg; i--)
	file << hlist[i] << "\n";
    }
  else
    {
      for (int i = hist_beg; i <= hist_end; i++)
	file << hlist[i] << "\n";
    }

  file.close ();

  return name;
}

static void
do_edit_history (int argc, const string_vector& argv)
{
  string name = mk_tmp_hist_file (argc, argv, 0, "edit_history");

  if (name.empty ())
    return;

  // Call up our favorite editor on the file of commands.

  string cmd = Veditor;
  cmd.append (" ");
  cmd.append (name);

  // Ignore interrupts while we are off editing commands.  Should we
  // maybe avoid using system()?

  volatile octave_interrupt_handler *old_interrupt_handler
    = octave_ignore_interrupts ();

  system (cmd.c_str ());

  octave_set_interrupt_handler (old_interrupt_handler);

  // Write the commands to the history file since parse_and_execute
  // disables command line history while it executes.

  fstream file (name.c_str (), ios::in);

  char *line;
  int first = 1;
  while ((line = edit_history_readline (file)) != 0)
    {
      // Skip blank lines.

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

  // Turn on command echo, so the output from this will make better
  // sense.

  begin_unwind_frame ("do_edit_history");
  unwind_protect_int (Vecho_executing_commands);
  unwind_protect_int (input_from_tmp_history_file);
  Vecho_executing_commands = ECHO_CMD_LINE;
  input_from_tmp_history_file = 1;

  parse_and_execute (name, 1);

  run_unwind_frame ("do_edit_history");

  // Delete the temporary file.  Should probably be done with an
  // unwind_protect.

  unlink (name.c_str ());
}

static void
do_run_history (int argc, const string_vector& argv)
{
  string name = mk_tmp_hist_file (argc, argv, 1, "run_history");

  if (name.empty ())
    return;

  // Turn on command echo so the output from this will make better
  // sense.

  begin_unwind_frame ("do_run_history");
  unwind_protect_int (Vecho_executing_commands);
  unwind_protect_int (input_from_tmp_history_file);
  Vecho_executing_commands = ECHO_CMD_LINE;
  input_from_tmp_history_file = 1;

  parse_and_execute (name, 1);

  run_unwind_frame ("do_run_history");

  // Delete the temporary file.

  // XXX FIXME XXX -- should probably be done using an unwind_protect.

  unlink (name.c_str ());
}

DEFUN_TEXT (edit_history, args, ,
  "edit_history [first] [last]\n\
\n\
edit commands from the history list")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("edit_history");

  if (error_state)
    return retval;

  do_edit_history (argc, argv);

  return retval;
}

DEFUN_TEXT (history, args, ,
  "history [N] [-w file] [-r file] [-q]\n\
\n\
display, save, or load command history")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("history");

  if (error_state)
    return retval;

  do_history (argc, argv);

  return retval;
}

DEFUN_TEXT (run_history, args, ,
  "run_history [first] [last]\n\
\n\
run commands from the history list")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("run_history");

  if (error_state)
    return retval;

  do_run_history (argc, argv);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
