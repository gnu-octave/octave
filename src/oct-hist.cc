/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2003,
              2005, 2006, 2007, 2008, 2009 John W. Eaton

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

/*

The functions listed below were adapted from similar functions from
GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

  do_history         edit_history_readline
  do_edit_history    edit_history_add_hist

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cstring>

#include <string>

#include <fstream>

#include <sys/types.h>
#include <unistd.h>

#include "cmd-hist.h"
#include "file-ops.h"
#include "lo-mappers.h"
#include "oct-env.h"
#include "oct-time.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-hist.h"
#include "oct-obj.h"
#include "pager.h"
#include "parse.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// TRUE means input is coming from temporary history file.
bool input_from_tmp_history_file = false;

static std::string
default_history_file (void)
{
  std::string file;

  std::string env_file = octave_env::getenv ("OCTAVE_HISTFILE");

  if (! env_file.empty ())
    file = env_file;

  if (file.empty ())
    file = file_ops::concat (octave_env::get_home_directory (),
                             ".octave_hist");

  return file;
}

// Where history is saved.
static std::string Vhistory_file = default_history_file ();

static int
default_history_size (void)
{
  int size = 1024;

  std::string env_size = octave_env::getenv ("OCTAVE_HISTSIZE");

  if (! env_size.empty ())
    {
      int val;

      if (sscanf (env_size.c_str (), "%d", &val) == 1)
        size = val > 0 ? val : 0;
    }

  return size;
}

// The number of lines to keep in the history file.
static int Vhistory_size = default_history_size ();

static std::string
default_history_timestamp_format (void)
{
  return
    std::string ("# Octave " OCTAVE_VERSION ", %a %b %d %H:%M:%S %Y %Z <")
    + octave_env::get_user_name ()
    + std::string ("@")
    + octave_env::get_host_name ()
    + std::string (">");
}

// The format of the timestamp marker written to the history file when
// Octave exits.
static std::string Vhistory_timestamp_format_string
  = default_history_timestamp_format ();

// TRUE if we are saving history.
bool Vsaving_history = true;

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
      std::string option = argv[i];

      if (option == "-r" || option == "-w" || option == "-a"
          || option == "-n")
        {
          if (i < argc - 1)
            command_history::set_file (argv[i+1]);

          if (option == "-a")
            // Append `new' lines to file.
            command_history::append ();

          else if (option == "-w")
            // Write entire history.
            command_history::write ();

          else if (option == "-r")
            // Read entire file.
            command_history::read ();

          else if (option == "-n")
            // Read `new' history from file.
            command_history::read_range ();

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

  string_vector hlist = command_history::list (limit, numbered_output);

  int len = hlist.length ();

  for (i = 0; i < len; i++)
    octave_stdout << hlist[i] << "\n";
}

// Read the edited history lines from STREAM and return them
// one at a time.  This can read unlimited length lines.  The
// caller should free the storage.

static char *
edit_history_readline (std::fstream& stream)
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
edit_history_repl_hist (const std::string& command)
{
  if (! command.empty ())
    {
      string_vector hlist = command_history::list ();

      int len = hlist.length ();

      if (len > 0)
        {
          int i = len - 1;

          std::string histent = command_history::get_entry (i);

          if (! histent.empty ())
            {
              std::string cmd = command;

              int cmd_len = cmd.length ();

              if (cmd[cmd_len - 1] == '\n')
                cmd.resize (cmd_len - 1);

              if (! cmd.empty ())
                command_history::replace_entry (i, cmd);
            }
        }
    }
}

static void
edit_history_add_hist (const std::string& line)
{
  if (! line.empty ())
    {
      std::string tmp = line;

      int len = tmp.length ();
        
      if (len > 0 && tmp[len-1] == '\n')
        tmp.resize (len - 1);

      if (! tmp.empty ())
        command_history::add (tmp);
    }
}

static std::string
mk_tmp_hist_file (int argc, const string_vector& argv,
                  int insert_curr, const char *warn_for) 
{
  std::string retval;

  string_vector hlist = command_history::list ();

  int hist_count = hlist.length ();

  // The current command line is already part of the history list by
  // the time we get to this point.  Delete it from the list.

  hist_count -= 2;

  if (! insert_curr)
    command_history::remove (hist_count);

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

  std::string name = octave_tempnam ("", "oct-");

  std::fstream file (name.c_str (), std::ios::out);

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
unlink_cleanup (const char *file)
{
  gnulib::unlink (file);
}

static void
do_edit_history (int argc, const string_vector& argv)
{
  std::string name = mk_tmp_hist_file (argc, argv, 0, "edit_history");

  if (name.empty ())
    return;

  // Call up our favorite editor on the file of commands.

  std::string cmd = VEDITOR;
  cmd.append (" \"");
  cmd.append (name);
  cmd.append ("\"");

  // Ignore interrupts while we are off editing commands.  Should we
  // maybe avoid using system()?

  volatile octave_interrupt_handler old_interrupt_handler
    = octave_ignore_interrupts ();

  system (cmd.c_str ());

  octave_set_interrupt_handler (old_interrupt_handler);

  // Write the commands to the history file since source_file
  // disables command line history while it executes.

  std::fstream file (name.c_str (), std::ios::in);

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

  unwind_protect frame;

  frame.add_fcn (unlink_cleanup, name.c_str ());
  frame.protect_var (Vecho_executing_commands);
  frame.protect_var (input_from_tmp_history_file);

  Vecho_executing_commands = ECHO_CMD_LINE;
  input_from_tmp_history_file = true;

  source_file (name);
}

static void
do_run_history (int argc, const string_vector& argv)
{
  std::string name = mk_tmp_hist_file (argc, argv, 1, "run_history");

  if (name.empty ())
    return;

  // Turn on command echo so the output from this will make better
  // sense.

  unwind_protect frame;

  frame.add_fcn (unlink_cleanup, name.c_str ());
  frame.protect_var (Vecho_executing_commands);
  frame.protect_var (input_from_tmp_history_file);

  Vecho_executing_commands = ECHO_CMD_LINE;
  input_from_tmp_history_file = true;

  source_file (name);
}

void
initialize_history (bool read_history_file)
{
  command_history::initialize (read_history_file, Vhistory_file, Vhistory_size);
}

void
octave_history_write_timestamp (void)
{
  octave_localtime now;

  std::string timestamp = now.strftime (Vhistory_timestamp_format_string);

  if (! timestamp.empty ())
    command_history::add (timestamp);
}

DEFUN (edit_history, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} edit_history [@var{first}] [@var{last}]\n\
If invoked with no arguments, @code{edit_history} allows you to edit the\n\
history list using the editor named by the variable @w{@env{EDITOR}}.  The\n\
commands to be edited are first copied to a temporary file.  When you\n\
exit the editor, Octave executes the commands that remain in the file.\n\
It is often more convenient to use @code{edit_history} to define functions \n\
rather than attempting to enter them directly on the command line.\n\
By default, the block of commands is executed as soon as you exit the\n\
editor.  To avoid executing any commands, simply delete all the lines\n\
from the buffer before exiting the editor.\n\
\n\
The @code{edit_history} command takes two optional arguments specifying\n\
the history numbers of first and last commands to edit.  For example,\n\
the command\n\
\n\
@example\n\
edit_history 13\n\
@end example\n\
\n\
@noindent\n\
extracts all the commands from the 13th through the last in the history\n\
list.  The command\n\
\n\
@example\n\
edit_history 13 169\n\
@end example\n\
\n\
@noindent\n\
only extracts commands 13 through 169.  Specifying a larger number for\n\
the first command than the last command reverses the list of commands\n\
before placing them in the buffer to be edited.  If both arguments are\n\
omitted, the previous command in the history list is used.\n\
@seealso{run_history}\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("edit_history");

  if (error_state)
    return retval;

  do_edit_history (argc, argv);

  return retval;
}

DEFUN (history, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} history options\n\
If invoked with no arguments, @code{history} displays a list of commands\n\
that you have executed.  Valid options are:\n\
\n\
@table @code\n\
@item -w @var{file}\n\
Write the current history to the file @var{file}.  If the name is\n\
omitted, use the default history file (normally @file{~/.octave_hist}).\n\
\n\
@item -r @var{file}\n\
Read the file @var{file}, replacing the current history list with its\n\
contents.  If the name is omitted, use the default history file\n\
(normally @file{~/.octave_hist}).\n\
\n\
@item @var{n}\n\
Display only the most recent @var{n} lines of history.\n\
\n\
@item -q\n\
Don't number the displayed lines of history.  This is useful for cutting\n\
and pasting commands using the X Window System.\n\
@end table\n\
\n\
For example, to display the five most recent commands that you have\n\
typed without displaying line numbers, use the command\n\
@kbd{history -q 5}.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("history");

  if (error_state)
    return retval;

  do_history (argc, argv);

  return retval;
}

DEFUN (run_history, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} run_history [@var{first}] [@var{last}]\n\
Similar to @code{edit_history}, except that the editor is not invoked,\n\
and the commands are simply executed as they appear in the history list.\n\
@seealso{edit_history}\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("run_history");

  if (error_state)
    return retval;

  do_run_history (argc, argv);

  return retval;
}

DEFUN (history_size, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} history_size ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} history_size (@var{new_val})\n\
Query or set the internal variable that specifies how many entries\n\
to store in the history file.  The default value is @code{1024},\n\
but may be overridden by the environment variable @w{@env{OCTAVE_HISTSIZE}}.\n\
@seealso{history_file, history_timestamp_format_string, saving_history}\n\
@end deftypefn")
{
  int saved_history_size = Vhistory_size;

  octave_value retval
    = SET_INTERNAL_VARIABLE_WITH_LIMITS (history_size, -1, INT_MAX);

  if (Vhistory_size != saved_history_size)
    command_history::set_size (Vhistory_size);

  return retval;
}

DEFUN (history_file, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} history_file ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} history_file (@var{new_val})\n\
Query or set the internal variable that specifies the name of the\n\
file used to store command history.  The default value is\n\
@file{~/.octave_hist}, but may be overridden by the environment\n\
variable @w{@env{OCTAVE_HISTFILE}}.\n\
@seealso{history_size, saving_history, history_timestamp_format_string}\n\
@end deftypefn")
{
  std::string saved_history_file = Vhistory_file;

  octave_value retval = SET_INTERNAL_VARIABLE (history_file);

  if (Vhistory_file != saved_history_file)
    command_history::set_file (Vhistory_file);

  return retval;
}

DEFUN (history_timestamp_format_string, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} history_timestamp_format_string ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} history_timestamp_format_string (@var{new_val})\n\
Query or set the internal variable that specifies the format string\n\
for the comment line that is written to the history file when Octave\n\
exits.  The format string is passed to @code{strftime}.  The default\n\
value is\n\
\n\
@example\n\
\"# Octave VERSION, %a %b %d %H:%M:%S %Y %Z <USER@@HOST>\"\n\
@end example\n\
@seealso{strftime, history_file, history_size, saving_history}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (history_timestamp_format_string);
}

DEFUN (saving_history, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} saving_history ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} saving_history (@var{new_val})\n\
Query or set the internal variable that controls whether commands entered\n\
on the command line are saved in the history file.\n\
@seealso{history_file, history_size, history_timestamp_format_string}\n\
@end deftypefn")
{
  octave_value retval = SET_INTERNAL_VARIABLE (saving_history);

  command_history::ignore_entries (! Vsaving_history);

  return retval;
}
